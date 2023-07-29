/**
 * Programming Language with LLVM
 *
 * Course info:
 * http://dmitrysoshnikov.com/courses/programming-language-with-llvm/
 *
 * (C) 2023-present Dmitry Soshnikov <dmitry.soshnikov@gmail.com>
 */

/**
 * Eva to LLVR IR compiler.
 */

#ifndef EvaLLVM_h
#define EvaLLVM_h

#include <iostream>
#include <map>
#include <regex>
#include <string>

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#include "./Environment.h"
#include "./Logger.h"
#include "./parser/EvaParser.h"

using syntax::EvaParser;

/**
 * Environment type.
 */
using Env = std::shared_ptr<Environment>;

/**
 * Class info. Contains struct type and field names.
 */
struct ClassInfo {
  llvm::StructType* cls;
  llvm::StructType* parent;
  std::map<std::string, llvm::Type*> fieldsMap;
  std::map<std::string, llvm::Function*> methodsMap;
};

/**
 * Index of the vTable in the class fields.
 */
static const size_t VTABLE_INDEX = 0;

/**
 * Each class has set of reserved fields at the
 * beginning of its layout. Currently it's only
 * the vTable used to resolve methods.
 */
static const size_t RESERVED_FIELDS_COUNT = 1;

// Generic binary operator:
#define GEN_BINARY_OP(Op, varName)         \
  do {                                     \
    auto op1 = gen(exp.list[1], env);      \
    auto op2 = gen(exp.list[2], env);      \
    return builder->Op(op1, op2, varName); \
  } while (false)

class EvaLLVM {
 public:
  EvaLLVM() : parser(std::make_unique<EvaParser>()) {
    moduleInit();
    setupExternFunctions();
    setupGlobalEnvironment();
    setupTargetTriple();
  }

  /**
   * Executes a program.
   */
  void exec(const std::string& program) {
    // 1. Parse the program
    auto ast = parser->parse("(begin " + program + ")");

    // 2. Compile to LLVM IR:
    compile(ast);

    // Print generated code.
    module->print(llvm::outs(), nullptr);

    std::cout << "\n";

    // 3. Save module IR to file:
    saveModuleToFile("./out.ll");
  }

 private:
  /**
   * Compiles an expression.
   */
  void compile(const Exp& ast) {
    // 1. Create main function:
    fn = createFunction(
        "main",
        llvm::FunctionType::get(/* return type */ builder->getInt32Ty(),
                                /* vararg */ false),
        GlobalEnv);

    createGlobalVar("VERSION", builder->getInt32(42));

    // 2. Compile main body:
    gen(ast, GlobalEnv);

    builder->CreateRet(builder->getInt32(0));
  }

  /**
   * Main compile loop.
   */
  llvm::Value* gen(const Exp& exp, Env env) {
    switch (exp.type) {
      /**
       * ----------------------------------------------
       * Numbers.
       */
      case ExpType::NUMBER:
          return builder->getInt32(exp.number);

      /**
       * ----------------------------------------------
       * Strings.
       */
      case ExpType::STRING: {
          // Unescape special chars, TODO: support all chars or handle in parser
          auto re = std::regex("\\\\n");
          auto str = std::regex_replace(exp.string, re, "\n");
          return builder->CreateGlobalStringPtr(str);
      }

      /**
       * ----------------------------------------------
       * Symbols (variables, operators).
       */
      case ExpType::SYMBOL:
        /**
         * Boolean.
         */
        if (exp.string == "true" || exp.string == "false") {
            return builder->getInt1(exp.string == "true" ? true : false);
        } else {
          // Variables and functions:
            auto varName = exp.string;
            auto value = env->lookup(varName);
          // 1. Local vars: (TODO)
            if (auto localVar = llvm::dyn_cast<llvm::AllocaInst>(value)) {
                return builder->CreateLoad(localVar->getAllocatedType(), localVar, varName.c_str());
            }
          // 2. Global vars:
            else if (auto globalVar = llvm::dyn_cast<llvm::GlobalVariable>(value)) {
                return builder->CreateLoad(globalVar->getInitializer()->getType(),
                    globalVar, varName.c_str());
            }
            // 3. Functions
            else {
                return value;
            }
        }

      /**
       * ----------------------------------------------
       * Lists.
       */
      case ExpType::LIST:
        auto tag = exp.list[0];

        /**
         * ----------------------------------------------
         * Special cases.
         */
        if (tag.type == ExpType::SYMBOL) {
          auto op = tag.string;

          // --------------------------------------------
          // Binary math operations:

          if (op == "+") {
              GEN_BINARY_OP(CreateAdd, "tmpadd");
          }

          else if (op == "-") {
              GEN_BINARY_OP(CreateSub, "tmpsub");
          }

          else if (op == "*") {
              GEN_BINARY_OP(CreateMul, "tmpmul");
          }

          else if (op == "/") {
              GEN_BINARY_OP(CreateDiv, "tmpdiv");
          }

          // --------------------------------------------
          // Compare operations: (> 5 10)

          // UGT - unsigned, greater than
          else if (op == ">") {
              GEN_BINARY_OP(CreateICmpUGT, "tmpcmp");
          }

          // ULT - unsigned, less than
          else if (op == "<") {
              GEN_BINARY_OP(CreateICmpULT, "tmpcmp");
          }

          // EQ - equal
          else if (op == "==") {
              GEN_BINARY_OP(CreateICmpEQ, "tmpcmp");
          }

          // NE - not equal
          else if (op == "!=") {
              GEN_BINARY_OP(CreateICmpNE, "tmpcmp");
          }

          // UGE - greater or equal
          else if (op == ">=") {
              GEN_BINARY_OP(CreateICmpUGE, "tmpcmp");
          }

          // ULE - less or equal
          else if (op == "<=") {
              GEN_BINARY_OP(CreateICmpULE, "tmpcmp");
          }

          // --------------------------------------------
          // Branch instruction:

          /**
           * (if <cond> <then> <else>)
           */
          else if (op == "if") {
            // Compile <cond>:
            auto cond = gen(exp.list[1], env);

            // Then block - appended right away
            auto thenBlock = createBB("then", fn);

            // else, if-end blocks - appended later
            // to handle nested if-expressions
            auto elseBlock = createBB("else");
            auto ifEndBlock = createBB("ifend");

            // Condition branch
            builder->CreateCondBr(cond, thenBlock, elseBlock);

            // Then branch
            builder->SetInsertPoint(thenBlock);
            auto thenRes = gen(exp.list[2], env);
            builder->CreateBr(ifEndBlock);

            // Restore the block to handle nested if-expressions
            // This is needed for phi instruction
            thenBlock = builder->GetInsertBlock();

            // Else branch
            // Append the block to the function now
            fn->getBasicBlocklist().push_back(elseBlock);
            builder->SetInsertPoint(elseBlock);
            auto elseRes = gen(exp.list[3], env);
            builder->CreateBr(ifEndBlock);

            // Restore the block for phi instruction
            elseBlock = builder->GetInsertBlock();

            // If-end block
            fn->getBasicBlockList().push_back(ifEndBlock);
            builder->SetInsertPoint(ifEndBlock);

            // Result of the if expression is phi
            auto phi = builder->CreatePHI(thenRes->getType(), 2, "tmpif");
            phi->addIncoming(thenRes, thenBlock);
            phi->addIncoming(elseRes, elseBlock);
            return phi;
          }

          // --------------------------------------------
          // While loop:

          /**
           * (while <cond> <body>)
           *
           */
          else if (op == "while") {
            // Condition
            auto condBlock = createBB("cond", fn);
            builder->CreateBr(condBlock);

            // Body, while-end blocks
            auto bodyBlock = createBB("body");
            auto loopEndBlock = createBB("loopend");

            // Compile <cond>
            builder->SetInsertPoint(condBlock);
            auto cond = gen(exp.list[1], env);

            // Condition branch
            builder->CreateCondBr(cond, bodyBlock, loopEndBlock);

            // Body
            fn->getBasicBlockList().push_back(bodyBlock);
            builder->SetInsertPoint(bodyBlock);
            gen(exp.list[2], env);
            builder->CreateBr(condBlock);

            fn->getBasicBlockList().push_back(loopEndBlock);
            builder->SetInsertPoint(loopEndBlock);

            return builder->getInt32(0);
          }

          // --------------------------------------------
          // Function declaration: (def <name> <params> <body>)
          //

          else if (op == "def") {
            return compileFunction(exp, /* name */ exp.list[1].string, env);
          }

          // --------------------------------------------
          // Variable declaration: (var x (+ y 10))
          //
          // Typed: (var (x number) 42)
          //
          // Note: locals are allocated on the stack.

          if (op == "var") {
              // Special case for class fields, which are already
              // defined during class info allocation
              if (cls != nullptr) {
                  return builder->getInt32(0);
              }

              auto varNameDecl = exp.list[1];
              auto varName = extractVarName(varNameDecl);

              //Special case for new as it allocates a variable
              if (isNew(exp.list[2])) {
                  auto instance = createInstance(exp.list[2], env, varName);
                  return env->define(varName, instance);
              }
              // Initializer
              auto init = gen(exp.list[2], env);
              // Type
              auto varTy = extractVarType(varNameDecl);
              // Variable
              auto varBinding = allocVar(varName, varTy, env);
              // Set value
              return builder->CreateStore(init, varBinding);
          }

          // --------------------------------------------
          // Variable update: (set x 100)
          // Property update (set (prop self x) 100)

          else if (op == "set") {
              // Value
              auto value = gen(exp.list[2], env);

              // 1. Properties
              // Special case for property writes
              if (isProp(exp.list[1])) {
                  auto instance = gen(exp.list[1].list[1], env);
                  auto fieldName = exp.list[1].list[2].string;
                  auto ptrName = std::string("p") + fieldName;
                  auto cls = (llvm::StructType*)(instance->getType()->getContainedType(0));
                  auto fieldIdx = getFieldIndex(cls, fieldName);
                  auto address = builder->CreateStructGEP(cls, instance, fieldIdx, ptrName);
                  builder->CreateStore(value, address);
                  return value;
              }
              // 2. Variables
              else {
                  auto varName = exp.list[1].string;
                  // Variable
                  auto varBinding = env->lookup(varName);
                  // Set value
                  builder->CreateStore(value, varBinding);
                  return value;
              }
          }

          // --------------------------------------------
          // Blocks: (begin <expressions>)

          else if (op == "begin") {
            // Block scope:
            auto blockEnv = std::make_shared<Environment>(
                std::map<std::string, llvm::Value*>{}, env);

            // Compile each expression within the block.
            // Result is the last evaluated expression.
            llvm::Value* blockRes;

            for (auto i = 1; i < exp.list.size(); i++) {
              // Generate expression code.
              blockRes = gen(exp.list[i], blockEnv);
            }

            return blockRes;
          }

          // --------------------------------------------
          // printf extern function:
          //
          // (printf "Value: %d" 42)
          //

          else if (op == "printf") {
              auto printfFn = module->getFunction("printf");
              std::vector<llvm::Value*> args{};
              for (auto i = 1; i < exp.list.size(); i++) {
                  args.push_back(gen(exp.list[i], env));
              }
              return builder->CreateCall(printfFn, args);
          }

          // --------------------------------------------
          // Class declaration:
          //
          // (class A <super> <body>)
          //

          else if (op == "class") {
              auto name = exp.list[1].string;
              auto parent = exp.list[2].string == "null" ? nullptr : getClassByName(exp.list[2].string);
              // Currently compiling class
              cls = llvm::StructType::create(*ctx, name);

              // Super class data always sit at the first element
              if (parent != nullptr) {
                  inheritClass(cls, parent);
              } else {
                  // Allocate a new class info
                  classMap_[name] = {/* class */ cls,
                                     /* parent */ parent,
                                     /* fields */ {},
                                     /* methods */ {}};
              }
              // Populate the class info with fields and methods
              buildClassInfo(cls, exp, env);
              // Compile the body
              gen(exp.list[3], env);
              // Reset the class after compiling, so normal function
              // don't pick the class name prefix
              cls = nullptr;
              return builder->getInt32(0);
          }

          // --------------------------------------------
          // New operator:
          //
          // (new <class> <args>)
          //

          else if (op == "new") {
            return createInstance(exp, env, "");
          }

          // --------------------------------------------
          // Prop access:
          //
          // (prop <instance> <name>)
          //

          else if (op == "prop") {
            // Instance
            auto instance = gen(exp.list[1], env);
            auto fieldName = exp.list[2].string;
            auto ptrName = std::string("p") + fieldName;
            auto cls = (llvm::Structure*)(instance->getType()->getContainedType(0));
            auto fieldIdx = getFieldIndex(cls, fieldName);
            auto address = builder->CreateStructGEP(cls, instance, fieldIdx, ptrName);
            return builder->CreateLoad(cls->getElementType(fieldIdx), address, fieldName);
          }

          // --------------------------------------------
          // Method access:
          //
          // (method <instance> <name>)
          //
          // (method (super <class>) <name>)
          //

          else if (op == "method") {
              auto methodName = exp.list[2].string;

              llvm::StructType* cls;
              llvm::Value* vTable;
              llvm::StructType* vTableTy;

              // (method (super <Class>) <name>)
              if (isSuper(exp.list[1])) {
                  auto className = exp.list[1].list[1].string;
                  cls = classMap_[className].parent;
                  auto parentName = std::string{ cls->getName().data() };
                  vTable = module->getNameGlobal(parentName + "_vTable");
                  vTableTy = llvm::StructType::getTypeByName(*ctx, parentName + "_vTable");
              }
              else {
                  // Instance
                  auto instance = gen(exp.list[1], env);
                  cls = (llvm::StructType*)(instance->getType()->getContainedType(0));
                  // 1. Load vTable
                  auto vTableAddr = builder->CreateStructGEP(cls, instance, VTABLE_INDEX);
                  vTable = builder->CreateLoad(cls->getElementType(VTABLE_INDEX), vTableAddr, "vt");
                  vTableTy = (llvm::StructType*)(vTable->getType()->getContainedType(0));
              }
              // 2. Load method from the vTable
              auto methodIdx = getMethodIndex(cls, methodName);
              auto methodTy = (llvm::FunctionType*)vTableTy->getElementType(methodIdx);
              auto methodAddr = builder->CreateStructGEP(vTableTy, vTable, methodIdx);
              return builder->CreateLoad(methodTy, methodAddr);
          }

          // --------------------------------------------
          // Function calls:
          //
          // (square 2)

          else {
            auto callable = gen(exp.list[0], env);
            auto fn = (llvm::Function*)callable;
            std::vector<llvm::Value*> args{};
            auto argIdx = 0;

            for (auto i = 1; i < exp.list.size(); i++) {
                auto argValue = gen(exp.list[i], env);
                auto paramTy = fn->getArg(argIdx)->getType();
                auto bitCastArgVal = builder->CreateBitCast(argValue, paramTy);
                args.push_back(bitCastArgVal);
            }
            
            return builder->CreateCall(fn, args);
          }
        }

        // --------------------------------------------
        // Method calls:
        //
        // ((method p getX) p 2)

        else {
          auto loadedMethod = (llvm::LoadInst*)gen(exp.list[0], env);

          auto fnTy = (llvm::FunctionType*)(loadedMethod->getPointerOperand()
                                                ->getType()
                                                ->getContainedType(0)
                                                ->getContainedType(0));

          std::vector<llvm::Value*> args();
          for (auto i = 1; i < exp.list.size(); i++) {
              auto argValue = gen(exp.list[i], env);
              // Need to cast to parameter type to support sub—classes:
              // we should be able to pass Point 3D instance for the type
              // of the parent class Point:
              auto paramTy = fnTy->getParamType(i - 1);
              if (argValue->getType() != paramTy) {
                  auto bitCastArgVal = builder->CreateBitCast(argValue, paramTy);
                  args.push_back(bitCastArgVal);
              }
              else {
                  args.push_back(argValue);
              }
              return builder->CreateCall(fnTy, loadedMethod, args);
          }
        }

        break;
    }

    // Unreachable:
    return builder->getInt32(0);
  }

  /**
   * Returns field index.
   */
  size_t getFieldIndex(llvm::StructType* cls, const std::string& fieldName) {
    auto fields = &classMap_[cls->getName().data()].fieldsMap;
    auto it = fields->find(fieldName);
    return std::distance(fields->begin(), it) + RESERVED_FIELDS_COUNT;
  }

  /**
   * Returns method index.
   */
  size_t getMethodIndex(llvm::StructType* cls, const std::string& methodName) {
      auto methods = &classMap_[cls->getName().data()].methodsMap;
      auto it = methods->find(methodName);
      return std::distance(methods->begin(), it);
  }

  /**
   * Creates an instance of a class.
   */
  llvm::Value* createInstance(const Exp& exp, Env env,
                              const std::string& name) {
      auto className = exp.list[1].string;
      auto cls = getClassByName(className);
      if (cls == nullptr) {
          DIE << "[EvaLLVM]: Unknown class " << cls;
      }

      // NOTE: Stack allocation
      //auto instance = name.empty() ? builder->CreateAlloca(cls)
      //                             : builder->CreateAlloca(cls, 0, name);
      //
      // We do not use stack allocation for objects, since we need to support constructor
      // (factory) pattern, i.e. return an object from a callee to the caller, outside
      // llvm::CallInst::CreateMalloc{...}

      // Heap Allocation
      auto instance = mallocInstance(cls, name);

      // Call constructor
      auto ctor = module->getFunction(className + "_constructor");
      std::vector<llvm::Value*> args{instance};
      for (auto i = 2; i < exp.list.size(); i++) {
          args.push_back(gen(exp.list[i], env));
      }
      builder->CreateCall(ctor, args);
      return instance;
  }

  /**
   * Allocates an object of a given class on the heap.
   */
  llvm::Value* mallocInstance(llvm::StructType* cls, const std::string& name) {
      auto typeSize = builder->getInt64(getTypeSize(cls));
      // void*
      auto mallocPtr = builder->CreateCall(module->getFunction("GC_malloc"), typeSize, name);
      // void* -> Point*
      auto instance = builder->CreatePointerCast(mallocPtr, cls->getPointerTo());
      // Install the vTable to lookup methods
      std::string className{cls->getName().data()};
      auto vTableName = className + "_vTable";
      auto vTableAddr = builder->CreateStructGEP(cls, instance, VTABLE_INDEX);
      auto vTable = module->getNamedGlobal(vTableName);
      builder->CreateStore(vTable, vTableAddr);
      return instance;
  }

  /**
   * Returns size of a type in bytes.
   */
  size_t getTypeSize(llvm::Type* type_) {
    return module->getDataLayout().getTypeAllocSize(type_);
  }

  /**
   * Inherits parent class fields.
   */
  void inheritClass(llvm::StructType* cls, llvm::StructType* parent) {
      auto parentClassInfo = &classMap_[parent->getName().data()];

      // Inherit the field and method names
      classMap_[cls->getName().data()] = {
          /* class */ cls,
          /* parent */ parent,
          /* fields */ parentClassInfo->fieldsMap,
          /* methods */ parentClassInfo->methodsMap};
  }

  /**
   * Extracts fields and methods from a class expression.
   */
  void buildClassInfo(llvm::StructType* cls, const Exp& clsExp, Env env) {
      auto className = clsExp.list[1].string;
      auto classInfo = &classMap_[className];
      // Body block (begin ...):
      auto body = clsExp.list[3];
      for (auto i = 1; i < body.list.size() : i++) {
          auto exp = body.list[i];
          if (isVar(exp)) {
              auto varNameDecl = exp.list[1];
              auto fieldName = extractVarName(varNameDecl);
              auto fieldTy = extractVarType(varNameDecl);
              classInfo->fieldsMap[fieldName] = fieldTy;
          }
          else if (isDef(exp)){
              auto methodName = exp.list[1].string;
              auto fnName = className + "_" + methodName;
              classInfo->methodsMap[methodName] = createFunctionProto(fnName, extractFunctionType(exp), env);
          }
      }
      // Create fields and vTable
      buildClassBody(cls);
  }

  /**
   * Builds class body from class info.
   */
  void buildClassBody(llvm::StructType* cls) {
      std::string className{cls->getName().data()};
      auto classInfo = &classMap_[className];

      // Allocate vTable to set its type in the body
      // The table itself is populated later in buildVTable
      auto vTableName = className + "_vTable";
      auto vTableTy = llvm::StructType::create(*ctx, vTableName);

      auto clsFields = std::vector<llvm::Type*>{
            // First element is always the vTable:
            vTableTy->getPointer(),
      };

      // Field types
      for (const auto& fieldInfo : classInfo->fieldsMap) {
          clsFields.push_back(fieldInfo.second);
      }

      cls->setBody(clsFields, /* packed */ false);

      // Methods:
      buildVTable(cls);
  }

  /**
   * Creates a vtable per class.
   *
   * vTable stores method references to support
   * inheritance and methods overloading.
   */
  void buildVTable(llvm::StructType* cls) {
      std::string className{cls->getName().data()};
      auto vTableName = className + "_vTable";
      
      // The vTable should already exist:
      auto vTableTy = llvm::StructType::getTypeByName(*ctx, vTableName);
      std::vector<llvm::Constant*> vTableMethods;
      std::vector<llvm::Type*> vTableMethodTys;

      for (auto& methodInfo : classMap_[className].methodsMap) {
          auto method = methodInfo.second;
          vTableMethods.push_back(method);
          vTableMethodTys.push_back(method->getType());
      }
      vTableTy->setBody(vTableMethodTys);
      auto vTableValue = llvm::ConstantStruct::get(vTableTy, vTableMethods);
      createGlobalVar(vTableName, vTableValue);
  }

  /**
   * Tagged lists.
   */
  bool isTaggedList(const Exp& exp, const std::string& tag) {
    return exp.type == ExpType::LIST && exp.list[0].type == ExpType::SYMBOL &&
           exp.list[0].string == tag;
  }

  /**
   * (var ...)
   */
  bool isVar(const Exp& exp) { return isTaggedList(exp, "var"); }

  /**
   * (def ...)
   */
  bool isDef(const Exp& exp) { return isTaggedList(exp, "def"); }

  /**
   * (new ...)
   */
  bool isNew(const Exp& exp) { return isTaggedList(exp, "new"); }

  /**
   * (prop ...)
   */
  bool isProp(const Exp& exp) { return isTaggedList(exp, "prop"); }

  /**
   * (super ...)
   */
  bool isSuper(const Exp& exp) { return isTaggedList(exp, "super"); }

  /**
   * Returns a type by name.
   */
  llvm::StructType* getClassByName(const std::string& name) {
    return llvm::StructType::getTypeByName(*ctx, name);
  }

  /**
   * Extracts var or parameter name considering type.
   *
   * x -> x
   * (x number) -> x
   */
  std::string extractVarName(const Exp& exp) {
    return exp.type == ExpType::LIST ? exp.list[0].string : exp.string;
  }

  /**
   * Extracts var or parameter type with i32 as default.
   *
   * x -> i32
   * (x number) -> number
   */
  llvm::Type* extractVarType(const Exp& exp) {
    return exp.type == ExpType::LIST ? getTypeFromString(exp.list[1].string)
                                     : builder->getInt32Ty();
  }

  /**
   * Returns LLVM type from string representation.
   */
  llvm::Type* getTypeFromString(const std::string& type_) {
    // number -> i32
    if (type_ == "number") {
      return builder->getInt32Ty();
    }

    // string -> i8* (aka char*)
    if (type_ == "string") {
      return builder->getInt8Ty()->getPointerTo();
    }

    // Classes:
    return classMap_[type_].cls->getPointerTo();
  }

  /**
   * Whether function has return type defined.
   */
  bool hasReturnType(const Exp& fnExp) {
    return fnExp.list[3].type == ExpType::SYMBOL &&
           fnExp.list[3].string == "->";
  }

  /**
   * Exp function to LLVM function params.
   *
   * (def square ((number x)) -> number ...)
   *
   * llvm::FunctionType::get(returnType, paramTypes, false);
   */
  llvm::FunctionType* extractFunctionType(const Exp& fnExp) {
    auto params = fnExp.list[2];

    // Return type:
    auto returnType = hasReturnType(fnExp)
                          ? getTypeFromString(fnExp.list[4].string)
                          : builder->getInt32Ty();

    // Parameter types:
    std::vector<llvm::Type*> paramTypes{};

    for (auto& param : params.list) {
      auto paramName = extractVarName(param);
      auto paramTy = extractVarType(param);

      // The `self` name is special, meaning instance of a class:
      paramTypes.push_back(
          paramName == "self" ? (llvm::Type*)cls->getPointerTo() : paramTy);
    }

    return llvm::FunctionType::get(returnType, paramTypes, /* varargs */ false);
  }

  /**
   * Compiles a function.
   *
   * Untyped: (def square (x) (* x x)) - i32 by default
   *
   * Typed: (def square ((x number)) -> number (* x x))
   */
  llvm::Value* compileFunction(const Exp& fnExp, std::string fnName, Env env) {
      auto params = fnExp.list[2];
      auto body = hasReturnType(fnExp) ? fnExp.list[5] : fnExp.list[3];
      
      // Save current fn
      auto prevFn = fn;
      auto prevBlock = builder->GetInsertBlock();

      auto origName = fnName;

      // Class method names
      if (cls != nullptr) {
          fnName = std::string(cls->getName().data()) + "_" + fnName;
      }

      // Override fn to compile body
      auto newFn = createFunction(fnName, extractFunctionType(fnExp), env);
      fn = newFn;

      // Set parameter names
      auto idx = 0;

      // Function environment for params
      auto fnEnv = std::make_shared<Environment>(
          std::map<std::string, llvm::Value*>{}, env);
      for (auto& arg : fn->args()) {
          auto param = params.list[idx++];
          auto argName = extractVarName(param);
          arg.setName(argName);
          // Allocate a local variable per argument to make arguments mutable
          auto argBinding = allocVar(argName, arg.getType(), fnEnv);
          builder->CreateStore(&arg, argBinding);
      }
      builder->CreateRet(gen(body, fnEnv));
      // Restore previous fn after compiling
      builder->SetInsertPoint(prevBlock);
      fn = prevFn;

      return newFn;
  }

  /**
   * Allocates a local variable on the stack. Result is the alloca instruction.
   */
  llvm::Value* allocVar(const std::string& name, llvm::Type* type_, Env env) {
    varsBuilder->SetInsertPoint(&fn->getEntryBlock());

    auto varAlloc = varsBuilder->CreateAlloca(type_, 0, name.c_str());

    // Add to the environment:
    env->define(name, varAlloc);

    return varAlloc;
  }

  /**
   * Creates a global variable.
   */
  llvm::GlobalVariable* createGlobalVar(const std::string& name,
                                        llvm::Constant* init) {
      module->getOrInsertGlobal(name, init->getType());
      auto variable = module->getNamedGlobal(name);
      variable->setAlignment(llvm::MaybeAlign(4));
      variable->setConstant(false);
      variable->setInitializer(init);
      return variable;
  }

  /**
   * Define external functions (from libc++)
   */
  void setupExternFunctions() {
    // i8* to substitute for char*, void*, etc
    auto bytePtrTy = builder->getInt8Ty()->getPointerTo();
    // int printf (const char* format, ...);
    module->getOrInsertFunction("printf", 
      llvm::FunctionType::get(
        /* return type */ builder->getInt32Ty(),
        /* format arg*/ bytePtrTy,
        /* vararg */ true));
    
    // void* malloc(size_t size), GC_malloc(size_t size)
    // size_t is i64
    module->getOrInsertFunction("GC_malloc", llvm::FunctionType::get(bytePtrTy, builder->getInt64Ty(),
                                /* vararg */ false));
  }

  /**
   * Creates a function.
   */
  llvm::Function* createFunction(const std::string& fnName,
                                 llvm::FunctionType* fnType, Env env) {
      // Function prototype might already be defined:
      auto fn = module->getfunction(fnName);
      // If not, allocate the function:
      if (fn == nullptr) {
          fn = createFunctionProto(fnName, fnType, env);
      }
      createFunctionBlock(fn);
      return fn;
  }

  /**
   * Creates function prototype (defines the function, but not the body)
   */
  llvm::Function* createFunctionProto(const std::string& fnName,
                                      llvm::FunctionType* fnType, Env env) {
    auto fn = llvm::Function::Create(fnType, llvm::Function::ExternalLinkage,
                                     fnName, *module);
    verifyFunction(*fn);

    // Install in the environment:
    env->define(fnName, fn);

    return fn;
  }

  /**
   * Creates function block.
   */
  void createFunctionBlock(llvm::Function* fn) {
      auto entry = createBB("entry", fn);
      builder->SetInsertPoint(entry);
  }

  /**
   * Creates a basic block. If the `fn` is passed, the block is
   * automatically appended to the parent function. Otherwise,
   * the block should later be appended manually via
   * fn->getBasicBlockList().push_back(block);
   */
  llvm::BasicBlock* createBB(std::string name, llvm::Function* fn = nullptr) {
    return llvm::BasicBlock::Create(*ctx, name, fn);
  }

  /**
   * Saves IR to file.
   */
  void saveModuleToFile(const std::string& fileName) {
    std::error_code errorCode;
    llvm::raw_fd_ostream outLL(fileName, errorCode);
    module->print(outLL, nullptr);
  }

  /**
   * Initialize the module.
   */
  void moduleInit() {
    // Open a new context and module.
    ctx = std::make_unique<llvm::LLVMContext>();
    module = std::make_unique<llvm::Module>("EvaLLVM", *ctx);

    // Create a new builder for the module.
    builder = std::make_unique<llvm::IRBuilder<>>(*ctx);

    // Vars builder:
    varsBuilder = std::make_unique<llvm::IRBuilder<>>(*ctx);
  }

  /**
   * Sets up The Global Environment.
   */
  void setupGlobalEnvironment() {
      std::map<std::string, llvm::Value*> globalObject{
          {"VERSION", builder->getInt32(42)}
      };
      std::map<std::string, llvm::Value*> globalRec{};

      for (auto& entry : globalObject) {
          globalRec[entry, first] = createGlobalVar(entry.first, (llvm::Constant*)entry.second);
      }

      GlobalEnv = std::make_shared<Environment>(globalRec, nullptr);
  }

  /**
   * Sets up target triple.
   */
  void setupTargetTriple() {
    // llvm::sys::getDefaultTargetTriple()
      module->setTargetTriple();
  }

  /**
   * Parser.
   */
  std::unique_ptr<EvaParser> parser;

  /**
   * Global Environment (symbol table).
   */
  std::shared_ptr<Environment> GlobalEnv;

  /**
   * Currently compiling class.
   */
  llvm::StructType* cls = nullptr;

  /**
   * Class info.
   */
  std::map<std::string, ClassInfo> classMap_;

  /**
   * Currently compiling function.
   */
  llvm::Function* fn;

  /**
   * Global LLVM context.
   * It owns and manages the core "global" data of LLVM's core
   * infrastructure, including the type and constant unique tables.
   */
  std::unique_ptr<llvm::LLVMContext> ctx;

  /**
   * A Module instance is used to store all the information related to an
   * LLVM module. Modules are the top level container of all other LLVM
   * Intermediate Representation (IR) objects. Each module directly contains a
   * list of globals variables, a list of functions, a list of libraries (or
   * other modules) this module depends on, a symbol table, and various data
   * about the target's characteristics.
   *
   * A module maintains a GlobalList object that is used to hold all
   * constant references to global variables in the module.  When a global
   * variable is destroyed, it should have no entries in the GlobalList.
   * The main container class for the LLVM Intermediate Representation.
   */
  std::unique_ptr<llvm::Module> module;

  /**
   * Extra builder for variables declaration.
   * This builder always prepends to the beginning of the
   * function entry block.
   */
  std::unique_ptr<llvm::IRBuilder<>> varsBuilder;

  /**
   * IR Builder.
   *
   * This provides a uniform API for creating instructions and inserting
   * them into a basic block: either at the end of a BasicBlock, or at a
   * specific iterator location in a block.
   */
  std::unique_ptr<llvm::IRBuilder<>> builder;
};

#endif