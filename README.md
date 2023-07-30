# Programming Language with LLVM

This is a repository for the [Programming Language with LLVM](http://dmitrysoshnikov.com/courses/programming-language-with-llvm/) course.
My goal is to understand about LLVM internals.

## What's next?
1. Optimizing compiler:
opt [...]
2. Arrays/lists
(list 1 2 3) —> llvm::ArrayType
3. Custom Garbage Collector hooks —> https://l1vm.org/docs/GarbageCottection.html
4. Interfaces
(interface Callable ... (def __call__ (self) throw))
(class Transformer Callable ...)
5. Rest arguments
(interface Callable ... (def __call__ (self ...) throw))
6. Opaque pointers: i32* -> ptr, i8* -> ptr, etc
7. LLVM IR & MLIR
8. (async def fetch (...) ...) -> (await fetch ...)
