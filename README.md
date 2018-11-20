# Config setup

## irony

set `LD_LIBRARY_PATH` to point to dir where `libclang.so` is contained, e.g. `export LD_LIBRARY_PATH=/opt/llvm/release_70/lib:$LD_LIBRARY_PATH`

don't forget to build compilation databases e.g. add `-DCMAKE_EXPORT_COMPILE_COMMANDS=ON` to `cmake`, read more [here](https://github.com/Sarcasm/irony-mode#compilation-database).