generate code
-------------
wasm-pack build --target web

inspect generated code
----------------------
wasm2wat pkg/hello_wasm_bg.wasm| less

optimize code
-------------
wasm-opt -O2 pkg/hello_wasm_bg.wasm -c -S -o pkg/hello.wat
