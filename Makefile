WGET ?= wget
WAT2WASM ?= wat2wasm

WASMTIME_VERSION ?= 0.22.0

examples := hello.wasm

test: libwasmtime.a $(addprefix examples/,$(examples))
	cabal run

examples/%.wasm : examples/%.wat
	$(WAT2WASM) $< -o $@

libwasmtime.a:
	case `uname` in Darwin) export OS="macos-x86_64" ;; Linux*) export OS="linux-x86_64";; esac; \
	$(WGET) https://github.com/bytecodealliance/wasmtime-go/raw/v$(WASMTIME_VERSION)/build/$${OS}/libwasmtime.a

.PHONY: test
