
PKG=system-io-handle

build:
	idris2 --build ${PKG}.ipkg

install:
	idris2 --install ${PKG}.ipkg

check:
	idris2 -p ${PKG} tests/Test-System-IO-Terminal.idr --cg chez -x main
	idris2 -p ${PKG} tests/Test-System-IO-Terminal.idr --cg node -x main
	idris2 -p ${PKG} tests/Test-System-IO-Terminal.idr --cg javascript -o test-javascript.js
	idris2 -p ${PKG} tests/Test-System-IO-Terminal.idr --cg refc -o test-refc
	build/exec/test-refc

.PHONY: build install check

