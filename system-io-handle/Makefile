
PKG=system-io-handle


build:
	idris2 --build ${PKG}.ipkg

install:
	idris2 --install ${PKG}.ipkg

clean:
	idris2 --clean ${PKG}.ipkg
	find . -name *~ -delete
	find . -name *.bak -delete

deepclean:
	rm -rf build

check:
	echo "ABCDE" | idris2 -p ${PKG} tests/Test-System-IO-Terminal.idr --cg chez -x main
	echo "ABCDE" | idris2 -p ${PKG} tests/Test-System-IO-Terminal.idr --cg node -x main
	idris2 -p ${PKG} tests/Test-System-IO-Terminal.idr --cg javascript -o test-javascript.js
	idris2 -p ${PKG} tests/Test-System-IO-Terminal.idr --cg refc -o test-refc
	echo "ABDE" | build/exec/test-refc

.PHONY: clean deepclean build install check check-local

