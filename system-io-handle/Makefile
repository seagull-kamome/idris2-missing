
PKG=system-io-handle


build:
	idris2 --build ${PKG}.ipkg

install:
	idris2 --install ${PKG}.ipkg

clean:
	idris2 --clean ${PKG}.ipkg
	rm -rf build

distclean:
	find . -name *~ -delete
	find . -name *.bak -delete

check:
	echo "ABCDE" | idris2 -p ${PKG} tests/Test-System-IO-Terminal.idr --cg chez -x main
	echo "ABCDE" | idris2 -p ${PKG} tests/Test-System-IO-Terminal.idr --cg node -x main
	# Javascript backend wont support prim__os
	# idris2 -p ${PKG} tests/Test-System-IO-Terminal.idr --cg javascript -o test-javascript.js
	#
	# refC backend wont support Data.Buffer.prim_getInt32
	# idris2 -p ${PKG} tests/Test-System-IO-Terminal.idr --cg refc -o test-refc
	# echo "ABDE" | build/exec/test-refc

.PHONY: clean deepclean build install check check-local

