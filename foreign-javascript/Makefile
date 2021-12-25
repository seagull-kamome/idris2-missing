
PKG=foreign-javascript
DEP=

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
	# idris2 -p ${PKG} ${DEP} tests/test000.idr --cg chez -x main
	idris2 -p ${PKG} ${DEP} tests/test000.idr --cg node -x main
	idris2 -p ${PKG} ${DEP} tests/test000.idr --cg javascript -o test-javascript.js
	# idris2 -p ${PKG} ${DEP} tests/test000.idr --cg refc -o test-refc
	# build/exec/test-refc

.PHONY: clean deepclean build install check
