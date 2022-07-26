
PKG=idris2-missing-numeric
DEP=-p contrib -p elab-util -p sop -p newtype-deriving


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
	idris2 -p ${PKG} ${DEP} tests/Main.idr --cg chez -x main
	# idris2 -p ${PKG} ${DEP} tests/Main.idr --cg gambit -x main
	idris2 -p ${PKG} ${DEP} tests/Main.idr --cg node -x main
	idris2 -p ${PKG} ${DEP} tests/Main.idr --cg javascript -o test-javascript.js
	idris2 -p ${PKG} ${DEP} tests/Main.idr --cg refc -o test-refc
	build/exec/test-refc

.PHONY: clean deepclean build install check

