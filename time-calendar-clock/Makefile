
PKG=time-calendar-clock
DEPS=-p idris2-missing-numeric


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
	idris2 ${DEPS} -p ${PKG} tests/Test-Data-Time-Clock.idr --cg chez -x main
	# idris2 ${DEPS} -p ${PKG} tests/Test-Data-Time-Clock.idr --cg gambit -x main
	idris2 ${DEPS} -p ${PKG} tests/Test-Data-Time-Clock.idr --cg node -x main
	# idris2 ${DEPS} -p ${PKG} tests/Test-Data-Time-Clock.idr --cg javascript -o test-javascript.js
	idris2 ${DEPS} -p ${PKG} tests/Test-Data-Time-Clock.idr --cg refc -o test-refc
	build/exec/test-refc

.PHONY: clean deepclean build install check

