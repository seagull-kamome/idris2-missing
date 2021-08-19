

build:
	idris2 --build time-calendar-clock.ipkg

install:
	idris2 --install time-calendar-clock.ipkg

check:
	idris2 -p time-calendar-clock -p idris2-missing-numeric tests/Test-Data-Integral.idr --cg chez -x main
	idris2 -p time-calendar-clock -p idris2-missing-numeric tests/Test-Data-Integral.idr --cg node -x main
	idris2 -p time-calendar-clock -p idris2-missing-numeric tests/Test-Data-Integral.idr --cg javascript -o test-javascript.js
	idris2 -p time-calendar-clock -p idris2-missing-numeric tests/Test-Data-Integral.idr --cg refc -o test-refc
	build/exec/test-refc

.PHONY: build install check

