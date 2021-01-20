
TESTS:= Test-Data-Bitset
# TESTS+= Test-Control-App-Fresh
TESTS+= Test-Data-Integral

all: libs tests

libs:
	idris2 --build idris2-missing.ipkg

tests:
	for i in ${TESTS}; do \
	  idris2 -p idris2-missing.ipkg tests/$$i.idr -x main; \
	done

.PHONY: all tests


