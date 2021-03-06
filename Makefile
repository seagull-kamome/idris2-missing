
TESTS:= Test-Data-Bitset
# TESTS+= Test-Control-App-Fresh
TESTS+= Test-Data-Integral
TESTS+= Test-Text-Format
TESTS+= Test-Data-Time-Clock
TESTS+= Test-Foreign-Memory-Storable
# TESTS+= Test-System-IO-Terminal
TESTS+=Test-Data-String-Missing

all: libs tests

libs:
	idris2 --build idris2-missing.ipkg

tests:
	for i in ${TESTS}; do \
	  idris2 -p contrib -p idris2-missing.ipkg tests/$$i.idr -x main; \
	done

.PHONY: all tests


