
TESTS:= Test-Data-Bitset
TESTS+= Test-Foreign-Memory-Storable
# TESTS+= Test-System-IO-Terminal
TESTS+=Test-Data-String-Missing

build: depends
	idris2 --build idris2-missing.ipkg

depends:
	make -C idris2-missing-numeric
	make -C time-calendar-clock

install:
	idris2 --install idris2-missing.ipkg


check:
	for i in ${TESTS}; do \
	  idris2 -p contrib -p idris2-missing tests/$$i.idr -x main; \
	done

clean:
	rm -rf build

.PHONY: all build depends install check clean


