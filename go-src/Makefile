all:
	$(buildInputs)/bin/go build -buildmode=c-archive

shared:
	go build -buildmode=c-shared -o libbucket.dylib

install:
	mkdir -p $(out)/include
	mkdir -p $(out)/lib
	cp *.h $(out)/include
	cp *.a $(out)/lib

fixed:
	install_name_tool -change libbucket.dylib $(shell pwd)/libbucket.dylib ../bin/process-image

clean:
	rm -f *.a
	rm -r *.h
