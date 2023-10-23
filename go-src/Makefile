all:
	echo $(buildInputs)/bin/go build -buildmode=c-archive

shared:
	go build -buildmode=c-shared -o libbucket.dylib

install:
	# echo install_name_tool -change libbucket.dylib $(shell pwd)/libbucket.dylib ../bin/process-image
	mkdir -p $(out)/include
	mkdir -p $(out)/lib
	cp *.h $(out)/include
	cp *.a $(out)/lib

clean:
	rm -f *.a
	rm -r *.h
