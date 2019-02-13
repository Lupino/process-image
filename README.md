# process-image

An image process system.

# Build

Recommand build `process-image` with [`stack`](https://docs.haskellstack.org/en/stable/README/)

    git clone https://github.com/Lupino/process-image.git
    cd process-image
    mkdir bin
    stack build
    stack install

Build `upload-file.go`

    export GOPATH=`pwd`
    cd app
    go get -d
    go build upload-file.go
    mv upload-file ../bin
    cd ..

Install `periodicd`

    wget https://github.com/Lupino/haskell-periodic/releases/download/v1.1.4.0/periodic-linux-v1.1.4.0.tar.bz2
    cd bin
    tar xvf ../periodic-linux-v1.1.4.0.tar.bz2
    cd ..

Build `guetzli`

    git clone https://github.com/google/guetzli.git
    cd guetzli
    make
    cp bin/Release/guetzli ../bin
    cd ..

# Quick start

    ./bin/periodicd
    ./bin/process-image config.yml
    ./bin/upload-file -bucket youbucket -accessKey youkey -accessID youid

# Process an image

    periodic submit save icon.png @icon.png
