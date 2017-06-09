# process-image

An image process system.

# Build

Recommand build `process-image` with [`stack`](https://docs.haskellstack.org/en/stable/README/)

    git clone https://github.com/Lupino/process-image.git
    cd process-image
    mkdir bin
    echo 'local-bin-path: bin' >> stack.yaml
    stack build
    stack install
    stack install share-fs-server

Build `upload-file.go`

    export GOPATH=`pwd`
    cd app
    go get -d
    go build upload-file.go
    mv upload-file ../bin
    cd ..
    go get github.com/Lupino/periodic/cmd/periodic

# Quick start

    ./bin/share-fs-server -H 127.0.0.1 -p 8080 --path share-fs
    ./bin/periodic -d -H tcp://127.0.0.1:5000
    ./bin/process-image config.yml
    ./bin/upload-file -periodic tcp://127.0.0.1:5000 -thread 10 -share-fs-host http://127.0.0.1:8080 -bucket youbucket -accessKey youkey -accessID youid

# Process an image

    curl -XPUT -F @icon.png http://127.0.0.1:8080/file/icon.png
    echo icon.png | ./bin/submit-image -H 127.0.0.1 -P 5000 -f upload-next-guetzli,resize-image-fw500,resize-image-fw192,resize-image-fw64
