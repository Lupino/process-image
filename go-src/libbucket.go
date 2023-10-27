package main

import (
	"C"
	"github.com/aliyun/aliyun-oss-go-sdk/oss"
	"log"
	"path/filepath"
)

var (
	bucket *oss.Bucket
)

//export GoUpload
func GoUpload(remotePath, fileName *C.char) int8 {
  return goUpload(C.GoString(remotePath), C.GoString(fileName))
}

func goUpload(remotePath, fileName string) int8 {
	var baseName = filepath.Base(fileName)

	if err := bucket.PutObjectFromFile(filepath.Join(remotePath, baseName), fileName); err != nil {
		log.Printf("bucket.PutObjectFromFile() failed (%s)", err)
		return 1
	}

	return 0
}

//export GoInitBucket
func GoInitBucket(accessID, accessKey, bucketName, endpoint *C.char) int8 {
	return goInitBucket(C.GoString(accessID), C.GoString(accessKey), C.GoString(bucketName), C.GoString(endpoint))
}

func goInitBucket(accessID, accessKey, bucketName, endpoint string) int8 {
	var (
		err     error
		client  *oss.Client
		isExist bool
	)
	if client, err = oss.New(endpoint, accessID, accessKey); err != nil {
		log.Printf("oss.New() failed (%s)\n", err)
		return 1
	}

	if isExist, err = client.IsBucketExist(bucketName); err != nil {
		log.Printf("client.IsBucketExist() failed (%s)\n", err)
		return 1
	}
	if !isExist {
		if err = client.CreateBucket(bucketName); err != nil {
			log.Printf("client.CreateBucket() failed (%s)\n", err)
			return 1
		}
	}

	if bucket, err = client.Bucket(bucketName); err != nil {
		log.Printf("client.Bucket() failed (%s)\n", err)
		return 1
	}
	return 0
}

func main() {

}
