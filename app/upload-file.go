package main

import (
	"flag"
	"github.com/Lupino/go-periodic"
	"github.com/aliyun/aliyun-oss-go-sdk/oss"
	"log"
	"net/http"
	"path/filepath"
)

var (
	periodicHost string
	thread       int
	worker       *periodic.Worker
	client       *periodic.Client
	bucketName   string
	bucket       *oss.Bucket
	endpoint     string
	accessID     string
	accessKey    string
	shareFSHost  string
	shareFSKey   string
)

func init() {
	flag.StringVar(&periodicHost, "periodic", "unix:///tmp/periodic.sock", "periodic server address")
	flag.IntVar(&thread, "thread", 2, "worker thread")
	flag.StringVar(&endpoint, "endpoint", "oss-cn-hangzhou.aliyuncs.com", "oss endpoint")
	flag.StringVar(&accessID, "accessID", "", "oss accessID")
	flag.StringVar(&accessKey, "accessKey", "", "oss accessKey")
	flag.StringVar(&bucketName, "bucket", "", "oss bucket")
	flag.StringVar(&shareFSHost, "share-fs-host", "http://gw.huabot.com", "ShareFS host")
	flag.StringVar(&shareFSKey, "share-fs-key", "", "ShareFS key")
	flag.Parse()
}

func main() {
	initBucket()
	client = periodic.NewClient()
	client.Connect(periodicHost)
	worker = periodic.NewWorker(thread)
	worker.Connect(periodicHost)
	worker.AddFunc("upload", uploadHandle)
	worker.AddFunc("upload-next-guetzli", uploadNextGuetzliHandle)
	worker.Work()
}

func doUpload(fileName string) error {
	var (
		err     error
		rsp     *http.Response
		fileUrl = shareFSHost + "/file/" + fileName + "?key=" + shareFSKey
	)
	var baseName = filepath.Base(fileName)

	if rsp, err = http.Get(fileUrl); err != nil {
		log.Printf("http.Get() failed (%s)\n", err)
		return err
	}
	defer rsp.Body.Close()

	if err = bucket.PutObject(baseName, rsp.Body); err != nil {
		log.Printf("bucket.PutObject() failed (%s)", err)
		return err
	}

	return nil
}

func uploadHandle(job periodic.Job) {
	if err := doUpload(job.Name); err != nil {
		if job.Raw.Counter > 20 {
			job.Done()
			return
		}
		job.SchedLater(int(job.Raw.Counter)*10, 1)
		return
	}
	job.Done()
}

func uploadNextGuetzliHandle(job periodic.Job) {
	if err := doUpload(job.Name); err != nil {
		if job.Raw.Counter > 20 {
			job.Done()
			return
		}
		job.SchedLater(int(job.Raw.Counter)*10, 1)
		return
	}
	job.Done()
	client.SubmitJob("guetzli", job.Name, nil)
}

func initBucket() {
	var (
		err     error
		client  *oss.Client
		isExist bool
	)
	if client, err = oss.New(endpoint, accessID, accessKey); err != nil {
		log.Fatalf("oss.New() failed (%s)\n", err)
	}

	if isExist, err = client.IsBucketExist(bucketName); err != nil {
		log.Fatalf("client.IsBucketExist() failed (%s)\n", err)
	}
	if !isExist {
		if err = client.CreateBucket(bucketName); err != nil {
			log.Fatalf("client.CreateBucket() failed (%s)\n", err)
		}
	}

	if bucket, err = client.Bucket(bucketName); err != nil {
		log.Fatalf("client.Bucket() failed (%s)\n", err)
	}
}
