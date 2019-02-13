package main

import (
	"flag"
	"github.com/Lupino/go-periodic"
	"github.com/aliyun/aliyun-oss-go-sdk/oss"
	"io/ioutil"
	"log"
	"path/filepath"
	"time"
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
	root         string
)

func init() {
	flag.StringVar(&periodicHost, "periodic", "unix:///tmp/periodic.sock", "periodic server address")
	flag.IntVar(&thread, "thread", 2, "worker thread")
	flag.StringVar(&endpoint, "endpoint", "oss-cn-hangzhou.aliyuncs.com", "oss endpoint")
	flag.StringVar(&accessID, "accessID", "", "oss accessID")
	flag.StringVar(&accessKey, "accessKey", "", "oss accessKey")
	flag.StringVar(&bucketName, "bucket", "", "oss bucket")
	flag.StringVar(&root, "root", "images", "Image root path")
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
	worker.AddFunc("remove-remote", removeRemoteFileHandle)
	worker.AddFunc("get-remote", getRemoteFileHandle)

	go checkAlive()

	worker.Work()
}

func checkAlive() {
	c := time.Tick(1 * time.Minute)
	for _ = range c {
		client.Ping()
		worker.Ping()
	}
}

func doUpload(fileName string) error {
	var baseName = filepath.Base(fileName)

	if err := bucket.PutObjectFromFile(baseName, filepath.Join(root, fileName)); err != nil {
		log.Printf("bucket.PutObjectFromFile() failed (%s)", err)
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

func removeRemoteFileHandle(job periodic.Job) {
	var baseName = filepath.Base(job.Name)

	if err := bucket.DeleteObject(baseName); err != nil {
		log.Printf("bucket.DeleteObject() failed (%s)", err)
		if job.Raw.Counter > 20 {
			job.Done()
			return
		}
		job.SchedLater(int(job.Raw.Counter)*10, 1)
		return
	}

	job.Done()
}

func doGetRemoteFile(fileName string) ([]byte, error) {
	var baseName = filepath.Base(fileName)

	body, err := bucket.GetObject(baseName)
	if err != nil {
		log.Printf("bucket.GetObject() failed (%s)", err)
		return nil, err
	}

	data, err := ioutil.ReadAll(body)
	if err != nil {
		log.Printf("ioutil.ReadAll() failed (%s)", err)
		return nil, err
	}

	return data, nil
}

func getRemoteFileHandle(job periodic.Job) {
	data, err := doGetRemoteFile(job.Name)
	if err != nil {
		if job.Raw.Counter > 20 {
			job.Done()
			return
		}
		job.SchedLater(int(job.Raw.Counter)*10, 1)
		return
	}
	job.Done(data)
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
