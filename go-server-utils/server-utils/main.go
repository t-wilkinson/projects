// TODO: instead of data.json, pass folder containing all routes and query each of those given prefix(es) path
// TODO similar but with content length?
package main

import (
	"encoding/json"
	"flag"
	"github.com/t-wilkinson/go-server-utils/redirect"
	"github.com/t-wilkinson/go-server-utils/requests"
	"os"
	"path/filepath"
	"time"
)

func repeat(n time.Duration, fn func(time.Duration)) {
	now := time.Now()
	tick := time.NewTicker(n)
	go fn(time.Since(now))
	for range tick.C {
		go fn(time.Since(now))
	}
}

func runRedirect(urls string) {
	data, err := os.ReadFile(urls)
	if err != nil {
		panic(err)
	}

	var jsondata struct {
		Redirect redirect.JsonData `json:"redirect"`
	}
	json.Unmarshal(data, &jsondata)

	redirect.Run(jsondata.Redirect)
	// repeat(time.Second*10, func(t time.Duration) {
	// 	redirect.Run(jsondata.Redirect)
	// })
}

func runRequests() {
	requests.Run()
}

func main() {
	redirectFlag := flag.NewFlagSet("redirect", flag.ContinueOnError)
	requestsFlag := flag.NewFlagSet("requests", flag.PanicOnError)

	if len(os.Args) < 2 {
		flag.PrintDefaults()
		os.Exit(1)
	}

	switch os.Args[1] {
	case "redirect":
		urls := redirectFlag.String("data", "", "json file containing routes to check redirects")
		err := redirectFlag.Parse(os.Args[2:])
		if err != nil {
			os.Exit(1)
		}

		if *urls == "" {
			dir, err := filepath.Abs(filepath.Dir(os.Args[0]))
			if err != nil {
				os.Exit(1)
			}
			*urls = dir + "/data.json"
		}

		runRedirect(*urls)

	case "requests":
		err := requestsFlag.Parse([]string{})
		if err != nil {
			requestsFlag.PrintDefaults()
			os.Exit(1)
		}
		runRequests()

	default:
		flag.PrintDefaults()
	}
}
