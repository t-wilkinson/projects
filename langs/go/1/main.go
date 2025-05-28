package main

import (
	"flag"
	"fmt"
	"log"
	"net/http"
	"os"
	"os/signal"
	re "regexp"
	"syscall"
	"time"

	"github.com/t-wilkinson/1/channels"
	"github.com/t-wilkinson/1/shell"
)

func runServer() {
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Method: %s\n", r.Method)
		fmt.Fprintf(w, "Hello, you've requested: %s\n", r.URL.Path)
	})

	// Serve static files
	fs := http.FileServer(http.Dir("static/"))
	http.Handle("/static/", http.StripPrefix("/static/", fs))

	http.ListenAndServe(":8080", nil)
}

func flags() {
	listCommand := flag.NewFlagSet("list", flag.ExitOnError)
	myflag := listCommand.String("name", "default", "help message")

	if len(os.Args) < 2 {
		fmt.Println("list sub command is required")
		os.Exit(1)
	}

	switch os.Args[1] {
	case "list", "any other":
		listCommand.Parse(os.Args[2:])
	case "bob":
		fmt.Println("Bob was passed")
	default:
		flag.PrintDefaults()
		os.Exit(1)
	}

	if listCommand.Parsed() {
		fmt.Println(*myflag)
		if bob := "bob"; *myflag == "" || bob == "one" {
			listCommand.PrintDefaults()
			fmt.Println(*myflag)
			os.Exit(1)
		}
	}
}

func slices() {
	prints := func(nums ...int) {
		for num := range nums {
			fmt.Print(num, " ")
		}
		fmt.Println()
	}

	arr := [6]int{1, 2, 3, 4, 5, 6}
	sli := make([]int, 6)
	sli2 := []int{1, 2, 3, 4, 5, 6}

	fmt.Println(arr[1:3])
	sli = append(sli, 3)
	copy(sli, sli)
	fmt.Println(len(sli))

	prints(1, 3, 4, 5)
	prints(sli...)
	prints(sli2...)
}

func regExp() {
	r, err := re.Compile("p([a-z]+)ch")

	if err != nil {
		panic("Could not compile regex :(")
	}

	r.MatchString("asd peach pitch")
}

func signals() {

	sigs := make(chan os.Signal, 1)
	done := make(chan bool, 1)

	signal.Notify(sigs, syscall.SIGINT, syscall.SIGTERM)

	go func() {
		sig := <-sigs
		fmt.Println()
		fmt.Println(sig)
		done <- true
	}()

	fmt.Println("awaiting signal")
	<-done
	fmt.Println("exiting")
}

func main() {
	signals()
	channels.Run()
	types()
	// flags()
	slices()
	regExp()
	crypto()

	time.Sleep(time.Millisecond * 500)

	err, out, _ := shell.RunShell("echo cwd $(pwd) '(curtesy of bash)'")
	fmt.Println(out)
	if err != nil {
		log.Fatal(err)
	}

	// runServer()
}
