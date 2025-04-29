package channels

import (
	"fmt"
	"time"
)

func concurrency() {
	worker := func(done chan<- bool, msg string) {
		fmt.Printf("Do stuff with %s\n", msg)
		done <- true
	}

	done := make(chan bool, 2)
	go worker(done, "message one")
	go worker(done, "message two")

	// wait for both channels to finish
	<-done
	<-done
}

func runSelect() {
	c := make(chan int)
	go func() {
		<-time.After(time.Second * 2)
		c <- 1
	}()

	select {
	case n := <-c:
		fmt.Println(n)
	case <-time.After(time.Millisecond * 100):
		fmt.Println("timeout!")
	default:
		fmt.Println("non-blocking!")
	}

	close(c)
}

func rangingOverChannels() {
	queue := make(chan int, 2)
	queue <- 1
	queue <- 2
	close(queue)

	for elem := range queue {
		fmt.Println(elem)
	}
}

func timing() {
	timer := time.NewTimer(time.Second)
	go func() {
		<-timer.C
	}()
	stopped := timer.Stop()
	if stopped {
		fmt.Println(stopped)
	}
}

func debounce(interval time.Duration, c chan string, cb func(arg string)) {
	var item string
	timer := time.NewTimer(interval)

	for {
		select {
		case item = <-c:
			timer.Reset(interval)
		case <-timer.C:
			if item != "" {
				cb(item)
			}
		}
	}
}

// This is a pretty neat comment
func Run() {
	c := make(chan string)
	go debounce(time.Microsecond, c, func(arg string) { fmt.Println(arg) })
	c <- "hello"
	runSelect()
	rangingOverChannels()
	timing()
	c <- "world"
}

func main() { Run() }
