package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func sayMore() {
	var x int
	x = 3
	y := 4 // initializes and defines
	sum, prod := learnMultiple(x, y)
	fmt.Println("Sum:", sum, "Prod:", prod)
}

func learnMultiple(x, y int) (sum, prod int) {
	return x + y, x * y
}

/*
this is for learning different types
*/
func learnTypes() {
	a1 := "learning types"
	var a2 uint = 7
	a3 := [...]uint{3, 1, 4, a2}
	a3copy := a3 // immutable copies
	a3copy[0] = 20

	fmt.Println("a1:", a1, "a3copy:", a3copy)
}

func inc(i int, c chan int) {
	c <- i + 1
}

/*
concurrency
*/
func doConcurrency() {
	c := make(chan int)

	/*
	   c <- val :: send value to channel
	   <- c :: receive value
	*/

	go inc(0, c)
	go inc(10, c)
	go inc(-100, c)

	fmt.Println(<-c, <-c, <-c)
}

// Define Stringer as an interface type with one method, String.
type Stringer interface {
	String() string
}

// Define pair as a struct with two fields, ints named x and y.
type pair struct {
	x, y int
}

func (p pair) String() string {
	return fmt.Sprintf("(%d, %d)", p.x, p.y)
}

func learnInterfaces() {
	p := pair{3, 4}
	var i Stringer // Declare i of interface type Stringer.
	i = p          // Valid because pair implements Stringer
	fmt.Println(i.String())
	fmt.Println(p) // Output same as above. Println calls String method.
}

func learnWebProgramming() {
	go func() {
		err := http.ListenAndServe(":8081", pair{})
		fmt.Println(err) // don't ignore errors
	}()
	requestServer()
}

func requestServer() {
	resp, err := http.Get("http://localhost:8081")
	fmt.Println(err)
	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	fmt.Printf("\nWebserver said: `%s`", string(body))
}

// Make pair an http.Handler by implementing its only method, ServeHTTP.
func (p pair) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	// Serve data with a method of http.ResponseWriter.
	w.Write([]byte("You learned Go in Y minutes!"))
}

func learnxiny() {
	fmt.Println("Hey bob")
	sayMore()
	learnTypes()
	doConcurrency()
	learnInterfaces()
	learnWebProgramming()
}
