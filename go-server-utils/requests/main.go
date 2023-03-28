package requests

import (
	"fmt"
	"net/http"
)

type handler struct {
	count chan int
}

func (h *handler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	n := <-h.count
	h.count <- n + 1
	fmt.Fprintln(w, n)
}

func Run() {
	http.HandleFunc("/test", func(w http.ResponseWriter, r *http.Request) {
		for name, header := range r.Header {
			fmt.Fprintf(w, "%s: %s\n", name, header)
		}
	})

	c := make(chan int, 1)
	c <- 0
	http.Handle("/count", &handler{c})

	http.ListenAndServe(":8090", nil)
}
