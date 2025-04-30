package main

import (
	"crypto/sha1"
	"fmt"
)

func crypto() {
	s := "Testing string"
	h := sha1.New()

	h.Write([]byte(s))
	bs := h.Sum(nil)

	fmt.Printf("%x\n", bs)
	fmt.Println(s)
}
