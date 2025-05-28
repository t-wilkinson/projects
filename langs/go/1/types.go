package main

import (
	"fmt"
)

type Readable interface {
	Read() string
}

type File struct{ path string }
type File2 struct{ path string }

func (f *File) Read() string { return f.path }
func (f File2) Read() string { return f.path }

func PrintReadable(r Readable) {
	fmt.Println(r.Read())
}

func types() {
	f := File{path: "{file path}"}
	PrintReadable(&f)

	f2 := File2{path: "{file path}"}
	PrintReadable(f2)

	fmt.Println("reading f path ", f.Read())

	var a int = 4
	var b *int = &a
	fmt.Println(b, *b)
}
