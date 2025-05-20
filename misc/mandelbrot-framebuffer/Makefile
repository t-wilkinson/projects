.PHONY: clean build build-asm test

build: main.c mandelbrot.asm
	nasm -f elf64 -o mandelbrot.o mandelbrot.asm && gcc main.c mandelbrot.o -o mandelbrot

build-asm: mandelbrot.asm
	nasm -f elf64 -o mandelbrot.o mandelbrot.asm && ld -o mandelbrot mandelbrot.o

clean:
	rm -f mandelbrot.o mandelbrot

test: clean build
	./mandelbrot /dev/fb0
