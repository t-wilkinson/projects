redis:
	./redis-6.0.9/src/redis-server

build:
	g++ store-titles.cpp -o store-titles -L/usr/local/lib64 -std=c++11 -lredox -lev -lhiredis

build-fast:
	g++ -O3 store-titles.cpp -o store-titles -L/usr/local/lib64 -std=c++11 -lredox -lev -lhiredis

clean:
	rm a.out

run:
	./download.py
