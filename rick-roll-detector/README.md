## Intro
A convoluted yet necessary solution to the Rickroll epidemic.

## Features
- Enter in a URL to see if it has any refereces to rick Rick Astley or "Never Gonna Give You Up".
- There is a 30% chance that checking a URL will Rick Roll you.
- An in-memory database which caches whether URLs or dirty or clean.

## Architecture
- Website
    - Simple HTML file.
    - Interfaces with webserver through simple http requests.
    - User can enter url and get a response detailing if the link is a rick roll.
- Webserver
    - Written in Go.
    - Returns the webpage at `/` or receives a url to detect at `/search?query=<query>`
- Rick roll detector service
    - Written in Go.
    - A go service running a rpc server. Way over the top.
- In-memory database
    - Written in C.
    - Caches search queries.
    - Periodically saves database to filesystem.
    - Interacts through TCP.

## Running
- `./scripts/start.sh` will compile and run the database and webserver

## Why?
- So I could get some intuition for system design topics in a contrived situation.
- I wanted to write a database from scratch in C.
- It was fun.
