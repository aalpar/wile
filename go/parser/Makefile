SOURCES := $(wildcard *.go)

.PHONY: build
build: $(SOURCES)
	go build .

.PHONY: test
test:
	go test .

.PHONY: cover
cover:
	go test -cover .
