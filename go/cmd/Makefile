SOURCES := $(shell go list -f '{{range .GoFiles}}{{.}} {{end}}' .)
SOURCES_TEST := $(shell go list -f '{{range .TestGoFiles}}{{.}} {{end}}' .)
BUILD_SHA:=$(shell git rev-parse --short HEAD 2>/dev/null || echo "0000000" )
BUILD_VERSION:=$(shell head -1 ../../VERSION)
SUBDIRS:=$(wildcard */.)
MY_BINDIR = ../..
MY_BIN = scheme

.PHONY: build
build: $(SOURCES)
	go build -o $(MY_BINDIR)/$(MY_BIN) -ldflags "-X main.BuildSHA=$(BUILD_SHA) -X main.BuildVersion=$(BUILD_VERSION)" .

.PHONY: test
test:
	go test .

go.sum: go.mod $(SOURCES)
	touch $@
	go mod tidy
