SOURCES=$(shell find . -type f -name "*.go" -print)
BUILD_SHA:=$(shell git rev-parse --short HEAD 2>/dev/null || echo "0000000" )
BUILD_VERSION:=$(shell cat ../VERSION 2>/dev/null || echo "0.0.0")
DIST_DIR=../dist
MY_BIN=scheme


.PHONY: build
build: $(DIST_DIR)/$(MY_BIN)

$(DIST_DIR)/$(MY_BIN): $(SOURCES)
	go build -o $(DIST_DIR)/$(MY_BIN) -ldflags "-X main.BuildSHA=$(BUILD_SHA) -X main.BuildVersion=$(BUILD_VERSION)" ./cmd

.PHONY: buildtest
buildtest:
	go test -v -c -o $(DIST_DIR)/$(MY_BIN) ./...

.PHONY: test
test:
	go test -v ./...

.PHONY: cover
cover:
	go test -cover -v ./...

.PHONY: lint
lint:
	golangci-lint -v run

.PHONY: fix
fix:
	golangci-lint -v run --fix

.PHONY: format
format:
	golangci-lint -v fmt

.PHONY: tidy
tidy:
	go mod tidy -e -x

.PHONY: buildclean
buildclean:
	go clean -cache

.PHONY: testclean
testclean:
	go clean -testcache -fuzzcache

.PHONY: modclean
modclean:
	go clean -modcache 

