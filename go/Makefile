GO=go
GOLANGCI_LINT=golangci-lint
GO_TEST=$(GO) test
GO_BUILD=$(GO) build
GO_CLEAN=$(GO) clean
GO_MOD=$(GO) mod

SOURCES=$(shell find . -type f -name "*.go" -print)
SOURCE_DIRS=$(shell go list -f "{{.Dir}}" ./...)
BUILD_SHA:=$(shell git rev-parse --short HEAD 2>/dev/null || echo "0000000" )
BUILD_VERSION:=$(shell cat ../VERSION 2>/dev/null || echo "0.0.0")
DIST_DIR=../dist
MY_BIN=scheme


.PHONY: build
build: $(DIST_DIR)/$(MY_BIN)

$(DIST_DIR)/$(MY_BIN): $(SOURCES)
	$(GO_BUILD) -o $(DIST_DIR)/$(MY_BIN) -ldflags "-X main.BuildSHA=$(BUILD_SHA) -X main.BuildVersion=$(BUILD_VERSION)" ./cmd

.PHONY: buildtest
buildtest:
	for dir in $(SOURCE_DIRS); do \
	    if [ -d "$$dir" ]; then \
	        $(GO_TEST) -c -o /dev/null $$dir/...; \
	    fi \
	done

.PHONY: test
test:
	$(GO_TEST) -v ./...

.PHONY: cover
cover:
	$(GO_TEST) -cover -v ./...

.PHONY: lint
lint:
	$(GOLANGCI_LINT) -v run ./...

.PHONY: fix
fix:
	$(GOLANGCI_LINT) -v run --fix ./...

.PHONY: format
format:
	$(GOLANGCI_LINT) -v fmt -v ./...

.PHONY: tidy
tidy:
	$(GO_MOD) tidy -e -x

.PHONY: buildclean
buildclean:
	$(GO_CLEAN) -cache

.PHONY: testclean
testclean:
	$(GO_CLEAN) -testcache -fuzzcache

.PHONY: modclean
modclean:
	$(GO_CLEAN) -modcache 

