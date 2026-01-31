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


# Build the scheme binary to ../dist/scheme with embedded git SHA and version.
# Rebuilds only when Go source files change.
#   make build
#
# Cross-compile for a specific OS/architecture:
#   GOOS=linux GOARCH=amd64 make build    # Linux x86-64
#   GOOS=linux GOARCH=arm64 make build    # Linux ARM64
#   GOOS=darwin GOARCH=arm64 make build   # macOS Apple Silicon
#   GOOS=darwin GOARCH=amd64 make build   # macOS Intel
#   GOOS=windows GOARCH=amd64 make build  # Windows x86-64
#
# Docker build (from the repo root):
#   docker build -f docker/Dockerfile -t wile ..
#
# Cross-platform Docker build:
#   docker build --platform linux/amd64 -f docker/Dockerfile -t wile ..
#   docker build --platform linux/arm64 -f docker/Dockerfile -t wile ..
.PHONY: build
build: $(DIST_DIR)/$(MY_BIN)

$(DIST_DIR)/$(MY_BIN): $(SOURCES)
	$(GO_BUILD) -o $(DIST_DIR)/$(MY_BIN) -ldflags "-X main.BuildSHA=$(BUILD_SHA) -X main.BuildVersion=$(BUILD_VERSION)" ./cmd

# Compile tests for all packages without running them.
# Useful for verifying that tests compile after refactoring.
#   make buildtest
.PHONY: buildtest
buildtest:
	for dir in $(SOURCE_DIRS); do \
	    if [ -d "$$dir" ]; then \
	        $(GO_TEST) -c -o /dev/null $$dir/...; \
	    fi \
	done

# Run all tests with verbose output.
#   make test
.PHONY: test
test:
	$(GO_TEST) -v ./...

# Run tests with coverage and print per-function coverage summary.
# Writes coverage profile to build/coverage.out.
#   make cover
.PHONY: cover
cover:
	@mkdir -p build
	$(GO_TEST) -coverprofile=build/coverage.out ./...
	$(GO) tool cover -func=build/coverage.out

# Run golangci-lint on all packages.
#   make lint
.PHONY: lint
lint:
	$(GOLANGCI_LINT) -v run ./...

# Run golangci-lint with --fix to auto-correct fixable issues.
#   make fix
.PHONY: fix
fix:
	$(GOLANGCI_LINT) -v run --fix ./...

# Format all Go source files via golangci-lint.
#   make format
.PHONY: format
format:
	$(GOLANGCI_LINT) -v fmt -v ./...

# Tidy go.mod: add missing and remove unused dependencies.
#   make tidy
.PHONY: tidy
tidy:
	$(GO_MOD) tidy -e -x

# Clear the Go build cache.
#   make buildclean
.PHONY: buildclean
buildclean:
	$(GO_CLEAN) -cache

# Clear the Go test and fuzz caches.
#   make testclean
.PHONY: testclean
testclean:
	$(GO_CLEAN) -testcache -fuzzcache

# Clear the Go module download cache.
#   make modclean
.PHONY: modclean
modclean:
	$(GO_CLEAN) -modcache

# Create an annotated git tag from the version in ../VERSION.
#   make tag
.PHONY: tag
tag:
	git tag -a $(BUILD_VERSION) -m "Release $(BUILD_VERSION)"
	@echo "Created tag $(BUILD_VERSION)"
