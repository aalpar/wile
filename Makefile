GO=go
GOLANGCI_LINT=golangci-lint
GO_TEST=$(GO) test
GO_BUILD=$(GO) build
GO_CLEAN=$(GO) clean
GO_MOD=$(GO) mod

GO_BUILD_DIR=./build
SH_TOOLS_DIR=./tools/sh

SOURCES=$(shell find . -type f -name "*.go" -print)
EMBED_SOURCES=$(shell find . -type f -name "*.scm" -print)
SOURCE_DIRS=$(shell go list -f "{{.Dir}}" ./...)
BUILD_SHA:=$(shell git rev-parse --short HEAD 2>/dev/null || echo "0000000" )
BUILD_VERSION:=$(shell cat ./VERSION 2>/dev/null || echo "0.0.0")
DIST_DIR=./dist
TEST_DIR=./test
MY_BIN=scheme

DOCKER_IMAGE ?= wile
DOCKER_PLATFORM ?=
DOCKER_SHELL ?=


# Build the scheme binary to ./dist/scheme with embedded git SHA and version.
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
# Docker build:
#   docker build -f docker/Dockerfile -t wile .
#   docker run wile                                  # run tests
#   docker run wile ./dist/scheme --file example.scm # run a file
#
# Cross-platform Docker build:
#   docker build --platform linux/amd64 -f docker/Dockerfile -t wile .
#   docker build --platform linux/arm64 -f docker/Dockerfile -t wile .
.PHONY: build
build: $(DIST_DIR)/$(MY_BIN)

$(DIST_DIR)/$(MY_BIN): $(SOURCES) $(EMBED_SOURCES)
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

# Run all benchmarks with memory allocation statistics.
#   make bench
.PHONY: bench
bench:
	$(GO_TEST) -bench=. -benchmem ./...

# Run tests with coverage and print per-function coverage summary.
# Writes coverage profile to ./build/coverage.out.
#   make cover
.PHONY: cover
cover:
	@mkdir -p ./build
	$(GO_TEST) -coverprofile=$(GO_BUILD_DIR)/coverage.out ./...
	$(GO) tool cover -func=$(GO_BUILD_DIR)/coverage.out

# Run tests with coverage and open an HTML report in the browser.
# Writes coverage profile to ./build/coverage.out and HTML to ./build/coverage.html.
#   make coverhtml
.PHONY: coverhtml
coverhtml:
	@mkdir -p ./build
	$(GO_TEST) -coverprofile=$(GO_BUILD_DIR)/coverage.out ./...
	$(GO) tool cover -html=$(GO_BUILD_DIR)/coverage.out -o $(GO_BUILD_DIR)/coverage.html
	@echo "Coverage report: $(GO_BUILD_DIR)/coverage.html"
	open $(GO_BUILD_DIR)/coverage.html 2>/dev/null || xdg-open $(GO_BUILD_DIR)/coverage.html 2>/dev/null || echo "Open $(GO_BUILD_DIR)/coverage.html in your browser"

# Run tests with coverage and enforce per-package threshold (80%).
# Excluded packages: cmd, repl, forms, extensions/*, registry/helpers,
# registry/testhelpers, examples/embedding, integration.
#   make covercheck
.PHONY: covercheck
covercheck:
	@mkdir -p ./build
	$(GO_TEST) -coverprofile=$(GO_BUILD_DIR)/coverage.out ./... || true
	@bash $(SH_TOOLS_DIR)/covercheck.sh 80 $(GO_BUILD_DIR)/coverage.out

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

# Remove all generated artifacts: build, test, module caches and output directories.
#   make clean
.PHONY: clean
clean: buildclean testclean modclean
	for dir in "$(DIST_DIR)" "$(TEST_DIR)"; do \
	    if [ -e "$$dir" ]; then rm -rf "$$dir"; fi \
	done

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

# Create an annotated git tag from the version in ./VERSION.
#   make tag
.PHONY: tag
tag:
	git tag -a $(BUILD_VERSION) -m "Release $(BUILD_VERSION)"
	@echo "Created tag $(BUILD_VERSION)"

# Bump the major version in VERSION (resets minor and patch to 0, preserves pre-release suffix).
#   make bump-major
#   v0.8.5-alpha → v1.0.0-alpha
.PHONY: bump-major
bump-major:
	$(SH_TOOLS_DIR)/bump-version.sh major

# Bump the minor version in VERSION (resets patch to 0, preserves pre-release suffix).
#   make bump-minor
#   v0.8.5-alpha → v0.9.0-alpha
.PHONY: bump-minor
bump-minor:
	$(SH_TOOLS_DIR)/bump-version.sh minor

# Bump the patch version in VERSION (preserves pre-release suffix).
#   make bump-patch
#   v0.8.5-alpha → v0.8.6-alpha
.PHONY: bump-patch
bump-patch:
	$(SH_TOOLS_DIR)/bump-version.sh patch

# Build the Docker image containing the Go toolchain and compiled binary.
# Delegates to tools/sh/docker-build.sh.
#   make docker-build
#
# Cross-platform Docker build:
#   make docker-build DOCKER_PLATFORM=linux/amd64
#   make docker-build DOCKER_PLATFORM=linux/arm64
.PHONY: docker-build
docker-build:
	DOCKER_IMAGE=$(DOCKER_IMAGE) DOCKER_PLATFORM=$(DOCKER_PLATFORM) $(SH_TOOLS_DIR)/docker-build.sh

# Open an interactive shell inside the Docker container.
# The first line of stdout is always the container ID.
# Delegates to tools/sh/docker-shell.sh.
#   make docker-shell
#   make docker-shell DOCKER_SHELL=/bin/sh
.PHONY: docker-shell
docker-shell:
	DOCKER_IMAGE=$(DOCKER_IMAGE) $(SH_TOOLS_DIR)/docker-shell.sh $(DOCKER_SHELL)
