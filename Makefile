COVER_DIR=cover
DIST_DIR=dist
TEST_DIR=test
DOCKER_IMAGE ?= wile
DOCKER_PLATFORM ?=
DOCKER_SHELL ?=
SUBDIRS=$(shell cd go && go list -f '{{.Dir}}' ./...)

# Build the scheme binary (delegates to go/Makefile).
#   make build
#
# Cross-compile for a specific OS/architecture:
#   GOOS=linux GOARCH=amd64 make build    # Linux x86-64
#   GOOS=linux GOARCH=arm64 make build    # Linux ARM64
#   GOOS=darwin GOARCH=arm64 make build   # macOS Apple Silicon
#   GOOS=darwin GOARCH=amd64 make build   # macOS Intel
#   GOOS=windows GOARCH=amd64 make build  # Windows x86-64
#
# Docker build (builds and runs tests inside a container):
#   docker build -f docker/Dockerfile -t wile .
#   docker run wile                                    # run tests
#   docker run wile ../dist/scheme --file example.scm  # run a file
#
# Cross-platform Docker build:
#   docker build --platform linux/amd64 -f docker/Dockerfile -t wile .
#   docker build --platform linux/arm64 -f docker/Dockerfile -t wile .
.PHONY: build
build:
	$(MAKE) -C go

# Compile tests for all packages without running them (delegates to go/Makefile).
#   make buildtest
.PHONY: buildtest
buildtest: go
	$(MAKE) -C $< $@

# Run all tests with verbose output (delegates to go/Makefile).
#   make test
.PHONY: test
test: go
	mkdir -p $(TEST_DIR)
	$(MAKE) -C $< $@

# Run tests with coverage reporting (delegates to go/Makefile).
#   make cover
.PHONY: cover
cover: go
	mkdir -p $(COVER_DIR)
	$(MAKE) -C $< $@

# Run golangci-lint with --fix to auto-correct fixable issues (delegates to go/Makefile).
#   make fix
.PHONY: fix
fix:
	$(MAKE) -C go $@

# Run golangci-lint on all packages (delegates to go/Makefile).
#   make lint
.PHONY: lint
lint:
	$(MAKE) -C go $@

# Format all Go source files (delegates to go/Makefile).
#   make format
.PHONY: format
format:
	$(MAKE) -C go $@

# Remove all generated artifacts: build, test, module caches and output directories.
#   make clean
.PHONY: clean
clean: buildclean testclean modclean
	for dir in "$(COVER_DIR)" "$(DIST_DIR)" "$(TEST_DIR)"; do \
	    if [ -e "$$dir" ]; then rm -rf "$$dir"; fi \
	done

# Clear the Go build cache (delegates to go/Makefile).
#   make buildclean
.PHONY: buildclean
buildclean:
	$(MAKE) -C go $@

# Clear the Go test and fuzz caches (delegates to go/Makefile).
#   make testclean
.PHONY: testclean
testclean:
	$(MAKE) -C go $@

# Clear the Go module download cache (delegates to go/Makefile).
#   make modclean
.PHONY: modclean
modclean:
	$(MAKE) -C go $@

# Tidy go.mod: add missing and remove unused dependencies (delegates to go/Makefile).
#   make tidy
.PHONY: tidy
tidy:
	$(MAKE) -C go $@

# Create an annotated git tag from VERSION (delegates to go/Makefile).
#   make tag
.PHONY: tag
tag:
	$(MAKE) -C go $@

# Build the Docker image containing the Go toolchain and compiled binary.
# Delegates to build/docker-build.sh.
#   make docker-build
#
# Cross-platform Docker build:
#   make docker-build DOCKER_PLATFORM=linux/amd64
#   make docker-build DOCKER_PLATFORM=linux/arm64
.PHONY: docker-build
docker-build:
	DOCKER_IMAGE=$(DOCKER_IMAGE) DOCKER_PLATFORM=$(DOCKER_PLATFORM) build/docker-build.sh

# Open an interactive shell inside the Docker container.
# The first line of stdout is always the container ID.
# Delegates to build/docker-shell.sh.
#   make docker-shell
#   make docker-shell DOCKER_SHELL=/bin/sh
.PHONY: docker-shell
docker-shell:
	DOCKER_IMAGE=$(DOCKER_IMAGE) build/docker-shell.sh $(DOCKER_SHELL)

# Create a new reno release note.
#   make reno-new NAME=my-feature
.PHONY: reno-new
reno-new:
ifndef NAME
	$(error NAME is required. Usage: make reno-new NAME=my-feature)
endif
	reno new $(NAME)

# Print the assembled release notes report.
#   make reno-report
.PHONY: reno-report
reno-report:
	reno report

# List all release note fragments.
#   make reno-list
.PHONY: reno-list
reno-list:
	reno list
