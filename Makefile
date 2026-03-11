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
BUILD_VERSION:=$(shell cat ./VERSION 2>/dev/null || echo "v0.0.0")
DIST_DIR=./dist
TEST_DIR=./test
MY_BIN=wile

GORELEASER=goreleaser

DOCKER_IMAGE ?= wile
DOCKER_PLATFORM ?=
DOCKER_SHELL ?=


# Build the scheme binary for the current platform to ./dist/{os}/{arch}/wile.
# Rebuilds only when Go source files change.
#   make build
#
# Build for a specific OS/architecture:
#   make build-darwin-arm64     # macOS Apple Silicon
#   make build-darwin-amd64     # macOS Intel
#   make build-linux-arm64      # Linux ARM64
#   make build-linux-amd64      # Linux x86-64
#   make build-all              # All OS/arch combinations
#
# Docker build:
#   docker build -f docker/Dockerfile -t wile .
#   docker run wile ./dist/${TARGETOS}/${TARGETARCH}/wile --file example.scm
#
# Cross-platform Docker build:
#   docker build --platform linux/amd64 -f docker/Dockerfile -t wile .
#   docker build --platform linux/arm64 -f docker/Dockerfile -t wile .

LDFLAGS=-ldflags "-X main.BuildSHA=$(BUILD_SHA) -X main.BuildVersion=$(BUILD_VERSION)"

# Detect host OS and architecture using Go conventions
HOST_OS := $(shell $(GO) env GOOS)
RAW_ARCH := $(shell uname -m)
ifeq ($(RAW_ARCH),x86_64)
HOST_ARCH := amd64
else
HOST_ARCH := $(RAW_ARCH)
endif

# Resolve the directory where 'go install' places binaries:
# GOBIN if set, otherwise $GOPATH/bin.
GOBIN := $(shell $(GO) env GOBIN)
ifeq ($(GOBIN),)
GOBIN := $(shell $(GO) env GOPATH)/bin
endif

.PHONY: build
build: $(DIST_DIR)/$(HOST_OS)/$(HOST_ARCH)/$(MY_BIN)
	@ln -sf $(HOST_OS)/$(HOST_ARCH)/$(MY_BIN) $(DIST_DIR)/$(MY_BIN)
	@echo "Created symlink: $(DIST_DIR)/$(MY_BIN) -> $(HOST_OS)/$(HOST_ARCH)/$(MY_BIN)"

# Generic build rule for any OS/arch combination
$(DIST_DIR)/%/$(MY_BIN): $(SOURCES) $(EMBED_SOURCES)
	$(eval OS_ARCH := $(subst /, ,$*))
	$(eval TARGET_OS := $(word 1,$(OS_ARCH)))
	$(eval TARGET_ARCH := $(word 2,$(OS_ARCH)))
	@mkdir -p $(DIST_DIR)/$*
	GOOS=$(TARGET_OS) GOARCH=$(TARGET_ARCH) $(GO_BUILD) -o $(DIST_DIR)/$*/$(MY_BIN) $(LDFLAGS) ./cmd/wile

.PHONY: build-darwin-arm64
build-darwin-arm64: $(DIST_DIR)/darwin/arm64/$(MY_BIN)

.PHONY: build-darwin-amd64
build-darwin-amd64: $(DIST_DIR)/darwin/amd64/$(MY_BIN)

.PHONY: build-linux-arm64
build-linux-arm64: $(DIST_DIR)/linux/arm64/$(MY_BIN)

.PHONY: build-linux-amd64
build-linux-amd64: $(DIST_DIR)/linux/amd64/$(MY_BIN)

.PHONY: build-all
build-all: build-darwin-arm64 build-darwin-amd64 build-linux-arm64 build-linux-amd64

# Install the wile binary to the same location as 'go install' ($GOBIN or $GOPATH/bin).
#   make install
.PHONY: install
install: build
	@mkdir -p $(GOBIN)
	cp $(DIST_DIR)/$(HOST_OS)/$(HOST_ARCH)/$(MY_BIN) $(GOBIN)/$(MY_BIN)
	@echo "Installed $(MY_BIN) to $(GOBIN)/$(MY_BIN)"

# Build all embedding examples. Verifies that the public API compiles.
#   make examples
.PHONY: examples
examples:
	$(GO_BUILD) -o /dev/null ./examples/embedding/
	$(GO_BUILD) -o /dev/null ./examples/embedding/source-tracking/

# ── CI: everything that must pass before merge ──────────────────────
# Set SKIP_LINT=1 when lint is handled externally (e.g., golangci-lint-action).
#   make ci
#   make ci SKIP_LINT=1
.PHONY: ci
ci: $(if $(SKIP_LINT),,lint) build-all test covercheck readme-check examples verify-mod
	@echo "CI passed"

# ── CD: release-specific validation ─────────────────────────────────
# Run before goreleaser on tagged releases. CI already passed on merge.
#   make cd
.PHONY: cd
cd: build test-examples test-schelog smoke-test bench-regression check-readme-links
	@echo "CD passed"

# run extensive builds and tests
.PHONY: all
all: lint test covercheck readme-check build-all

# Compile tests for all packages without running them.
# Useful for verifying that tests compile after refactoring.
#   make buildtest
.PHONY: buildtest
buildtest: examples
	for dir in $(SOURCE_DIRS); do \
	    if [ -d "$$dir" ]; then \
	        $(GO_TEST) -c -o /dev/null $$dir/...; \
	    fi \
	done

# Verify that Go code blocks in README.md compile against the current API.
#   make readme-check
.PHONY: readme-check
readme-check:
	$(GO_TEST) -v -run TestREADMEGoSnippetsCompile .

# Run all tests with verbose output.
#   make test
.PHONY: test
test: build
	$(GO_TEST) ./...
	@$(MAKE) test-scheme

.PHONY: test-race
test-race: build
	$(GO_TEST) -race ./...

# Run Scheme-level test suite.
# Override SCHEME to test against different implementations:
#   make test-scheme                                    # Use Wile (default)
#   make test-scheme SCHEME=chez-scheme                 # Test with Chez Scheme
#   make test-scheme SCHEME=./old-dist/wile           # Test with old Wile version
#   make test-scheme SCHEME=/usr/local/bin/chibi-scheme # Test with Chibi-Scheme
.PHONY: test-scheme
test-scheme:
	@echo ""
	@echo "Running Scheme tests..."
	@if [ -z "$(SCHEME)" ]; then \
		SCHEME=$(DIST_DIR)/$(HOST_OS)/$(HOST_ARCH)/$(MY_BIN) ./test/run-all.sh; \
	else \
		SCHEME=$(SCHEME) ./test/run-all.sh; \
	fi

# Run all benchmarks with memory allocation statistics.
#   make bench
.PHONY: bench
bench:
	$(GO_TEST) -bench=. -benchmem -short -timeout 3m ./...

# Run the Schelog integration benchmark (logic programming stress test).
# Runs the Zebra puzzle and basic schelog operations in a single process.
# Measures time and memory usage. Useful for detecting performance regressions.
#   make bench-schelog
SCHELOG_DIR=examples/logic/schelog
SCHELOG_LIBS=-f $(SCHELOG_DIR)/schelog.scm \
             -f $(SCHELOG_DIR)/toys.scm \
             -f $(SCHELOG_DIR)/puzzle.scm \
             -f $(SCHELOG_DIR)/mapcol.scm \
             -f $(SCHELOG_DIR)/games.scm

.PHONY: bench-schelog
bench-schelog: build
	@if command -v gtime >/dev/null 2>&1; then \
		gtime -v $(DIST_DIR)/$(HOST_OS)/$(HOST_ARCH)/$(MY_BIN) -q $(SCHELOG_LIBS) -f $(SCHELOG_DIR)/benchmark.scm 2>&1; \
	elif [ -x /usr/bin/time ]; then \
		/usr/bin/time -l $(DIST_DIR)/$(HOST_OS)/$(HOST_ARCH)/$(MY_BIN) -q $(SCHELOG_LIBS) -f $(SCHELOG_DIR)/benchmark.scm 2>&1; \
	else \
		time $(DIST_DIR)/$(HOST_OS)/$(HOST_ARCH)/$(MY_BIN) -q $(SCHELOG_LIBS) -f $(SCHELOG_DIR)/benchmark.scm; \
	fi

# Run the miniKanren benchmark suite (logic programming via R7RS libraries).
# Three benchmarks: Zebra puzzle, appendo scaling, relational arithmetic.
# Exercises unification, stream interleaving, and deep recursive goals.
#   make bench-kanren
KANREN_DIR=examples/logic/kanren

.PHONY: bench-kanren
bench-kanren: build
	@if command -v gtime >/dev/null 2>&1; then \
		SCHEME_LIBRARY_PATH=lib gtime -v $(DIST_DIR)/$(HOST_OS)/$(HOST_ARCH)/$(MY_BIN) --file $(KANREN_DIR)/benchmark.scm 2>&1; \
	elif [ -x /usr/bin/time ]; then \
		SCHEME_LIBRARY_PATH=lib /usr/bin/time -l $(DIST_DIR)/$(HOST_OS)/$(HOST_ARCH)/$(MY_BIN) --file $(KANREN_DIR)/benchmark.scm 2>&1; \
	else \
		SCHEME_LIBRARY_PATH=lib time $(DIST_DIR)/$(HOST_OS)/$(HOST_ARCH)/$(MY_BIN) --file $(KANREN_DIR)/benchmark.scm; \
	fi

# Run canonical Gabriel benchmark suite (16 benchmarks).
# These benchmarks are comparable across Scheme implementations.
# Saves timestamped CSV results to examples/benchmarks/canonical-results-*.csv.
#   make bench-gabriel
.PHONY: bench-gabriel
bench-gabriel: build
	@cd examples/benchmarks && SCHEME=../../$(DIST_DIR)/$(HOST_OS)/$(HOST_ARCH)/$(MY_BIN) ./run-canonical.sh

# Run all Scheme benchmarks (canonical + non-canonical).
# Includes 21 total benchmarks, some of which are Wile-specific.
#   make bench-gabriel-all
.PHONY: bench-gabriel-all
bench-gabriel-all: build
	@cd examples/benchmarks && SCHEME=../../$(DIST_DIR)/$(HOST_OS)/$(HOST_ARCH)/$(MY_BIN) ./run-all.sh

# Run Larceny R7RS benchmark suite (standard cross-implementation benchmarks).
# Uses single iteration by default for a quick check (~60s).
# Override COUNT for full benchmark runs: make bench-larceny COUNT=
#   make bench-larceny                   # Quick run (1 iteration each)
#   make bench-larceny BENCHMARKS=quick  # Fast subset only
#   make bench-larceny COUNT= BENCHMARKS=gabriel  # Gabriel group, original counts
#   make bench-larceny BENCHMARKS="fib tak ack"   # Specific benchmarks
LARCENY_COUNT ?= 1
LARCENY_BENCHMARKS ?= all

.PHONY: bench-larceny
bench-larceny: build
	@WILE_SCHEME=$(DIST_DIR)/$(HOST_OS)/$(HOST_ARCH)/$(MY_BIN) \
		./benchmarks/larceny/bench.sh $(if $(LARCENY_COUNT),-n $(LARCENY_COUNT)) -q $(LARCENY_BENCHMARKS)

# Compare Wile against other Scheme implementations.
# Requires other Schemes installed (chez, racket, chibi, guile).
# Saves comparison results to examples/benchmarks/comparison-*.csv.
#   make bench-gabriel-compare
#   make bench-gabriel-compare BENCHMARKS="tak fib ack deriv"
.PHONY: bench-gabriel-compare
bench-gabriel-compare: build
	@cd examples/benchmarks && ./compare-schemes.sh

PROFILE_DIR=$(GO_BUILD_DIR)/profiles

# Run CPU and memory profiling on the zebra puzzle benchmark.
# The zebra puzzle is a brute-force constraint satisfaction problem that
# exercises heavy backtracking with occurs-check — a good stress test for
# the Schelog logic programming subsystem.
# View with: go tool pprof -http=:8080 ./build/profiles/zebra-cpu.prof
#   make profile-zebra
.PHONY: profile-zebra
profile-zebra:
	@mkdir -p $(PROFILE_DIR)
	$(GO_TEST) -run='^$$' -bench=BenchmarkZebraPuzzle \
		-cpuprofile=$(PROFILE_DIR)/zebra-cpu.prof \
		-memprofile=$(PROFILE_DIR)/zebra-mem.prof \
		-benchmem -timeout 30m .
	@echo "CPU profile: $(PROFILE_DIR)/zebra-cpu.prof"
	@echo "Mem profile: $(PROFILE_DIR)/zebra-mem.prof"
	@echo "View with: go tool pprof -http=:8080 $(PROFILE_DIR)/zebra-cpu.prof"

# Run CPU profiling on all benchmarks.
# Writes per-package profiles to ./build/profiles/cpu/ then merges into cpu.prof.
# View with: go tool pprof -http=:8080 ./build/profiles/cpu.prof
#   make profile-cpu
#   make profile-cpu PKG=./values/...    # Profile a single package

.PHONY: profile-cpu
profile-cpu:
	@mkdir -p $(PROFILE_DIR)/cpu
	@rm -vf $(PROFILE_DIR)/cpu/*.prof
	@for pkg in $$($(GO) list $(or $(PKG),./...)); do \
		name=$$(echo "$$pkg" | tr '/' '_'); \
		$(GO_TEST) -run='^$$' -bench=. -cpuprofile=$(PROFILE_DIR)/cpu/$$name.prof -benchmem "$$pkg"; \
	done
	@profs=$$(find $(PROFILE_DIR)/cpu -name '*.prof' -size +0c 2>/dev/null); \
	if [ -n "$$profs" ]; then \
		$(GO) tool pprof -proto $$profs > $(PROFILE_DIR)/cpu.prof; \
		echo "CPU profile: $(PROFILE_DIR)/cpu.prof"; \
		echo "View with: go tool pprof -http=:8080 $(PROFILE_DIR)/cpu.prof"; \
	else \
		echo "No benchmarks found"; \
	fi

# Run memory profiling on all benchmarks.
# Writes per-package profiles to ./build/profiles/mem/ then merges into mem.prof.
# View with: go tool pprof -http=:8080 ./build/profiles/mem.prof
#   make profile-mem
#   make profile-mem PKG=./values/...    # Profile a single package
.PHONY: profile-mem
profile-mem:
	@mkdir -p $(PROFILE_DIR)/mem
	@rm -vf $(PROFILE_DIR)/mem/*.prof
	@for pkg in $$($(GO) list $(or $(PKG),./...)); do \
		name=$$(echo "$$pkg" | tr '/' '_'); \
		$(GO_TEST) -run='^$$' -bench=. -memprofile=$(PROFILE_DIR)/mem/$$name.prof -benchmem "$$pkg"; \
	done
	@profs=$$(find $(PROFILE_DIR)/mem -name '*.prof' -size +0c 2>/dev/null); \
	if [ -n "$$profs" ]; then \
		$(GO) tool pprof -proto $$profs > $(PROFILE_DIR)/mem.prof; \
		echo "Memory profile: $(PROFILE_DIR)/mem.prof"; \
		echo "View with: go tool pprof -http=:8080 $(PROFILE_DIR)/mem.prof"; \
	else \
		echo "No benchmarks found"; \
	fi

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
	$(GO_TEST) -coverprofile=$(GO_BUILD_DIR)/coverage.out ./...
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
	for dir in "$(DIST_DIR)" "$(GO_BUILD_DIR)"; do \
	    if [ -e "$$dir" ]; then rm -rvf "$$dir"; fi \
	done; \
	for dir in $(SOURCE_DIRS); do \
	    if [ -e "$$dir" ]; then find "$$dir" -name "*.test" -type f -exec rm -v \{\} \; ; fi \
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

# Validate the .goreleaser.yml configuration.
#   make release-check
.PHONY: release-check
release-check:
	$(GORELEASER) check

# Build a local snapshot release without publishing.
# Produces archives and checksums in ./dist/.
#   make release-snapshot
.PHONY: release-snapshot
release-snapshot:
	$(GORELEASER) release --snapshot --clean

# Build and publish a release to GitHub.
# Requires a clean git tag (v*) on HEAD and GITHUB_TOKEN set.
#   make release
.PHONY: release
release:
	$(GORELEASER) release --clean

# Verify go.sum integrity.
#   make verify-mod
.PHONY: verify-mod
verify-mod:
	$(GO_MOD) verify

# Run all Scheme examples (non-benchmark, non-schelog) and verify they exit 0.
#   make test-examples
.PHONY: test-examples
test-examples: build
	@$(SH_TOOLS_DIR)/run-examples.sh $(DIST_DIR)/$(HOST_OS)/$(HOST_ARCH)/$(MY_BIN)

# Run schelog validation suite.
#   make test-schelog
.PHONY: test-schelog
test-schelog: build
	@examples/logic/schelog/run-all-tests.sh

# Smoke test: verify the built binary starts, prints version, and evaluates.
#   make smoke-test
.PHONY: smoke-test
smoke-test: build
	@$(SH_TOOLS_DIR)/smoke-test.sh $(DIST_DIR)/$(HOST_OS)/$(HOST_ARCH)/$(MY_BIN)

# Run Gabriel benchmarks and compare against checked-in baseline.
# Fails if geo-mean regresses more than 5%.
#   make bench-regression
.PHONY: bench-regression
bench-regression: build
	@cd examples/benchmarks && \
		SCHEME=../../$(DIST_DIR)/$(HOST_OS)/$(HOST_ARCH)/$(MY_BIN) \
		BASELINE=canonical-baseline.csv \
		THRESHOLD=5 \
		../../$(SH_TOOLS_DIR)/bench-regression.sh

# Validate links in README.md.
#   make check-readme-links
.PHONY: check-readme-links
check-readme-links:
	@$(SH_TOOLS_DIR)/check-readme-links.sh README.md

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
