# tools/Makefile — build, test, and run the Wile tool binaries.
#
# Everything under tools/ that has to be compiled, tested, or invoked lives
# here, so the root Makefile does not accumulate one target per tool. The root
# delegates with `$(MAKE) -C tools <target>`; this file is also usable directly
# from this directory.
#
# Recursive rather than `include`d: the tools operate ON the tree rather than
# being part of the shipped binary, and a separate make process keeps their
# target names from colliding with the root's (`build`, `test`, `clean` all
# exist in both, and mean different things).
#
# Two directories matter and they are not the same one:
#   - the tools live under ./cmd, relative to THIS file
#   - the tree they analyze is $(ROOT), which the root Makefile overrides
# Reports print paths relative to $(ROOT), so overriding it is what keeps a
# citation like pkg/machine/peephole.go:282 repo-relative instead of ../pkg/...

GO ?= go

# The tree the tools operate on. The repo root by default, since that is what
# every current tool wants; overridable to scope a run to one package.
ROOT ?= ..

# Every Go package under tools/: the commands in ./cmd and the libraries beside
# them. Wildcards rather than an explicit list: adding a package should not
# require editing this file.
#
# The libraries (./cxmeasure, ./deadscan) are importable, and wile-goast imports
# them — it is the static-analysis tool Wile runs on its own source, so the
# measurements belong where its call-graph and CFG analyses can join them.
TOOL_PKGS = ./...

# cxmeasure defaults. TOP bounds every ranking; ARMS names one function for the
# wide-structure test and has no sensible default, so its target requires it.
TOP ?= 25
ARMS ?=

# The nesting gate. Set to the current worst depth in the tree, so it forbids
# anything DEEPER while the depth>=5 tail is flattened; ratchet it down (5, then
# 4) as those functions are refactored. The blocker on dropping it to 5 is
# Matcher.MatchSyntaxWithLiterals — see the complexity inventory plan.
NEST_MAX ?= 6

.PHONY: all
all: build

# Compile every tool, discarding the binaries. This is a build check, not an
# install: the tools are invoked with `go run`, so nothing needs to land on
# disk, but a tool that stopped compiling should still fail a build.
#   make -C tools build
.PHONY: build
build:
	$(GO) build -o /dev/null $(TOOL_PKGS)

# Run the tools' own tests. The root's `go test ./...` already covers these, so
# this target exists for working on a tool in isolation, not for CI.
#   make -C tools test
.PHONY: test
test:
	$(GO) test $(TOOL_PKGS)

# Vet every tool package.
#   make -C tools vet
.PHONY: vet
vet:
	$(GO) vet $(TOOL_PKGS)

# The project's standalone lint passes, run against $(ROOT). These enforce
# conventions the gocritic/ruleguard DSL cannot express:
#   - singlelinefunclint: the no-single-line-function rule (a positional rule)
#   - typeswitchlint:     exhaustiveness of //exhaustive-marked values.Value switches
#   - nestinglint:        the shallow-nesting rule (CODING_STYLE.md "Keep
#                         Indentation Shallow"), gated at -max $(NEST_MAX)
#
# Each exits non-zero on a violation, so this target gates the build; the root's
# `lint` calls it after golangci-lint. Unlike the complexity reports above,
# these are gates, which is why they run from $(ROOT) with `.`: a finding must
# cite pkg/foo.go:42, not ../pkg/foo.go:42.
#   make -C tools lint
#   make -C tools lint NEST_MAX=5
.PHONY: lint
lint:
	cd $(ROOT) && $(GO) run ./tools/cmd/singlelinefunclint .
	cd $(ROOT) && $(GO) run ./tools/cmd/typeswitchlint .
	cd $(ROOT) && $(GO) run ./tools/cmd/nestinglint -max $(NEST_MAX) .

# Rank the worst individual functions in $(ROOT) by cognitive complexity.
# The cyc column is there to be read against cog: when cyclomatic is the larger
# of the two, the function is WIDE (a dispatch table, a type switch), and
# `complexity-arms` should be run before filing it as a refactoring candidate.
#   make -C tools complexity
#   make -C tools complexity TOP=50
.PHONY: complexity
complexity:
	@$(GO) run ./cmd/cxmeasure -by func -top $(TOP) $(ROOT)

# Rank files by cognitive complexity per function: how hard the average
# function in the file is, rather than how many lines it has.
#   make -C tools complexity-files
.PHONY: complexity-files
complexity-files:
	@$(GO) run ./cmd/cxmeasure -by file -top $(TOP) $(ROOT)

# Rank packages by cognitive complexity per function-LOC.
#   make -C tools complexity-packages
.PHONY: complexity-packages
complexity-packages:
	@$(GO) run ./cmd/cxmeasure -by pkg -top $(TOP) $(ROOT)

# Report the exported symbols no production code consumes. A REPORTER, not a
# gate — `deadcode` does not run on this toolchain and `unused` skips exported
# identifiers, so this is the only measurement of the exported surface.
#
# MODULES is the list of consumers whose references count. It must name every
# workspace module that builds against wile, or the ext column is zero and the
# dead list is overstated; the report warns when it saw only one. Override it to
# scope a run.
#   make -C tools deadscan
#   make -C tools deadscan MODULES=./pkg/environment/...
MODULES ?= github.com/aalpar/wile/... github.com/aalpar/wile-goast/... github.com/aalpar/wile-extension-example/...

.PHONY: deadscan
deadscan:
	@cd $(ROOT) && $(GO) run ./tools/cmd/deadscan -dir . $(MODULES)

# The same scan emitted as JSON, for a script rather than a reader.
#   make -C tools deadscan-json > syms.json
.PHONY: deadscan-json
deadscan-json:
	@cd $(ROOT) && $(GO) run ./tools/cmd/deadscan -json -dir . $(MODULES)

# The wide-structure test: re-measure one function's switch arms as though each
# were already its own function. A large collapse means the function is wide
# rather than complex, and splitting it would relocate the score without making
# any arm easier to read.
#   make -C tools complexity-arms ARMS=pkg/machine/machine_context.go:Run
.PHONY: complexity-arms
complexity-arms:
	@test -n "$(ARMS)" || { \
	    echo "complexity-arms needs ARMS=<file.go>:<FuncName>" >&2; \
	    echo "  e.g. make -C tools complexity-arms ARMS=pkg/machine/machine_context.go:Run" >&2; \
	    exit 2; \
	}
	@cd $(ROOT) && $(GO) run ./tools/cmd/cxmeasure -arms "$(ARMS)"

# Remove the Go build cache entries for the tools. Deliberately narrow: the
# root's `clean` owns dist/ and build/, and nothing here writes to either.
.PHONY: clean
clean:
	$(GO) clean $(TOOL_PKGS)
