PROJECT=scheme
SOURCES := $(shell go list -f '{{range .GoFiles}}{{$$.Dir}}/{{.}} {{end}}' ./...)
SOURCES_TEST := $(shell go list -f '{{range .TestGoFiles}}{{$$.Dir}}/{{.}} {{end}}' ./...)
SUBDIRS=cmd define_syntax environment forms machine match parser runtime syntax tokenizer utils values

.PHONY: all
all: $(SUBDIRS)

.PHONY: test
test:
	go test -v ./...

.PHONY: cover
cover:
	mkdir -p /Users/aalpar/projects/wile/cover
	go test -coverprofile=/Users/aalpar/projects/wile/cover/coverage.out ./...
	go tool cover -html=/Users/aalpar/projects/wile/cover/coverage.out -o /Users/aalpar/projects/wile/cover/coverage.html

go.sum: go.mod $(SOURCES)
	touch $@
	go mod tidy

.PHONY: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@

.PHONY: build
build: $(SOURCES)
	go build -v ./...

.PHONY: lint
lint:
	golangci-lint run ./...

.PHONY: lintfix
lintfix:
	golangci-lint run --fix ./...

.PHONY: clean
clean:
	go clean -x -cache -fuzzcache -testcache

.PHONY: modclean
modclean:
	go clean -x -modcache

.PHONY: tidy
tidy:
	go mod tidy -x

.PHONY: fmt
fmt:
	gofmt -w $(SOURCES); \

.PHONY: buildtest
buildtest:
	@for pkg in $$(go list ./...); do \
		go test -c -o /dev/null $$pkg 2>&1 || exit 1; \
	done
