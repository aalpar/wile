PROJECT=scheme
SOURCES=$(sort $(wildcard *.go) $(wildcard **/*.go))
SOURCES_TEST=$(shell find . \( -name "*_test.go" -print \) -o \( -name "cmd" -prune \))
SUBDIRS=cmd define_syntax environment machine match parser runtime syntax tokenizer transformer utils values

.PHONY: all
all: $(SUBDIRS)

.PHONY: test
test:
	go test -v ./...

go.sum: go.mod $(SOURCES)
	touch $@
	go mod tidy

.PHONY: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@

.PHONY: build
build: $(SOURCES)
	go build -v ./...

.PHONY: clean
clean:
	go clean -x -cache -fuzzcache -testcache

.PHONY: modclean
modclean:
	go clean -x -modcache

.PHONY: tidy
tidy:
	go mod tidy -x


