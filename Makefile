PROJECT=scheme
SUBDIRS=go
DIST_DIR=dist
SOURCES=$(shell cd go && go list -f '{{range .GoFiles}}{{$$.ImportPath}}/{{.}} {{end}}' ./... | sed 's|wile/|go/|g')

.PHONY: build
build:
	@mkdir -p $(DIST_DIR)
	cd go && go build -o ../$(DIST_DIR)/$(PROJECT) ./cmd

.PHONY: lint
lint:
	cd go && golangci-lint run ./...

.PHONY: lintfix
lintfix:
	cd go && golangci-lint run --fix ./...

.PHONY: all
all: $(SUBDIRS)

.PHONY: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@

.PHONY: test
test: $(SUBDIRS)
	make -C go test

.PHONY: cover
cover:
	make -C go cover

.PHONY: clean
clean:
	make -C go clean
	rm -rf $(DIST_DIR)

.PHONY: modclean
modclean:
	make -C go modclean

.PHONY: tidy
tidy:
	make -C go tidy

.PHONY: fmt
fmt:
	make -C go fmt

.PHONY: buildtest
buildtest:
	make -C go buildtest
