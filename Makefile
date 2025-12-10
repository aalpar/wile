PROJECT=scheme
SUBDIRS=go
DIST_DIR=dist

.PHONY: build
build:
	@mkdir -p $(DIST_DIR)
	cd go && go build -o ../$(DIST_DIR)/$(PROJECT) ./cmd

.PHONY: all
all: $(SUBDIRS)

.PHONY: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@

.PHONY: test
test: $(SUBDIRS)
	make -C go test

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


