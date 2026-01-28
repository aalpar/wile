COVER_DIR=cover
DIST_DIR=dist
TEST_DIR=test
SUBDIRS=$(shell cd go && go list -f '{{.Dir}}' ./...)

.PHONY: build
build:
	$(MAKE) -C go

.PHONY: buildtest
buildtest: go
	$(MAKE) -C $< $@

.PHONY: test
test: go
	mkdir -p $(TEST_DIR)
	$(MAKE) -C $< $@

.PHONY: cover
cover: go
	mkdir -p $(COVER_DIR)
	$(MAKE) -C $< $@

.PHONY: fix
fix:
	$(MAKE) -C go $@

.PHONY: lint
lint:
	$(MAKE) -C go $@

.PHONY: format
format:
	$(MAKE) -C go $@

.PHONY: clean
clean: buildclean testclean modclean
	for dir in "$(COVER_DIR)" "$(DIST_DIR)" "$(TEST_DIR)"; do \
	    if [ -e "$$dir" ]; then rm -rf "$$dir"; fi \
	done

.PHONY: buildclean
buildclean:
	$(MAKE) -C go $@

.PHONY: testclean
testclean:
	$(MAKE) -C go $@

.PHONY: modclean
modclean:
	$(MAKE) -C go $@

.PHONY: tidy
tidy:
	$(MAKE) -C go $@

.PHONY: tag
tag:
	$(MAKE) -C go $@

# Release notes using reno
# Usage: make reno-new NAME=my-feature
.PHONY: reno-new
reno-new:
ifndef NAME
	$(error NAME is required. Usage: make reno-new NAME=my-feature)
endif
	reno new $(NAME)

.PHONY: reno-report
reno-report:
	reno report

.PHONY: reno-list
reno-list:
	reno list
