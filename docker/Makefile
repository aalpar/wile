IMAGE_NAME=wile
DOCKER=docker
PLATFORMS=linux/amd64,linux/arm64

# Optional: set ARCH=amd64 or ARCH=arm64 to target a specific architecture
# Example: make build ARCH=amd64
#          make shell ARCH=arm64
ARCH?=
ifdef ARCH
  PLATFORM_FLAG=--platform linux/$(ARCH)
  TAG_SUFFIX=:$(ARCH)
else
  PLATFORM_FLAG=
  TAG_SUFFIX=
endif

.PHONY: build
build:
	$(DOCKER) build -f Dockerfile $(PLATFORM_FLAG) -t $(IMAGE_NAME)$(TAG_SUFFIX) ..

.PHONY: build-all
build-all:
	$(DOCKER) buildx build -f Dockerfile --platform $(PLATFORMS) -t $(IMAGE_NAME) ..

.PHONY: push
push:
	$(DOCKER) buildx build -f Dockerfile --platform $(PLATFORMS) -t $(IMAGE_NAME) --push ..

.PHONY: test
test: build
	$(DOCKER) run --rm $(PLATFORM_FLAG) $(IMAGE_NAME)$(TAG_SUFFIX)

.PHONY: shell
shell: build
	$(DOCKER) run --rm -it $(PLATFORM_FLAG) $(IMAGE_NAME)$(TAG_SUFFIX) bash

.PHONY: repl
repl: build
	$(DOCKER) run --rm -it $(PLATFORM_FLAG) $(IMAGE_NAME)$(TAG_SUFFIX) bash -c './dist/$$TARGETOS/$$TARGETARCH/wile'

.PHONY: dev
dev: build
	$(DOCKER) run --rm -it $(PLATFORM_FLAG) -v $(CURDIR)/..:/wile $(IMAGE_NAME)$(TAG_SUFFIX) bash

.PHONY: clean
clean:
	$(DOCKER) rmi $(IMAGE_NAME) 2>/dev/null || true
	$(DOCKER) rmi $(IMAGE_NAME):amd64 2>/dev/null || true
	$(DOCKER) rmi $(IMAGE_NAME):arm64 2>/dev/null || true

