# Dockerfile for building the Wile Scheme interpreter
# Includes Go toolchain and GNU development tools
#
# Build from docker/ directory:
#   ./build.sh
# Or manually:
#   docker build -f Dockerfile -t wile ..
#
#   docker buildx build --platform linux/amd64,linux/arm64 -t your-username/multiarch-image:latest . --push
#

FROM golang:1.25-bookworm

# TARGETOS and TARGETARCH are set automatically by Docker from --platform (e.g., linux/amd64)
ARG TARGETOS=linux
ARG TARGETARCH
ENV TARGETOS=${TARGETOS}
ENV TARGETARCH=${TARGETARCH}

# Install GNU development tools and build essentials
RUN apt-get update && apt-get install -y --no-install-recommends \
    # Core Tools
    man \
    gawk \
    less \
    vim \
    emacs \
    tar \
    wdiff \
    colordiff \
    diffutils \
    # Core GNU build tools
    build-essential \
    make \
    autoconf \
    automake \
    libtool \
    # Compilers and linkers
    gcc \
    g++ \
    gccgo \
    binutils \
    # Debugging and profiling
    gdb \
    valgrind \
    strace \
    # Text processing (useful for Scheme development)
    gawk \
    sed \
    # Version control
    git \
    # Archive tools
    tar \
    gzip \
    bzip2 \
    xz-utils \
    # Additional development libraries
    libreadline-dev \
    libncurses-dev \
    # Linting (golangci-lint)
    curl

# Install golangci-lint
RUN curl -sSfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh | sh -s -- -b /usr/local/bin

# Set working directory
WORKDIR /wile

# Copy go module files first for better layer caching
COPY go.mod go.sum ./

# Download dependencies
RUN go mod download

# Copy the rest of the source
COPY . .

# Build the project for the target OS/architecture
RUN make build-${TARGETOS}-${TARGETARCH}

# Verify build
RUN ./dist/${TARGETOS}/${TARGETARCH}/wile --version || true

# Default command: run tests
CMD ["make", "test"]
