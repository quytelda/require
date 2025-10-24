FROM docker.io/library/alpine:latest

# Set up build user
RUN adduser -h /data -D build

# Install packages
RUN set -eux \
    && apk update \
    && apk add cabal g++ git gmp-dev gmp-static libc-dev zlib-dev zlib-static

USER build:build

RUN git clone https://github.com/quytelda/require.git /data/require

WORKDIR /data/require

RUN set -eux \
    && cabal update \
    && cabal build --enable-executable-static
