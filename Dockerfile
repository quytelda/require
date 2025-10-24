FROM docker.io/library/alpine:latest

# Set up build user
RUN adduser -h /data -D build

# Install packages
RUN set -eux \
    && apk update \
    && apk add cabal g++ git gmp-dev gmp-static libc-dev zlib-dev zlib-static

USER build:build

# Add project files
ADD --chown=build:build . /data/require/
WORKDIR /data/require

RUN set -eux \
    && cabal update \
    && cabal build

EXPOSE 11073/tcp

ENTRYPOINT ["/usr/bin/cabal", "run"]
