FROM fpco/stack-build:lts-14.15 as build
RUN mkdir /opt/build
WORKDIR /opt/build
COPY stack.yaml /opt/build
COPY Dod.cabal /opt/build
RUN stack build --system-ghc --only-dependencies
COPY . /opt/build
RUN stack build
FROM ubuntu:18.04
RUN mkdir -p /opt/dod
ARG BINARY_PATH
WORKDIR /opt/dod
COPY --from=build /opt/build/.stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/dod/dod .
CMD ["/opt/dod/dod"]
