FROM debian:buster AS solvers

# Install needed packages for building
RUN apt-get update \
    && apt-get install -y curl cmake gcc g++ git libreadline-dev unzip
RUN useradd -m user
RUN install -d -o user -g user /solvers
USER user
WORKDIR /solvers
RUN mkdir -p rootfs/usr/local/bin

# Get Z3 4.8.8 from GitHub
RUN curl -L https://github.com/Z3Prover/z3/releases/download/z3-4.8.8/z3-4.8.8-x64-ubuntu-16.04.zip --output z3.zip
RUN unzip z3.zip
RUN mv z3-*/bin/z3 rootfs/usr/local/bin

# Build abc from GitHub. (Latest version.)
RUN git clone https://github.com/berkeley-abc/abc.git
RUN cd abc && make -j$(nproc)
RUN cp abc/abc rootfs/usr/local/bin

# Build Boolector release 3.2.1 from source
RUN curl -L https://github.com/Boolector/boolector/archive/3.2.1.tar.gz | tar xz
RUN cd boolector* && ./contrib/setup-lingeling.sh && ./contrib/setup-btor2tools.sh && ./configure.sh && cd build && make -j$(nproc)
RUN cp boolector*/build/bin/boolector rootfs/usr/local/bin

# Install Yices 2.6.2
RUN curl -L https://yices.csl.sri.com/releases/2.6.2/yices-2.6.2-x86_64-pc-linux-gnu-static-gmp.tar.gz | tar xz
RUN cp yices*/bin/yices-smt2 rootfs/usr/local/bin \
    && cp yices*/bin/yices rootfs/usr/local/bin

# Install CVC4 1.8
RUN curl -L https://github.com/CVC4/CVC4/releases/download/1.8/cvc4-1.8-x86_64-linux-opt --output rootfs/usr/local/bin/cvc4

# Install MathSAT 5.6.3 - Uncomment if you are in compliance with MathSAT's license.
# RUN curl -L https://mathsat.fbk.eu/download.php?file=mathsat-5.6.3-linux-x86_64.tar.gz | tar xz
# RUN cp mathsat-5.6.3-linux-x86_64/bin/mathsat rootfs/usr/local/bin

# Set executable and run tests
RUN chmod +x rootfs/usr/local/bin/*

FROM haskell:8.8.4 AS build
USER root
RUN apt-get update && apt-get install -y wget libncurses-dev unzip
COPY --from=solvers /solvers/rootfs /
RUN useradd -m saw
COPY --chown=saw:saw . /home/saw
USER saw
WORKDIR /home/saw
ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8
COPY cabal.GHC-8.8.4.config cabal.project.freeze
RUN cabal v2-update && cabal v2-build -j exe:saw-remote-api
RUN mkdir -p /home/saw/rootfs/usr/local/bin
RUN cp $(cabal v2-exec which saw-remote-api) /home/saw/rootfs/usr/local/bin/saw-remote-api
WORKDIR /home/saw
USER root
RUN chown -R root:root /home/saw/rootfs

FROM debian:buster-slim
RUN apt-get update \
    && apt-get install -y libgmp10 libgomp1 libffi6 wget libncurses5 unzip
COPY --from=build /home/saw/rootfs /
COPY --from=solvers /solvers/rootfs /
RUN useradd -m saw && chown -R saw:saw /home/saw
USER saw
ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8
ENTRYPOINT ["/usr/local/bin/saw-remote-api"]
CMD ["http", "--host", "0.0.0.0", "--port", "8080", "/"]
EXPOSE 8080
