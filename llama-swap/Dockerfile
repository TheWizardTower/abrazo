FROM docker.io/rocm/dev-ubuntu-24.04:latest

ARG LLAMA_REF=master
ARG AMDGPU_TARGETS=gfx1151

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      git cmake ninja-build ccache \
      libcurl4-openssl-dev libssl-dev ca-certificates && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /src
RUN git clone --depth 1 --branch ${LLAMA_REF} https://github.com/ggml-org/llama.cpp.git

WORKDIR /src/llama.cpp
RUN cmake -S . -B build -G Ninja \
      -DCMAKE_BUILD_TYPE=Release \
      -DGGML_HIP=ON \
      -DAMDGPU_TARGETS=${AMDGPU_TARGETS} \
      -DCMAKE_HIP_ARCHITECTURES=${AMDGPU_TARGETS} \
      -DLLAMA_CURL=ON && \
    cmake --build build -j "$(nproc)" && \
    cmake --install build --prefix /opt/llama && \
    rm -rf /src/llama.cpp/build

ENV PATH=/opt/llama/bin:${PATH}
WORKDIR /models
EXPOSE 8080
ENTRYPOINT ["/opt/llama/bin/llama-server"]
CMD ["--help"]
