ARG BASE_IMAGE=noaaepic/ufschem-spack-base-ubuntu-gcc-13-dev:latest
FROM ${BASE_IMAGE}

SHELL ["/bin/bash", "-c"]

# Kokkos support toggle; matches the CMake option of the same name.
# MUSICA requires the Kokkos-backed C++ core, so it is disabled when OFF.
ARG CATCHEM_ENABLE_KOKKOS=ON

# No apt Kokkos: the distro package predates CATCHEM_KOKKOS_MIN_VERSION, so
# Kokkos-ON builds always use CMake's pinned FetchContent fallback (verified
# from Kokkos::print_configuration in the test logs).
USER root
RUN apt-get update --yes && \
    apt-get install --yes libyaml-cpp-dev

COPY . /opt/catchem/src

RUN source /opt/ufschem/spack-stack/setup.sh && \
    spack env activate ufschem && \
    cd /opt/catchem/src && \
    mkdir build && \
    cd build && \
    cmake .. \
      -DCATCHEM_BUILD_TESTING=ON \
      -DCATCHEM_BUILD_NUOPC=ON \
      -DCATCHEM_BUILD_MUSICA=${CATCHEM_ENABLE_KOKKOS} \
      -DCATCHEM_ENABLE_KOKKOS=${CATCHEM_ENABLE_KOKKOS} \
      -DCMAKE_INSTALL_PREFIX=/opt/catchem/install \
      -DCMAKE_BUILD_TYPE=Debug && \
    make -j$(nproc) && \
    make install && \
    ctest --output-on-failure

RUN cat <<EOF >> ~/.bashrc
# enable spack env by default
source /opt/ufschem/spack-stack/setup.sh
spack env activate ufschem
EOF

WORKDIR /opt

CMD ["/bin/bash"]
