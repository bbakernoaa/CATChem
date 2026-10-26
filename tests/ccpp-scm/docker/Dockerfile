ARG BASE_IMAGE=ghcr.io/noaa-emc/ci-common-build-cache/ufs-utils-ubuntu-24.04-gcc-13-mpich-x@sha256:081212acfb78e790a3ee6471cd3dab41737f60214678e9a2c428fe8b6fdedaa7
FROM ${BASE_IMAGE}

USER root

# SCM's run_scm.py needs f90nml, NumPy and netCDF4. The base image already supplies
# GCC 13, MPICH, NetCDF, bacio, ip/sp and w3emc through its Spack view.
RUN apt-get update \
    && apt-get install -y --no-install-recommends python3-pip time wget libyaml-cpp-dev \
    && rm -rf /var/lib/apt/lists/*

RUN python3 -m pip install --break-system-packages --no-cache-dir \
    f90nml==1.4.4 numpy==1.26.4 netCDF4==1.6.5 pyyaml==6.0.2
