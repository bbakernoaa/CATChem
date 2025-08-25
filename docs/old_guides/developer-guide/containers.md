# Container Guide

This guide covers containerized deployment of CATChem using Docker, Singularity, and Apptainer.

## Overview

Container technologies provide several benefits for CATChem deployment:

- **Reproducible environments** across different systems
- **Simplified dependency management** with pre-built images
- **Portable deployment** from workstations to HPC systems
- **Consistent runtime environment** for operational use
- **Easy distribution** of complete software stack

## Supported Container Platforms

- **Docker** - Development and testing environments
- **Singularity/Apptainer** - HPC and production environments
- **Podman** - Alternative to Docker for rootless containers
- **Charliecloud** - Lightweight HPC containers

## Docker Containers

### Pre-built Images

**Official CATChem Images:**
```bash
# Pull latest stable version
docker pull noaagsl/catchem:latest

# Pull specific version
docker pull noaagsl/catchem:v2.1.0

# Pull development version
docker pull noaagsl/catchem:develop
```

**Available Tags:**
- `latest` - Latest stable release
- `v2.1.0`, `v2.0.0` - Specific versions
- `develop` - Development branch
- `gpu` - GPU-enabled version
- `mpi` - MPI-enabled version

### Running CATChem with Docker

**Basic Usage:**
```bash
# Run with input files
docker run -v $PWD:/data noaagsl/catchem:latest \
  catchem_driver /data/config.yml

# Interactive shell
docker run -it --rm noaagsl/catchem:latest bash

# With GPU support
docker run --gpus all -v $PWD:/data noaagsl/catchem:gpu \
  catchem_driver /data/config.yml
```

**Advanced Usage:**
```bash
# With specific resource limits
docker run --memory=4g --cpus=2 \
  -v $PWD:/data noaagsl/catchem:latest \
  catchem_driver /data/config.yml

# With network configuration
docker run --network=host \
  -v $PWD:/data noaagsl/catchem:mpi \
  mpirun -np 4 catchem_driver /data/config.yml
```

### Building Custom Docker Images

**Basic Dockerfile:**
```docker
FROM ubuntu:22.04

# Install system dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    gfortran \
    cmake \
    git \
    libopenmpi-dev \
    libnetcdff-dev \
    libhdf5-dev \
    && rm -rf /var/lib/apt/lists/*

# Set environment variables
ENV FC=gfortran \
    CC=gcc \
    CXX=g++ \
    OMPI_ALLOW_RUN_AS_ROOT=1 \
    OMPI_ALLOW_RUN_AS_ROOT_CONFIRM=1

# Create working directory
WORKDIR /opt/catchem

# Copy source code
COPY . .

# Build CATChem
RUN mkdir build && cd build && \
    cmake .. -DCMAKE_BUILD_TYPE=Release \
             -DMPI=ON \
             -DOPENMP=ON && \
    make -j$(nproc) && \
    make install

# Set runtime environment
ENV PATH=/opt/catchem/build/bin:$PATH
WORKDIR /data

# Default command
CMD ["catchem_driver", "--help"]
```

**GPU-Enabled Dockerfile:**
```docker
FROM nvidia/cuda:11.8-devel-ubuntu22.04

# Install NVIDIA HPC SDK
RUN wget https://developer.download.nvidia.com/hpc-sdk/22.7/nvhpc_2022_227_Linux_x86_64_cuda_11.7.tar.gz && \
    tar xpzf nvhpc_2022_227_Linux_x86_64_cuda_11.7.tar.gz && \
    nvhpc_2022_227_Linux_x86_64_cuda_11.7/install && \
    rm -rf nvhpc_2022_227_Linux_x86_64_cuda_11.7*

# Set compiler environment
ENV NVHPC_ROOT=/opt/nvidia/hpc_sdk/Linux_x86_64/22.7 \
    PATH=/opt/nvidia/hpc_sdk/Linux_x86_64/22.7/compilers/bin:$PATH \
    FC=nvfortran \
    CC=nvc \
    CXX=nvc++

# Build with GPU support
RUN cmake .. -DCMAKE_BUILD_TYPE=Release \
             -DGPU=ON \
             -DACCELERATOR=OPENACC \
             -DMPI=ON
```

**Multi-stage Build:**
```docker
# Build stage
FROM ubuntu:22.04 AS builder

RUN apt-get update && apt-get install -y \
    build-essential gfortran cmake git \
    libopenmpi-dev libnetcdff-dev libhdf5-dev

WORKDIR /build
COPY . .
RUN mkdir build && cd build && \
    cmake .. -DCMAKE_BUILD_TYPE=Release && \
    make -j$(nproc)

# Runtime stage
FROM ubuntu:22.04

RUN apt-get update && apt-get install -y \
    libopenmpi3 libnetcdff7 libhdf5-103 \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/build/bin/* /usr/local/bin/
COPY --from=builder /build/build/lib/* /usr/local/lib/

WORKDIR /data
CMD ["catchem_driver", "--help"]
```

## Singularity/Apptainer Containers

### Converting from Docker

**Basic Conversion:**
```bash
# Build from Docker image
singularity build catchem.sif docker://noaagsl/catchem:latest

# Build from Docker Hub
apptainer build catchem.sif docker://noaagsl/catchem:latest
```

### Definition Files

**Basic Definition:**
```singularity
Bootstrap: docker
From: ubuntu:22.04

%environment
    export PATH=/opt/catchem/bin:$PATH
    export LD_LIBRARY_PATH=/opt/catchem/lib:$LD_LIBRARY_PATH
    export OMPI_ALLOW_RUN_AS_ROOT=1
    export OMPI_ALLOW_RUN_AS_ROOT_CONFIRM=1

%post
    # Update and install dependencies
    apt-get update && apt-get install -y \
        build-essential \
        gfortran \
        cmake \
        git \
        wget \
        libopenmpi-dev \
        libnetcdff-dev \
        libhdf5-dev

    # Build CATChem
    cd /opt
    git clone https://github.com/NOAA-GSL/CATChem.git catchem-src
    cd catchem-src
    mkdir build && cd build

    cmake .. \
        -DCMAKE_INSTALL_PREFIX=/opt/catchem \
        -DCMAKE_BUILD_TYPE=Release \
        -DMPI=ON \
        -DOPENMP=ON

    make -j$(nproc)
    make install

    # Cleanup
    cd /
    rm -rf /opt/catchem-src

%runscript
    exec /opt/catchem/bin/catchem_driver "$@"

%help
    CATChem - Community Atmospheric Transport Chemistry Model

    Usage:
        singularity run catchem.sif [config.yml]
        singularity exec catchem.sif catchem_driver --help
```

**MPI-Enabled Definition:**
```singularity
Bootstrap: docker
From: ubuntu:22.04

%environment
    export PATH=/opt/catchem/bin:$PATH
    export LD_LIBRARY_PATH=/opt/catchem/lib:$LD_LIBRARY_PATH

%post
    # Install MPI (matching host system)
    apt-get update && apt-get install -y \
        libopenmpi-dev \
        openmpi-bin \
        openmpi-common

    # Build CATChem with MPI
    # ... (build steps similar to basic definition)

%runscript
    # Allow MPI execution
    if [ $# -eq 0 ]; then
        echo "Usage: mpirun -np N singularity exec catchem.sif catchem_driver config.yml"
        exit 1
    fi
    exec /opt/catchem/bin/catchem_driver "$@"
```

**GPU Definition:**
```singularity
Bootstrap: docker
From: nvidia/cuda:11.8-devel-ubuntu22.04

%environment
    export CUDA_ROOT=/usr/local/cuda
    export PATH=/opt/catchem/bin:$CUDA_ROOT/bin:$PATH
    export LD_LIBRARY_PATH=/opt/catchem/lib:$CUDA_ROOT/lib64:$LD_LIBRARY_PATH

%post
    # Install NVIDIA HPC SDK
    wget -q https://developer.download.nvidia.com/hpc-sdk/22.7/nvhpc_2022_227_Linux_x86_64_cuda_11.7.tar.gz
    tar xzf nvhpc_2022_227_Linux_x86_64_cuda_11.7.tar.gz
    cd nvhpc_2022_227_Linux_x86_64_cuda_11.7
    ./install -silent

    # Set build environment
    export NVHPC_ROOT=/opt/nvidia/hpc_sdk/Linux_x86_64/22.7
    export PATH=$NVHPC_ROOT/compilers/bin:$PATH

    # Build with GPU support
    # ... (build with GPU flags)

%runscript
    exec /opt/catchem/bin/catchem_driver "$@"
```

### Running Singularity Containers

**Basic Execution:**
```bash
# Build container
singularity build catchem.sif catchem.def

# Run with bind mounts
singularity run -B $PWD:/data catchem.sif /data/config.yml

# Interactive shell
singularity shell -B $PWD:/data catchem.sif

# Execute specific command
singularity exec -B $PWD:/data catchem.sif catchem_driver config.yml
```

**MPI Execution:**
```bash
# Hybrid approach (MPI outside, OpenMP inside)
mpirun -np 4 singularity exec catchem.sif catchem_driver config.yml

# Pure container MPI
singularity exec catchem.sif mpirun -np 4 catchem_driver config.yml
```

**GPU Execution:**
```bash
# With NVIDIA GPU support
singularity run --nv -B $PWD:/data catchem.sif config.yml

# With AMD GPU support (ROCm)
singularity run --rocm -B $PWD:/data catchem.sif config.yml
```

## HPC Integration

### SLURM Integration

**Job Script with Singularity:**
```bash
#!/bin/bash
#SBATCH --job-name=catchem-container
#SBATCH --ntasks=16
#SBATCH --ntasks-per-node=4
#SBATCH --time=02:00:00
#SBATCH --partition=compute

# Load Singularity module
module load singularity

# Set bind paths
export SINGULARITY_BIND="$PWD:/data,$SCRATCH:/scratch"

# Run with MPI
srun singularity exec catchem.sif catchem_driver /data/config.yml
```

**GPU Job Script:**
```bash
#!/bin/bash
#SBATCH --job-name=catchem-gpu
#SBATCH --ntasks=4
#SBATCH --gpus-per-task=1
#SBATCH --time=01:00:00
#SBATCH --partition=gpu

module load singularity

# Run with GPU support
srun --gpu-bind=closest singularity exec --nv catchem.sif \
  catchem_driver --gpu /data/config.yml
```

### PBS/Torque Integration

**Job Script:**
```bash
#!/bin/bash
#PBS -N catchem-container
#PBS -l nodes=2:ppn=8
#PBS -l walltime=02:00:00
#PBS -q standard

cd $PBS_O_WORKDIR
module load singularity

# Run across nodes
mpirun -hostfile $PBS_NODEFILE \
  singularity exec catchem.sif catchem_driver config.yml
```

## Container Best Practices

### Image Optimization

**Layer Optimization:**
```docker
# Combine RUN commands to reduce layers
RUN apt-get update && apt-get install -y \
    package1 package2 package3 \
    && rm -rf /var/lib/apt/lists/*

# Use multi-stage builds for smaller images
FROM ubuntu:22.04 AS builder
# ... build steps ...

FROM ubuntu:22.04
COPY --from=builder /opt/catchem/bin /usr/local/bin/
```

**Size Optimization:**
```docker
# Use slim base images
FROM python:3.9-slim

# Remove unnecessary packages
RUN apt-get autoremove -y && apt-get clean

# Use specific versions
FROM ubuntu:22.04
```

### Security Considerations

**User Management:**
```docker
# Create non-root user
RUN groupadd -r catchem && useradd -r -g catchem catchem
USER catchem

# Or use existing user ID
ARG USER_ID=1000
RUN useradd -u $USER_ID -m catchem
USER catchem
```

**Singularity Security:**
```bash
# Run with specific user namespace
singularity exec --userns catchem.sif catchem_driver

# Bind minimal directories
singularity exec -B /data:/data --no-home catchem.sif catchem_driver
```

### Performance Optimization

**Resource Limits:**
```bash
# Docker resource limits
docker run --memory=8g --cpus=4 \
  noaagsl/catchem:latest catchem_driver

# Singularity resource binding
singularity exec -B /dev/shm:/dev/shm catchem.sif catchem_driver
```

**Storage Optimization:**
```bash
# Use tmpfs for temporary files
docker run --tmpfs /tmp:exec,size=1g \
  noaagsl/catchem:latest catchem_driver

# Bind high-performance storage
singularity exec -B /scratch:/scratch catchem.sif catchem_driver
```

## Container Registry

### Docker Hub

**Pushing Images:**
```bash
# Build and tag
docker build -t noaagsl/catchem:v2.1.0 .
docker tag noaagsl/catchem:v2.1.0 noaagsl/catchem:latest

# Push to registry
docker push noaagsl/catchem:v2.1.0
docker push noaagsl/catchem:latest
```

### Private Registries

**Harbor Registry:**
```bash
# Tag for private registry
docker tag catchem:latest registry.example.com/project/catchem:latest

# Push to private registry
docker push registry.example.com/project/catchem:latest

# Pull from private registry
singularity build catchem.sif docker://registry.example.com/project/catchem:latest
```

## Troubleshooting

### Common Issues

**Permission Problems:**
```bash
# Docker permission denied
sudo usermod -aG docker $USER
newgrp docker

# Singularity bind mount issues
singularity exec -B $PWD:/data:rw catchem.sif ls /data
```

**MPI Issues:**
```bash
# Check MPI compatibility
singularity exec catchem.sif mpirun --version
mpirun --version

# Debug MPI connectivity
mpirun -np 2 singularity exec catchem.sif hostname
```

**GPU Issues:**
```bash
# Check GPU access
singularity exec --nv catchem.sif nvidia-smi

# Check CUDA version compatibility
singularity exec --nv catchem.sif nvcc --version
```

### Debugging

**Container Shell Access:**
```bash
# Docker debugging
docker run -it --entrypoint /bin/bash noaagsl/catchem:latest

# Singularity debugging
singularity shell -B $PWD:/data catchem.sif
```

**Logging:**
```bash
# Docker logs
docker logs <container-id>

# Runtime debugging
singularity exec -B $PWD:/data catchem.sif \
  strace -o trace.log catchem_driver config.yml
```

## Production Deployment

### Kubernetes

**Deployment YAML:**
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: catchem-deployment
spec:
  replicas: 1
  selector:
    matchLabels:
      app: catchem
  template:
    metadata:
      labels:
        app: catchem
    spec:
      containers:
      - name: catchem
        image: noaagsl/catchem:latest
        resources:
          requests:
            memory: "4Gi"
            cpu: "2"
          limits:
            memory: "8Gi"
            cpu: "4"
        volumeMounts:
        - name: data-volume
          mountPath: /data
      volumes:
      - name: data-volume
        persistentVolumeClaim:
          claimName: catchem-data-pvc
```

### Container Orchestration

**Docker Compose:**
```yaml
version: '3.8'
services:
  catchem:
    image: noaagsl/catchem:latest
    volumes:
      - ./data:/data
      - ./output:/output
    environment:
      - OMP_NUM_THREADS=4
    deploy:
      resources:
        limits:
          memory: 8G
          cpus: '4'
```

## References

- [HPC Installation Guide](hpc-installation.md)
- [Build System](build-system.md)
- [Performance Guide](performance.md)
- [Docker Documentation](https://docs.docker.com/)
- [Singularity Documentation](https://sylabs.io/docs/)
