#!/bin/bash
export ESMF_INC="/opt/homebrew/Cellar/esmf/8.8.0/mod/modO/Darwin.gfortran.64.openmpi.default"
export MPI_INC="/opt/homebrew/Cellar/open-mpi/5.0.8/include"

cmake -B build \
  -DCMAKE_MODULE_PATH="/opt/homebrew/Cellar/esmf/8.8.0/cmake" \
  -DESMFMKFILE="/opt/homebrew/Cellar/esmf/8.8.0/lib/libO/Darwin.gfortran.64.openmpi.default/esmf.mk" \
  -DCMAKE_Fortran_FLAGS="-I${ESMF_INC} -I${MPI_INC}"
