set -e

module load PrgEnv-gnu
module load cray-hdf5-parallel
module load cray-netcdf-hdf5parallel

set -x

XIOS_NAME="xios-3"
#XIOS_NAME="xios-2.6.1.0"


XIOS_DIR=$(pwd)/../${XIOS_NAME}

XIOS_INCDIR=${XIOS_DIR}/inc
XIOS_LIBDIR=${XIOS_DIR}/lib
export FFLAGS="-O2 -g -fmax-errors=1 -I ${XIOS_INCDIR}"
export FC=ftn

export LDFLAGS="-L${XIOS_LIBDIR} -I ${XIOS_INCDIR}"


make xios_bench -j 16

mv xios_bench xios-bench-${XIOS_NAME}