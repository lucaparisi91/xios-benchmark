#!/bin/bash

# Slurm job options (name, compute nodes, job time)
#SBATCH --job-name=xios_bench
#SBATCH --time=0:20:00
#SBATCH --nodes=2
#SBATCH --tasks-per-node=16
#SBATCH --cpus-per-task=1
#SBATCH --account=z04
#SBATCH --partition=standard
#SBATCH --qos=standard
#SBATCH --exclusive

module load PrgEnv-gnu
module load cray-hdf5-parallel
module load cray-netcdf-hdf5parallel
module load xthi

set -x
export OMP_NUM_THREADS=1
export SRUN_CPUS_PER_TASK=$SLURM_CPUS_PER_TASK
export FI_CXI_RX_MATCH_MODE=hybrid
export FI_CXI_OPTIMIZED_MRS=false
export FI_CXI_DEFAULT_CQ_SIZE=1048576

XIOS_DIR="xios-3"

SERVER_EXE="../${XIOS_DIR}/bin/xios_server.exe"
BENCH_EXE="./xios-bench-${XIOS_DIR}"

set +x
#export FI_LOG_LEVEL=Debug
module load spack 
spack env activate ../environments/xios3_dev
spack load hpctoolkit

#export LD_LIBRARY_PATH=/work/z04/z04/shared/lparisi/queries/opt/libfabric-1.22.0-v2/lib:$LD_LIBRARY_PATH

set -x
#LAUNCHER="hpcrun -e context-switches -o hpctoolkit-$XIOS_DIR-ctx-switches"
LAUNCHER=""

rm -f *.nc

#LAUNCHER=""

# export HDF5_USE_FILE_LOCKING=0
# export MPICH_MPIIO_STATS=2
# export MPICH_MPIIO_TIMERS=1
# export MPICH_MPIIO_STATS_FILE=stats_mpiio

cp config-small.nml config.nml
srun  --hint=nomultithread --nodes=1 --ntasks=16 --cpus-per-task=1 --distribution=block:block --unbuffered $LAUNCHER $BENCH_EXE :  --nodes=1 --ntasks=4 --cpus-per-task=4  --distribution=block:block --hint=nomultithread $LAUNCHER ${SERVER_EXE}