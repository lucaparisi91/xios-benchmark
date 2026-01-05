#!/bin/bash

# Slurm job options (name, compute nodes, job time)
#SBATCH --job-name=xios_bench
#SBATCH --time=0:20:00
#SBATCH --nodes=70
#SBATCH --tasks-per-node=288
#SBATCH --cpus-per-task=1
#SBATCH --account=z04
#SBATCH --partition=standard
#SBATCH --qos=largescale
#SBATCH --exclusive

module load PrgEnv-gnu
module load cray-hdf5-parallel
module load cray-netcdf-hdf5parallel
module load xthi

export OMP_NUM_THREADS=1
export SRUN_CPUS_PER_TASK=$SLURM_CPUS_PER_TASK
export FI_CXI_RX_MATCH_MODE=hybrid
export FI_CXI_OPTIMIZED_MRS=false
export FI_CXI_DEFAULT_CQ_SIZE=1048576

XIOS_DIR="xios-3"

SERVER_EXE="../${XIOS_DIR}/bin/xios_server.exe"
BENCH_EXE="./xios-bench-${XIOS_DIR}"
export FI_LOG_LEVEL=Debug
module load spack 
spack env activate ../environments/xios3_dev
spack load hpctoolkit

export LD_LIBRARY_PATH=/work/z04/z04/shared/lparisi/queries/opt/libfabric-1.22.0-v2/lib:$LD_LIBRARY_PATH


srun  --hint=nomultithread --nodes=66 --ntasks=18816 --distribution=block:block --unbuffered  $BENCH_EXE :  --nodes=4 --ntasks=16 --cpus-per-task=72  --distribution=block:block --hint=nomultithread  ${SERVER_EXE}