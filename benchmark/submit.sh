#!/bin/bash

# Slurm job options (name, compute nodes, job time)
#SBATCH --job-name=xios_bench
#SBATCH --time=0:20:00
#SBATCH --nodes=1
#SBATCH --tasks-per-node=4
#SBATCH --cpus-per-task=1
#SBATCH --account=z04
#SBATCH --partition=standard
#SBATCH --qos=standard

module load PrgEnv-gnu
module load cray-hdf5-parallel
module load cray-netcdf-hdf5parallel

export OMP_NUM_THREADS=1

export SRUN_CPUS_PER_TASK=$SLURM_CPUS_PER_TASK

srun --hint=nomultithread --distribution=block:block ./xios_bench