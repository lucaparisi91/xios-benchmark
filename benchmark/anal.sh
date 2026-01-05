#!/bin/bash

# Slurm job options (name, compute nodes, job time)
#SBATCH --job-name=xios_bench
#SBATCH --time=0:30:00
#SBATCH --nodes=1
#SBATCH --tasks-per-node=36
#SBATCH --cpus-per-task=1
#SBATCH --account=z04
#SBATCH --partition=standard
#SBATCH --qos=standard


module load PrgEnv-gnu
module load cray-hdf5-parallel
module load cray-netcdf-hdf5parallel
module load xthi


module load spack 
spack env activate ../environments/xios3_dev
spack load hpctoolkit

HPCTOOLKIT_DIR=hpctoolkit-xios-3-trace


hpcstruct $HPCTOOLKIT_DIR
hpcprof $HPCTOOLKIT_DIR

