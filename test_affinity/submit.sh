#!/bin/bash

# Slurm job options (name, compute nodes, job time)
#SBATCH --job-name=xios_bench
#SBATCH --time=0:20:00
#SBATCH --nodes=2
#SBATCH --tasks-per-node=4
#SBATCH --cpus-per-task=1
#SBATCH --account=z04
#SBATCH --partition=standard
#SBATCH --qos=standard
#SBATCH --exclusive

module load PrgEnv-gnu
module load xthi

srun --hint=nomultithread --distribution=block:block --nodes=1 --cpus-per-task=1  ./test : --nodes=1 --ntasks=1 --cpus-per-task=4 --hint=nomultithread ./test