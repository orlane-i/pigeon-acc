#!/bin/bash

#SBATCH --job-name=classify_KM_pigeon
#SBATCH --partition=std
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --cpus-per-task=3
#SBATCH --time=00-02:00:00

export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}

## Load software modules
module load R/4.5.1

## Move data (if necessary) and launch application
## (use 'srun' for launching mpi applications)
R --save -f /scratch/ioosso/pigeon-acc/scripts/classify_KM_pigeon.R

## Finish gracefully
exit 0
