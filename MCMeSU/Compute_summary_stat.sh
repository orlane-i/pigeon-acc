#!/bin/bash

#SBATCH --job-name=Compute_summary_stat
#SBATCH --mail-user=orlane.iooss@etudiant.univ-rennes.fr
#SBATCH --mail-type=ALL
#SBATCH --partition=std
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --cpus-per-task=3
#SBATCH --time=00-01:00:00

export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK}

## Load software modules
module load R/4.5.1

## Move data (if necessary) and launch application
## (use 'srun' for launching mpi applications)
R --save -f /scratch/ioosso/pigeon-acc/scripts/2-Compute_summary_stat.R

## Finish gracefully
exit 0
