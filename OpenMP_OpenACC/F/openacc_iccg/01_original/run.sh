#! /bin/sh
#PBS -q u-lecture
#PBS -l select=1:mpiprocs=1:ompthreads=18
#PBS -W group_list=gt00
#PBS -l walltime=00:05:00

cd $PBS_O_WORKDIR

. /etc/profile.d/modules.sh
module load pgi/18.7

./run -n 128 -c -20 -nt 18

