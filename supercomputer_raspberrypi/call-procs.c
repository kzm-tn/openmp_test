#include <mpi.h>
#include <stdio.h>

int main(int argc, char* argv[])
{
  int num_processes;
  int curr_rank;
  int proc_name_len;
  char proc_name[MPI_MAX_PROCESSOR_NAME];
  
  MPI_Init(&argc, &argv);

  MPI_Comm_size(MPI_COMM_WORLD, &num_processes);
  MPI_Comm_rank(MPI_COMM_WORLD, &curr_rank);
  MPI_Get_processor_name(proc_name, &proc_name_len);

  printf("Calling process %d out of %d on %s \n",
          curr_rank, num_processes,
          proc_name);
  MPI_Finalize();

  return 0;
}
