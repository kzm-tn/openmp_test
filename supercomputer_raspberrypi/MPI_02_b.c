#include <mpi.h>
#include <math.h>
#include <stdio.h>

int main(int argc, char* argv[])
{
  int total_iter;
  int n, rank, length, numprocs, i;
  double pi, width, sum, x, rank_integral;
  char hostname[MPI_MAX_PROCESSOR_NAME];
  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Get_processor_name(hostname, &length);

  if(rank == 0)
  {
    printf("\n");
    printf("###############################");
    printf("\n\n");
    printf("Master node name: %s\n", hostname);
    printf("\n");
    printf("Enter number of intervals:\n");
    printf("\n");
    scanf("%d", &n);
    printf("\n");
  }

  MPI_Bcast(&n, 1, MPI_INT, 0, MPI_COMM_WORLD);

  for(total_iter = 1; total_iter < n; total_iter++)
  {
    sum = 0.0;
    width = 1.0 / (double)total_iter;

    for(i = rank + 1; i <= total_iter; i += numprocs)
    {
      x = width * ((double)i - 0.5);
      sum += 4.0 / (1.0 + x*x);
    }

    rank_integral = width * sum;

    MPI_Reduce(&rank_integral, &pi, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
  }

  if (rank == 0)
  {
    printf("\n\n");
    printf("     Calculated pi = %.16f\n", pi);
    printf("              M_PI = %.16f\n", M_PI);
    printf("    Relative Error = %.16f\n", fabs(pi - M_PI));
  }

  MPI_Finalize();
  return 0;
}
