/**
 * @file reduction.c
 * @brief This example is to illustrate the use of a reduction clause.
 * @details You will find two folders in this example, and will in all examples
 * provided in this session:
 * - src: contains the source code.
 * - bin: contains the binary produced.
 *
 * The makefile provided already sets everything up for you:
 * - To compile: `make`.
 * - To execute: `./bin/reduction`.
 *
 * If you have any questions, do not hesitate.
 * @author Ludovic Capelli (l.capelli@epcc.ed.ac.uk)
 **/

#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief The master thread allocates an array, and other threads wait. Once the
 * allocation is complete, each thread updates the array. Then, analystics about
 * the array values are calculated: sum, min and max. Finally, one thread, any
 * thread, prints the value of those metrics.
 **/
int main()
{
	int* my_array = NULL;
	int sum = 0;
	int min = 0;
	int max = 0;

	#pragma omp parallel default(none) shared(my_array, sum, min, max)
	{
		#pragma omp master
		{
			my_array = (int*)malloc(sizeof(int) * omp_get_num_threads());
			min = omp_get_num_threads() - 1;
		}
		#pragma omp barrier
		my_array[omp_get_thread_num()] = omp_get_thread_num();
		#pragma omp barrier
		#pragma omp for reduction(+: sum) reduction(min: min) reduction(max: max)
		for(int i = 0; i < omp_get_num_threads(); i++)
		{
			sum += my_array[i];
			min = (min > my_array[i]) ? my_array[i] : min;
			max = (max < my_array[i]) ? my_array[i] : max;
		}
		#pragma omp single
		{
			printf("Sum = %d, min = %d, max = %d\n", sum, min, max);
		}
	}

	return EXIT_SUCCESS;
}
