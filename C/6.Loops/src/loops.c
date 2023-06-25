/**
 * @file loops.c
 * @brief This example is to illustrate basic loop worksharing directives.
 * @details You will find two folders in this example, and will in all examples
 * provided in this session:
 * - src: contains the source code.
 * - bin: contains the binary produced.
 *
 * The makefile provided already sets everything up for you:
 * - To compile: `make`.
 * - To execute: `./bin/loops`.
 *
 * If you have any questions, do not hesitate.
 * @author Ludovic Capelli (l.capelli@epcc.ed.ac.uk)
 **/

#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief The master thread allocates an array, and other threads wait. Once the
 * allocation is complete, each thread updates the array, before one thread, any
 * thread, prints the value of each element. Update this code to use a loop for
 * the value initialisation phase.
 **/
int main()
{
	int* my_array = NULL;

	#pragma omp parallel default(none) shared(my_array)
	{
		#pragma omp master
		{
			my_array = (int*)malloc(sizeof(int) * omp_get_num_threads());
		}
		#pragma omp barrier
		#pragma omp critical
		{
			my_array[omp_get_thread_num()] = omp_get_thread_num();
		}
		#pragma omp barrier
		#pragma omp single
		{
			for(int i = 0; i < omp_get_num_threads(); i++)
			{
				printf("my_array[%d] = %d\n", i, my_array[i]);
			}
		}
	}

	return EXIT_SUCCESS;
}
