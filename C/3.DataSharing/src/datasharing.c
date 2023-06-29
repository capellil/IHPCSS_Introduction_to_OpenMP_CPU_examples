/**
 * @file datasharing.c
 * @brief This example is to illustrate the basic data-sharing attributes.
 * @details You will find two folders in this example, and will in all examples
 * provided in this session:
 * - src: contains the source code.
 * - bin: contains the binary produced.
 *
 * The makefile provided already sets everything up for you:
 * - To compile: `make`.
 * - To execute: `./bin/datasharing`.
 *
 * If you have any questions, do not hesitate.
 * @author Ludovic Capelli (l.capelli@epcc.ed.ac.uk)
 **/

#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Performs a few calculations, each requiring the corresponding variable
 * to be properly setup when passed to the parallel construct.
 **/
int main()
{
	int thread_count = 0;
	#pragma omp parallel
	{
		if(omp_get_thread_num() == 0)
		{
			thread_count = omp_get_num_threads();
		}
	}
	int* common_array = (int*)malloc(sizeof(int) * thread_count);

	// Restriction: do not pass variable "step" as "shared".
	const int step = 123;
	/* Restriction: do not move the declaration of variable "my_thread_id"
	   inside the parallel region. */
	int my_thread_id = 0;
	#pragma omp parallel
	{
		my_thread_id = omp_get_thread_num();
		common_array[my_thread_id] = my_thread_id * step;
	}

	for(int i = 0; i < thread_count; i++)
	{
		printf("Value of common_array[%d] = %d\n", i, common_array[i]);
	}

	return EXIT_SUCCESS;
}
