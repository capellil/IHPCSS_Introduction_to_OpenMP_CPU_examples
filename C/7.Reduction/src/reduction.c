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
	int thread_count = 0;
	#pragma omp parallel default(none) shared(thread_count)
	{
		#pragma omp atomic
		thread_count++;

		#pragma omp barrier
		#pragma omp single
		{
			// Restriction: the print statement must be kept inside the parallel region.
			printf("There are %d threads.\n", thread_count);
		}
	}

    thread_count = 0;
	#pragma omp parallel default(none) reduction(+:thread_count)
	{
		thread_count++;
	}
	printf("There are %d threads.\n", thread_count);

	return EXIT_SUCCESS;
}
