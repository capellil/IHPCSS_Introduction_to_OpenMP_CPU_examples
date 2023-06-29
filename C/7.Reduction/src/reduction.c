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
 * @brief The objective is for each thread increments a common "thread_count"
 * variable, to manually count the number of threads available.
 * @details To do so, you must use two different approaches, without relying on
 * the use of a barrier or critical construct.
 **/
int main()
{
	// Approach 1
	int thread_count = 0;
	#pragma omp parallel
	{
		thread_count++;
	}
	printf("There are %d threads.\n", thread_count);

	// Approach 2
    thread_count = 0;
	#pragma omp parallel
	{
		thread_count++;
	}
	printf("There are %d threads.\n", thread_count);

	return EXIT_SUCCESS;
}
