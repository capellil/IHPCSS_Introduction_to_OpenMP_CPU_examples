/**
 * @file synchronisation.c
 * @brief This example is to illustrate basic synchronisation constructs.
 * @details You will find two folders in this example, and will in all examples
 * provided in this session:
 * - src: contains the source code.
 * - bin: contains the binary produced.
 *
 * The makefile provided already sets everything up for you:
 * - To compile: `make`.
 * - To execute: `./bin/synchronisation`.
 *
 * If you have any questions, do not hesitate.
 * @author Ludovic Capelli (ludovic.capelli@csiro.au)
 **/

#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Threads manually count how many they are by incrementing a common
 * variable. Once all threads have incremented this variable, one thread (not
 * any one thread in particular) prints the value of that incremented variable.
 **/
int main()
{
	int thread_count = 0;

	#pragma omp parallel
	{
		thread_count++;
		// Restriction: the print statement must be kept inside the parallel region.
		printf("There are %d threads.\n", thread_count);
	}

	return EXIT_SUCCESS;
}
