/**
 * @file whoseturn.c
 * @brief This example is to illustrate basic worksharing directives.
 * @details You will find two folders in this example, and will in all examples
 * provided in this session:
 * - src: contains the source code.
 * - bin: contains the binary produced.
 *
 * The makefile provided already sets everything up for you:
 * - To compile: `make`.
 * - To execute: `./bin/whoseturn`.
 *
 * If you have any questions, do not hesitate.
 * @author Ludovic Capelli (l.capelli@epcc.ed.ac.uk)
 **/

#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief One thread, not one thread in particular, prints a statement. The
 * master thread also has a statement to print, followed with a waiting period.
 * @details Both variables must be properly initialised.
 **/
int main()
{
	int my_thread_id = 0;

	// START: master thread's workload
	printf("[Thread %d] This is to be printed by the master thread only.\n", my_thread_id);
	int start = omp_get_wtime();
	while(omp_get_wtime() - start < 1.0f)
	{

	}
	// END: master thread's workload

	// START: one thread's workload
	printf("[Thread %d] This is to be printed by one thread, any one.\n", my_thread_id);
	// END: one thread's workload

	return EXIT_SUCCESS;
}
