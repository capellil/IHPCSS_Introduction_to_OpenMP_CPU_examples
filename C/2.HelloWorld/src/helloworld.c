/**
 * @file helloworld.c
 * @brief This example is to write the OpenMP hello world.
 * @details You will find two folders in this example, and will in all examples
 * provided in this session:
 * - src: contains the source code.
 * - bin: contains the binary produced.
 *
 * The makefile provided already sets everything up for you:
 * - To compile: `make`.
 * - To execute: `./bin/helloworld`.
 *
 * If you have any questions, do not hesitate.
 * @author Ludovic Capelli (ludovic.capelli@csiro.au)
 **/

#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Just prints a hello world message, displaying the thread identifier
 * and the number of threads.
 * @details Both variables must be properly initialised.
 **/
int main()
{
	int my_thread_id = 0;
	int thread_count = 0;
	printf("Hello world, I am thread %d. We are %d threads.\n", my_thread_id, thread_count);

	return EXIT_SUCCESS;
}
