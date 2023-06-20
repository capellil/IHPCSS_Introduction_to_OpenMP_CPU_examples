/**
 * @file preamble.c
 * @brief This example is to check that the compilation process works.
 * @details You will find two folders in this example, and will in all examples
 * provided in this session:
 * - src: contains the source code.
 * - bin: contains the binary produced.
 *
 * The makefile provided already sets everything up for you:
 * - To compile: `make`.
 * - To execute: `./bin/preamble`.
 *
 * If you have any questions, do not hesitate.
 * @author Ludovic Capelli (l.capelli@epcc.ed.ac.uk)
 **/

#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

/**
 * @brief Just prints a hello world message.
 **/
int main()
{
	int num_threads = omp_get_num_threads();
	printf("Hello world, there are %d threads (i.e.: I am alone... T_T).\n", num_threads);

	return EXIT_SUCCESS;
}
