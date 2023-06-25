/**
 * @file schedules.c
 * @brief This example is to illustrate the different schedules available.
 * @details You will find two folders in this example, and will in all examples
 * provided in this session:
 * - src: contains the source code.
 * - bin: contains the binary produced.
 *
 * The makefile provided already sets everything up for you:
 * - To compile: `make`.
 * - To execute: `./bin/schedules`.
 *
 * If you have any questions, do not hesitate.
 * @author Ludovic Capelli (l.capelli@epcc.ed.ac.uk)
 **/

#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

void wait_for(double s)
{
	double start = omp_get_wtime();
	while(omp_get_wtime() - start < s)
	{

	}
}

/**
 * @brief There are three loops in this example, all of which expose a different
 * workload. Find the best schedule for each loop.
 **/
int main()
{
	int thread_count;
	#pragma omp parallel default(none) shared(thread_count)
	{
		#pragma omp single
		{
			thread_count = omp_get_num_threads();
			printf("You are using %d threads.\n", thread_count);
		}
	}
	const int MAX = 10;
	double wait_time_granularity = 0.1;
	double start = omp_get_wtime();
	for(int i = 0; i < MAX; i++)
	{
		wait_for(wait_time_granularity);
	}
	double serial_time = MAX * wait_time_granularity;
	double elapsed_time = omp_get_wtime() - start;
	double speedup = serial_time / elapsed_time;
	double efficiency = speedup / thread_count * 100;
	printf("The first loop has taken %.2f seconds (%.1fx faster, %.0f%% efficiency).\n", elapsed_time, speedup, efficiency);

	start = omp_get_wtime();
	for(int i = 0; i < MAX; i++)
	{
		wait_for(wait_time_granularity * (MAX - i));
	}
	serial_time = (((double)MAX) * ((double)(MAX + 1))) / 2.0 * wait_time_granularity;
	elapsed_time = omp_get_wtime() - start;
	speedup = serial_time / elapsed_time;
	efficiency = speedup / thread_count * 100;
	printf("The second loop has taken %.2f seconds (%.1fx faster, %.0f%% efficiency).\n", elapsed_time, speedup, efficiency);

	start = omp_get_wtime();
	srand(123456);
	int rands = 0;
	for(int i = 0; i < MAX; i++)
	{
		int last_rand = rand() % 10;
		rands += last_rand;
		wait_for(wait_time_granularity * last_rand);
	}
	serial_time = rands * wait_time_granularity;
	elapsed_time = omp_get_wtime() - start;
	speedup = serial_time / elapsed_time;
	efficiency = speedup / thread_count * 100;
	printf("The third loop has taken %.2f seconds (%.1fx faster, %.0f%% efficiency).\n", elapsed_time, speedup, efficiency);

	return EXIT_SUCCESS;
}
