!> @file schedules.f08
!> @brief This example is to illustrate the different schedules available.
!> @details You will find two folders in this example, and will in all examples
!> provided in this session:
!> - src: contains the source code.
!> - bin: contains the binary produced.
!> 
!> The makefile provided already sets everything up for you:
!> - To compile: `make`.
!> - To execute: `./bin/schedules`.
!> 
!> If you have any questions, do not hesitate.
!> @author Ludovic Capelli (ludovic.capelli@csiro.au)

SUBROUTINE wait_for(s)
	USE OMP_LIB

	IMPLICIT NONE
	
	REAL(KIND=8) :: s
	REAL(KIND=8) :: start
	
	start = omp_get_wtime()
	DO WHILE (omp_get_wtime() - start < s)
	END DO
END SUBROUTINE

!> @brief There are three loops in this example, all of which expose a different
!> workload. Find the best schedule for each loop.
PROGRAM main
	USE OMP_LIB

	IMPLICIT NONE

	INTEGER :: thread_count
	INTEGER :: max = 100
	REAL(KIND=8) :: wait_time_granularity = 0.01
	REAL(KIND=8) :: start
	INTEGER :: loop_i
	REAL(KIND=8) :: serial_time
	REAL(KIND=8) :: elapsed_time
	REAL(KIND=8) :: speedup
	REAL(KIND=8) :: efficiency
	INTEGER :: rands = 0
	INTEGER :: last_rand

	!$OMP PARALLEL DEFAULT(NONE) SHARED(thread_count)
		!$OMP SINGLE
			thread_count = omp_get_num_threads()
			WRITE(*, '(A,I0,A)') 'You are using ', thread_count, ' threads.'
		!$OMP END SINGLE
	!$OMP END PARALLEL

	start = omp_get_wtime()
	DO loop_i = 0, max -1
		CALL wait_for(wait_time_granularity)
	END DO
	serial_time = DBLE(max) * wait_time_granularity
	elapsed_time = omp_get_wtime() - start
	speedup = serial_time / elapsed_time
	efficiency = speedup / DBLE(thread_count) * 100.0
	WRITE(*, '(A,F0.2,A,F0.1,A,F0.0,A)') 'The first loop has taken ', elapsed_time, ' seconds (', speedup, 'x faster, ', &
								   efficiency, '% efficiency).'

	start = omp_get_wtime()
	DO loop_i = 0, max - 1
		CALL wait_for(wait_time_granularity * DBLE(max - loop_i) / 10.0)
	END DO
	serial_time = (DBLE(max) * DBLE(max + 1)) / 2.0 * wait_time_granularity / 10.0
	elapsed_time = omp_get_wtime() - start
	speedup = serial_time / elapsed_time
	efficiency = speedup / DBLE(thread_count) * 100.0
	WRITE(*, '(A,F0.2,A,F0.1,A,F0.0,A)') 'The second loop has taken ', elapsed_time, ' seconds (', speedup, 'x faster, ', &
								   efficiency, '% efficiency).'

	start = omp_get_wtime()

	CALL srand(4)
	DO loop_i = 0, max - 1
		last_rand = INT(rand() * 10.0)
		rands = rands + last_rand
		CALL wait_for(wait_time_granularity * DBLE(last_rand))
	END DO
	serial_time = rands * wait_time_granularity
	elapsed_time = omp_get_wtime() - start
	speedup = serial_time / elapsed_time
	efficiency = speedup / DBLE(thread_count) * 100.0
	WRITE(*, '(A,F0.2,A,F0.1,A,F0.0,A)') 'The third loop has taken ', elapsed_time, ' seconds (', speedup, 'x faster, ', &
								   efficiency, '% efficiency).'
END PROGRAM main