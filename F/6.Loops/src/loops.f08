!> @file loops.c
!> @brief This example is to illustrate basic loop worksharing directives.
!> @details You will find two folders in this example, and will in all examples
!> provided in this session:
!> - src: contains the source code.
!> - bin: contains the binary produced.
!> 
!> The makefile provided already sets everything up for you:
!> - To compile: `make`.
!> - To execute: `./bin/loops`.
!> 
!> If you have any questions, do not hesitate.
!> @author Ludovic Capelli (ludovic.capelli@csiro.au)

!> @brief The master thread allocates an array, and other threads wait. Once the
!> allocation is complete, each thread updates the array, before one thread, any
!> thread, prints the value of each element. Update this code to use a loop for
!> the value initialisation phase.
PROGRAM main
	USE OMP_LIB

	IMPLICIT NONE

	INTEGER, ALLOCATABLE :: my_array(:)
	INTEGER :: loop_i

	!$OMP PARALLEL DEFAULT(NONE) SHARED(my_array)
		!$OMP MASTER
			ALLOCATE(my_array(0:omp_get_num_threads()-1))
		!$OMP END MASTER
		!$OMP BARRIER
		!$OMP CRITICAL
			my_array(omp_get_thread_num()) = omp_get_thread_num()
		!$OMP END CRITICAL
		!$OMP BARRIER
		!$OMP SINGLE
			DO loop_i = 0, omp_get_num_threads() - 1
				WRITE(*, '(A,I0,A,I0)') 'my_array(', loop_i, ') = ', my_array(loop_i)
			END DO
		!$OMP END SINGLE
	!$OMP END PARALLEL
END PROGRAM main
