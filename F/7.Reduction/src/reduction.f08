!> @file reduction.c
!> @brief This example is to illustrate the use of a reduction clause.
!> @details You will find two folders in this example, and will in all examples
!> provided in this session:
!> - src: contains the source code.
!> - bin: contains the binary produced.
!> 
!> The makefile provided already sets everything up for you:
!> - To compile: `make`.
!> - To execute: `./bin/reduction`.
!> 
!> If you have any questions, do not hesitate.
!> @author Ludovic Capelli (l.capelli@epcc.ed.ac.uk)

!> @brief The master thread allocates an array, and other threads wait. Once the
!> allocation is complete, each thread updates the array. Then, analystics about
!> the array values are calculated: sum, min and max. Finally, one thread, any
!> thread, prints the value of those metrics.
PROGRAM main
	USE OMP_LIB

	IMPLICIT NONE

	INTEGER :: thread_count = 0

	!$OMP PARALLEL DEFAULT(NONE) SHARED(thread_count)
		!$OMP ATOMIC
		thread_count = thread_count + 1

		!$OMP BARRIER
		!$OMP SINGLE
			! Restriction: the print statement must be kept inside the parallel region.
			WRITE(*, '(A,I0,A)') 'There are ', thread_count, ' threads.'
		!$OMP END SINGLE
	!$OMP END PARALLEL

    thread_count = 0
	!$OMP PARALLEL DEFAULT(NONE) REDUCTION(+:thread_count)
		thread_count = thread_count + 1
	!$OMP END PARALLEL
	WRITE(*, '(A,I0,A)') 'There are ', thread_count, ' threads.'
END PROGRAM main
