!> @file synchronisation.c
!> @brief This example is to illustrate basic synchronisation constructs.
!> @details You will find two folders in this example, and will in all examples
!> provided in this session:
!> - src: contains the source code.
!> - bin: contains the binary produced.
!> 
!> The makefile provided already sets everything up for you:
!> - To compile: `make`.
!> - To execute: `./bin/synchronisation`.
!> 
!> If you have any questions, do not hesitate.
!> @author Ludovic Capelli (l.capelli@epcc.ed.ac.uk)

!> @brief Threads manually count how many they are by incrementing a common
!> variable. Once all threads have incremented this variable, one thread (not
!> any one thread in particular) prints the value of that incremented variable.
PROGRAM main
	USE OMP_LIB

	IMPLICIT NONE

	INTEGER :: thread_count = 0
	!$OMP PARALLEL
		thread_count = thread_count + 1
		! Restriction: the print statement must be kept inside the parallel region.
		WRITE(*, '(A,I0,A)') 'There are ', thread_count, ' threads.'
	!$OMP END PARALLEL
END PROGRAM main