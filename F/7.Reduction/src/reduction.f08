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

!> @brief The objective is for each thread increments a common "thread_count"
!> variable, to manually count the number of threads available.
!> @details To do so, you must use two different approaches, without relying on
!> the use of a barrier or critical construct.
PROGRAM main
	USE OMP_LIB

	IMPLICIT NONE

	INTEGER :: thread_count = 0

	!$OMP PARALLEL
		thread_count = thread_count + 1
	!$OMP END PARALLEL
	WRITE(*, '(A,I0,A)') 'There are ', thread_count, ' threads.'

    thread_count = 0
	!$OMP PARALLEL
		thread_count = thread_count + 1
	!$OMP END PARALLEL
	WRITE(*, '(A,I0,A)') 'There are ', thread_count, ' threads.'
END PROGRAM main
