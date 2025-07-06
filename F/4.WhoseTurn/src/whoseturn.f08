!> @file whoseturn.f08
!> @brief This example is to illustrate basic worksharing directives.
!> @details You will find two folders in this example, and will in all examples
!> provided in this session:
!> - src: contains the source code.
!> - bin: contains the binary produced.
!> 
!> The makefile provided already sets everything up for you:
!> - To compile: `make`.
!> - To execute: `./bin/whoseturn`.
!> 
!> If you have any questions, do not hesitate.
!> @author Ludovic Capelli (ludovic.capelli@csiro.au)

!> @brief One thread, not one thread in particular, prints a statement. The
!> master thread also has a statement to print, followed with a waiting period.
!> @details Both variables must be properly initialised.
PROGRAM main
	USE OMP_LIB

	IMPLICIT NONE

	INTEGER :: my_thread_id = 0
	INTEGER :: start = 0

	! START: master thread's workload
	WRITE(*, '(A,I0,A)') '[Thread ', my_thread_id, '] This is to be printed by the master thread only.'
	start = omp_get_wtime()
	DO WHILE (omp_get_wtime() - start < 1.0)
	END DO
	! END: master thread's workload

	! START: one thread's workload
	WRITE(*, '(A,I0,A)') '[Thread ', my_thread_id, '] This is to be printed by one thread, any one.'
	! END: one thread's workload
END PROGRAM main