!> @file helloworld.f08
!> @brief This example is to write the OpenMP hello world.
!> @details You will find two folders in this example, and will in all examples
!> provided in this session:
!> - src: contains the source code.
!> - bin: contains the binary produced.
!> 
!> The makefile provided already sets everything up for you:
!> - To compile: `make`.
!> - To execute: `./bin/helloworld`.
!> 
!> If you have any questions, do not hesitate.
!> @author Ludovic Capelli (ludovic.capelli@csiro.au)

!> @brief Just prints a hello world message, displaying the thread identifier
!> and the number of threads.
!> @details Both variables must be properly initialised.
PROGRAM main
	USE OMP_LIB

	IMPLICIT NONE

	INTEGER :: my_thread_id = 0
	INTEGER :: thread_count = 0
	
	WRITE(*, '(A,I0,A,A,I0,A)') 'Hello world, I am thread ', my_thread_id, '.', &
								' We are ', thread_count, ' threads.'
END PROGRAM main