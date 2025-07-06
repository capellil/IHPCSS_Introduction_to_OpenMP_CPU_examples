!> @file preamble.f08
!> @brief This example is to check that the compilation process works.
!> @details You will find two folders in this example, and will in all examples
!> provided in this session:
!> - src: contains the source code.
!> - bin: contains the binary produced.
!> 
!> The makefile provided already sets everything up for you:
!> - To compile: `make`.
!> - To execute: `./bin/preamble`.
!> 
!> If you have any questions, do not hesitate.
!> @author Ludovic Capelli (l.capelli@epcc.ed.ac.uk)

!> @brief Just prints a desperate hello world message.
PROGRAM main
	USE OMP_LIB

	IMPLICIT NONE

	INTEGER :: num_threads
	num_threads = omp_get_num_threads()

	WRITE(*, '(A,I0,A)') 'Hello world, there are ', num_threads, ' threads (i.e.: I am alone... T_T).'
END PROGRAM main