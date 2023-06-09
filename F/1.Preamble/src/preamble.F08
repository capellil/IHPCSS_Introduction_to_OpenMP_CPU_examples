!> @file preamble.c
!> @brief This example is to check that the compilation process works.
!> @details You will find two folders in this example, and will in all examples
!> provided in this session:
!> - src: contains the source code.
!> - bin: contains the binary produced.
!>
!> The makefile provided already sets everything up for you:
!> - To compile it, you just need to issue the command `make`.
!> - To run it, simply issue the command `./bin/preamble`.
!>
!> If you have any questions, do not hesitate.
!> @author Ludovic Capelli
!> @email l.capelli@epcc.ed.ac.uk

!>@brief Just prints a hello world message.
PROGRAM main
	USE OMP_LIB

	IMPLICIT NONE

	INTEGER :: num_threads

	num_threads = omp_get_num_threads();
	WRITE(*, '(A,I0,A)') 'Hello world, there are ', num_threads, ' threads (i.e.: I am alone... T_T).'
END PROGRAM main
