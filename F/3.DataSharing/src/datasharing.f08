!> @file datasharing.c
!> @brief This example is to illustrate the basic data-sharing attributes.
!> @details You will find two folders in this example, and will in all examples
!> provided in this session:
!> - src: contains the source code.
!> - bin: contains the binary produced.
!> 
!> The makefile provided already sets everything up for you:
!> - To compile: `make`.
!> - To execute: `./bin/datasharing`.
!> 
!> If you have any questions, do not hesitate.
!> @author Ludovic Capelli (l.capelli@epcc.ed.ac.uk)

!> @brief Performs a few calculations, each requiring the corresponding variable
!> to be properly setup when passed to the parallel construct. The last loop
!> is not even parallelised at all, parallelise it and take care of the data-
!> sharing attributes.
PROGRAM main
	USE OMP_LIB

	IMPLICIT NONE

	INTEGER :: thread_count = 0
	INTEGER, ALLOCATABLE :: common_array(:)
	! Restriction: do not pass variable "step" as "shared".
	INTEGER :: step = 123;
	! Restriction: do not move the declaration of variable "my_thread_id"
	! inside the parallel region.
	INTEGER :: my_thread_id = 0
	INTEGER :: loop_i = 0

	!$OMP PARALLEL
		IF (omp_get_thread_num() .eq. 0) THEN
			thread_count = omp_get_num_threads()
		END IF
	!$OMP END PARALLEL
	ALLOCATE(common_array(0:thread_count-1));

	!$OMP PARALLEL
		my_thread_id = omp_get_thread_num();
		common_array(my_thread_id) = my_thread_id * step;
	!$OMP END PARALLEL

	DO loop_i = 0, thread_count-1
		WRITE(*, '(A,I0,A,I0,A)') 'Value of common_array[', loop_i, '] = ', common_array(loop_i)
	END DO
END PROGRAM main
