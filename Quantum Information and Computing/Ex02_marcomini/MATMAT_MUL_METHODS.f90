module MATMAT_MUL_METHODS
! Define useful subroutines for standard Mat Mat multiplication methods
    implicit none

contains

    subroutine random_mat_mult(size, DEBUG, output_time)
        use DEBUGGER
        ! Initializes two random squared matrices, multiplies them

        ! IO declaration
        logical, intent(in) :: DEBUG 
        integer, intent(in) :: size(3)
        real, dimension(4), intent(out) :: output_time
        ! Introduce matrices: C = AB
        real :: A(size(1),size(2)), B(size(2),size(3)), C(size(1),size(3)), test(size(1),size(3))
        ! Time register
        real :: start, finish
        ! Debug message
        character(len=:), allocatable :: message


        ! Initialize randomly the matrices
        call random_number(A)
        call random_number(B)  

        ! Method number one
        call cpu_time(start)
        call MM_method_one(size,A,B,C)
        call cpu_time(finish)
        output_time(2) = finish - start
        test = C

        ! Method number two
        call cpu_time(start)
        call MM_method_two(size,A,B,C)
        call cpu_time(finish)
        output_time(3) = finish - start

        ! Checkpoint for debugging
        test = test - C
        message = 'Sum of squared differences (first, second method) below 1E-6:'
        call Checkpoint(DEBUG, message, SUM(test*test) < 1.0E-6)
        test = C

        ! Built-in method
        call cpu_time(start)
        C = MATMUL(A,B)
        call cpu_time(finish)
        output_time(4) = finish - start

        ! Postcondition on CPU time
        IF ((finish - start) < 0.) THEN
            print*, 'ERROR: Negative multiplication time - something went worng...'
            stop
        END IF 

        ! Checkpoint for debugging
        test = test - C
        message = 'Sum of squared differences (second method, built-in) below 1E-6:'
        call Checkpoint(DEBUG, message, SUM(test*test) < 1.0E-6)

    end subroutine random_mat_mult



    subroutine MM_method_one(size, A, B, C)
        ! First method: external loop over A's rows

        ! IO declaration
        integer, intent (in)  :: size(3)              
        real, intent (in)  :: A(size(1),size(2)), B(size(2),size(3))
        real, intent (out) :: C(size(1),size(3)) 
        ! Indeces
        integer :: I,J,K

        ! Restet output
        C = 0.0

        ! Iterative procedure
        do I = 1,size(1)
            do J = 1,size(3)
                do K = 1, size(2)
                    C(I,J) = C(I,J) + A(I,K)*B(K,J)
                end do
            end do
        end do
    end subroutine
    


    subroutine MM_method_two(size, A, B, C)
        ! Second method: external loop over A's columns

        ! IO declaration
        integer, intent (in)  :: size(3)              
        real, intent (in)  :: A(size(1),size(2)), B(size(2),size(3))
        real, intent (out) :: C(size(1),size(3)) 
        ! Indeces
        integer :: I,J,K

        ! Restet output
        C = 0.0

        ! Iterative procedure
        do J = 1,size(3)
            do I = 1,size(1)
                do K = 1, size(2)
                    C(I,J) = C(I,J) + A(I,K)*B(K,J)
                end do
            end do
        end do
    end subroutine

END MODULE