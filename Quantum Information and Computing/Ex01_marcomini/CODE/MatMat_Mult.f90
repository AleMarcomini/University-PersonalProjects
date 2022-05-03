program MatMat_Mult
    ! Performs squared random matrix - matrix multiplication
    
    implicit none

    ! TUNABLE PARAMETERS -------------------------------------

    logical, parameter :: DEBUG = .FALSE.
    ! If .TRUE., prints compatibility result between different method outputs
    
    integer*2, parameter :: SIZE = 1000
    ! Size of the N times N matrices

    logical, parameter :: one_shot = .FALSE.
    ! If .TRUE. -> one shot (select size, returns methods compatibility) 
    ! If .FALSE. -> input sweep (collects CPU time as input scales, updates relative file)

    ! OTHER VARIABLES ----------------------------------------

    integer*2 :: N
    real, dimension(4) :: OUTPUT_TIME

    ! MAIN ---------------------------------------------------

    IF (one_shot) THEN
        call random_mat_mult(SIZE, DEBUG, OUTPUT_TIME)
        print*, 'Size:', SIZE
        print*, 'CPU time (s) first method:', OUTPUT_TIME(2)
        print*, 'CPU time (s) second method:', OUTPUT_TIME(3)
        print*, 'CPU time (s) built-in method:', OUTPUT_TIME(4)

    ELSE
        open(89, file = 'CPU_TIME_MATMULT.csv')

        DO N = 1, SIZE, 100
            OUTPUT_TIME(1) = N
            call random_mat_mult(N, DEBUG, OUTPUT_TIME)
            write(89,*) OUTPUT_TIME
        END DO

        close(89)

    END IF

end program MatMat_Mult



! SUBROUTINES ------------------------------------------------

subroutine random_mat_mult(size, debug, output_time)
    ! Initializes two random squared matrices, multiplies them

    ! IO declaration
    logical, intent(in) :: debug 
    integer*2, intent(in) :: size
    real, dimension(4), intent(out) :: output_time
    ! Introduce matrices: C = AB
    real, dimension(size,size) :: A, B, C, test
    ! Time register
    real :: start, finish

    ! Initialize randomly the matrices
    call random_number(A)
    call random_number(B)  

    ! Method number one
    call cpu_time(start)
    call MM_method_one(size,A,B,C)
    call cpu_time(finish)
    output_time(2) = finish - start

    IF (debug) THEN
        ! print*, 'Result first method:', C
        test = C
    END IF

    ! Method number two
    call cpu_time(start)
    call MM_method_two(size,A,B,C)
    call cpu_time(finish)
    output_time(3) = finish - start

    IF (debug) THEN
        ! print*, 'Result second method:', C
        test = test - C
        print*, 'Sum of squared differences (first, second method) below 1E-6:', SUM(test*test) < 1.0E-6
        test = C
    END IF

    ! Built-in method
    call cpu_time(start)
    C = MATMUL(A,B)
    call cpu_time(finish)
    output_time(4) = finish - start

    IF (debug) THEN
        ! print*, 'Result built-in method:', C
        test = test - C
        print*, 'Sum of squared differences (second method, built-in) below 1E-6:', SUM(test*test) < 1.0E-6
    END IF
end subroutine random_mat_mult



subroutine MM_method_one(size, A, B, C)
    ! First method: external loop over A's rows

    ! IO declaration
    integer*2, intent (in)  :: size              
    real, intent (in)  :: A(size,size), B(size,size)     
    real, intent (out) :: C(size,size) 
    ! Indeces
    integer*2 :: I,J,K

    ! Restet output
    C = 0.0

    ! Iterative procedure
    do I = 1,size
        do J = 1,size
            do K = 1, size
                C(I,J) = C(I,J) + A(I,K)*B(K,J)
            end do
        end do
    end do
end subroutine
  


subroutine MM_method_two(size, A, B, C)
    ! Second method: external loop over A's columns

    ! IO declaration
    integer*2, intent (in)  :: size              
    real, intent (in)  :: A(size,size), B(size,size)     
    real, intent (out) :: C(size,size) 
    ! Indeces
    integer*2 :: I,J,K

    ! Restet output
    C = 0.0

    ! Iterative procedure
    do J = 1,size
        do I = 1,size
            do K = 1, size
                C(I,J) = C(I,J) + A(I,K)*B(K,J)
            end do
        end do
    end do
end subroutine