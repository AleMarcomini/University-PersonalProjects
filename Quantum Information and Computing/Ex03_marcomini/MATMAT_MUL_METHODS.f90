subroutine random_mat_mult(size, output_time)
    ! Initializes two random squared matrices, multiplies them

    ! IO declaration
    integer, intent(in) :: size(3)
    real, dimension(4), intent(out) :: output_time
    ! Introduce matrices: C = AB
    real :: A(size(1),size(2)), B(size(2),size(3)), C(size(1),size(3)), test(size(1),size(3))
    ! Time register
    real :: start, finish
    ! Indexes
    integer :: I,J,K


    ! Initialize randomly the matrices
    call random_number(A)
    call random_number(B)  

    ! Method number one
    call cpu_time(start)
    ! Restet output
    C = 0.0

    ! Iterative procedure - method 1
    do I = 1,size(1)
        do J = 1,size(3)
            do K = 1, size(2)
                C(I,J) = C(I,J) + A(I,K)*B(K,J)
            end do
        end do
    end do
    call cpu_time(finish)
    output_time(2) = finish - start
    test = C

    ! Method number two
    call cpu_time(start)
    ! Restet output
    C = 0.0

    ! Iterative procedure - method 2
    do J = 1,size(3)
        do I = 1,size(1)
            do K = 1, size(2)
                C(I,J) = C(I,J) + A(I,K)*B(K,J)
            end do
        end do
    end do
    call cpu_time(finish)
    output_time(3) = finish - start

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

end subroutine random_mat_mult