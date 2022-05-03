program MatMat_Mult
    ! Performs random matrix - matrix multiplication
    ! NOTE: update only tunable parameters

    use DEBUGGER
    use MATMAT_MUL_METHODS
    implicit none

    ! TUNABLE PARAMETERS -------------------------------------

    logical, parameter :: DEBUG = .TRUE.
    ! If .TRUE., activates debug mode and
    ! prints CPU time and compatibility result between different method outputs (Only for 'ALL' mode) 
    
    integer, dimension(3), parameter :: SIZE = (/10, 20, 24/)
    ! Size of matrices: (SIZE(1),SIZE(3)) = (SIZE(1),SIZE(2)) x (SIZE(2),SIZE(3))
    ! NOTE: Forcing matrix to be multiplied to share dimension along multiplication axes

    character(len=*), parameter :: MODE = 'ALL'
    ! 'MODE_1' : arcivates first mat-mat mult sequential method
    ! 'MODE_2' : arcivates second mat-mat mult sequential method
    ! 'BUILT_IN' : arcivates built-in method "MATMUL"
    ! 'ALL' : returns them all, together with CPU times


    ! OTHER VARIABLES ----------------------------------------

    real, dimension(4) :: OUTPUT_TIME

    ! MAIN ---------------------------------------------------

    ! Add precondition on input sizes: if wrong, return error and stop program
    IF (ANY(SIZE<1)) THEN
        print*, "ERROR: matrix sizes must be positive integers [line 15]"
        stop
    END IF


    ! Main code execution
    IF (mode=='ALL') THEN

        call random_mat_mult(SIZE, DEBUG, OUTPUT_TIME)

        call Checkpoint(DEBUG, message='Sizes where (N x M) = (N x K) x (K x M):')
        call Checkpoint(DEBUG, 'N = ', SIZE(1))
        call Checkpoint(DEBUG, 'K = ', SIZE(2))
        call Checkpoint(DEBUG, 'M = ', SIZE(3))
        call Checkpoint(DEBUG, 'CPU time (s) first method:', OUTPUT_TIME(2))
        call Checkpoint(DEBUG, 'CPU time (s) second method:', OUTPUT_TIME(3))
        call Checkpoint(DEBUG, 'CPU time (s) built-in method:', OUTPUT_TIME(4))

        !open(89, file = 'CPU_TIME_MATMULT.csv')
        !write(89,*) OUTPUT_TIME
        !close(89)

    ELSE IF (MODE == 'MODE_1') THEN

        block 

            ! Introduce matrices: C = AB
            real :: A(SIZE(1),SIZE(2)), B(SIZE(2),SIZE(3)), C(SIZE(1),SIZE(3))
            ! Time register
            real :: start, finish

            ! Initialize randomly the matrices
            call random_number(A)
            call random_number(B) 

            ! Method number one
            call cpu_time(start)
            call MM_method_one(size,A,B,C)
            call cpu_time(finish)

            ! Postcondition on CPU time
            IF ((finish - start) < 0.) THEN
                print*, 'ERROR: Negative multiplication time - something went worng...'
                stop
            END IF 

            call Checkpoint(DEBUG, 'CPU time (s) first method:', finish - start)

        end block

    ELSE IF (MODE == 'MODE_2') THEN

        block 

            ! Introduce matrices: C = AB
            real :: A(SIZE(1),SIZE(2)), B(SIZE(2),SIZE(3)), C(SIZE(1),SIZE(3))
            ! Time register
            real :: start, finish

            ! Initialize randomly the matrices
            call random_number(A)
            call random_number(B) 

            ! Method number one
            call cpu_time(start)
            call MM_method_two(size,A,B,C)
            call cpu_time(finish)

            ! Postcondition on CPU time
            IF ((finish - start) < 0.) THEN
                print*, 'ERROR: Negative multiplication time - something went worng...'
                stop
            END IF 

            call Checkpoint(DEBUG, 'CPU time (s) second method:', finish - start)


        end block

    ELSE IF (MODE == 'BUILT_IN') THEN

        block 

            ! Introduce matrices: C = AB
            real :: A(SIZE(1),SIZE(2)), B(SIZE(2),SIZE(3)), C(SIZE(1),SIZE(3))
            ! Time register
            real :: start, finish

            ! Initialize randomly the matrices
            call random_number(A)
            call random_number(B) 

            ! Method number one
            call cpu_time(start)
            C = MATMUL(A,B)
            call cpu_time(finish)

            ! Postcondition on CPU time
            IF ((finish - start) < 0.) THEN
                print*, 'ERROR: Negative multiplication time - something went worng...'
                stop
            END IF 

            call Checkpoint(DEBUG, 'CPU time (s) built-in method:', finish - start)

        end block
      
    ! Precondition: require valid operative mode
    ELSE 
        print*, 'ERROR: Invalid operative mode [line 18]'
        stop
    END IF

end program MatMat_Mult