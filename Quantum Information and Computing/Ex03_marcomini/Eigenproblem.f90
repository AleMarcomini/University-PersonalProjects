program test
    use COMPLEX_MATRIX
    use DEBUGGER
    implicit none

    ! TUNABLE PARAMS ----------------------------------------------------
    integer, parameter :: SIZE = 1000
    logical :: DEBUG = .FALSE.
    logical :: LOOP = .TRUE.
    integer :: idx, LOOP_SIZE = 10

    ! OTHER VARS ---------------------------------------------------------
    type(c_matrix) :: H, H_prime

    ! ZHEEV VARS ---------------------------------------------------------
    integer :: N = SIZE, LDA = SIZE, LWORK=max(1,2*SIZE-1), INFO=0
    complex(16), dimension(max(1,max(1,2*SIZE-1))) :: WORK
    double precision, dimension(SIZE) :: W 
    double precision, dimension(max(1, 3*SIZE-2)) :: RWORK 


    ! SINGLE TEST RUN ----------------------------------------------------

    IF (.NOT. LOOP) THEN
        call new_random_cmat(H,SIZE,SIZE,hermit=.TRUE.)
        call trace_eval(H)

        H_prime = H
        call adj(H)
        call Checkpoint(DEBUG, 'Check (H†) = H (sum of abs diffs < 1E-6):', SUM(ABS(H%mel-H_prime%mel))<1.E-6)
        call save_csv(H,'example.csv')

        call zheev( 'Numbers', 'Lower', N, H%mel, LDA, W, WORK, LWORK, RWORK, INFO )
        
        print*, 'The eigenvalues of the random matrix are:', W

    ELSE

        open(89, file = 'eigenvals.csv')

        DO idx = 1, LOOP_SIZE
            IF (MOD(idx,LOOP_SIZE/10)==0) THEN 
                PRINT*, 100*idx/LOOP_SIZE, '%'
            END IF
            call new_random_cmat(H,SIZE,SIZE,hermit=.TRUE.)
            call tr(H)
            call zheev( 'Numbers', 'Lower', N, H%mel, LDA, W, WORK, LWORK, RWORK, INFO )
            IF ((SUM(W) - REALPART(H%tr)) >= 1.E-6) THEN
                print*, 'WARNING: sum of eigenvals found differs from original trace by more than 1E-6'
            END IF
            write(89,'(*(G0.6,:,","))') W
        END DO

        close(89)

        open(90, file = 'randomvals.csv')
        DO idx = 1, LOOP_SIZE
            call random_number(W)
            write(90,'(*(G0.6,:,","))') W
        END DO
        close(90)

    END IF

    

end program test