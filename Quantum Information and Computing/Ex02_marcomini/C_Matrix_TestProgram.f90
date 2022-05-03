program test
! Test program for the correct functioning of COMPLEX_MATRIX method
    use COMPLEX_MATRIX 
    use DEBUGGER
    implicit none

    ! TUNABLE PARAMETERS ---------------------------------------

    integer, parameter :: size1 = 100, size2 = 100
    ! matrix axes sizes
    
    logical, parameter :: DEBUG = .TRUE.
    ! Debug mode for feedback printing

    ! OTHER VARIABLES -----------------------------------------
    type(c_matrix) :: m,m_prime

    ! MAIN ----------------------------------------------------
    
    call new_random_cmat(m,size1,size2)

    call Checkpoint(DEBUG, 'First matrix element:', m%mel(1,1))
    call Checkpoint(DEBUG, 'Matrix dimensions:', m%dims(1))
    call Checkpoint(DEBUG, var=m%dims(2))
    
    call tr(m)
    call Checkpoint(DEBUG, 'Trace evaluation:', m%tr)

    ! Performs two times the andjoint, then checks compatibility with identity
    m_prime = m
    call adj(m)
    call adj(m)
    call Checkpoint(DEBUG, 'Check (M†)† = M (sum of abs diffs < 1E-6):', SUM(ABS(m%mel-m_prime%mel))<1.E-6)
    
    call save_csv(m,'test_random_cmatrix.csv')

end program test