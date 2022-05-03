program TISchrod

    use HAMILTONIAN
    implicit none

    ! TUNABLE PARAMENTERS ----------------------------------------------------
    integer, parameter :: L = 10           !Interval semilength
    integer, parameter :: N = 2000         !# discretization points for space
    integer, parameter :: LDZ = 2000       !# discretization points for eigenfuncs
    real(8), parameter :: omega = 1.
    real(8), parameter :: hbar = 1.

    ! OTHER VARS ------------------------------------------------------------
    real(8), parameter :: dx = 2*REAL(L)/(REAL(N)-1)
    integer :: ii, INFO
    real(8), dimension(N) :: V_diag, X = [(ii*dx-L, ii = 0, N-1, 1)]
    real(8) :: D(N), E(N-1), WORK(max(1,2*N-2)), Z(LDZ, N)

    ! MAIN -----------------------------------------------------------------

    if (L<=0) then
        print*, 'ERROR: Unvalid L value. Please insert a positive value.'
        stop
    end if

    if (N<=0) then
        print*, 'ERROR: Unvalid N value. Please insert a positive value.'
        stop
    end if

    if (LDZ<=0) then
        print*, 'ERROR: Unvalid LDZ value. Please insert a positive value.'
        stop
    end if

    V_diag = (X**2)*(omega**2)

    D = (2./(dx**2))*hbar**2 + V_diag
    E = [(-1.,ii=1,N-1,1)]*(1./(dx**2))*hbar**2

    call DSTEV( 'V', N, D, E, Z, LDZ, WORK, INFO)
    if (INFO/=0) then
        print*, 'ERROR: DSTEV failed to converge'
    end if


    call SaveTxt(D,'Eigenvals.txt')
    call SaveCsv(Z,'Eigenvectors.csv')

    
end program TISchrod







