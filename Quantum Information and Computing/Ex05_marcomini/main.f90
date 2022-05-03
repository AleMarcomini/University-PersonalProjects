! TO COMPILE in this directory:
! gfortran main.f90 DFT.f90 DEBUGGER.f90 HAMILTONIAN.f90 -I/usr/local/include -L/usr/local/lib -lfftw3 -lm -llapack -o main.out


program HarmonicTranslation

    ! Needed for FFTW
    use, intrinsic :: iso_c_binding 
    
    ! User-defined libraries for the exercise
    use dft
    use debugger
    use hamiltonian
    implicit none

    ! Space paramters (dx = L/N) - tunable
    integer, parameter :: N = 2048                  ! discretization points for space (optimized for fftw)
    double precision, parameter :: Lx = 10.

    ! Time paramters (dt = T/M) - tunable
    integer, parameter :: M = 2000                  ! discretization points for time
    double precision, parameter :: T = 1.           ! tmax, also T parameter in hamiltonian definition
    logical, parameter :: DEBUG = .FALSE. 

    ! Hamiltonian paramters - tunable
    integer, parameter :: LDZ = N                   ! discretization points for eigenfuncs
    double precision, parameter :: omega = 1.
    double precision, parameter :: hbar = 1.

    ! OTHER VARS
    double precision, parameter :: dx = 2.*Lx/N
    double precision, parameter :: Lp = 4.D0*DATAN(1.D0)*N/(2*Lx)
    double precision, parameter :: dp = 2.*Lp/N
    double precision, parameter :: dt = T/M
    double precision, parameter :: eps = 1.D-6
    double complex, dimension(N) :: psi_x, psi_p, V, K
    integer*8 :: plan, ii, INFO
    double precision :: D(N), E(N-1), WORK(max(1,2*N-2)), Z(LDZ, N), X(N), P(N)

    ! Open filenames for saving (change at needs)
    open(89, file = 'psi_evolutionT1.csv')
    open(90, file = 'V_evolutionT1.csv')

    ! MAIN

    if (DEBUG) then
        ! Heaviside function test
        psi_x = 0.
        psi_x(N/2:N) = 1.
        psi_x = psi_x/cnorm(psi_x, dx)
        X = realpart(psi_x)

        call FT(plan, psi_x, psi_p)
        call IFT(plan, psi_p, psi_x)
        call Checkpoint(DEBUG, 'Inverse FT of FT norm before normalization:', cnorm(psi_x, dx))
        psi_x = psi_x/cnorm(psi_x, dx)
        call Checkpoint(DEBUG, 'Inverse FT of FT norm after normalization:', cnorm(psi_x, dx))
        call Checkpoint(DEBUG, 'Inverse FT of FT returns identity on heaviside up to 1E-6:', (norm2(X-realpart(psi_x)) < eps)) 
    end if

    ! Linspaces creation
    X = [(ii*dx-Lx+dx/2., ii = 0, N-1, 1)]
    P = [(ii*dp-Lp+dp/2., ii = 0, N-1, 1)]
    call adjust_order(P)                         ! Adjust order of frequencies according to fftw documentation

    ! Prepare kinetic term
    K = P**2/2.                                  ! Diagonal of K(p) in p-space
    K = complex(0.D0,-1.D0)*K*dt                 ! Time evolution exponent
    call ExpMatrix(K)

    ! Compute and collect ground state at t=0
    D = (1./(dx**2))*hbar**2 + (X**2)*(omega**2)/2.
    E = [(-1.,ii=1,N-1,1)]*(1./(2.*(dx**2)))*hbar**2

    call DSTEV( 'V', N, D, E, Z, LDZ, WORK, INFO)
    if (INFO/=0) then
        print*, 'ERROR: DSTEV failed to converge'
    end if

    psi_x = Z(:,1)
    psi_x = psi_x/cnorm(psi_x, dx)
    write(89,'(*(G0.10,:,","))') real(psi_x*conjg(psi_x))
    
    ! TIME EVOLUTION
    do ii=1,M
        call Checkpoint(DEBUG, 'Percentage complete:', ii/M*100)

        ! Renormalization step
        psi_p = 0.
        psi_x = psi_x/cnorm(psi_x, dx)

        ! Potential preparation
        V = ((X-real(ii)*dt)**2)*(omega**2)/2.      ! Diagonal of V(x) in x-space
        write(90,'(*(G0.10,:,","))') realpart(V)
        V = complex(0.D0,-1.D0)*V*dt/2.             ! Half time evolution exponent
        call ExpMatrix(V)

        ! Apply first potential term
        psi_x = V*psi_x

        ! Move to momenta domain
        call FT(plan, psi_x, psi_p)

        ! Apply kinetic term
        psi_p = K*psi_p
        psi_p = psi_p/cnorm(psi_p, dp)

        ! Move back to coord domain
        call IFT(plan, psi_p, psi_x)

        ! Potential preparation
        V = ((X-(real(ii)+0.5)*dt)**2)*(omega**2)/2.     ! Diagonal of V(x) in x-space
        write(90,'(*(G0.10,:,","))') realpart(V)
        V = complex(0.D0,-1.D0)*V*dt/2.                  ! Half time evolution exponent
        call ExpMatrix(V)   

        ! Apply second potential term
        psi_x = V*psi_x
        psi_x = psi_x/cnorm(psi_x, dx)
        write(89,'(*(G0.10,:,","))') real(psi_x*conjg(psi_x))

    end do

    close(89)
    close(90)
end program HarmonicTranslation


