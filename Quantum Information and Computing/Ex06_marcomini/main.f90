! TO COMPILE in this directory:
! gfortran main.f90 DENSITYMATRICES.f90 DEBUGGER.f90 -o main.out

program DMat

    use DensityMatrices
    use debugger

    ! PLEASE SELECT MODE: 
    ! 1 : INITIALIZE GENERAL AND SEPARABLE MANY-BODY STATES
    ! 2 : STUDY OF BIPARTITE SYSTEM, ARBITRARY DIMENSION
    ! 3 : TEST ON QUBITS (MULTIPLE EXAMPLES)
    integer, parameter :: mod = 3
    print*, 'Selected mode: ', mod                    

    ! PART I : INITIALIZE GENERAL AND SEPARABLE MANY-BODY STATES
    if (mod == 1) then     

        block 
        
        ! TUNABLE parameters
        integer, parameter :: n = 5                     ! Number of subsystems
        integer, parameter :: d = 2                     ! Dimension of subsystems
        logical :: DEBUG = .TRUE.

        ! Other vars
        double complex :: psi(d*n)                      ! WF of whole system
        double complex :: rho(d**n,d**n)                ! DM of whole system
        integer :: ManyBodyQuantumState(n)              ! Sequence of subsystems states           

        ! Example: from integer representation to subsystem single states
        ManyBodyQuantumState = ToBasisConverter(100,D=d,N=n)

        ! Create single-subsystem wfs and store altogether
        psi = NewSeparableState(d*n)

        !Create new density matrix (pure many-body state)
        rho = NewDensityMatrix(d**n)

        ! Check if Rho is a pure-state density matrix:
        call Checkpoint(DEBUG, 'Trace of Rho: (expected 1)', trace(rho))                
        call Checkpoint(DEBUG, 'Trace of Rho^2: (expected 1)', trace(matmul(rho,rho)))

        end block




    ! PART II : BIPARTITE SYSTEM, ARBITRARY DIMENSION
    else if (mod == 2) then

        block 
    
        ! TUNABLE parameters
        integer, parameter :: n = 2                       ! Number of subsystems
        integer, parameter :: d = 5                     ! Dimension of subsystems
        logical :: DEBUG = .TRUE.
    
        ! Other variables
        integer, parameter :: A = 1
        integer, parameter :: B = 0
        double complex :: rho(d**n,d**n)                 ! DM of whole system
        double complex :: rho_A(d,d), rho_B(d,d)         ! DM of subsystems
    
        ! Random pure density matrix 
        rho = NewDensityMatrix(d**n)
    
        ! Reduced density matrices
        rho_A = ReducedDensityMatrix(rho, A, d)
        rho_B = ReducedDensityMatrix(rho, B, d)
    
        call Checkpoint(DEBUG, 'Trace of Rho_A:', trace(rho_A))
        call Checkpoint(DEBUG, 'Trace of Rho_A^2:', trace(matmul(rho_A,rho_A)))
        call Checkpoint(DEBUG, 'Trace of Rho_B:', trace(rho_B))
        call Checkpoint(DEBUG, 'Trace of Rho_B^2:', trace(matmul(rho_B,rho_B)))
    
        end block
    
    ! PART III : TEST ON QUBITS
    else if (mod == 3) then

        block 
        
        ! There are no tunable parameters here - Qubit case
        integer, parameter :: n = 2                     ! Number of subsystems
        integer, parameter :: d = 2                     ! Dimension of subsystems
        logical :: DEBUG = .TRUE.
        integer, parameter :: A = 1
        integer, parameter :: B = 0
        double complex :: rho(4,4)                       ! DM of whole system
        double complex :: rho_A(2,2), rho_B(2,2)         ! DM of subsystems
        double complex :: psi(4)                         ! WF of whole system

        ! Test for random pure AB state:

        rho = NewDensityMatrix(d**n)

        rho_A = ReducedDensityMatrix(rho, A, d)
        rho_B = ReducedDensityMatrix(rho, B, d)

        call Checkpoint(DEBUG, 'Trace of Rho_A:', trace(rho_A))
        call Checkpoint(DEBUG, 'Trace of Rho_A^2:', trace(matmul(rho_A,rho_A)))
        call Checkpoint(DEBUG, 'Trace of Rho_B:', trace(rho_B))
        call Checkpoint(DEBUG, 'Trace of Rho_B^2:', trace(matmul(rho_B,rho_B)))
        
        ! Test for AB state: |00>

        psi = 0.d0
        psi(1) = complex(1,0)
        rho = OuterProduct(psi,conjg(psi))

        rho_A = ReducedDensityMatrix(rho, A, d)
        rho_B = ReducedDensityMatrix(rho, B, d)

        print*, '----------- AB in | 00 > -----------'
        print*, 'rho_A:'
        write(*, "(*('('sf6.2xspf6.2x'i)':x))") rho_A(1,:)
        write(*, "(*('('sf6.2xspf6.2x'i)':x))") rho_A(2,:)
        print*, 'rho_B:'
        write(*, "(*('('sf6.2xspf6.2x'i)':x))") rho_B(1,:)
        write(*, "(*('('sf6.2xspf6.2x'i)':x))") rho_B(2,:)

        ! Test for AB state: |psi+> (Bell state)

        rho = 0.d0
        rho(1,1) = 1./2.
        rho(1,4) = 1./2.
        rho(4,1) = 1./2.
        rho(4,4) = 1./2.

        rho_A = ReducedDensityMatrix(rho, A, d)
        rho_B = ReducedDensityMatrix(rho, B, d)

        print*, ''
        print*, '---------- AB in |psi + > ----------'
        print*, 'rho_A:'
        write(*, "(*('('sf6.2xspf6.2x'i)':x))") rho_A(1,:)
        write(*, "(*('('sf6.2xspf6.2x'i)':x))") rho_A(2,:)
        print*, 'rho_B:'
        write(*, "(*('('sf6.2xspf6.2x'i)':x))") rho_B(1,:)
        write(*, "(*('('sf6.2xspf6.2x'i)':x))") rho_B(2,:)

        end block

    else 
        print*, 'ERROR: invalid mode entered: please select 1,2 or 3'
    end if


end program DMat


