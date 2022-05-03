! TO COMPILE in this directory:
! gfortran main.f90 HAMILTONIAN.f90 TFI.f90 -llapack -o main.out

program TrasverseFieldIsisngModel

    use Hamiltonian
    use TFI

    ! TUNABLE PARAMETERS
    integer, parameter :: N = 4           ! System size
    double precision :: lbd = 3.d0        ! Interaction strength
    double precision :: eps = 1.D-9       ! Convergence threshold
    logical :: TEST = .FALSE.
    logical :: PRINT_CSV = .FALSE.
    
    ! Matrices
    integer, parameter :: space_dim = 2**N           ! System size
    double complex, dimension(space_dim,space_dim) :: H_half, H_int_L, H_int_R
    double complex, dimension(space_dim**2,space_dim**2) :: H_tot, H_int, A
    double complex, dimension(space_dim**2,space_dim) :: M    ! Change of basis for reduction

    ! Diagonalization parameters
    ! ZHEEV VARS ---------------------------------------------------------
    integer :: LDA = space_dim**2, LWORK=max(1,2*space_dim**2-1), INFO=0
    double complex, dimension(max(1,max(1,2*space_dim**2-1))) :: WORK
    double precision, dimension(space_dim**2) :: W 
    double precision, dimension(max(1, 3*space_dim**2-2)) :: RWORK 

    ! Time check
    double precision :: start, finish

    ! Others
    double precision :: new_E, old_E
    logical :: thermod_limit = .FALSE.
    integer :: ii,jj

    ! Test 
    if (TEST) then
        block
            double complex :: Tensor(6,6)

            Tensor = TensorProduct(Identity(3), sigma_Y())
            Call Visualize(Tensor, 'Tensor product id(3) x sigma_Y')
        end block
    end if

    ! Interaction term between subsystems
    H_int_L = TensorProduct(Identity(2**(N-1)),sigma_X())
    H_int_R = TensorProduct(sigma_X(),Identity(2**(N-1)))
    
    ! Generate TFI hamiltonian
    call TFI_generator(H_half, System_size = N,lambda = lbd, periodic = .FALSE.)
    !call Visualize(H(1:4,:), 'TFI Hamiltonian beginning')

    ii = 1
    old_E = 100.0
    open(89, file = 'GroundStates.csv', position='append')
    

    do while (.NOT. thermod_limit .AND. ii < 40) 

        ! Construct full system Hamiltonian
        H_int = TensorProduct(H_int_L,H_int_R)
        H_tot = TensorProduct(H_half,Identity(space_dim)) + TensorProduct(Identity(space_dim),H_half) + H_int
        A = H_tot

        ! Diagonalize
        call zheev( 'Vectors', 'Lower', space_dim**2, A, LDA, W, WORK, LWORK, RWORK, INFO)
        if (INFO/=0) then
            print*, 'ERROR: ZHEEV failed to converge'
        end if

        ! Check Energy
        new_E = W(1)/(2**ii*N)
        thermod_limit = abs((new_E - old_E)/old_E) < eps

        ! Update subsystem parts
        M = A(:,1:space_dim)
        H_half = matmul(adjoint(M), matmul(H_tot,M))
        H_int_L = matmul(adjoint(M), matmul(TensorProduct(Identity(space_dim),H_int_L),M))
        H_int_R = matmul(adjoint(M), matmul(TensorProduct(H_int_R,Identity(space_dim)),M))
        H_int = TensorProduct(H_int_L,H_int_R)
        old_E = new_E

        print*, 'It:', ii, 'E/site:', new_E, ', Total sites:', 2**ii*N
        write(89,'(*(G0.20,:,","))') ii, new_E, 2**ii*N, N, lbd

        ii = ii + 1

    end do

    close(89)

    !Save outputs 
    if (PRINT_CSV) then
        call SaveCsv(W,'Eigenvals_N4_L1.csv')
        !call SaveCsv(realpart(H_tot(:,1)),'GroundState_N4_L1.csv')
    end if
    !print*, 'CPU time: ', finish-start

end program TrasverseFieldIsisngModel


