! TO COMPILE in this directory:
! gfortran main.f90 HAMILTONIAN.f90 TFI.f90 -llapack -o main.out

program TrasverseFieldIsisngModel

    use Hamiltonian
    use TFI

    ! TUNABLE PARAMETERS
    integer, parameter :: N = 8           ! System size
    double precision :: lbd = 1.d0        ! Interaction strength
    logical :: TEST = .FALSE.
    logical :: PRINT_CSV = .FALSE.
    
    ! Matrices
    integer, parameter :: space_dim = 2**N           ! System size
    double complex :: H(space_dim,space_dim)

    ! Diagonalization parameters
    ! ZHEEV VARS ---------------------------------------------------------
    integer :: LDA = space_dim, LWORK=max(1,2*space_dim-1), INFO=0
    double complex, dimension(max(1,max(1,2*space_dim-1))) :: WORK
    double precision, dimension(space_dim) :: W 
    double precision, dimension(max(1, 3*space_dim-2)) :: RWORK 

    ! Time check
    double precision :: start, finish

    ! Test 
    if (TEST) then
        block
            double complex :: Tensor(6,6)

            Tensor = TensorProduct(Identity(3), sigma_Y())
            Call Visualize(Tensor, 'Tensor product id(3) x sigma_Y')
        end block
    end if
    
    ! Generate TFI hamiltonian
    call TFI_generator(H, System_size = N,lambda = lbd, periodic = .FALSE.)
    !call Visualize(H(1:4,:), 'TFI Hamiltonian beginning')

    ! Diagonalize
    call cpu_time(start)
    call zheev( 'Vectors', 'Lower', space_dim, H, LDA, W, WORK, LWORK, RWORK, INFO)
    call cpu_time(finish)
    if (INFO/=0) then
        print*, 'ERROR: ZHEEV failed to converge'
    end if

    !Save outputs 
    if (PRINT_CSV) then
        call SaveCsv(W,'Eigenvals_N4_L1.csv')
        call SaveCsv(realpart(H(:,1)),'GroundState_N4_L1.csv')
    end if
    print*, finish-start

end program TrasverseFieldIsisngModel


