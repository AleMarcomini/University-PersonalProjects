module TFI
    use Hamiltonian
    implicit none
    
contains

    subroutine TFI_generator(H,System_size,lambda,periodic)
        integer, intent(in) :: System_size
        double precision, intent(in) :: lambda
        logical, intent(in) :: periodic
        double complex, intent(inout), dimension(2**System_size,2**System_size) :: H
        integer :: ii
        double complex :: sx_X_sx(4,4), sz_X_id(4,4)

        H = 0.d0

        ! Define once for all the tensor product of two sigma X
        sx_X_sx = TensorProduct(sigma_X(),sigma_X())
        ! Define once for all the tensor product of sigma Z and ID(2)
        sz_X_id = TensorProduct(sigma_Z(),Identity(2))

        ! First multi-site term
        H = - TensorProduct(sx_X_sx,Identity(2**(System_size-2)))

        ! First interaction term
        H = H + lambda * TensorProduct(sigma_Z(),Identity(2**(System_size-1)))

        ! Other multi-site terms
        do ii=2,System_size-1

            H = H - TensorProduct(TensorProduct(Identity(2**(ii-1)),sx_X_sx),Identity(2**(System_size-ii-1)))
            H = H + lambda * TensorProduct(TensorProduct(Identity(2**(ii-1)),sz_X_id),Identity(2**(System_size-ii-1)))
            
        end do

        ! Last interaction term
        H = H + lambda * TensorProduct(Identity(2**(System_size-1)), sigma_Z())

    end subroutine

    function adjoint(M) result(M_dag)
        ! Performs the complex coniugation + transposition (adjoint)
    
            double complex, dimension(:,:) :: M
            double complex, dimension(size(M,2),size(M,1)) :: M_dag
    
            M_dag = TRANSPOSE(CONJG(M))
            
    end function

    function Identity(N) result(I)
        integer :: N
        double complex, dimension(N,N) :: I
        integer :: ii

        I = complex(0.d0,0.d0)
        do ii = 1,N
            I(ii,ii) = 1.d0
        end do

    end function

    function Sigma_X() result(sx)
        double complex, dimension(2,2) :: sx

        sx = complex(0.d0,0.d0)
        sx(1,2) = 1.d0
        sx(2,1) = 1.d0

    end function

    function Sigma_Y() result(sy)
        double complex, dimension(2,2) :: sy

        sy = complex(0.d0,0.d0)
        sy(1,2) = complex(0.d0,-1.d0)
        sy(2,1) = complex(0.d0,1.d0)

    end function

    function Sigma_Z() result(sz)
        double complex, dimension(2,2) :: sz

        sz = complex(0.d0,0.d0)
        sz(1,1) = 1.d0
        sz(2,2) = -1.d0

    end function
end module TFI