module Hamiltonian
    implicit none

    interface ExpMatrix
        module procedure ExpRealVector
        module procedure ExpComplexVector
        module procedure ExpRealMatrix
        module procedure ExpComplexMatrix    
    end interface ExpMatrix

    interface SaveCsv
        module procedure SaveCsvVec
        module procedure SaveCsvComplexVec
        module procedure SaveCsvMat 
        module procedure SaveCsvComplexMat
    end interface SaveCsv

contains

    function TensorProduct(A,B) result(C)
        double complex, dimension(:,:) :: A, B
        double complex, dimension(size(A,1)*size(B,1),size(A,2)*size(B,2)) :: C
        integer :: ii, jj

        C = complex(0.d0,0.d0)
        
        do ii = 0, size(A,1)-1
            do jj = 0, size(A,2)-1
                C(ii*size(B,1)+1:(ii+1)*size(B,1), jj*size(B,2)+1:(jj+1)*size(B,2)) = A(ii+1,jj+1)*B
            end do
        end do

    end function

    subroutine KineticTerm(K,hbar)
        real(4), intent(inout) :: K(:,:)
        real(4), intent(in) :: hbar
        integer :: ii

        K = 0.

        if (size(K,1)/=size(K,2)) then
            print*, "ERROR: non-squared Hamiltonian"
            continue
        end if

        do ii = 1,size(K,1)
            K(ii,ii) = 2.
            if (ii<size(K,1)) then
                K(ii,ii+1) = 1.
                K(ii+1,ii) = 1.
            end if
        end do

        K = hbar*K

    end subroutine


    subroutine PotentialTerm(V,V_diag)
        real(4), intent(in) :: V_diag(:)
        real(4), intent(inout) :: V(:,:)
        integer :: ii

        if (size(V,1)/=size(V,2)) then
            print*, "ERROR: non-squared Hamiltonian"
            continue
        end if
    
        do ii = 1,size(V_diag)
            V(ii,ii) = V(ii,ii) + V_diag(ii)
        end do
    
    end subroutine

    subroutine SaveCsvMat(H,filename)
        ! Save two-dim array as csv
        real(8), intent(in) :: H(:,:)
        character(len=*), intent(in) :: filename
        integer :: ii

        open(89, file = filename)
        do ii = 1,size(H,1)
            write(89,'(*(G0.20,:,","))') H(ii,:)
        end do
        close(89)

    end subroutine

    subroutine SaveCsvComplexMat(H,filename)
        ! Save two-dim array as csv
        double complex, intent(in) :: H(:,:)
        character(len=*), intent(in) :: filename
        integer :: ii

        open(89, file = filename)
        do ii = 1,size(H,1)
            write(89,'(*(G0.20,:,","))') H(ii,:)
        end do
        close(89)

    end subroutine

    subroutine SaveCsvVec(vec,filename)
        ! Save one-dim array as txt
        real(8), intent(in) :: vec(:)
        character(len=*), intent(in) :: filename

        open(89, file = filename)
        write(89,'(*(G0.20,:,","))') vec
        close(89)

    end subroutine

    subroutine SaveCsvComplexVec(vec,filename)
        ! Save one-dim array as txt
        double complex, intent(in) :: vec(:)
        character(len=*), intent(in) :: filename

        open(89, file = filename)
        write(89,'(*(G0.20,:,","))') vec
        close(89)

    end subroutine

    subroutine Visualize(Mat, message)
        double complex, dimension(:,:), intent(in) :: Mat
        character(len=*), intent(in) :: message
        integer :: ii

        print*, ''
        print*, message
        print*, ''
        do ii=1,size(Mat,1)
            write(*, "(*('('sf6.2xspf6.2x'i)':x))") Mat(ii,:)
        end do
    end subroutine

    subroutine ExpRealVector(M)
        double precision, intent(inout) :: M(:)
        integer :: ii

        do ii=1,size(M)
            M(ii) = exp(M(ii))
        end do

    end subroutine

    subroutine ExpComplexVector(M)
        double complex, intent(inout) :: M(:)
        integer :: ii

        do ii=1,size(M)
            M(ii) = exp(M(ii))
        end do

    end subroutine

    subroutine ExpRealMatrix(M)
        double precision, intent(inout) :: M(:,:)
        integer :: ii

        do ii=1,size(M,1)
            M(ii,ii) = exp(M(ii,ii))
        end do

    end subroutine

    subroutine ExpComplexMatrix(M)
        double complex, intent(inout) :: M(:,:)
        integer :: ii

        do ii=1,size(M,1)
            M(ii,ii) = exp(M(ii,ii))
        end do

    end subroutine

    function cnorm(psi, dx) result(norm_psi)
        ! Returns the complex norm of an array
            double complex :: psi(:)
            double precision :: dx, norm_psi
            norm_psi = sqrt(real(sum(conjg(psi)*psi)*dx))
    
    end function

end module