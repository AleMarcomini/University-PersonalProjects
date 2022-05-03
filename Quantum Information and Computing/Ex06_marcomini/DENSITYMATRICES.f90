module DensityMatrices
! Contains functions to initialize and manipulate density matrices

    implicit none

contains

    function NewDensityMatrix(D) result(rho)
    ! Randomly initialize a DM for PURE STATE

        integer :: D                                    ! Dimension of rho: (D x D)
        double complex, dimension(D) :: psi
        double complex, dimension(D,D) :: rho

        psi = ComplexRandom(D)
        psi = psi/cnorm(psi,1.D0)
        rho = OuterProduct(psi,conjg(psi))

    end function

    function NewSeparableState(D) result(psi)
    ! Randomly initialize a Coeffs for a wf vector of separable states

        integer :: D                                    ! Dimension of psi: D
        double complex, dimension(D) :: psi

        psi = ComplexRandom(D)
        psi = psi/cnorm(psi,1.D0)

    end function

    function CollectMatrixElement(mm,nn,rho_AB,system_rank,subsys_dim) result(mel)
    ! Returns the reduced DM of a bipartite system tracing out the other 

        integer :: mm, nn                               ! Indexes of reduced matrix element to compute
        integer :: system_rank                          ! To compute rho_A: system_rank = 1, else 0
        integer :: subsys_dim                           ! Subsystem rho dimension : (subsys_dim x subsys_dim)

        integer :: ii, ket_idx, bra_idx                 
        double complex, dimension(:,:) :: rho_AB
        double complex :: mel

        mel = 0.d0

        do ii = 0, subsys_dim-1
            bra_idx = mm * subsys_dim**system_rank + ii * subsys_dim**(1-system_rank) + 1
            ket_idx = nn * subsys_dim**system_rank + ii * subsys_dim**(1-system_rank) + 1 
            mel = mel + rho_AB(bra_idx,ket_idx)
        end do

    end function

    function ReducedDensityMatrix(rho_AB,system_rank,subsys_dim) result(partial_rho)
    ! Computed the reduced density matrix on subsystem "system_rank" starting from the joint DM "rho_AB"

        double complex, dimension(:,:) :: rho_AB                                ! Two-system DM
        integer :: system_rank                                                  ! To compute rho_A: system_rank = 1, else 0
        integer :: subsys_dim                                                   ! Subsystem rho dimension : (subsys_dim x subsys_dim)
        integer :: ii, jj 
        double complex, dimension(subsys_dim,subsys_dim) :: partial_rho

        do ii = 0, subsys_dim-1
            do jj = 0, subsys_dim-1
                partial_rho(ii+1,jj+1) = CollectMatrixElement(mm=ii,nn=jj,rho_AB=rho_AB, &
                                                            system_rank=system_rank,subsys_dim=subsys_dim)
            end do
        end do

    end function
    
    
    function ToDecimalConverter(SubsystemStateSeq, D) result(DecimalSeq)
    ! Given a sequence of N subsystem states (each from 0 to D-1), returns its decimal notation

        integer, dimension(:) :: SubsystemStateSeq
        integer :: D, ii, DecimalSeq
        integer :: total = 0

        do ii = 0, size(SubsystemStateSeq)-1
            total = total + D**(ii)*SubsystemStateSeq(size(SubsystemStateSeq)-ii)
        end do

        DecimalSeq = total

    end function

    function ToBasisConverter(DecimalSeq, D, N) result(SubsystemStateSeq)
    ! Given a decimal notation, returns the sequence of N subsystem states (each from 0 to D-1) 
    
        integer :: DecimalSeq, D, N, ii, division_val, total
        integer, dimension(N) :: SubsystemStateSeq

        total = DecimalSeq

        do ii = N-1,0,-1
            division_val = floor(real(total/(D**ii)))
            SubsystemStateSeq(N-ii) = division_val
            total = total - division_val*D**ii
        end do

    end function

    function ComplexRandom(L) result(arr)
    ! Creates a L-long array of complex numbers
    
        integer, intent(in) :: L
        real :: re(L), im(L), sig_re(L), sig_im(L)
        complex :: arr(L) 

        call random_number(re) 
        call random_number(im)
        call random_number(sig_re) 
        call random_number(sig_im)
        re = sign(re, sig_re - 0.5)
        im = sign(im, sig_im - 0.5)
        arr = cmplx(re,im) 
    
    end function

    function OuterProduct(a,b) result(c)
    ! Performs outer product of two vectors: c = |axb|

        double complex, dimension(:) :: a,b 
        double complex, dimension(size(a),size(b)) :: c

        c = spread(a,dim=2,ncopies=size(b)) * spread(b,dim=1,ncopies=size(a)) 

    end function

    function cnorm(psi, dx) result(norm_psi)
    ! Returns the complex norm of an array
        
        double complex :: psi(:)
        double precision :: dx, norm_psi
        norm_psi = sqrt(real(sum(conjg(psi)*psi)*dx))

    end function

    function trace(mat) result(tr) 
    ! Returns the trace of a matrix 

        double complex, dimension(:,:) :: mat
        double complex :: tr
        integer :: ii

        tr = 0.d0

        do ii = 1, size(mat,1)
            tr = tr + mat(ii,ii)
        end do

    end function

end module DensityMatrices