module COMPLEX_MATRIX
! MODULE containing new type C-MATRIX and all 
! operations for initialize, calculate and handle it
    implicit none

    ! Type definition
    type :: c_matrix
    COMPLEX(16), allocatable, dimension(:,:) :: mel ! matrix elements
    INTEGER(4), allocatable, dimension(:) :: dims   ! axes dimensions
    COMPLEX(16) :: tr, det                          ! trace & determinant
    end type

    ! Declare subroutines interfaces
    interface tr
        module procedure trace_eval
    end interface tr

    interface adj
        module procedure adjoint
    end interface adj

    interface save_csv
        module procedure save_matrix_csv
    end interface save_csv


contains 

    function complex_random(L) result(arr)
    ! Creates a L-long array of complex numbers

        integer, intent(in) :: L
        real :: re(L), im(L) 
        complex :: arr(L) 

        call random_number(re) 
        call random_number(im) 
        arr = cmplx(re,im) 

    end function

    subroutine new_random_cmat(mymat,size1,size2)
    ! Given a C-MATRIX type and the size of the two axes,
    ! randomly initializes the matrix elements and updates the "dims" attribute

        integer, intent(in) :: size1, size2
        type(c_matrix), intent(inout) :: mymat
        integer, dimension(2) :: cmat_size 
        complex(16), dimension(size1, size2) :: cmat_mel
        
        cmat_size = (/ size1, size2 /)
        cmat_mel = reshape(complex_random(size1*size2), (/size1,size2/))
        
        mymat%mel = cmat_mel
        mymat%dims = cmat_size

    end subroutine

    subroutine trace_eval(mat)
    ! Evaluates trace of the input C-MATRIX and stores it in
    ! the "tr" attribute
    
        type(c_matrix), intent(inout) :: mat
        integer :: ii
        complex(16) :: total = (0.,0.)

        IF (mat%dims(1) == mat%dims(2)) THEN
            DO ii = 0, mat%dims(1)
                total = total + mat%mel(ii,ii)
            END do
        ELSE
            PRINT*, "ERROR: Cannot compute trace of non-square matrix" 
        END IF

        mat%tr = total

    end subroutine

    subroutine adjoint(mat)
    ! Performs the complex coniugation + transposition (adjoint)
    ! of input C-MATRIX. Consistently, updates also the dimension, trace
    ! and determinant attributes

        type(c_matrix), intent(inout) :: mat

        mat%mel = TRANSPOSE(CONJG(mat%mel))
        mat%dims = (/mat%dims(2),mat%dims(1)/)
        mat%tr = CONJG(mat%tr)
        mat%det = CONJG(mat%det)
        
    end subroutine

    subroutine save_matrix_csv(mat, filename)
    ! Given a C-MATRIX in input, it saves all its attributes
    ! in a CSV file with input name, providing labels for clear
    ! visualization

        type(c_matrix), intent(in) :: mat
        character(len=*), intent(in) :: filename
        complex(16), dimension(mat%dims(2),mat%dims(1)) :: aux_transpose
        integer :: i

        aux_transpose = TRANSPOSE(mat%mel)   ! Needed to have consistent viualization (first index : rows)
        
        open(89, file = filename)
        
            write(89,*) 'REAL_PART_MATRIX_ELEMENTS'
            DO i = 1,mat%dims(1)
                write(89,'(*(G0.6,:,","))') REALPART(aux_transpose(:,i))
            END DO
            
            write(89,*) 'IMAG_PART_MATRIX_ELEMENTS'
            DO i = 1,mat%dims(1)
                write(89,'(*(G0.6,:,","))') IMAGPART(aux_transpose(:,i))
            END DO
            
            write(89,*) 'MATRIX_DIMENSIONS'
            write(89,'(*(G0.6,:,","))') mat%dims
            
            write(89,*) 'MATRIX_TRACE_(Re/Im)'
            write(89,*) mat%tr
            
            write(89,*) 'MATRIX_DETERMINANT_(Re/Im)'
            write(89,*) mat%det

        close(89)

    end subroutine

end module