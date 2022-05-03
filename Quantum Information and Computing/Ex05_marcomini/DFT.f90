module DFT
! Simplifies interface for FFTW module

    implicit none
    
    ! parameter settings for FFTW
    integer FFTW_FORWARD,FFTW_BACKWARD
    parameter (FFTW_FORWARD=-1,FFTW_BACKWARD=1)

    integer FFTW_REAL_TO_COMPLEX,FFTW_COMPLEX_TO_REAL
    parameter (FFTW_REAL_TO_COMPLEX=-1,FFTW_COMPLEX_TO_REAL=1)

    integer FFTW_ESTIMATE,FFTW_MEASURE
    parameter (FFTW_ESTIMATE=0,FFTW_MEASURE=64)

    integer FFTW_OUT_OF_PLACE,FFTW_IN_PLACE,FFTW_USE_WISDOM
    parameter (FFTW_OUT_OF_PLACE=0)
    parameter (FFTW_IN_PLACE=8,FFTW_USE_WISDOM=16)

    integer FFTW_THREADSAFE
    parameter (FFTW_THREADSAFE=128)

contains
    
    subroutine FT(plan, signal_array, dft_array)
    ! Computes Forward FFT from signal array (coord domain) to dft_array(frequency domain)

        integer, parameter :: FFTW_ESTIMATE = 0
        double precision, parameter :: eps = 1.D-6
        double complex, dimension(:), intent(in) :: signal_array
        double complex, dimension(:), intent(out) :: dft_array
        double complex, dimension(size(signal_array)) :: plan_signal
        integer*8 :: plan, N 

        N = size(signal_array)

        ! Optimal plan determination
        call dfftw_plan_dft_1d(plan, N, plan_signal, dft_array, FFTW_FORWARD, FFTW_ESTIMATE)
        ! Variable assignment
        plan_signal = signal_array
        ! dft execution
        call dfftw_execute_dft(plan, plan_signal, dft_array, FFTW_FORWARD)
        ! Plan reset
        call dfftw_destroy_plan(plan)

    end subroutine

    subroutine IFT(plan, dft_array, signal_array)
    ! Computes Backward FFT from dft_array(frequency domain) to signal array (coord domain)

        integer, parameter :: FFTW_ESTIMATE = 0
        double precision, parameter :: eps = 1.D-6
        double complex, dimension(:), intent(in) :: dft_array
        double complex, dimension(:), intent(out) :: signal_array
        double complex, dimension(size(dft_array)) :: plan_dft
        integer*8 :: plan, N

        N = size(signal_array)

        ! Optimal plan determination
        call dfftw_plan_dft_1d(plan, N, plan_dft, signal_array, FFTW_BACKWARD, FFTW_ESTIMATE)
        ! Variable assignment
        plan_dft = dft_array
        ! dft execution
        call dfftw_execute_dft(plan, plan_dft, signal_array, FFTW_BACKWARD)
        ! Plan reset
        call dfftw_destroy_plan(plan)

    end subroutine

    function cnorm(psi, dx) result(norm_psi)
    ! Returns the complex norm of an array
        double complex :: psi(:)
        double precision :: dx, norm_psi
        norm_psi = sqrt(real(sum(conjg(psi)*psi)*dx))

    end function

    subroutine reverse(arr)
    ! Inverts order of elements in a complex array
        double complex, dimension(:), intent(inout) :: arr
        double complex :: temp
        integer :: ii

        do ii = 1, int(size(arr)/2)
            temp = arr(ii)
            arr(ii) = arr(size(arr)-ii+1)
            arr(size(arr)-ii+1) = temp
        end do
    end subroutine

    subroutine adjust_order(arr)
    ! Adjusts order of frequencies in distretization to FFTW rules
        double precision, dimension(:), intent(inout) :: arr
        double precision :: temp
        integer :: ii

        do ii = 1, int(size(arr)/2)
            temp = arr(ii)
            arr(ii) = arr(size(arr)/2+ii)
            arr(size(arr)/2+ii) = temp
        end do
    end subroutine

end module DFT