program DoubleSum
    ! Performs the sum of two large real numbers 
    ! (comparing single and double precision)

    implicit none

    ! Define PI and useful variables
    real*4, parameter :: PI4=4.D0*DATAN(1.D0)
    real*8, parameter :: PI8=4.D0*DATAN(1.D0)

    real*4 :: a, b, result4
    real*8 :: c, d, result8

    ! Single precision
    a = PI4*1.E32
    b = SQRT(2.E0)*1.E21
    result4 = a + b
    print *, 'order(E32) + order(E21) (real*4) = ', result4  
    
    ! Double precision
    c = PI8*1.D32
    d = SQRT(2.D0)*1.D21
    result8 = c+d
    print *, 'order(E32) + order(E21) (real*8) = ', result8

    ! Comparison
    print *, 'Result difference smaller than 1E-12:', (result8-result4) < 1.D-12
    
end program DoubleSum