program IntSum
    ! Performs the sum of two integer numbers 
    ! (comparing 2-byte and 4-byte precision)

    implicit none

    ! Define variables
    integer*2 :: a, b, result2
    integer*4 :: c, d, result4

    ! INT*2 computation (note: overflow detected,
    ! need to compute with -fno-range-check flag)
    a = 2000000
    b = 1
    result2 = a + b
    print *, '2.000.000 + 1 (integer*2) = ', result2
    
    ! INT*4 computation
    c = 2000000
    d = 1
    result4 = c + d
    print *, '2.000.000 + 1 (integer*4) = ', result4

end program IntSum