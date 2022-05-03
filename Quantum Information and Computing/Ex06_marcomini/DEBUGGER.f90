module DEBUGGER
! Define useful operations to facilitate debugging in different scenarios
  implicit none

contains

    subroutine Checkpoint(debug, message, var)
    ! Adds checkpoint where to print a message and/or 
    ! a scalar variable according to the debug flag

        ! IO declaration
        logical, intent(in) :: debug
        character(len=*), intent(in), optional :: message
        class(*), intent(in), optional :: var

        ! Main
        if (debug) then

            if (present(message)) then
                print*, message
            end if

            if (present(var)) then
                select type(var)

                    type is (integer(2))
                        print*, 'Type INTEGER(2): ', var

                    type is (integer(4))
                        print*, 'Type INTEGER(4): ', var
                    
                    type is (real(4))
                        print*, 'Type REAL(4): ', var

                    type is (real(8))
                        print*, 'Type REAL(8): ', var

                    type is (complex(8))
                        print*, 'Type COMPLEX(8): ', var
                        
                    type is (complex(16))
                        print*, 'Type COMPLEX(16): ', var

                    type is (logical)
                        print*, 'LOGICAL:', var

                    type is (character(len=*))
                        print*, 'CHARACTER: ', var
                        
                end select
            end if
        end if

    end subroutine

end module
