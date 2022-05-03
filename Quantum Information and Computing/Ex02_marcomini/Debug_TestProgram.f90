program test
! Test program to veryfy DEBUGGER correct functioning on 
! Different input types

  use DEBUGGER
  implicit none

  ! Sets DEBUG MODE
  logical :: DEBUG = .TRUE.

  ! TUNABLE : Defines vars and message to be sent to Checkpoint
  character(len=:), allocatable  :: message
  integer(2) :: var1 = 12
  integer(4) :: var2 = 12
  real(4) :: var3 = 12.
  real(8) :: var4 = 12.
  complex(8) :: var5 = (12.,12.)
  complex(16) :: var6 = (12.,12.)
  logical :: var7 = .TRUE.
  character(len=6) :: var8 = 'Twelve'

  message='Test'

  ! Main execution -----------------------------------------

  call Checkpoint(debug, message, var1)
  call Checkpoint(debug, message, var2)
  call Checkpoint(debug, message, var3)
  call Checkpoint(debug, message, var4)
  call Checkpoint(debug, message, var5)
  call Checkpoint(debug, message, var6)
  call Checkpoint(debug, message, var7)
  call Checkpoint(debug, message, var8)
  
end program test