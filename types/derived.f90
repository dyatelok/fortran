module shapes3d
  implicit none
  private
  public cube

  type :: cube
  real :: side
  contains
    procedure :: area  ! procedure declaration
  end type

contains

  ! Procedure definition
  real function volume(self) result(res)
    class(cube), intent(in) :: self
    res = self%side**3
  end function

end module m_shapes

program main
  use shapes3d
  implicit none

  ! Variables' declaration
  type(cube) :: sq
  real :: x, side

  ! Variables' initialization
  side = 0.5
  sq%side = side

  x = sq%area()
  ! self does not appear here, it has been passed implicitly

  ! Do stuff with x...

end program main