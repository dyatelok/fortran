module m_shapes
  implicit none
  private
  public t_square

  type :: t_square
  real :: side
  contains
    procedure :: area  ! procedure declaration
  end type

contains

  ! Procedure definition
  real function area(self) result(res)
    class(t_square), intent(in) :: self
    res = self%side**2
  end function

end module m_shapes

program main
  use m_shapes
  implicit none

  ! Variables' declaration
  type(t_square) :: sq
  real :: x, side

  ! Variables' initialization
  side = 0.5
  sq%side = side

  x = sq%area()
  ! self does not appear here, it has been passed implicitly

  ! Do stuff with x...

end program main