module shapes3d
  implicit none
  private
  public t_cube

  type :: t_cube
  real :: side
  contains
    procedure :: volume
  end type

contains

  real function volume(self) result(res)
    class(t_cube), intent(in) :: self
    res = self%side**3
  end function

end module shapes3d

module inner_mod
  contains
  
    function inner() result(res)
      implicit none
      logical :: res
      character :: ans

      res = .false.
  
      do
        print *, 'Do you want to continue computing shape volumes? (y/n)'

        read(*,*) ans

        if (ans == 'y') then
          res = .false.
          exit
        else if (ans == 'n') then
          print *, 'Thanks for using this app!'
          res = .true.
          exit
        else 
          print *, 'Pleace enter `n` in you`d like to exit and `y` if you want to continue'
          cycle
        end if
      end do

    end function inner
  
end module inner_mod

program main
  use shapes3d
  use inner_mod
  implicit none

  logical :: do_exit
  integer :: shape_id
  character :: ans
  type(t_cube) :: cube
  real :: vol, side

  print *, 'Welcome to this program for the scientific computing!'
  
  outer_loop: do 
    print *, 'Here are all the shapes you can compute volume for:'
    print *, '1 - Cube'
    print *, 'Pleace enter id of the shepe you`d like to compute volume for:'
  
    read(*,*) shape_id

    print *, 'You`ve entered shape if:', shape_id
  
    if (shape_id == 1) then
      print *, 'You`ve chosen cube. Pleace enter it`s side. (e.g. 1.0):'

      read(*,*) side

      cube%side = side

      vol = cube%volume()

      print *, 'Volume of your shape is:', vol

      do_exit = inner()

      if (do_exit) then
        exit outer_loop
      else
        cycle outer_loop
      end if
    
    else
      print *, 'Unknown shape id. Pleace enter valid shape id.'

      do_exit = inner()

      if (do_exit) then
        exit outer_loop
      else
        cycle outer_loop
      end if

    end if
  end do outer_loop   

end program main