module shapes3d
  implicit none
  private
  public t_parallelepiped
  public t_cylinder


  real :: pi = acos(-1.0)

  type :: t_parallelepiped
  real :: side_a
  real :: side_b
  real :: side_c
  contains
    procedure :: parallelepiped_read
    procedure :: parallelepiped_volume
  end type
  
  type :: t_cylinder
  real :: height
  real :: radius
  contains
    procedure :: cylinder_read
    procedure :: cylinder_volume
  end type

contains

  subroutine parallelepiped_read(self)
    class(t_parallelepiped), intent(out) :: self
    real :: side_a, side_b, side_c
    
    print *, 'You`ve chosen cube. Pleace enter it`s side a. (e.g. 1.0):'
    read(*,*) side_a
    self%side_a = side_a

    print *, 'Pleace enter it`s side b. (e.g. 1.0):'
    read(*,*) side_b
    self%side_b = side_b

    print *, 'Pleace enter it`s side c. (e.g. 1.0):'
    read(*,*) side_c
    self%side_c = side_c

  end subroutine parallelepiped_read
  
  real function parallelepiped_volume(self) result(res)
    class(t_parallelepiped), intent(in) :: self
    res = self%side_a * self%side_b * self%side_c
  end function

  subroutine cylinder_read(self)
    class(t_cylinder), intent(out) :: self
    real :: height, radius
    
    print *, 'You`ve chosen cylinder. Pleace enter it`s height. (e.g. 1.0):'
    read(*,*) height
    self%height = height
  
    print *, 'Pleace enter it`s radius. (e.g. 1.0):'
    read(*,*) radius
    self%radius = radius

  end subroutine cylinder_read
  
  real function cylinder_volume(self) result(res)
    class(t_cylinder), intent(in) :: self
    res = self%radius**2 * self%height * pi
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
  type(t_parallelepiped) :: parallelepiped
  type(t_cylinder) :: cylinder
  real :: vol

  print *, 'Welcome to this program for the scientific computing!'
  
  outer_loop: do 
    print *, 'Here are all the shapes you can compute volume for:'
    print *, '1 - Rectangular Parallelepiped'
    print *, '2 - Cylinder'
    print *, 'Pleace enter id of the shepe you`d like to compute volume for:'
  
    read(*,*) shape_id

    print *, 'You`ve entered shape id:', shape_id
  
    if (shape_id == 1) then
      call parallelepiped%parallelepiped_read()
      vol = parallelepiped%parallelepiped_volume()
      print *, 'Volume of your shape is:', vol

      do_exit = inner()

      if (do_exit) then
        exit outer_loop
      else
        cycle outer_loop
      end if
    
    elseif (shape_id == 2) then
      call cylinder%cylinder_read()
      vol = cylinder%cylinder_volume()
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
