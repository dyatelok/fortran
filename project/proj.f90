module shapes3d
  implicit none
  private determinant
  private units
  public parallelepiped
  public pyramid
  public cylinder
  public cylinder1
  public cylinder2

  real :: pi = acos(-1.0)
contains

  character(len=16) function units(unit) result(res)
    real, intent(in) :: unit
        
    if (unit == 1) then
      res = 'Meters     '
    elseif (unit == 100) then 
      res = 'Centimeters'
    elseif (unit == 1000) then
      res = 'Millimeters'
    else
      res = 'Undefined  '
    endif 
  end function units

  subroutine parallelepiped(mod1, mod2)
    real, intent(in) :: mod1, mod2
    real :: a, b, c, vol
    
    print *, 'You`ve chosen a Rectangular Parallelepiped.'
    print *, 'Your input units are ', units(mod1)
    
    print *, 'Pleace enter it`s side a. (e.g. 1.0):'
    read(*,*) a
    a = a * mod1
    
    print *, 'Pleace enter it`s side b. (e.g. 1.0):'
    read(*,*) b
    b = b * mod1

    print *, 'Pleace enter it`s side c. (e.g. 1.0):'
    read(*,*) c
    c = c * mod1

    vol = a * b * c
    vol = vol * mod2**3
    print *, 'Volume of your shape is: ', vol, ' ', units(mod2), '**3'
  end subroutine parallelepiped
  
  subroutine pyramid(mod1, mod2)
    real, intent(in) :: mod1, mod2
    real :: a, b, c, p, q, r, vol
    real, dimension(5, 5) :: mat;
    
    print *, 'You`ve chosen a Triangular Pyramid.'
    print *, 'Your input units are ', units(mod1)
    
    print *, 'Pleace enter it`s side a. (e.g. 1.0):'
    read(*,*) a
    a = a / mod1
    
    print *, 'Pleace enter it`s side b. (e.g. 1.0):'
    read(*,*) b
    b = b / mod1
    
    print *, 'Pleace enter it`s side c. (e.g. 1.0):'
    read(*,*) c
    c = c / mod1
    
    print *, 'Pleace enter it`s side p. (e.g. 1.0):'
    read(*,*) p
    p = p / mod1
    
    print *, 'Pleace enter it`s side q. (e.g. 1.0):'
    read(*,*) q
    q = q / mod1
    
    print *, 'Pleace enter it`s side r. (e.g. 1.0):'
    read(*,*) r
    r = r / mod1

   
    mat(1,1) =  0.0
    mat(1,2) = r**2
    mat(1,3) = q**2
    mat(1,4) = a**2
    mat(1,5) =  1.0
    mat(2,1) = r**2
    mat(2,2) =  0.0
    mat(2,3) = p**2
    mat(2,4) = b**2
    mat(2,5) =  1.0
    mat(3,1) = q**2
    mat(3,2) = p**2
    mat(3,3) =  0.0
    mat(3,4) = c**2
    mat(3,5) =  1.0
    mat(4,1) = a**2
    mat(4,2) = b**2
    mat(4,3) = c**2
    mat(4,4) =  0.0
    mat(4,5) =  1.0
    mat(5,1) =  1.0
    mat(5,2) =  1.0
    mat(5,3) =  1.0
    mat(5,4) =  1.0
    mat(5,5) =  0.0
    
    vol = determinant(mat, 5)
    vol = vol * mod2**3
    print *, 'Volume of your shape is: ', vol, ' ', units(mod2), '**3'
    
  end subroutine pyramid

  real function determinant(matrix, n)
    real, dimension(:,:), intent(in) :: matrix
    integer, intent(in) :: n
    real, dimension(:,:), allocatable :: lu
    integer :: i, j, k
    real :: det
    real :: sign

    allocate(lu(n, n))
    lu = matrix

    sign = 1.0d0
    det = 1.0d0

    do i = 1, n-1
      if (lu(i, i) == 0.0d0) then
        do k = i+1, n
          if (lu(k, i) /= 0.0d0) then
            lu(i, i:n) = lu(k, i:n)
            lu(k, i:n) = matrix(i, i:n)
            sign = -sign
            exit
          end if
        end do
      end if

      do k = i+1, n
        lu(k, i:n) = lu(k, i:n) - lu(k, i) / lu(i, i) * lu(i, i:n)
      end do
    end do

    do i = 1, n
      det = det * lu(i, i)
    end do

    det = sign * det

    deallocate(lu)

  end function determinant
  
  subroutine obelisk(mod1, mod2)
    real, intent(in) :: mod1, mod2
    real :: height, a, a1, b, b1, vol
    
    print *, 'You`ve chosen Obelisk.'
    print *, 'Your input units are ', units(mod1)

    print *, 'Pleace enter it`s height. (e.g. 1.0):'
    read(*,*) height
    height = height / mod1
  
    print *, 'Pleace enter it`s a. (e.g. 1.0):'
    read(*,*) a
    a = a / mod1
    
    print *, 'Pleace enter it`s a1. (e.g. 1.0):'
    read(*,*) a1
    a1 = a1 / mod1
    
    print *, 'Pleace enter it`s b. (e.g. 1.0):'
    read(*,*) b
    b = b / mod1
    
    print *, 'Pleace enter it`s b1. (e.g. 1.0):'
    read(*,*) b1
    b1 = b1 / mod1

    vol = height * ( (2*a + a1) * b + (2*a1 + a) * b1) / 6
    vol = vol * mod2**3

    print *, 'Volume of your shape is: ', vol, ' ', units(mod2), '**3'
  end subroutine obelisk

  subroutine wedge(mod1, mod2)
    real, intent(in) :: mod1, mod2

    real :: height, a, a1, b, b1, vol
    
    print *, 'You`ve chosen Wedge.'
    print *, 'Your input units are ', units(mod1)

    print *, 'Pleace enter it`s height. (e.g. 1.0):'
    read(*,*) height
    height = height / mod1
  
    print *, 'Pleace enter it`s a. (e.g. 1.0):'
    read(*,*) a
    a = a / mod1
    
    print *, 'Pleace enter it`s a1. (e.g. 1.0):'
    read(*,*) a1
    a1 = a1 / mod1
    
    print *, 'Pleace enter it`s b. (e.g. 1.0):'
    read(*,*) b
    b = b / mod1
    

    vol = (2*a + a1) * b * height / 6
    vol = vol * mod2**3
    
    print *, 'Volume of your shape is: ', vol, ' ', units(mod2), '**3'
  end subroutine wedge
  
  subroutine cylinder(mod1, mod2)
    real, intent(in) :: mod1, mod2
    real :: height, radius, vol
    
    print *, 'You`ve chosen cylinder.'
    print *, 'Your input units are ', units(mod1)

    print *, 'Pleace enter it`s height. (e.g. 1.0):'
    read(*,*) height
    height = height / mod1 
  
    print *, 'Pleace enter it`s radius. (e.g. 1.0):'
    read(*,*) radius
    radius = radius / mod1

    vol = radius**2 * height * pi
    vol = vol * mod2**3

    print *, 'Volume of your shape is: ', vol, ' ', units(mod2), '**3'
  end subroutine cylinder

  subroutine cylinder1(mod1, mod2)
    real, intent(in) :: mod1, mod2
    real :: h1, h2, radius, vol
    
    print *, 'You`ve chosen Truncated Circular Cylinder.'
    print *, 'Your input units are ', units(mod1)

    print *, 'Pleace enter it`s h1. (e.g. 1.0):'
    read(*,*) h1
    h1 = h1 / mod1 
    
    print *, 'Pleace enter it`s h2. (e.g. 1.0):'
    read(*,*) h2
    h2 = h2 / mod1 
  
    print *, 'Pleace enter it`s radius. (e.g. 1.0):'
    read(*,*) radius
    radius = radius / mod1

    vol = radius**2 * pi * (h1 + h2) / 2
    vol = vol * mod2**3

    print *, 'Volume of your shape is: ', vol, ' ', units(mod2), '**3'
  end subroutine cylinder1
  
  subroutine cylinder2(mod1, mod2)
    real, intent(in) :: mod1, mod2
    real :: height, radius, a, b, alpha, vol
    
    print *, 'You`ve chosen Segment of a Cylinder.'
    print *, 'Your input units are ', units(mod1)

    print *, 'Pleace enter it`s height. (e.g. 1.0):'
    read(*,*) height
    height = height / mod1 
  
    print *, 'Pleace enter it`s radius. (e.g. 1.0):'
    read(*,*) radius
    radius = radius / mod1
    
    print *, 'Pleace enter it`s a. (e.g. 1.0):'
    read(*,*) a
    a = a / mod1
    
    print *, 'Pleace enter it`s b. (e.g. 1.0):'
    read(*,*) b
    b = b / mod1
    
    print *, 'Pleace enter it`s alpha. (e.g. 1.0):'
    read(*,*) alpha

    vol = height * (a * (3 * radius**2 - a**2) + 3 * radius**2 * (b - radius) * alpha) / (3 * b)
    vol = vol * mod2**3

    print *, 'Volume of your shape is: ', vol, ' ', units(mod2), '**3'
  end subroutine cylinder2

  subroutine pipe(mod1, mod2)
    real, intent(in) :: mod1, mod2
    real :: height, radius_in, radius_out, vol
    
    print *, 'You`ve chosen Pipe.'
    print *, 'Your input units are ', units(mod1)

    print *, 'Pleace enter it`s height. (e.g. 1.0):'
    read(*,*) height
    height = height / mod1 
  
    print *, 'Pleace enter it`s outer radius. (e.g. 1.0):'
    read(*,*) radius_out
    radius_out = radius_out / mod1

    print *, 'Pleace enter it`s inner radius. (e.g. 1.0):'
    read(*,*) radius_in
    radius_in = radius_in / mod1

    vol = (radius_out**2 - radius_in**2) * height * pi
    vol = vol * mod2**3

    print *, 'Volume of your shape is: ', vol, ' ', units(mod2), '**3'
  end subroutine pipe

  subroutine cone1(mod1, mod2)
    real, intent(in) :: mod1, mod2
    real :: height, radius, vol
    
    print *, 'You`ve chosen Sircular Straight Cone.'
    print *, 'Your input units are ', units(mod1)

    print *, 'Pleace enter it`s height. (e.g. 1.0):'
    read(*,*) height
    height = height / mod1 
  
    print *, 'Pleace enter it`s radius. (e.g. 1.0):'
    read(*,*) radius
    radius = radius / mod1

    vol = radius**2 * height * pi / 3
    vol = vol * mod2**3

    print *, 'Volume of your shape is: ', vol, ' ', units(mod2), '**3'
  end subroutine cone1

  subroutine cone2(mod1, mod2)
    real, intent(in) :: mod1, mod2
    real :: height, radius_l, radius_t, vol
    
    print *, 'You`ve chosen Truncated Straight Cone.'
    print *, 'Your input units are ', units(mod1)

    print *, 'Pleace enter it`s height. (e.g. 1.0):'
    read(*,*) height
    height = height / mod1 
  
    print *, 'Pleace enter it`s lower radius. (e.g. 1.0):'
    read(*,*) radius_l
    radius_l = radius_l / mod1

    print *, 'Pleace enter it`s top radius. (e.g. 1.0):'
    read(*,*) radius_t
    radius_t = radius_t / mod1

    vol = (radius_l**2 + radius_t**2 + radius_l * radius_t) * height * pi / 3
    vol = vol * mod2**3

    print *, 'Volume of your shape is: ', vol, ' ', units(mod2), '**3'
  end subroutine cone2

  subroutine sphere1(mod1, mod2)
    real, intent(in) :: mod1, mod2
    real :: radius, vol
    
    print *, 'You`ve chosen Sphere.'
    print *, 'Your input units are ', units(mod1)

    print *, 'Pleace enter it`s radius. (e.g. 1.0):'
    read(*,*) radius
    radius = radius / mod1

    vol = radius**3 * 4 / 3
    vol = vol * mod2**3

    print *, 'Volume of your shape is: ', vol, ' ', units(mod2), '**3'
  end subroutine sphere1
  
  subroutine sphere2(mod1, mod2)
    real, intent(in) :: mod1, mod2
    real :: radius, height, vol
    
    print *, 'You`ve chosen Spherical Sector.'
    print *, 'Your input units are ', units(mod1)

    print *, 'Pleace enter it`s radius. (e.g. 1.0):'
    read(*,*) radius
    radius = radius / mod1

    print *, 'Pleace enter it`s height. (e.g. 1.0):'
    read(*,*) height
    height = height / mod1

    vol = radius**2 * height * pi * 2 / 3
    vol = vol * mod2**3

    print *, 'Volume of your shape is: ', vol, ' ', units(mod2), '**3'
  end subroutine sphere2
  
  subroutine sphere3(mod1, mod2)
    real, intent(in) :: mod1, mod2
    real :: radius, height, a, vol
    
    print *, 'You`ve chosen Spherical Segment.'
    print *, 'Your input units are ', units(mod1)

    print *, 'Pleace enter it`s radius. (e.g. 1.0):'
    read(*,*) radius
    radius = radius / mod1

    print *, 'Pleace enter it`s height. (e.g. 1.0):'
    read(*,*) height
    height = height / mod1

    print *, 'Pleace enter it`s a. (e.g. 1.0):'
    read(*,*) a
    a = a / mod1

    vol = pi * height * (3 * a**2 + height**2) / 6
    vol = vol * mod2**3

    print *, 'Volume of your shape is: ', vol, ' ', units(mod2), '**3'
  end subroutine sphere3
  
  subroutine sphere4(mod1, mod2)
    real, intent(in) :: mod1, mod2
    real :: radius, height, a, b, vol
    
    print *, 'You`ve chosen Spherical Layer.'
    print *, 'Your input units are ', units(mod1)

    print *, 'Pleace enter it`s radius. (e.g. 1.0):'
    read(*,*) radius
    radius = radius / mod1

    print *, 'Pleace enter it`s height. (e.g. 1.0):'
    read(*,*) height
    height = height / mod1

    print *, 'Pleace enter it`s a. (e.g. 1.0):'
    read(*,*) a
    a = a / mod1
    
    print *, 'Pleace enter it`s b. (e.g. 1.0):'
    read(*,*) b
    b = b / mod1

    vol = pi * height * (3 * a**2 + 3 * b**2 + height**2) / 6
    vol = vol * mod2**3

    print *, 'Volume of your shape is: ', vol, ' ', units(mod2), '**3'
  end subroutine sphere4
  
  subroutine torus(mod1, mod2)
    real, intent(in) :: mod1, mod2
    real :: ring_radius, radius, vol
    
    print *, 'You`ve chosen Torus.'
    print *, 'Your input units are ', units(mod1)

    print *, 'Pleace enter it`s ring radius (R). (e.g. 1.0):'
    read(*,*) ring_radius
    ring_radius = ring_radius / mod1

    print *, 'Pleace enter it`s radius (r). (e.g. 1.0):'
    read(*,*) radius
    radius = radius / mod1

    vol = 2 * pi**2 * ring_radius * radius**2
    vol = vol * mod2**3

    print *, 'Volume of your shape is: ', vol, ' ', units(mod2), '**3'
  end subroutine torus
  
  subroutine barrel1(mod1, mod2)
    real, intent(in) :: mod1, mod2
    real :: height, top_d, mid_d, vol
    
    print *, 'You`ve chosen Spherical Barrel.'
    print *, 'Your input units are ', units(mod1)

    print *, 'Pleace enter it`s height. (e.g. 1.0):'
    read(*,*) height
    height = height / mod1

    print *, 'Pleace enter it`s top/bottom diameter. (e.g. 1.0):'
    read(*,*) top_d
    top_d = top_d / mod1

    print *, 'Pleace enter it`s middle diameter. (e.g. 1.0):'
    read(*,*) mid_d
    mid_d = mid_d / mod1

    vol = 0.262 * height * (2 * mid_d**2 + top_d**2)
    vol = vol * mod2**3

    print *, 'Approx volume of your shape is: ', vol, ' ', units(mod2), '**3'
  end subroutine barrel1
    
  subroutine barrel2(mod1, mod2)
    real, intent(in) :: mod1, mod2
    real :: height, top_d, mid_d, vol
    
    print *, 'You`ve chosen Parabolic Barrel.'
    print *, 'Your input units are ', units(mod1)

    print *, 'Pleace enter it`s height. (e.g. 1.0):'
    read(*,*) height
    height = height / mod1

    print *, 'Pleace enter it`s top/bottom diameter. (e.g. 1.0):'
    read(*,*) top_d
    top_d = top_d / mod1

    print *, 'Pleace enter it`s middle diameter. (e.g. 1.0):'
    read(*,*) mid_d
    mid_d = mid_d / mod1

    vol = pi * height * (8 * mid_d**2 + 4 * mid_d * top_d + 3 * top_d**2) / 60
    vol = vol * mod2**3

    print *, 'Volume of your shape is: ', vol, ' ', units(mod2), '**3'
  end subroutine barrel2
  
end module shapes3d

program main
  use shapes3d
  implicit none

  real :: vol, mod1, mod2
  logical :: do_exit
  ! logical :: sd
  integer :: shape_id, option
  character :: ans

  mod1 = 1.0
  mod2 = 1.0
  
  print *, 'Welcome to this program for the scientific computing!'
  
  main_menu: do
    print *, 'Here are your options:'
    print *, '0 - Exit'
    print *, '1 - Transition to the work menu'
    
    read(*,*) option

    if (option == 0) then
      exit main_menu
    elseif (option == 1) then
      print *, 'Youve transitioned to the work menu.'
    else
      print *, 'Unknown option'
      cycle main_menu
    endif

    work_menu: do
      print *, 'Here are your options:' 
      print *, '0 - Exit to the main menu'
      print *, '1 - Change units of measurement (meters by default)'
      print *, '2 - Calculate volumes'

      read(*,*) option

      if (option == 0) then
        exit work_menu
      elseif (option == 1) then
        print *, 'Here are your options for the imputs:' 
        print *, '0 - Meters'
        print *, '1 - Centimeters'
        print *, '2 - Millimeters'
  
        read(*,*) option

        if (option == 0) then
          mod1 = 1
          print *, 'Input units are set to Meters'
        elseif (option == 1) then
          mod1 = 100
          print *, 'Input units are set to Centieters'
        elseif (option == 2) then
          mod1 = 1000
          print *, 'Input units are set to Millieters'
        else
          print *, 'Input units are left untouched (Meters by default)'
        endif
  
        print *, 'Here are your options for the outputs:' 
        print *, '0 - Meters'
        print *, '1 - Centimeters'
        print *, '2 - Millimeters'

        read(*,*) option

        if (option == 0) then
          mod2 = 1
          print *, 'Output units are set to Meters'
        elseif (option == 1) then
          mod2 = 100
          print *, 'Output units are set to Centieters'
        elseif (option == 2) then
          mod2 = 1000
          print *, 'Output units are set to Millieters'
        else
          print *, 'Output units are left untouched (Meters by default)'
        endif        
  
        cycle work_menu
      elseif (option == 2) then
        print *, 'Here are all the shapes you can compute volume for:'
        print *, ' 1 - Rectangular Parallelepiped'
        print *, ' 2 - Triangular Pyramid'
        print *, ' 3 - Obelisk'
        print *, ' 4 - Wedge'
        print *, ' 5 - Circular Straight Cylinder'
        print *, ' 6 - Truncated Circular Cylinder'
        print *, ' 7 - Segment of a Cylinder'
        print *, ' 8 - Cylindrical Pipe'
        print *, ' 9 - Circular Straight Cone'
        print *, '10 - Truncated Straight Cone'
        print *, '11 - Sphere'
        print *, '12 - Spherical Sector'
        print *, '13 - Spherical Segment'
        print *, '14 - Spherical Layer'
        print *, '15 - Torus'
        print *, '16 - Spherical Barrel'
        print *, '17 - Parabolic Barrel'
        print *, 'Pleace enter id of the shepe you`d like to compute volume for:'

        read(*,*) shape_id

        print *, 'You`ve entered shape id:', shape_id

        if (shape_id == 1) then
          call parallelepiped(mod1, mod2)
        elseif (shape_id == 2) then
          call pyramid(mod1, mod2)
        elseif (shape_id == 3) then
          call obelisk(mod1, mod2)
        elseif (shape_id == 4) then
          call wedge(mod1, mod2)
        elseif (shape_id == 5) then
          call cylinder(mod1, mod2)
        elseif (shape_id == 6) then
          call cylinder1(mod1, mod2)
        elseif (shape_id == 7) then
          call cylinder2(mod1, mod2)
        elseif (shape_id == 8) then
          call pipe(mod1, mod2)
        elseif (shape_id == 9) then
          call cone1(mod1, mod2)
        elseif (shape_id == 10) then
          call cone2(mod1, mod2)
        elseif (shape_id == 11) then
          call sphere1(mod1, mod2)
        elseif (shape_id == 12) then
          call sphere2(mod1, mod2)
        elseif (shape_id == 13) then
          call sphere3(mod1, mod2)
        elseif (shape_id == 14) then
          call sphere4(mod1, mod2)
        elseif (shape_id == 15) then
          call torus(mod1, mod2)
        elseif (shape_id == 16) then
          call barrel1(mod1, mod2)
        elseif (shape_id == 17) then
          call barrel2(mod1, mod2)
        else
          print *, 'Unknown shape id. Pleace enter valid shape id.'
        end if
      else 
        print *, 'Unknown option'
      endif
 
    end do work_menu
  end do main_menu

  print *, 'Thanks for using this program!'

end program main
