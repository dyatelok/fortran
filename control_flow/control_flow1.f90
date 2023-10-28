program control_flow1
  implicit none

  real :: angle
  
  print *, 'Please enter your angle: '
  read(*,*) angle

  print *, 'Your angle is: ', angle

  if (angle < 90.0) then
    print *, 'Angle is acute'
  else if (angle < 180.0) then
    print *, 'Angle is obtuse'
  else
    print *, 'Angle is reflex'
  end if

end program control_flow1