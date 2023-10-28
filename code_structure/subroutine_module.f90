module my_mod
  implicit none

  private  ! All entities are now module-private by default
  public public_var, print_matrix  ! Explicitly export public entities

  real, parameter :: public_var = 2
  integer :: private_var

contains

  ! Print matrix A to screen
  subroutine print_matrix(A)
    real, intent(in) :: A(:,:)  ! An assumed-shape dummy argument

    integer :: i

    do i = 1, size(A,1)
      print *, A(i,:)
    end do

  end subroutine print_matrix

end module my_mod

program use_mod
  use my_mod
  implicit none

  real :: mat(10, 10)

  mat(:,:) = public_var

  call print_matrix(mat)

end program use_mod