program ArrayAverage
  implicit none

  ! Declare variables
  integer, parameter :: max_dim = 10 ! Maximum dimension for the array
  integer :: nrows, ncols, i, j, end
  real :: array(max_dim, max_dim)
  real :: average

  ! Prompt the user for array dimensions
  write(*,*) "Enter the number of rows (up to ", max_dim, "):"
  read(*,*) nrows
  write(*,*) "Enter the number of columns (up to ", max_dim, "):"
  read(*,*) ncols

  ! Check for valid dimensions
  if (nrows <= 0 .or. nrows > max_dim .or. ncols <= 0 .or. ncols > max_dim) then
    write(*,*) "Invalid array dimensions."
    stop
  end if

  ! Initialize the random number generator
  call srand(1)

  ! Populate the array with random integer values between 1 and 100
  do i = 1, nrows
    do j = 1, ncols
      array(i, j) = rand() * 100.0
    end do
  end do

  ! Display the array
  write(*,*) "Array:"
  do i = 1, nrows
    do j = 1, ncols
      write(*,'(I3)', advance='no') int(array(i, j))
    end do
    write(*,*)
  end do

  ! Calculate the average of the elements in the array
  average = 0.0
  do i = 1, nrows
    do j = 1, ncols
      average = average + array(i, j)
    end do
  end do
  average = average / (nrows * ncols)

  ! Display the average
  write(*,*) "Average:", average
  read(*,*) end

end program ArrayAverage
