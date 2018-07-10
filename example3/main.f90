module constants
  real(8), parameter :: PI = 2.d0 * asin(1.d0)
end module constants

program main
  use constants
  implicit none

  real(8), dimension(:), allocatable :: f, res
  real(8) :: dx, rsum
  integer :: i, j, N, M
  real(8) :: t0, t1, wallclock

#ifdef _CUDA
  print*, "TODO: Enable GPU support by modifying these files! Exiting!"
  stop
#endif

  ! Taken from AUSURF112 case
  N = 821 ! Number of grid points
  M = 274865 ! Number of integrals

  allocate(f(N))
  allocate(res(M))

  ! Assume equispaced grid from 0 to 1
  dx = 1.d0 / (N-1)

  t0 = wallclock()
  do i = 1, M
    ! Compute and store function values for integration
    do j = 1, N
      f(j) = cos(mod(i-1, 10)*PI * (j-1)*dx)
    end do

    ! Integrate with Simpson's rule. Store result in res array
    rsum = 0.d0
    do j = 2, N-1, 2
      rsum = rsum + f(j-1) + 4.d0*f(j) + f(j+1)
    end do

    res(i) = dx * rsum / 3.d0

  end do
  t1 = wallclock()

  ! Check results (Should be 1 if mod(i-1,10) == 0, 0 otherwise)
  do i = 1, M
    if (mod(i-1, 10) == 0 .and. abs(res(i)) /= 1.d0) then
      print*, "test failed."
      stop
    else if (mod(i-1, 10) /= 0 .and. abs(res(i)) > 1d-12) then
      print*, "test failed."
      stop
    endif
  end do

  print*, "test passed!"
  print "(i6,1x,a,1x,f9.5,1x,a)", M, "integrals completed in", t1 - t0, "seconds"


end program main
