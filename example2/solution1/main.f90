program main
#ifdef _CUDA
  use cudafor
#endif
  use vector_ops
  implicit none

  real(8), dimension(:), allocatable :: a, b
#ifdef _CUDA
  ! In this example, we are creating separate device copies of arrays a and b. 
  ! A suffix of `_d` is a typical choice to denote this.
  real(8), dimension(:), allocatable, device :: a_d, b_d
  integer :: istat
#endif
  real(8) :: x
  integer :: i, N
  real(8) :: t0, t1, add_time, dot_time, wallclock

#ifdef _CUDA
  print*, "Running GPU version!"
#endif
  
  N = 1024*1024*1024/8
  allocate(a(N), b(N))
  
  a = 2.d0
  b = 4.d0

#ifdef _CUDA
  ! Again, we are using a source allocation to make array copies. This time, we are making device
  ! copies of host arrays.
  allocate(a_d, source = a)
  allocate(b_d, source = b)
#endif

  ! Add entries of array b to entries of array a
  t0 = wallclock()
  call vector_add(a, b, N)
  add_time = wallclock() - t0

  ! Compute dot product of a and b, store result in x
  t0 = wallclock()
  x = dot(a, b, N)
  dot_time = wallclock() - t0

  print*, "x = ", x
  
  ! Check for errors
  if (any(a /= 6.d0)) then
    print*, "vector_add failed!"
  elseif (x /= 24.d0*N) then
    print*, "dot failed!"
  else
    print*, "CPU passed!"
  end if 

  print "(a,1x,f9.5,1x,a,1x,f9.5)", "add time:", add_time, "dot time:", dot_time
  print*  

#ifdef _CUDA
  t0 = wallclock()
  ! Here, we call vector_add with our device arrays, which should use the vector_add_gpu routine
  ! defined in our interface.
  call vector_add(a_d, b_d, N)
  istat = cudaDeviceSynchronize()
  add_time = wallclock() - t0

  t0 = wallclock()
  ! Similarly, we call dot with our device arrays, which should use the dot_gpu routine
  ! defined in our interface.
  x = dot(a_d, b_d, N)
  istat = cudaDeviceSynchronize()
  dot_time = wallclock() - t0

  print*, "x = ", x
  
  a = a_d
  if (any(a /= 6.d0)) then
    print*, "vector_add failed!"
  elseif (x /= 24.d0*N) then
    print*, "dot failed!"
  else
    print*, "GPU passed!"
  end if 

  print "(a,1x,f9.5,1x,a,1x,f9.5)", "add time:", add_time, "dot time:", dot_time
#endif

end program main
