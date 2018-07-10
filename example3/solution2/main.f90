module constants
  real(8), parameter :: PI = 2.d0 * asin(1.d0)
end module constants

! This a module containing our custom CUDA kernel to compute the desired integrals. Note that
! CUDA kernels must be defined within modules. 
module simpson_kernel_m
#ifdef _CUDA
  use cudafor
  implicit none
 
  contains 

    ! Here is the CUDA kernel definition. The `attributes(global)` indicates that this
    ! routine will be performed on a global grid of threads. 
    attributes(global) &
    subroutine simpson_kernel(res, dx, N, M)
       use constants, only: PI
       ! Array arguments to kernels are automatically assumed to be on the device
       real(8), dimension(M) :: res

       ! Scalar arguments from the host should be passed by value
       real(8), value :: dx
       integer, value :: N, M
  
       integer :: i, j, tx, ty
       real(8) :: val, mysum
  
       ! Threads in CUDA are organized in blocks with multiple blocks forming a larger grid.
       ! threadIdx is the index of a given thread within a block (x-dimension is leading 
       ! dimension). In CUDA Fortran, these indices are 1-based. 
       tx = threadIdx%x
       ty = threadIdx%y

       ! blockIdx is the index of a given block within the grid, and blockDim is the dimension 
       ! of the block (in number of threads). Here, we are assigning the 'i'th integral a 
       ! full x-row (a warp if blockDim%x == 32) of threads
       i = (blockIdx%x - 1) * blockDim%y + ty

       ! We may have extra threads assigned beyond what we require. We can force a quick return
       ! for those threads. 
       if (i > M) return
  
       ! In the following bit of code, each thread accumulates a local sum of several values of
       ! the 'i'th integral it is assigned to, with coefficients consistent with Simpson's rule.  
       mysum = 0.d0
  
       do j = tx, N, blockDim%x
         val = cos(mod(i-1, 10) * PI * (j-1)*dx)
         if (j == 1 .or. j == N) then
           mysum = mysum + val
         else if (mod(j,2)) then
           mysum = mysum + 2.d0*val
         else
           mysum = mysum + 4.d0*val
         endif
       end do
  
       ! Now, we need to reduce the values across threads assigned to the 'i'th integral.
       ! **Assuming** a full warp (32 threads) was assigned to each integral, we can
       ! complete this task using "shuffle" instructions. Alternative logic is required
       ! if multiple warps (more than 32 threads) within a block are assigned to a single 
       ! integral (using a shared memory buffer). 
       mysum = mysum + __shfl_down(mysum,1)
       mysum = mysum + __shfl_down(mysum,2)
       mysum = mysum + __shfl_down(mysum,4)
       mysum = mysum + __shfl_down(mysum,8)
       mysum = mysum + __shfl_down(mysum,16)
  
       ! First thread in each warp will contain final reduced result. It is tasked with
       ! assigning final result to res array.
       if (tx == 1) then
         res(i) = dx * mysum / 3.d0
       endif
    end subroutine simpson_kernel

#endif
end module simpson_kernel_m

program main
#ifdef _CUDA
  use cudafor 
#endif
  use constants
  use simpson_kernel_m
  implicit none

  real(8), dimension(:), allocatable :: f, res
  real(8) :: dx, rsum
  integer :: i, j, N, M
  real(8) :: t0, t1, wallclock

#ifdef _CUDA
  attributes(device) :: f, res
  real(8), dimension(:), allocatable :: res_h
  integer :: istat

  ! To launch our kernel, we will need to pass a thread and block configuration. These arguments
  ! are of type(dim3) declared here.
  type(dim3) :: blocks, threads
#endif

#ifdef _CUDA
  print*, "Running GPU version (using CUDA kernel)!"
#endif

  ! From AUSURF112
  N = 821 ! Number of grid points
  M = 274865 ! Number of integrals

  allocate(f(N))
  allocate(res(M))
  !@cuf allocate(res_h, mold=res)

  ! Assume equispaced grid from 0 to 1
  dx = 1.d0 / (N-1)

  t0 = wallclock()

#ifdef _CUDA

  ! For our example, we are using 2D thread blocks of dimension 32 x 4 for a total of 128 threads
  ! per block. The first dimension 32 is important here, as we want to assign a full warp 
  ! to each integral. 
  threads = dim3(32, 4, 1)

  ! Here, we use a 1D grid, comprised of our 2D thread blocks. We need to launch enough blocks to
  ! complete all 'M' integrals.  
  blocks = dim3(ceiling(real(M)/4), 1, 1)

  ! Launching the kernel here with the "triple-chevron" arguments to define the thread and block
  ! configuration. This single kernel replaces the entire existing loop structure from the 
  ! original example.
  call simpson_kernel<<<blocks, threads>>>(res, dx, N, M)

  istat = cudaDeviceSynchronize()

#else

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

#endif
  t1 = wallclock()

  ! Check results (Should be 1 if mod(i-1,10) == 0, 0 otherwise)
  !@cuf res_h = res
  !@cuf associate(res => res_h)
  do i = 1, M
    if (mod(i-1, 10) == 0 .and. abs(res(i)) /= 1.d0) then
      print*, "test failed."
      stop
    else if (mod(i-1, 10) /= 0 .and. abs(res(i)) > 1d-12) then
      print*, "test failed."
      stop
    endif
  end do
  !@cuf end associate

  print*, "test passed!"
  print "(i6,1x,a,1x,f9.5,1x,a)", M, "integrals completed in", t1 - t0, "seconds"


end program main
