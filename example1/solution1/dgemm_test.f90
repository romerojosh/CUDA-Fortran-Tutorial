!
! Basic program which runs a GEMM on matrices A,B,C:
! C = alpha A*B + beta C
!
program main
#ifdef _CUDA
  ! Need to include a couple of modules:
  !   cublas: required to use generic BLAS interface
  !   cudafor: required to use CUDA runtime API routines (e.g. cudaDeviceSynchronize)
  !            not explicitly required if file has *.cuf suffix 
  use cublas
  use cudafor
#endif
  implicit none

  ! Declare variables on host
  real(8), dimension(:,:), allocatable :: A, B, C
  real(8) :: c_ref
  integer :: i, j, m
  real(8) :: t0, t1, wallclock
#ifdef _CUDA
  ! To get arrays A, B, and C into device memory, we simply need to add the
  ! 'device' varible attribute. 
  attributes(device) :: A, B, C

  ! Declaring additional host resident version of C for error checking 
  real(8), dimension(:,:), allocatable :: C_h

  integer :: istat
#endif

#ifdef _CUDA
  ! Printing message to let us know we are running on GPU. 
  print*, "Running GPU version!"
#endif
  
  do m = 128,4096,64
    allocate(A(m, m)) 
    allocate(B(m, m))
    allocate(C(m, m))
    
    ! Initialize the matrices A, B and C with constants
    A = 1.d0
    B = 2.d0
    C = 3.d0
    
    ! Compute reference solution (all entries in C should equal c_ref after GEMM)
    c_ref = 2.d0 * m + 3.d0
    
    ! Perform DGEMM computation
#ifdef _CUDA
    ! DGEMM computation is a CUDA kernel. Kernels operate asynchronously with respect
    ! to the host. To get an accurate timing, explicit device synchronizes are required
    istat = cudaDeviceSynchronize() 
#endif
    t0 = wallclock()
    
    call DGEMM('N', 'N', m, m, m, 1.d0, A, m, B, m, 1.d0, C, m)
    
#ifdef _CUDA
    istat = cudaDeviceSynchronize() 
#endif
    t1 = wallclock()
    
    ! Print timing information
    print "(i5,1x,a,1x,f9.5,2x,a,f12.4)", m, " time =", t0 - t1, & 
      " MFLOPS = ", 1.d-6 * 2.d0 * m * m * m / (t0 - t1)
    
    ! Check the result
#ifdef _CUDA
    ! We can use a source allocation to create a host copy of C. Alternatively, we
    ! could use a standard allocate statement, followed by C_h = C to copy device data
    ! to the host
    allocate(C_h, source = C)

    ! To reuse the following code block, we can use an associate statement to rename variable C
    ! to be our new host array copy
    associate(C => C_h)
#endif
    do j = 1,m
      do i = 1,m
        if (abs(C(i,j) - c_ref) .gt. 1.d-8) then
          print*, "DGEMM failed", i, j, abs(C(i,j) - c_ref), C(i,j) 
          exit
        end if
      end do
    end do
#ifdef _CUDA
    end associate
    deallocate(C_h)
#endif
             
    deallocate(A, B, C)

  end do

end program main

