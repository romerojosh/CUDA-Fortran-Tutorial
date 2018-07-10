!
! Basic program which runs a GEMM on matrices A,B,C:
! C = alpha A*B + beta C
!
program main
  implicit none

  ! Declare variables on host
  real(8), dimension(:,:), allocatable :: A, B, C
  real(8) :: c_ref
  integer :: i, j, m
  real(8) :: t0, t1, wallclock

#ifdef _CUDA
  print*, "TODO: Enable GPU support by modifying these files! Exiting!"
  stop
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
    t0 = wallclock()
    
    call DGEMM('N', 'N', m, m, m, 1.d0, A, m, B, m, 1.d0, C, m)
    
    t1 = wallclock()
    
    ! Print timing information
    print "(i5,1x,a,1x,f9.5,2x,a,f12.4)", m, " time =", t0 - t1, & 
      " MFLOPS = ", 1.d-6 * 2.d0 * m * m * m / (t0 - t1)
    
    ! Check the result
    do j = 1,m
      do i = 1,m
        if (abs(C(i,j) - c_ref) .gt. 1.d-8) then
          print*, "DGEMM failed", i, j, abs(C(i,j) - c_ref), C(i,j) 
          exit
        end if
      end do
    end do
             
    deallocate(A, B, C)

  end do

end program main

