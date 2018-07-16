program main
  use cublas
  use cudafor
  use nvtx
  implicit none

  ! Declare variables on host/device
  real(8), dimension(:,:), allocatable :: A, B, C
  real(8), dimension(:,:), allocatable, device :: A_d, B_d, C_d
  integer :: i, j, m, istat
  real(8) :: t0, t1, wallclock

  m = 2048

  allocate(A(m, m)) 
  allocate(B(m, m))
  allocate(C(m, m))
  
  ! Initialize the matrices A, B and C with constants
  A = 1.d0
  B = 2.d0
  C = 3.d0

  allocate(A_d, source = A)
  allocate(B_d, source = B)
  allocate(C_d, source = C)
 
  t0 = wallclock()
  ! Do 5 iterations
  do i = 1,5
    ! Perform 2 CPU DGEMM
    do j = 1,2
      call DGEMM('N', 'N', m, m, m, 1.d0, A, m, B, m, 1.d0, C, m)
    end do

    ! Perform 200 GPU DGEMM
    do j = 1,200
      call DGEMM('N', 'N', m, m, m, 1.d0, A_d, m, B_d, m, 1.d0, C_d, m)
    end do
  end do

  istat = cudaDeviceSynchronize()
  t1 = wallclock()

  print "(a,1x,f9.5)", "time to complete:", t1 - t0

end program main
