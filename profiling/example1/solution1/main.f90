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

  ! Here, we add NVTX ranges to mark features of interest. Each start range
  ! must be paired with an end range call (treated as a stack)
  call nvtxStartRange("CPU ALLOCATE", 10)
  allocate(A(m, m)) 
  allocate(B(m, m))
  allocate(C(m, m))
  call nvtxEndRange
  
  ! Initialize the matrices A, B and C with constants
  A = 1.d0
  B = 2.d0
  C = 3.d0

  call nvtxStartRange("GPU ALLOCATE", 11)
  allocate(A_d, source = A)
  allocate(B_d, source = B)
  allocate(C_d, source = C)
  call nvtxEndRange()
 
  t0 = wallclock()
  ! Do 5 iterations
  call nvtxStartRange("MAIN LOOP", 20)
  do i = 1,5
    call nvtxStartRange("ITER", i)
    ! Perform 2 CPU DGEMM

    ! By adding NVTX range here around CPU DGEMMs, we will now be able to
    ! visualize it in our GPU profile.
    call nvtxStartRange("CPU DGEMM", 1)
    do j = 1,2
      call DGEMM('N', 'N', m, m, m, 1.d0, A, m, B, m, 1.d0, C, m)
    end do
    call nvtxEndRange()

    ! Perform 200 GPU DGEMM
    call nvtxStartRange("GPU DGEMM", 2)
    do j = 1,200
      call DGEMM('N', 'N', m, m, m, 1.d0, A_d, m, B_d, m, 1.d0, C_d, m)
    end do
    call nvtxEndRange()
    call nvtxEndRange()
  end do
  call nvtxEndRange()

  istat = cudaDeviceSynchronize()
  t1 = wallclock()

  print "(a,1x,f9.5)", "time to complete:", t1 - t0

end program main
