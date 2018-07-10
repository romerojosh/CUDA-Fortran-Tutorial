! To reduce source duplication, we define a simple ADD_SUFFIX macro to automatically
! append _cpu or _gpu to the routine names in this file, based on whether
! it is compiled with -Mcuda or not. Using this method, with this
! single file we can create two distinct object files, one with routines compiled
! for CPU, and one with routines compiled for GPU.

#ifdef _CUDA
#define ADD_SUFFIX(x)  x##_gpu
#else
#define ADD_SUFFIX(x)  x##_cpu
#endif

subroutine ADD_SUFFIX(vector_add)(a, b, N)
  implicit none
  real(8) :: a(*), b(*)
#ifdef _CUDA
  attributes(device) :: a, b
#endif
  integer :: i, N

#ifdef _CUDA
  print*, "Running vector_add_gpu..."
#else
  print*, "Running vector_add_cpu..."
#endif

  !$cuf kernel do 
  do i = 1,N
    a(i) = a(i) + b(i)
  end do
  
end subroutine ADD_SUFFIX(vector_add)


real(8) function ADD_SUFFIX(dot)(a, b, N)
  implicit none
  real(8) :: a(*), b(*)
#ifdef _CUDA
  attributes(device) :: a, b
#endif
  real(8) :: x
  integer :: i, N

#ifdef _CUDA
  print*, "Running dot_gpu..."
#else
  print*, "Running dot_cpu..."
#endif

  x = 0.d0
  !$cuf kernel do 
  do i = 1,N
    x = x + a(i) * b(i)
  end do

  ADD_SUFFIX(dot) = x
  
end function ADD_SUFFIX(dot)
