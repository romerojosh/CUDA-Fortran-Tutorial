module vector_ops
  ! Here we define our interfaces for vector_add and dot with CPU and GPU versions defined.
  interface vector_add
    module procedure vector_add_cpu
#ifdef _CUDA
    module procedure vector_add_gpu
#endif
  end interface

  interface dot
    module procedure dot_cpu
#ifdef _CUDA
    module procedure dot_gpu
#endif
  end interface

  contains
    subroutine vector_add_cpu(a, b, N)
      implicit none
      real(8) :: a(*), b(*)
      integer :: i, N
      print*, "Running vector_add_cpu..."
    
      do i = 1,N
        a(i) = a(i) + b(i)
      end do
      
    end subroutine vector_add_cpu

#ifdef _CUDA
    subroutine vector_add_gpu(a, b, N)
      implicit none
      real(8), device :: a(*), b(*)
      integer :: i, N
      print*, "Running vector_add_gpu..."
     
      ! For this loop, we can use a CUF kernel to automatically generate a GPU kernel
      ! for this operation. Simple loops with pointwise computations are perfect candidates
      ! for CUF kernels. 
      ! It's not shown here but scalar variables defined outside of the loop
      ! can also be present in the CUF kernel and will automatically be passed to the device.
      !$cuf kernel do 
      do i = 1,N
        a(i) = a(i) + b(i)
      end do
      
    end subroutine vector_add_gpu
#endif
    
    real(8) function dot_cpu(a, b, N)
      implicit none
      real(8) :: a(*), b(*)
      real(8) :: x
      integer :: i, N
      print*, "Running dot_cpu..."
    
      x = 0.d0
      do i = 1,N
        x = x + a(i) * b(i)
      end do
    
      dot_cpu = x
      
    end function dot_cpu

#ifdef _CUDA
    real(8) function dot_gpu(a, b, N)
      implicit none
      real(8), device :: a(*), b(*)
      real(8) :: x
      integer :: i, N
      print*, "Running dot_gpu..."
    
      ! CUF kernels can also perform simple scalar reductions. In this case here, the reduced
      ! scalar `x` is stored in host memory. The CUF kernel automatically handles copying the
      ! initial scalar to the device for the computation and copying out the result back to 
      ! the host.
      x = 0.d0
      !$cuf kernel do 
      do i = 1,N
        x = x + a(i) * b(i)
      end do
    
      dot_gpu = x
      
    end function dot_gpu
#endif
end module vector_ops
