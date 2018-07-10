module vector_ops

  ! In this solution, we are defining the routines outside of the module. As such,
  ! we need to be more explicit in our interface definitions. The added effort here is
  ! worth it to reduce code duplication.
  interface vector_add
    subroutine vector_add_cpu(a, b, N)
      real(8) :: a(*), b(*)
      integer :: N
    end subroutine vector_add_cpu
#ifdef _CUDA
    subroutine vector_add_gpu(a, b, N)
      real(8), device :: a(*), b(*)
      integer :: N
    end subroutine vector_add_gpu
#endif
  end interface

  interface dot
    real(8) function dot_cpu(a, b, N)
      real(8) :: a(*), b(*)
      integer :: N
    end function dot_cpu
#ifdef _CUDA
    real(8) function dot_gpu(a, b, N)
      real(8), device :: a(*), b(*)
      integer :: N
    end function dot_gpu
#endif
  end interface

end module vector_ops
