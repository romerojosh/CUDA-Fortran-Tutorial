module vector_ops
  contains
    ! subroutine to add entries of vector b to vector a, pointwise
    subroutine vector_add(a, b, N)
      implicit none
      real(8) :: a(*), b(*)
      integer :: i, N
    
      do i = 1,N
        a(i) = a(i) + b(i)
      end do
      
    end subroutine vector_add
    
    ! function to compute dot product
    real(8) function dot(a, b, N)
      implicit none
      real(8) :: a(*), b(*)
      real(8) :: x
      integer :: i, N
    
      x = 0.d0
      do i = 1,N
        x = x + a(i) * b(i)
      end do
    
      dot = x
      
    end function dot
end module vector_ops
