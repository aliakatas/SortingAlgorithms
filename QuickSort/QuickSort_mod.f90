   !> Module implementing the QuickSort Algorithm
   !  for:
   !      - integer
   !      - real (single precision)
   !      - real (double precision)
   ! An interface is provided to be able to access
   ! all three procedures accordingly.
   
   module quicksort_mod
   
   implicit none
   private
   
   integer, parameter         :: SP = kind(1.0)
   integer, parameter         :: DP = kind(1.d0)
   
   public :: QuickSort
   public :: SP, DP
   
   interface QuickSort
   procedure :: QSort_int, QSort_real, QSort_dp
   end interface
   
   contains
   
   ! ==================================================
   ! INTEGER 
   recursive subroutine QSort_int(a, na)
   ! DUMMY ARGUMENTS
   integer, intent(in)     :: na
   integer, intent(inout)  :: a(na)
 
   ! LOCAL VARIABLES
   integer  :: left, right
   real     :: random
   integer  :: pivot
   integer  :: temp
   integer  :: marker
   
   if (na > 1) then
      
      call random_number(random)
      pivot = a(int(random * real(na - 1)) + 1) 
      left = 0
      right = na + 1
      
      do while (left < right)
         
         right = right - 1
         do while (a(right) > pivot)
            right = right - 1
         end do
         
         left = left + 1
         do while (a(left) < pivot)
            left = left + 1
         end do
         
         if (left < right) then
            temp = a(left)
            a(left) = a(right)
            a(right) = temp
         end if
      end do
      
      if (left == right) then
         marker = left + 1
      else
         marker = left
      end if
      
      call QSort_int(a(:marker - 1), marker - 1)
      call QSort_int(a(marker:), na - marker + 1)
   end if
   
   end subroutine QSort_int
   
   ! ==================================================
   ! REAL (SINGLE PRECISION)
   recursive subroutine QSort_real(a, na)
   ! DUMMY ARGUMENTS
   integer, intent(in)     :: na
   real(SP), intent(inout) :: a(na)
 
   ! LOCAL VARIABLES
   integer  :: left, right
   real     :: random
   real(SP) :: pivot
   real(SP) :: temp
   integer  :: marker
   
   if (na > 1) then
      
      call random_number(random)
      pivot = a(int(random * real(na - 1)) + 1) 
      left = 0
      right = na + 1
      
      do while (left < right)
         
         right = right - 1
         do while (a(right) > pivot)
            right = right - 1
         end do
         
         left = left + 1
         do while (a(left) < pivot)
            left = left + 1
         end do
         
         if (left < right) then
            temp = a(left)
            a(left) = a(right)
            a(right) = temp
         end if
      end do
      
      if (left == right) then
         marker = left + 1
      else
         marker = left
      end if
      
      call QSort_real(a(:marker - 1), marker - 1)
      call QSort_real(a(marker:), na - marker + 1)
   end if
   
   end subroutine QSort_real
   
   ! ==================================================
   ! REAL (DOUBLE PRECISION)
   recursive subroutine QSort_dp(a, na)
   ! DUMMY ARGUMENTS
   integer, intent(in)     :: na
   real(DP), intent(inout) :: a(na)
 
   ! LOCAL VARIABLES
   integer  :: left, right
   real     :: random
   real(DP) :: pivot
   real(DP) :: temp
   integer  :: marker
   
   if (na > 1) then
      
      call random_number(random)
      pivot = a(int(random * real(na - 1)) + 1) 
      left = 0
      right = na + 1
      
      do while (left < right)
         
         right = right - 1
         do while (a(right) > pivot)
            right = right - 1
         end do
         
         left = left + 1
         do while (a(left) < pivot)
            left = left + 1
         end do
         
         if (left < right) then
            temp = a(left)
            a(left) = a(right)
            a(right) = temp
         end if
      end do
      
      if (left == right) then
         marker = left + 1
      else
         marker = left
      end if
      
      call QSort_dp(a(:marker - 1), marker - 1)
      call QSort_dp(a(marker:), na - marker + 1)
   end if
   
   end subroutine QSort_dp
   
 
end module quicksort_mod