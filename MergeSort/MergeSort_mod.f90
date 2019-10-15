   ! *********************************************************
   !                  MODULE: MERGESORT_MOD
   !                  ---------------------
   !> MergeSort algorithm for sorting integer, real and double 
   !  precision arrays in ascending order. 
   !
   ! *********************************************************
   
   module MergeSort_mod
   use accuracy_mod
   use Merge_mod
   
   implicit none
   private
   
   public :: MergeSort
   
   interface MergeSort
   procedure :: MergeSort_int, MergeSort_real, MergeSort_dp
   end interface
   
   contains
   
   ! ====================================================================
   ! INTEGER
   recursive function MergeSort_int(a, n) result(res)
   integer, intent(in)     :: n
   integer, intent(in)     :: a(n)
   integer                 :: res(n)
   
   integer                 :: nl, nr
      
   if (n < 2) then
      res(:) = a(:)
      return
   end if
   
   nl = (n + 1) / 2
   nr = n - nl
   
   res = mergeThem(MergeSort_int(a(1), nl), MergeSort_int(A(nl + 1), nr))
   end function MergeSort_int
   
   ! ====================================================================
   ! REAL (SINGLE PRECISION)
   recursive function MergeSort_real(a, n) result(res)
   integer, intent(in)     :: n
   real(SP), intent(in)    :: a(n)
   real(SP)                :: res(n)
   
   integer                 :: nl, nr
      
   if (n < 2) then
      res(:) = a(:)
      return
   end if
   
   nl = (n + 1) / 2
   nr = n - nl
   
   res = mergeThem(MergeSort_real(a(1), nl), MergeSort_real(A(nl + 1), nr))
   end function MergeSort_real
   
   ! ====================================================================
   ! REAL (DOUBLE PRECISION)
   recursive function MergeSort_dp(a, n) result(res)
   integer, intent(in)     :: n
   real(DP), intent(in)    :: a(n)
   real(DP)                :: res(n)
   
   integer                 :: nl, nr
      
   if (n < 2) then
      res(:) = a(:)
      return
   end if
   
   nl = (n + 1) / 2
   nr = n - nl
   
   res = mergeThem(MergeSort_dp(a(1), nl), MergeSort_dp(A(nl + 1), nr))
   end function MergeSort_dp
   
   end module MergeSort_mod
   
   
   