
   !> Testing QuickSort Module
   program quicksort_test
   use QuickSort_mod
   use printers_mod
   
   implicit none
 
   integer, parameter      :: k = 8
   integer                 :: A_int(k), A_int_ord(k)
   real(SP)                :: A_real(k), A_real_ord(k)
   real(DP)                :: A_dp(k), A_dp_ord(k)
   integer                 :: i
 
   A_int = [5, 3, 9, 13, 0, 7, 21, 3]
   A_int_ord = A_int
   call QuickSort(A_int_ord, k)
   call showMe(A_int, A_int_ord, k)

   A_real = [9.81_SP, 6.64_SP, -273.15_SP, 42.0_SP, 1.013_SP, 0.5_SP, 28.28_SP, 3.142_SP]
   A_real_ord = A_real
   call QuickSort(A_real_ord, k)
   call showMe(A_real, A_real_ord, k)

   A_dp = [9.81_DP, 6.64_DP, -273.15_DP, 42.0_DP, 1.013_DP, 0.5_DP, 28.28_DP, 3.142_DP]
   A_dp_ord = A_dp
   call QuickSort(A_dp_ord, k)
   call showMe(A_dp, A_dp_ord, k)
   
   read(*,*)
   
   end program quicksort_test