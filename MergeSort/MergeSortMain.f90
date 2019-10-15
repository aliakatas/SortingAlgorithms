
   !> Testing MergeSort Module
   program MergeSortMain
   use accuracy_mod
   use MergeSort_mod
   use printers_mod
   
   implicit none

   ! Variables
   integer, parameter      :: N = 11
   integer                 :: A_int(N)
   real(SP)                :: A_real(N)
   real(DP)                :: A_dp(N)

   integer                 :: A_int_ord(N)
   real(SP)                :: A_real_ord(N)
   real(DP)                :: A_dp_ord(N)
    
   ! Body of MergeSortMain
   A_int = [3, 5, 8, 7, 11, 1, 4, 2, 6, 10, 9]
   A_real = [3.2_SP, 4.6_SP, 3.5_SP, 4.77_SP, 6.0_SP, 9.12_SP, 3.51_SP, 1.11_SP, 0.03_SP, 9.99_SP, 12.76_SP]
   A_dp = [2.92_DP, 6.4_DP, 5.1_DP, 7.44_DP, 11.0_DP, 2.92_DP, 1.53_DP, 1.11_DP, 0.03_DP, 9.99_DP, 2.376_DP]
    
   A_int_ord = MergeSort(A_int, N)
   call showMe(A_int, A_int_ord, N)
   
   A_real_ord = MergeSort(A_real, N)
   call showMe(A_real, A_real_ord, N)
   
   A_dp_ord = MergeSort(A_dp, N)
   call showMe(A_dp, A_dp_ord, N)
   
   read(*,*)
    
   end program MergeSortMain
    
    
   
      
      
   
   
