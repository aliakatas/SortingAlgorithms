
   !> Module implementing the merging procedure
   !  as part of a MergeSort algorithm.
   !  Covering: integer, real (single), real (double).
   
   module Merge_mod
   use accuracy_mod
   implicit none
   private
   
   public   :: mergeThem
   
   interface mergeThem
   procedure :: merge_int, merge_real, merge_dp
   end interface
   
   contains
   
   ! ==========================================================
   ! INTEGER
   function merge_int(a, b) result(combo)
   integer, intent(in)   :: a(:), b(:)
   integer, allocatable  :: combo(:)
      
   integer     :: na, nb, nc
   integer     :: i, j, k
   logical     :: flagA, flagB
      
   flagA = .false.
   flagB = .false.
   na = size(a)
   nb = size(b)
   nc = na + nb
   allocate(combo(nc))
      
   i = 1
   j = 1
   k = 1
   do 
      if (a(i) <= b(j)) then
         combo(k) = a(i)
         i = i + 1
         k = k + 1
      else
         combo(k) = b(j)
         j = j + 1
         k = k + 1
      end if
      
      if (i > na) then
         flagA = .true.
         exit
      else if (j > nb) then
         flagB = .true.
         exit
      end if
   end do
      
   if (flagA) then
      do 
         combo(k) = b(j)
         k = k + 1
         j = j + 1
         if (k > nc .or. j > nb) exit
      end do
   else if (flagB) then
      do 
         combo(k) = a(i)
         k = k + 1
         i = i + 1
         if (k > nc .or. i > na) exit
      end do
   end if
   end function merge_int
   
   ! ==========================================================
   ! REAL (SINGLE PRECISION)
   function merge_real(a, b) result(combo)
   real(SP), intent(in)    :: a(:), b(:)
   real(SP), allocatable   :: combo(:)
      
   integer     :: na, nb, nc
   integer     :: i, j, k
   logical     :: flagA, flagB
      
   flagA = .false.
   flagB = .false.
   na = size(a)
   nb = size(b)
   nc = na + nb
   allocate(combo(nc))
      
   i = 1
   j = 1
   k = 1
   do 
      if (a(i) <= b(j)) then
         combo(k) = a(i)
         i = i + 1
         k = k + 1
      else
         combo(k) = b(j)
         j = j + 1
         k = k + 1
      end if
      
      if (i > na) then
         flagA = .true.
         exit
      else if (j > nb) then
         flagB = .true.
         exit
      end if
   end do
      
   if (flagA) then
      do 
         combo(k) = b(j)
         k = k + 1
         j = j + 1
         if (k > nc .or. j > nb) exit
      end do
   else if (flagB) then
      do 
         combo(k) = a(i)
         k = k + 1
         i = i + 1
         if (k > nc .or. i > na) exit
      end do
   end if
   end function merge_real
   
   ! ==========================================================
   ! REAL (DOUBLE PRECISION)
   function merge_dp(a, b) result(combo)
   real(DP), intent(in)    :: a(:), b(:)
   real(DP), allocatable   :: combo(:)
      
   integer     :: na, nb, nc
   integer     :: i, j, k
   logical     :: flagA, flagB
      
   flagA = .false.
   flagB = .false.
   na = size(a)
   nb = size(b)
   nc = na + nb
   allocate(combo(nc))
      
   i = 1
   j = 1
   k = 1
   do 
      if (a(i) <= b(j)) then
         combo(k) = a(i)
         i = i + 1
         k = k + 1
      else
         combo(k) = b(j)
         j = j + 1
         k = k + 1
      end if
      
      if (i > na) then
         flagA = .true.
         exit
      else if (j > nb) then
         flagB = .true.
         exit
      end if
   end do
      
   if (flagA) then
      do 
         combo(k) = b(j)
         k = k + 1
         j = j + 1
         if (k > nc .or. j > nb) exit
      end do
   else if (flagB) then
      do 
         combo(k) = a(i)
         k = k + 1
         i = i + 1
         if (k > nc .or. i > na) exit
      end do
   end if
   end function merge_dp
   
   end module Merge_mod
   
   