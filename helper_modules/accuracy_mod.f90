
   !> Module to share parameters
   !  for defining data type kinds.
   
   module accuracy_mod
   implicit none
   public
   
   integer, parameter         :: SP = kind(1.0)
   integer, parameter         :: DP = kind(1.d0)
   
   end module accuracy_mod