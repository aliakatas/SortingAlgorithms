
   !> Module implementing the printing
   !  procedures for:
   !      - integer
   !      - real (single precision)
   !      - real (double precision)
   ! An interface is provided to be able to access
   ! all three procedures accordingly.
   
   module printers_mod
   use accuracy_mod
   
   implicit none
   private
   
   public   :: showMe
   
   interface showMe
   procedure   :: showMe_int, showMe_real, showMe_dp 
   end interface
   
   contains
   
   ! ==================================================
   ! INTEGER 
   subroutine showMe_int(A, Ao, n)
   integer, intent(in)  :: n
   integer, intent(in)  :: A(n), Ao(n)
   integer              :: i

   write(*,*) ' '
   write(*,'(A)') ' ****************************'
   write(*,'(A)') '            INTEGER          '
   write(*,'(A)') '            -------          '
   write(*,'(A,3X,A)') ' Not sorted', 'Sorted'
   do i = 1, n
      write(*,'(3X,I4,5X,I4)') A(i), Ao(i)
   end do
   end subroutine showMe_int

   ! ==================================================
   ! REAL (SINGLE PRECISION)
   subroutine showMe_real(A, Ao, n)
   integer, intent(in)  :: n
   real(SP), intent(in) :: A(n), Ao(n)
   integer              :: i

   write(*,*)
   write(*,'(A)') ' ****************************'
   write(*,'(A)') '             REAL            '
   write(*,'(A)') '             ----            '
   write(*,'(A,5X,A)') ' Not sorted', 'Sorted'
   do i = 1, n
      write(*,'(2X,F8.3,4X,F8.3)') A(i), Ao(i)
   end do
   end subroutine showMe_real

   ! ==================================================
   ! REAL (DOUBLE PRECISION)
   subroutine showMe_dp(A, Ao, n)
   integer, intent(in)  :: n
   real(DP), intent(in) :: A(n), Ao(n)
   integer              :: i

   write(*,*)
   write(*,'(A)') ' ****************************'
   write(*,'(A)') '          REAL(DP)           '
   write(*,'(A)') '          --------           '
   write(*,'(A,5X,A)') ' Not sorted', 'Sorted'
   do i = 1, n
      write(*,'(2X,F8.3,4X,F8.3)') A(i), Ao(i)
   end do
   end subroutine showMe_dp
   
   end module printers_mod
   