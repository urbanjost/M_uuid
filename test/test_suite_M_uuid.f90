module scr_uuid
use M_msg
use M_verify, only : unit_check_command, unit_check_keep_going, unit_check_level
use M_uuid, only : generate_uuid
private
public test_suite_m_uuid
contains
subroutine test_suite_m_uuid()
! this should contains tests for all public procedures in the module
   call test_generate_uuid()
end subroutine test_suite_m_uuid
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine test_generate_uuid()
use M_verify, only : unit_check, unit_check_start, unit_check_good, unit_check_bad, unit_check_done

!   This just checks that we can generate the various types of UUID
!   (without crashing) and checks that they have the correct syntax. We
!   could also check that the UUID changes for each call and I think there
!   is an additional check we could make within the UUID itself. But for
!   now this is enough.

character(len=36) :: uuid
   call unit_check_start('generate_uuid') ! start tests

   uuid = generate_uuid(0)
   call unit_check('generate_uuid',check_uuid(uuid).and.(uuid =='00000000-0000-0000-0000-000000000000'),msg='Version 0 '//uuid)

   uuid = generate_uuid(1)
   call unit_check('generate_uuid',check_uuid(uuid),msg='Version 1 '//uuid)

   uuid = generate_uuid(2)
   call unit_check('generate_uuid',uuid=='',msg='Version 2 (NOT IMPLEMENTED)')

   uuid = generate_uuid(3)
   call unit_check('generate_uuid',uuid=='',msg='Version 3 (NOT IMPLEMENTED)')

   uuid = generate_uuid(4)
   call unit_check('generate_uuid',check_uuid(uuid),msg='Version 4 '//uuid)

   uuid = generate_uuid(5)
   call unit_check('generate_uuid',uuid=='',msg='Version 5 (NOT IMPLEMENTED)')

   call unit_check('compare',exercise(),msg='test for duplicates')
   call unit_check_done('generate_uuid')
!==================================================================================================================================!
contains
!==================================================================================================================================!
function check_uuid(chars) result(lout)

! Return true if the string is permitted by the UUID BFN in RFC

character(len=*) :: chars
character(len=22), parameter :: hex = '0123456789abcdefABCDEF'
logical :: lout

   lout = (len_trim(chars) == 36)
   if (lout) then
       lout = lout.and.(verify(chars(1:8), hex) == 0)
       lout = lout.and.(verify(chars(9:9), '-') == 0)
       lout = lout.and.(verify(chars(10:13), hex) == 0)
       lout = lout.and.(verify(chars(14:14), '-') == 0)
       lout = lout.and.(verify(chars(15:18), hex) == 0)
       lout = lout.and.(verify(chars(19:19), '-') == 0)
       lout = lout.and.(verify(chars(20:23), hex) == 0)
       lout = lout.and.(verify(chars(24:24), '-') == 0)
       lout = lout.and.(verify(chars(25:36), hex) == 0)
   endif

end function check_uuid
!==================================================================================================================================!
end subroutine test_generate_uuid
!==================================================================================================================================!
function exercise()
logical :: exercise
integer,parameter :: sz=27777777
character(len=36),allocatable :: uuid(:)
integer :: i,j
   exercise=.true.
   TYPES: do j=1,4,3
      if(allocated(uuid))deallocate(uuid)
      allocate(uuid(sz))
      do i=1,sz
         uuid(i)=generate_uuid(j)
      enddo
      write(*,*)'looking for duplicates in ',size(uuid),' values for type ',j
      INFINITE: do
      do i=1,size(uuid)-1,2
         if(uuid(i).eq.uuid(i+1))then
            write(*,*)'error: duplicates found at ',i,uuid(i),uuid(i+1)
            exercise=.false.
            exit TYPES
         endif
      enddo
      write(*,*)size(uuid)/2,' adjacent values were different for type',j
      uuid=uuid(1::2)
      if(size(uuid).le.2)exit INFINITE
      enddo INFINITE
   enddo TYPES
end function exercise
!==================================================================================================================================!
end module scr_uuid
!==================================================================================================================================!
program runtest
use M_msg
use M_verify, only : unit_check_command, unit_check_keep_going, unit_check_level
use scr_uuid
implicit none
   unit_check_command=''
   unit_check_keep_going=.true.
   unit_check_level=0
   call test_suite_M_uuid()
end program runtest
!==================================================================================================================================!
