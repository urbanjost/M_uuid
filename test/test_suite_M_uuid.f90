subroutine test_suite_m_uuid()
! this should contains tests for all public procedures in the module
call test_generate_uuid()
end subroutine test_suite_m_uuid
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine test_generate_uuid()
use M_verify, only : unit_check, unit_check_start, unit_check_good, unit_check_bad, unit_check_done
$DOCUMENT COMMENT

   This just checks that we can generate the various types of UUID
   (without crashing) and checks that they have the correct syntax. We
   could also check that the UUID changes for each call and I think there
   is an additional check we could make within the UUID itself. But for
   now this is enough.

$DOCUMENT END
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
