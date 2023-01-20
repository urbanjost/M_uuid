!>
!! generate_uuid(3f) was originally derived from the xmlf90 codebase, (c)
!! Alberto Garcia & Jon Wakelin, 2003-2004.  It also calls RNG routines from
!! Scott Ladd <scott.ladd@coyotegulch.com>, and the libFoX modules. Although
!! some sections have been replaced, generate_uuid(3f) was originally based
!! on the libFoX version, with licensing as follows:
!!
!!     (c) 2005-2009 Toby White <tow@uszla.me.uk>
!!     (c) 2007-2009 Gen-Tao Chiang <gtc25@cam.ac.uk>
!!     (c) 2008-2012 Andrew Walker <a.walker@ucl.ac.uk>
!!
!! All rights reserved.
!!
!!  + Redistribution and use in source and binary forms, with or without
!!    modification, are permitted provided that the following conditions
!!    are met:
!!
!!  + Redistributions of source code must retain the above copyright notice,
!!    this list of conditions and the following disclaimer.
!!
!!  + Redistributions in binary form must reproduce the above copyright
!!    notice, this list of conditions and the following disclaimer in the
!!    documentation and/or other materials provided with the distribution.
!!
!!  + Neither the name of the copyright holder nor the names of its
!!    contributors may be used to endorse or promote products derived from
!!    this software without specific prior written permission.
!!
!!    This software is provided by the copyright holders and contributors
!!    "AS IS" and any express or implied warranties, including, but not
!!    limited to, the implied warranties of merchantability and fitness for
!!    a particular purpose are disclaimed. in no event shall the copyright
!!    owner or contributors be liable for any direct, indirect, incidental,
!!    special, exemplary, or consequential damages (including, but not
!!    limited to, procurement of substitute goods or services; loss of use,
!!    data, or profits; or business interruption) however caused and on any
!!    theory of liability, whether in contract, strict liability, or tort
!!    (including negligence or otherwise) arising in any way out of the use
!!    of this software, even if advised of the possibility of such damage.
module M_uuid
!>
!!##NAME
!!    M_uuid(3f) - [M_uuid::INTRO] a module of UUID (Universally Unique IDentifier) procedures
!!    (LICENSE:BSD-4-Clause)
!!
!!##SYNOPSIS
!!
!!    use M_uuid, only : generate_uuid
!!
!!##QUOTE
!!    Remember you are unique, just like everyone else.
!!
!!##DESCRIPTION
!!
!!    A universally unique identifier (UUID) is a 128-bit number used to
!!    identify information in computer systems.
!!
!!    When generated according to the standard methods, UUIDs are for
!!    practical purposes unique, without depending for their uniqueness
!!    on a central registration authority or coordination between the
!!    parties generating them, unlike most other numbering schemes. While
!!    the probability that a UUID will be duplicated is not zero, it is
!!    close enough to zero to be negligible.
!!
!!    Thus, anyone can create a UUID and use it to identify something with
!!    near certainty that the identifier does not duplicate one that has
!!    already been or will be created to identify something else. Information
!!    labeled with UUIDs by independent parties can therefore be later
!!    combined into a single database, or transmitted on the same channel,
!!    without needing to resolve conflicts between identifiers.
!!
!!    Adoption of UUIDs and GUIDs is widespread. Many computing platforms
!!    provide support for generating them, and for parsing their textual
!!    representation.
!!
!!    RFC 4122 defines a Uniform Resource Name (URN) namespace for UUIDs.
!!    A UUID presented as a URN appears as follows:
!!
!!       urn:uuid:123e4567-e89b-12d3-a456-426655440000
!!
!! -- Wikipedia
!!
!!##PROCEDURES
!!
!!    generate_uuid(version)   generate 36-character UUID string
!===================================================================================================================================
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, dp=>real128
!! provide for routines extracted from other modules (M_time and M_random)
implicit none
integer,parameter          :: realtime=kind(0.0d0)            ! type for unix epoch time and julian days
! Kind types for IEEE 754/IEC 60559 single- and double-precision reals
integer, parameter :: IEEE32 = selected_real_kind(  6,  37 )
integer, parameter :: IEEE64 = selected_real_kind( 15, 307 )
! Constants
integer(INT32), parameter :: N = 624_INT32
integer(INT32), parameter :: M = 397_INT32
real(kind=realtime),parameter,private :: SECDAY=86400.0d0     ! 24:00:00 hours as seconds
type mtprng_state
   integer(INT32)                   :: mti = -1
   integer(INT64), dimension(0:N-1) :: mt
end type
!===================================================================================================================================
!===================================================================================================================================
private

! ident_1="@(#) M_uuid M_uid(3fm) generate UUIDs according to RFC 4122"

! Only versions  0(Nil), 1 (time-based) and 4 (pseudo-RNG-based) are implemented.

integer, parameter       :: i4b = selected_int_kind(9)
integer, parameter       :: i8b = selected_int_kind(18)
type(mtprng_state), save :: rng_state
logical, save            :: initialized = .false.
integer, save            :: values_save              ! must be default for date_and_time
integer(kind=i4b), save  :: hires_count = 0
integer, save            :: clock_seq = 0 ! a random number constant for the lifetime of the process. best we can do per S 4.1.5

public                   :: generate_uuid

contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    generate_uuid(3f) - [M_uuid] generate a UUID (Universally Unique IDentifier) string per RFC 4122
!!    (LICENSE:BSD-4-Clause)
!!
!!##SYNOPSIS
!!
!!    function generate_uuid(version) result(uuid)
!!
!!     integer, intent(in), optional :: version
!!     character(len=36) :: uuid
!!
!!##DESCRIPTION
!!    A universally unique identifier (UUID) is a 128-bit number used to
!!    identify information in computer systems. When generated according
!!    to standard methods UUIDs are for practical purposes unique.
!!    generate_uuid(3f) converts the UUID to a standard string format
!!    per RFC 4122.
!!
!!##AUTHORS
!!    based on previous work from Alberto Garcia & Jon Wakelin, 2003-2004.
!!    RNG routines from Scott Ladd <scott.ladd@coyotegulch.com>, and
!!    the libFoX library( Toby White <tow@uszla.me.uk>, Gen-Tao Chiang
!!    <gtc25@cam.ac.uk>, Andrew Walker <a.walker@ucl.ac.uk>).
!!
!!##OPTIONS
!!    version  Indicates which standard method as described in RFC 4122
!!             is used to generate the string. Versions 0,1, and 4 are supported.
!!
!!             0.  Nil UUID (ie. '00000000-0000-0000-0000-000000000000')
!!             1.  time-based UUID
!!             2.  Not implemented
!!             3.  Not implemented
!!             4.  pseudo-RNG(Random Number Generator) based
!!             5.  Not implemented
!!
!!##EXAMPLE
!!
!!   Sample usage:
!!
!!    program demo_generate_uuid
!!    use M_uuid, only : generate_uuid
!!    implicit none
!!    character(len=36) :: uuid
!!       !
!!       uuid=generate_uuid(1)  ! version 1 (time-based UUID)
!!       write(*,'(a36)')uuid
!!       !
!!       uuid=generate_uuid(4)  ! version 4 (pseudo-RNG-based), default
!!       !
!!       ! RFC 4122 defines a Uniform Resource Name (URN) namespace for UUIDs.
!!       write(*,'("urn:uuid:",a36)')uuid
!!       !
!!       ! a good scratch file name
!!       open(file='/tmp/scratch_'//uuid,unit=10)
!!       !
!!    end program demo_generate_uuid
!!
!!   Typical output:
!!
!!     e769adf4-4af7-11e8-7421-3c9dfbfe9aab
!!     urn:uuid:5b0946b8-0eb4-4966-619d-047b7f7e2056
function generate_uuid(version) result(uuid)

! ident_2="@(#) M_uuid generate_uuid(3f) generate(approximately) a UUID (Universally Unique IDentifier) string per RFC 4122"

integer, intent(in), optional :: version
character(len=36) :: uuid

integer(kind=i8b) :: timestamp, node
integer(kind=i4b) :: clock_sequence

integer(kind=i4b) :: time_low, time_mid, time_hi_and_version
integer(kind=i4b) :: clk_seq_hi_res, clk_seq_low

integer :: values(8) ! must be default for date_and_time
integer(kind=i4b) :: variant, v

   if (.not.initialized) then
      ! Use the current date and time to init mtprng but this gives limited variability, so mix the result up.
      ! Can we do better? In any case, this gets passed through a quick generator inside mtprng_init.
      call date_and_time(values=values)
      values(7) = values(7)*1000+values(5)*100+values(3)*10+values(1)
      values(8) = values(2)*1000+values(4)*100+values(6)*10+values(8)
      call mtprng_init(int(values(7)*10000+values(8), i4b), rng_state)
      clock_seq = int(mtprng_rand64(rng_state), i4b)
      initialized = .true.
   endif

   variant = 1

   if (present(version)) then
      v = version
   else
      v = 4
   endif

   select case (v)
   case (0)
      uuid='00000000-0000-0000-0000-000000000000' ! Nil UUID
      return
   case(1)                                                                           !  version 1(time-based)
      call date_and_time(values=values)
      ! In case of too-frequent requests, we will replace time_low with the count below ...
      if (all(values==values_save)) then
         hires_count = hires_count + 1
      else
         hires_count = 0
      endif
      timestamp = get_utc_since_1582(values)
      clock_sequence = clock_seq                                                      ! clock sequence (14 bits)
      node = ior(mtprng_rand64(rng_state), ishft(mtprng_rand64(rng_state), 16))       ! node ( 48 bits)
      ! No MAC address accessible - see section 4.5 !FIXME
   case(2-3,5) ! Unimplemented
      uuid = ''
      return
   case(4)                                                                           ! version 4 (pseudo-RNG-based)
      timestamp = ior(mtprng_rand64(rng_state), ishft(mtprng_rand64(rng_state), 28))
      clock_sequence = int(mtprng_rand64(rng_state), i4b)                             ! clock sequence (14 bits)
      node = ior(mtprng_rand64(rng_state), ishft(mtprng_rand64(rng_state), 16))       ! node ( 48 bits)
   case default ! Unspecified
      uuid = ''
      return
   end select

   time_low = ibits(timestamp, 0, 32)
   time_mid = ibits(timestamp, 32, 16)

   if (hires_count==0) then
      time_hi_and_version = ior(int(ibits(timestamp, 48, 12), i4b), ishft(v, 12))
   else
      time_hi_and_version = ior(hires_count, ishft(v, 12))
   endif

   clk_seq_low = ibits(clock_sequence, 0, 8)
   clk_seq_hi_res = ior(ibits(clock_sequence, 8, 6), ishft(variant, 6))

   uuid = int32ToHexOctets(time_low, 4)//"-"// &
      int32ToHexOctets(time_mid, 2)//"-"// &
      int32ToHexOctets(time_hi_and_version, 2)//"-"// &
      int32ToHexOctets(clk_seq_hi_res, 1)// &
      int32ToHexOctets(clk_seq_low, 1)//"-"// &
      int64ToHexOctets(node, 6)

contains
!==================================================================================================================================!
function int32ToHexOctets(b, n) result(s)
integer(i4b), intent(in) :: b
integer, intent(in)      :: n ! number of octets to print
character(len=2*n)       :: s
character, parameter  :: hexdigits(0:15) = ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f']
integer               :: i

   do i = 0, 2*n-1
      s(2*n-i:2*n-i) = hexdigits(ibits(b, i*4, 4))
   enddo

end function int32ToHexOctets
!==================================================================================================================================!
function int64ToHexOctets(b, n) result(s)
integer(i8b), intent(in) :: b
integer, intent(in)      :: n ! number of octets to print
character(len=2*n)       :: s
character, parameter  :: hexdigits(0:15) = ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f']
integer               :: i

  do i = 0, 2*n-1
     s(2*n-i:2*n-i) = hexdigits(ibits(b, i*4, 4))
  enddo

end function int64ToHexOctets
!==================================================================================================================================!
end function generate_uuid
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
function get_utc_since_1582(values) result(ns)

! returns the number of 100-ns intervals since 1582-10-15T00:00:00-0

! Not really: Assuming only used as an internal routine for M_UUID(3fm)
!   Fortran date time arrays only report up to the millisecond,
!   and assuming any date given is after 2017 (because added leapseconds up till that date)
!   and not taking account of leapseconds after 2017, and assuming
!   if get same answer on multiple calls that caller will correct or try again, as goal is to generate unique values

integer,intent(in)  :: values(8)
integer(kind=i8b)   :: ns
real(kind=realtime) :: unixtime
real(kind=realtime) :: starttime
integer             :: ierr
integer             :: clicks,maxclicks
real                :: rate
real(kind=dp)       :: rate8,frac8
integer(kind=i8b)   :: frac
integer,parameter   :: ref(8)=[1582,10,15,0,0,0,0,0]
   call date_to_unix(ref,starttime,ierr)                                       ! seconds from 1582-10-15-00-00-00 to Unix Epoch Time
   call date_to_unix(values,unixtime,ierr)                                     ! seconds from given date to Unix Epoch Time
   ! if system clock is higher resolution use it even though that makes fractional second wrong
   call system_clock(count=clicks,count_rate=rate,count_max=maxclicks)
   if(rate > 1000)then                                                        ! system clock available and higher resolution
      rate8=real(rate,kind=dp)
      frac8=mod(real(clicks,kind=dp),rate8)/rate8*10000000_i8b                 ! MOD(A,P) == A - INT (A/P) * P.
      frac=int(frac8)                                                          ! truncate to one remainder of one second
      ns=int((unixtime-starttime)*10000000_i8b,kind=i8b)+frac                  ! get date and time to nearest second and add frac
   else                                                                        ! use date even though accurate only to 1/1000 second
      ns=int(unixtime*10000000_i8b,kind=i8b)-int(starttime*10000000_i8b,kind=i8b)
   endif
   ns=ns+26_i8b                                                                ! leap seconds as of 2016 at 23:59:60 UTC
end function get_utc_since_1582
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
! ROUTINES EXTRACTED FROM OTHER MODULES TO PROVIDE PORTABILITY
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    date_to_julian(3f) - [M_time:JULIAN] converts DAT date-time array to
!!    Julian Date
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine date_to_julian(dat,juliandate,ierr)
!!
!!     integer,intent(in)               :: dat(8)
!!     real(kind=realtime),intent(out)  :: juliandate
!!     integer,intent(out)              :: ierr
!!
!!##DESCRIPTION
!!    Converts a DAT date-time array to a Unix Epoch Time (UET) value.
!!    UET is the number of seconds since 00:00 on January 1st, 1970, UTC.
!!
!!##OPTIONS
!!    dat   Integer array holding a "DAT" array, similar in structure
!!          to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!           dat=[ year,month,day,timezone,hour,&
!!               & minutes,seconds,milliseconds]
!!
!!##RETURNS
!!    juliandate  A Julian Ephemeris Date (JED) is the number of days since
!!                noon (not midnight) on January 1st, 4713 BC.
!!    ierr        Error code. If 0 no error occurred.
!!
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!     program demo_date_to_julian
!!     use M_time, only : date_to_julian,realtime
!!     implicit none
!!     integer             :: dat(8)
!!     real(kind=realtime) :: juliandate
!!     integer             :: ierr
!!        ! generate DAT array
!!        call date_and_time(values=dat)
!!        ! show DAT array
!!        write(*,'(" Today is:",*(i0:,":"))')dat
!!        ! convert DAT to Julian Date
!!        call date_to_julian(dat,juliandate,ierr)
!!        write(*,*)'Julian Date is ',juliandate
!!        write(*,*)'ierr is ',ierr
!!     end program demo_date_to_julian
!!
!!    results:
!!
!!     Today is:2016:7:19:-240:11:3:13:821
!!     Julian Date is    2457589.1272432986
!!     ierr is            0
!!
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
subroutine date_to_julian(dat,julian,ierr)
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!! AUTHOR:    John S. Urban
!!##VERSION:   1.0 2015-12-21
!! REFERENCE: From Wikipedia, the free encyclopedia 2015-12-19
! * There is no year zero
! * Julian Date must be non-negative
! * Julian Date starts at noon; while Civil Calendar date starts at midnight
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_3="@(#) M_time date_to_julian(3f) Converts proleptic Gregorian DAT date-time array to Julian Date"

integer,intent(in)               :: dat(8)   ! array like returned by DATE_AND_TIME(3f)
real(kind=realtime),intent(out)  :: julian   ! Julian Date (non-negative, but may be non-integer)
integer,intent(out)              :: ierr     ! Error return: 0 =successful execution,-1=invalid year,-2=invalid month,-3=invalid day
                                             ! -4=invalid date (29th Feb, non leap-year)
integer                          :: year, month, day, utc, hour, minute
real(kind=realtime)              :: second
integer                          :: A, Y, M, JDN
!-----------------------------------------------------------------------------------------------------------------------------------
   year   = dat(1)                        ! Year
   month  = dat(2)                        ! Month
   day    = dat(3)                        ! Day
   utc    = dat(4)*60                     ! Delta from UTC, convert from minutes to seconds
   hour   = dat(5)                        ! Hour
   minute = dat(6)                        ! Minute
   second = dat(7)-utc+dat(8)/1000.0d0    ! Second   ! correction for time zone and milliseconds
!-----------------------------------------------------------------------------------------------------------------------------------
   julian = -HUGE(99999)                  ! this is the date if an error occurs and IERR is < 0
!-----------------------------------------------------------------------------------------------------------------------------------
   if(year==0 .or. year .lt. -4713) then
      ierr=-1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  You must compute first the number of years (Y) and months (M) since March 1st -4800 (March 1, 4801 BC)
   A=(14-month)/12 ! A will be 1 for January or February, and 0 for other months, with integer truncation
   Y=year+4800-A
   M=month+12*A-3  ! M will be 0 for March and 11 for February
!  All years in the BC era must be converted to astronomical years, so that 1BC is year 0, 2 BC is year "-1", etc.
!  Convert to a negative number, then increment towards zero
!  Staring from a Gregorian calendar date
   JDN=day + (153*M+2)/5 + 365*Y + Y/4 - Y/100 + Y/400 - 32045  !  with integer truncation
!  Finding the Julian Calendar date given the JDN (Julian day number) and time of day
   julian=JDN + dble(hour-12)/24.0d0 + dble(minute)/1440.0d0 + second/86400.0d0
!-----------------------------------------------------------------------------------------------------------------------------------
   if(julian.lt.0.d0) then                  ! Julian Day must be non-negative
      ierr=1
   else
      ierr=0
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine date_to_julian
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    date_to_unix(3f) - [M_time:UNIX_EPOCH] converts DAT date-time array to Unix
!!    Epoch Time
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine date_to_unix(dat,unixtime,ierr)
!!
!!     integer,intent(in)               :: dat(8)
!!     real(kind=realtime),intent(out)  :: unixtime
!!     integer,intent(out)              :: ierr
!!
!!##DESCRIPTION
!!    Converts a DAT date-time array to a UET (Unix Epoch Time).
!!
!!##OPTIONS
!!    dat   Integer array holding a "DAT" array, similar in structure
!!          to the array returned by the intrinsic DATE_AND_TIME(3f):
!!
!!              dat=[ year,month,day,timezone,hour,&
!!               & minutes,seconds,milliseconds]
!!##RETURNS
!!    unixtime  The "Unix Epoch" time, or the number of seconds since
!!              00:00:00 on January 1st, 1970, UTC.
!!    ierr      Error code. If 0 no error occurred.
!!
!!##EXAMPLE
!!
!!     Sample program:
!!
!!      program demo_date_to_unix
!!      use M_time, only : date_to_unix, realtime
!!      implicit none
!!      integer             :: dat(8)
!!      real(kind=realtime) :: unixtime
!!      integer             :: ierr
!!         call date_and_time(values=dat)
!!         write(*,'(" Today is:",*(i0:,":"))')dat
!!         call date_to_unix(dat,unixtime,ierr)
!!         write(*,*)'Unix Epoch time is ',unixtime
!!         write(*,*)'ierr is ',ierr
!!      end program demo_date_to_unix
!!
!!     results:
!!
!!      Today is:2016:7:18:-240:23:44:20:434
!!      Unix Epoch time is    1468899860.4340105
!!      ierr is            0
!!##AUTHOR
!!    John S. Urban, 2015
!!##LICENSE
!!    Public Domain
subroutine date_to_unix(dat,unixtime,ierr)

! ident_4="@(#) M_time date_to_unix(3f) Convert DAT date-time array to Unix Epoch Time"

integer,intent(in)              :: dat(8)       ! date time array similar to that returned by DATE_AND_TIME
real(kind=realtime),intent(out) :: unixtime     ! Unix time (seconds)
integer,intent(out)             :: ierr         ! return 0 on success, otherwise 1
real(kind=realtime)             :: julian
real(kind=realtime),save        :: julian_at_epoch
logical,save                    :: first=.true.
integer,parameter   :: ref(8)=[1970,1,1,0,0,0,0,0]
!-----------------------------------------------------------------------------------------------------------------------------------
if(first) then                                        ! Convert zero of Unix Epoch Time to Julian Date and save
   call date_to_julian(ref,julian_at_epoch,ierr)
   if(ierr.ne.0) return                               ! Error
   first=.false.
endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call date_to_julian(dat,julian,ierr)
   if(ierr.ne.0) return                               ! Error
   unixtime=(julian-julian_at_epoch)*secday
end subroutine date_to_unix
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    mtprng_init(3f) - [M_random:MERSENNE TWISTER] Initialize the Mersenne Twister random number generator with "seed"
!!    (LICENSE:BSD-4-Clause)
!!
!!##SYNOPSIS
!!
!!    subroutine mtprng_init(seed, state)
!!    integer(INT32),     intent(in)  :: seed
!!    type(mtprng_state), intent(out) :: state
!!
!!##DESCRIPTION
!!    Initializes the Mersenne Twister random number generator with "seed"
!!
!!##OPTIONS
!!    seed   A seed value is used to start a specific sequence of pseudo-random numbers
!!    state  generator state initialized by mtprng_init(3f) or mtprng_init_array(3f)
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_mtprng_init
!!    use M_random, only : mtprng_state, mtprng_init, mtprng_rand64
!!    use, intrinsic :: iso_fortran_env, only : int32, int64
!!    implicit none
!!    integer(INT32) :: seed
!!    type(mtprng_state) :: state
!!       GET_SEED: block
!!       integer :: count
!!       integer :: count_rate
!!          call system_clock(count, count_rate)
!!          seed=count
!!       endblock GET_SEED
!!      call mtprng_init(seed, state)
!!      ! returns a INT64 integer with a range in 0 .. 2^32-1
!!      write(*,*) mtprng_rand64(state)
!!    end program demo_mtprng_init
!!
!!   Sample Results:
!!
!!      867010878
subroutine mtprng_init(seed, state)

! ident_5="@(#) M_random mtprng_int(3f) Initializes the Mersenne Twister random number generator with "seed""

! arguments
integer(INT32),     intent(in)  :: seed
type(mtprng_state), intent(out) :: state
   ! working storage
   integer :: i
   ! save seed
   state%mt(0) = seed

   ! Set the seed using values suggested by Matsumoto & Nishimura, using
   !   a generator by Knuth. See original source for details.
   do i = 1, N - 1
      state%mt(i) = iand(4294967295_INT64,1812433253_INT64 * ieor(state%mt(i-1),ishft(state%mt(i-1),-30_INT64)) + i)
   enddo

   state%mti = N

end subroutine mtprng_init
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    mtprng_rand64(3f) - [M_random:MERSENNE TWISTER] Obtain the next 64-bit integer in the pseudo-random sequence
!!    (LICENSE:BSD-4-Clause)
!!
!!##SYNOPSIS
!!
!!    function mtprng_rand64(state) result(r)
!!    type(mtprng_state), intent(inout) :: state
!!    integer(INT64) :: r
!!
!!##DESCRIPTION
!!    Obtain the next 64-bit integer in the pseudo-random sequence in the range 0 to 2^32-1.
!!    Note that the range is considerably below the value of HUGE(0_int64).
!!
!!##OPTIONS
!!    state  generator state initialized by mtprng_init(3f) or mtprng_init_array(3f)
!!
!!##RETURNS
!!    r      next pseudo-random value in the range 0 to 2^32-1
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_mtprng_rand64
!!    use M_random, only : mtprng_state, mtprng_init, mtprng_rand64
!!    use, intrinsic :: iso_fortran_env, only : int32, int64
!!    implicit none
!!    integer(INT32) :: seed
!!    type(mtprng_state) :: state
!!      GET_SEED: block
!!      integer :: count
!!      integer :: count_rate
!!         call system_clock(count, count_rate)
!!      seed = count
!!      endblock GET_SEED
!!      call mtprng_init(seed, state)
!!      write(*,*) mtprng_rand64(state)
!!    end program demo_mtprng_rand64
function mtprng_rand64(state) result(r)

! ident_6="@(#) M_random mtprng_rand64(3f) Obtain the next 64-bit integer in the pseudo-random sequence"

! arguments
type(mtprng_state), intent(inout) :: state
!return type
integer(INT64) :: r

   ! internal constants
   integer(INT64), dimension(0:1), parameter :: mag01 = (/ 0_INT64, -1727483681_INT64 /)

   ! Period parameters
   integer(INT64), parameter :: UPPER_MASK =  2147483648_INT64
   integer(INT64), parameter :: LOWER_MASK =  2147483647_INT64

   ! Tempering parameters
   integer(INT64), parameter :: TEMPERING_B = -1658038656_INT64
   integer(INT64), parameter :: TEMPERING_C =  -272236544_INT64

   ! Note: variable names match those in original example
   integer(INT32) :: kk

   ! Generate N words at a time
   if (state%mti >= N) then
      ! The value -1 acts as a flag saying that the seed has not been set.
      if (state%mti == -1) call mtprng_init(4357_INT32,state)

      ! Fill the mt array
      do kk = 0, N - M - 1
         r = ior(iand(state%mt(kk),UPPER_MASK),iand(state%mt(kk+1),LOWER_MASK))
         state%mt(kk) = ieor(ieor(state%mt(kk + M),ishft(r,-1_INT64)),mag01(iand(r,1_INT64)))
      enddo

      do kk = N - M, N - 2
         r = ior(iand(state%mt(kk),UPPER_MASK),iand(state%mt(kk+1),LOWER_MASK))
         state%mt(kk) = ieor(ieor(state%mt(kk + (M - N)),ishft(r,-1_INT64)),mag01(iand(r,1_INT64)))
      enddo

      r = ior(iand(state%mt(N-1),UPPER_MASK),iand(state%mt(0),LOWER_MASK))
      state%mt(N-1) = ieor(ieor(state%mt(M-1),ishft(r,-1)),mag01(iand(r,1_INT64)))

      ! Start using the array from first element
      state%mti = 0
   endif

   ! Here is where we actually calculate the number with a series of
   !   transformations
   r = state%mt(state%mti)
   state%mti = state%mti + 1

 !-------------------------
 !!r = ieor(r,ishft(r,-11))
   r = ieor(r,ishft(iand(4294967295_INT64,r),-11)) ! Added a 32-bit mask to first r shift
 !-------------------------

   r = iand(4294967295_INT64,ieor(r,iand(ishft(r, 7),TEMPERING_B)))
   r = iand(4294967295_INT64,ieor(r,iand(ishft(r,15),TEMPERING_C)))
   r = ieor(r,ishft(r,-18))

end function mtprng_rand64
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
end module M_uuid
