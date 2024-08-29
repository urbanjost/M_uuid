[![](docs/images/M_uuid_small.gif)](https://urbanjost.github.io/M_uuid/fpm-ford/index.html)
# [M_uuid](https://urbanjost.github.io/M_uuid/man3.html)

## Name
   M_uuid(3f) - a module of UUID (Universally Unique IDentifier) procedures
   (LICENSE:BSD-4-Clause)
## Synopsis
```text
       use M_uuid, only : generate_uuid
       function generate_uuid(version) result(uuid)
       integer, intent(in), optional :: version
       character(len=36) :: uuid
```
## Description

####   __Remember you are unique, just like everyone else__.

   _from Wikipedia_ ...

   A universally unique identifier (UUID) is a 128-bit number used to
   identify information on computer systems. In particular, they are
   commonly used to create unique filenames and database and table keys.

   When generated according to the standard methods, UUIDs are for
   practical purposes unique, without depending for their uniqueness
   on a central registration authority or coordination between the
   parties generating them, unlike most other numbering schemes. While
   the probability that a UUID will be duplicated is not zero, it is
   close enough to zero to be negligible.

   Thus, anyone can create a UUID and use it to identify something with
   near certainty that the identifier does not duplicate one that has
   already been or will be created to identify something else. Information
   labeled with UUIDs by independent parties can therefore be later
   combined into a single database, or transmitted on the same channel,
   without needing to resolve conflicts between identifiers.

   Adoption of UUIDs and GUIDs is widespread. Many computing platforms
   provide support for generating them, and for parsing their textual
   representation.

   Note that several UUID types contain information that can be decyphered
   to recreate the creation time or the MAC address of the device that
   generated the UUID which has several uses for verifying when and
   where data was generated.

   [RFC 4122](https://tools.ietf.org/html/rfc4122)
   defines a Uniform Resource Name (URN) namespace for UUIDs.
   A UUID presented as a URN appears as follows:
```text
             urn:uuid:123e4567-e89b-12d3-a456-426655440000
```
   + Wikipedia contributors. (2021, September 21). Universally unique identifier. 
     In Wikipedia, The Free Encyclopedia. Retrieved 01:41, September 25, 2021, from
     https://en.wikipedia.org/w/index.php?title=Universally_unique_identifier&oldid=1045581110

## Example
```text
     program demo_generate_uuid
     ! generate 36-character UUID string
     use M_uuid, only : generate_uuid
     implicit none
     character(len=36)   :: uuid
     character(len=4096) :: filename
        !
	! version 1 (time-based UUID)
        uuid=generate_uuid(version=1) 
        write(*,'(a36)')uuid
        !
	! version 4 (pseudo-RNG-based), default
        uuid=generate_uuid(version=4)  
        write(*,'(a36)')uuid
        !
        ! RFC 4122 defines a Uniform Resource Name (URN) namespace for UUIDs.
        write(*,'("urn:uuid:",a36)')uuid
        !
        ! a good scratch file name
        open(file='/tmp/scratch_'//uuid,unit=10)
	inquire(unit=10,name=filename)
	write(*,'(*(g0))') trim(filename)
	close(unit=10,status='delete')
     end program demo_generate_uuid
```
Results:
```text
   > 14d8c5e4-65a3-11ef-73c0-7686f8f3c380
   > bdd1f289-24b7-40a9-625b-a4c4deefb0f2
   > urn:uuid:bdd1f289-24b7-40a9-625b-a4c4deefb0f2
   > /tmp/scratch_bdd1f289-24b7-40a9-625b-a4c4deefb0f2
```
---
![gmake](docs/images/gnu.gif)
---
## Building the Module using make(1)
     git clone https://github.com/urbanjost/M_uuid.git
     cd M_uuid/src
     # change Makefile if not using one of the listed compilers
     
     # for gfortran
     make clean
     make F90=gfortran gfortran
     
     # for ifort
     make clean
     make F90=ifort ifort

     # for nvfortran
     make clean
     make F90=nvfortran nvfortran

This will compile the Fortran module and basic example
program that exercise the routine.

---
![-](docs/images/fpm_logo.gif)
---
## Build and Test with FPM

   Alternatively, download the github repository and build it with
   fpm ( as described at [Fortran Package Manager](https://github.com/fortran-lang/fpm) )

   ```bash
        git clone https://github.com/urbanjost/M_uuid.git
        cd M_uuid
        fpm run "*"
        fpm run --example "*"
        fpm test
   ```

   or just list it as a dependency in your fpm.toml project file.

```toml
        [dependencies]
        M_uuid        = { git = "https://github.com/urbanjost/M_uuid.git" }
```
---
![docs](docs/images/docs.gif)
---
## Documentation

### User
   - A single page that uses javascript to combine all the HTML
     descriptions of the man-pages is at 
     [BOOK_M_uuid](https://urbanjost.github.io/M_uuid/BOOK_M_uuid.html).

   - a simple index to the man-pages in HTML form for the
   [routines](https://urbanjost.github.io/M_uuid/man3.html) 
   and [programs](https://urbanjost.github.io/M_uuid/man1.html) 

   - There are man-pages in the repository download in the docs/ directory
     that may be installed on ULS (Unix-Like Systems).

   - ![man-pages](docs/images/manpages.gif)
      + [manpages.zip](https://urbanjost.github.io/M_uuid/manpages.zip)
      + [manpages.tgz](https://urbanjost.github.io/M_uuid/manpages.tgz)

   - [CHANGELOG](docs/CHANGELOG.md) provides a history of significant changes

### Developer
   - [ford(1) output](https://urbanjost.github.io/M_uuid/fpm-ford/index.html).
<!--
   - [doxygen(1) output](https://urbanjost.github.io/M_uuid/doxygen_out/html/index.html).
-->
   - [github action status](docs/STATUS.md) 
---
## Pedigree
   This is a modified version of generate_uuid(3f).  generate_uuid(3f)
   was originally derived from the xmlf90 codebase, (c) Alberto Garcia &
   Jon Wakelin, 2003-2004.  It also calls RNG routines from Scott Ladd
   <scott.ladd@coyotegulch.com>, and the libFoX modules. Although some
   sections have been replaced, generate_uuid(3f) was originally based
   on the libFoX version.
---
![-](docs/images/ref.gif)
---
## References

   * [RFC-4122](https://tools.ietf.org/html/rfc4122)
<!--
   * [RFC-4122](https://datatracker.ietf.org/doc/html/rfc4122)
-->
   * [Wikipedia](https://en.wikipedia.org/wiki/Universally_unique_identifier)
   * [FOX](http://fortranwiki.org/fortran/show/FoX)
---
