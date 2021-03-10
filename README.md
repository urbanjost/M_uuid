## NAME
   M_uuid(3f) - a module of UUID (Universally Unique IDentifier) procedures
   (LICENSE:CUSTOM OPEN)
## SYNOPSIS
```text
       use m_uuid, only : generate_uuid
```
## QUOTE
   `Remember you are unique, just like everyone else.`
## DESCRIPTION
   A universally unique identifier (UUID) is a 128-bit number used to
   identify information in computer systems.

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

   RFC 4122 defines a Uniform Resource Name (URN) namespace for UUIDs.
   A UUID presented as a URN appears as follows:
```text
             urn:uuid:123e4567-e89b-12d3-a456-426655440000
```
       **-- Wikipedia**

## PROCEDURES
       * generate_uuid(version) ! generate 36-character UUID string

                                                          March 09, 2021                                                 M_uuid(3)
## BUILDING THE MODULE USING make(1)
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

## USER DOCUMENTATION
   - a simple [index](https://urbanjost.github.io/M_uuid/) to
     the individual manpages in HTML form

   - A single page that uses javascript to combine all the HTML
     descriptions of the manpages is at 
     [BOOK_M_draw](https://urbanjost.github.io/M_uuid/BOOK_M_uuid.html).

   - Or all the HTML appended together is in 
     [M_draw](https://urbanjost.github.io/M_draw/M_uuid.html)

## UNIT TESTS
There are no automated unit tests per-se. Running the example programs
and demo programs ensures the library is functioning.

## SUPPORTS FPM ![fpm](docs/images/fpm_logo.gif)

   Alternatively, download the github repository and build it with
   fpm ( as described at [Fortran Package Manager](https://github.com/fortran-lang/fpm) )

   ```bash
        git clone https://github.com/urbanjost/M_uuid.git
        cd M_uuid
        fpm run
        fpm run --example
        fpm test
   ```

   or just list it as a dependency in your fpm.toml project file.

```toml
        [dependencies]
        M_uuid        = { git = "https://github.com/urbanjost/M_uuid.git" }
```
