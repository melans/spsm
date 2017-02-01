! $Id: IO_constants.f90 1078 2010-10-04 15:25:47Z lsong $
! Author: Simon Mason
MODULE IO_constants
!
! Parameters
!
! Integer parameters
  INTEGER, PARAMETER, PUBLIC :: iin=11    ! - input file unit number -
  INTEGER, PARAMETER, PUBLIC :: iout=21   ! - file output unit number -
  INTEGER, PARAMETER, PUBLIC :: ioutb0=30 ! - bootstrap output unit number -
  INTEGER, PARAMETER, PUBLIC :: ioutb1=31 ! - bootstrap output unit number -
  INTEGER, PARAMETER, PUBLIC :: ioutb2=32 ! - bootstrap output unit number -
  INTEGER, PARAMETER, PUBLIC :: ioutb3=33 ! - bootstrap output unit number -
  INTEGER, PARAMETER, PUBLIC :: ioutb4=34 ! - bootstrap output unit number -
  INTEGER, PARAMETER, PUBLIC :: ioutc0=40 ! - contingency-table output unit number -
  INTEGER, PARAMETER, PUBLIC :: ioutc1=41 ! - contingency-table output unit number -
  INTEGER, PARAMETER, PUBLIC :: ioutc2=42 ! - contingency-table output unit number -
  INTEGER, PARAMETER, PUBLIC :: ioutf0=50 ! - forecast output unit number -
  INTEGER, PARAMETER, PUBLIC :: ioutf1=51 ! - forecast output unit number -
  INTEGER, PARAMETER, PUBLIC :: ioutf2=52 ! - forecast output unit number -
  INTEGER, PARAMETER, PUBLIC :: ioutf3=53 ! - forecast output unit number -
  INTEGER, PARAMETER, PUBLIC :: ioutf4=54 ! - forecast output unit number -
  INTEGER, PARAMETER, PUBLIC :: ioutf5=55 ! - forecast output unit number -
  INTEGER, PARAMETER, PUBLIC :: ioutf6=56 ! - forecast output unit number -
  INTEGER, PARAMETER, PUBLIC :: ioutf7=57 ! - forecast output unit number -
  INTEGER, PARAMETER, PUBLIC :: ioutf8=58 ! - forecast output unit number -
  INTEGER, PARAMETER, PUBLIC :: iouts0=60 ! - scatter plot output unit number -
  INTEGER, PARAMETER, PUBLIC :: ioutv0=70 ! - verification output unit number -
  INTEGER, PARAMETER, PUBLIC :: ioutv1=71 ! - verification output unit number -
  INTEGER, PARAMETER, PUBLIC :: ioutv2=72 ! - verification output unit number -
  INTEGER, PARAMETER, PUBLIC :: ioutv3=73 ! - verification output unit number -
!
  INTEGER, PARAMETER, PUBLIC :: nfmt=2 ! - number of format options -
  INTEGER, PARAMETER, PUBLIC :: nacc=2 ! - number of access options -
  INTEGER, PARAMETER, PUBLIC :: nprc=2 ! - number of data precisions -
  INTEGER, PARAMETER, PUBLIC :: ngf=2  ! - number of graphics file formats -
!
  INTEGER, PARAMETER, PUBLIC :: lfli=2**24          ! - maximum number of characters in input line -
  INTEGER, PARAMETER, PUBLIC :: ltag=2**12          ! - maximum length of tag line -
  INTEGER, PARAMETER, PUBLIC :: ldat=12             ! - maximum length of date -
  INTEGER, PARAMETER, PUBLIC :: lprd=25             ! - maximum length of period -
  INTEGER, PARAMETER, PUBLIC :: lstn=16             ! - maximum length of station names -
  INTEGER, PARAMETER, PUBLIC :: lvar=32             ! - maximum length of variable names and units -
  INTEGER, PARAMETER, PUBLIC :: lss=lstn+1          ! - maximum space required for station names -
  INTEGER, PARAMETER, PUBLIC :: ldir=150            ! - maximum length of directory -
  INTEGER, PARAMETER, PUBLIC :: lnam=36             ! - maximum length of file name -
  INTEGER, PARAMETER, PUBLIC :: lext=4              ! - maximum length of file extension -
  INTEGER, PARAMETER, PUBLIC :: lfil=ldir+lnam+lext ! - maximum length of file  -
  INTEGER, PARAMETER, PUBLIC :: ldsc=37             ! - maximum length of file description -
  INTEGER, PARAMETER, PUBLIC :: lstr=16             ! - maximum length of file structure description -
  INTEGER, PARAMETER, PUBLIC :: lstr_v10=10         ! - old maximum length of file structure description -
!
! Character parameters
  CHARACTER(LEN= 1), PARAMETER, PUBLIC :: cdir='/'         ! - directory marker -
  CHARACTER(LEN= 7), PARAMETER, PUBLIC :: default_ini= &   ! - default CPT initialization file -
     'CPT.ini'
  CHARACTER(LEN= 5), PARAMETER, PUBLIC :: cxmlns= &        ! - XML namespace -
     'xmlns'
  CHARACTER(LEN=32), PARAMETER, PUBLIC :: cxmlns_cpt= &    ! - CPT XML namespace -
     'http://iri.columbia.edu/CPT/v10/'
  CHARACTER(LEN=19), PARAMETER, PUBLIC :: cxmlns_wmo= &    ! - WMO XML namespace -
     'http://www.wmo.int/'
  CHARACTER(LEN=10), PARAMETER, PUBLIC :: cxmlns_cf= &     ! - cf XML namespace -
     'http://cf/'
  CHARACTER(LEN= 4), PARAMETER, PUBLIC :: gext(ngf)= &     ! - graphic file extensions -
     (/'.jpg','.bmp'/)
  CHARACTER(LEN=11), PARAMETER, PUBLIC :: ffmts(nfmt)= &   ! - file formats -
     (/'unformatted','formatted  '/)
  CHARACTER(LEN=10), PARAMETER, PUBLIC :: faccs(nacc)= &   ! - file accesses -
     (/'sequential','direct    '/)
  CHARACTER(LEN= 6), PARAMETER, PUBLIC :: cprcs(nprc)= &   ! - data precisions -
     (/'single','double'/)
!
END MODULE IO_constants
