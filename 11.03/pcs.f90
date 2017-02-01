MODULE pcs
!
! Modules
  USE numbers, ONLY: rp
!
! Implicit declarations
  IMPLICIT NONE
!
! Scalars
!
! Integer scalars
  INTEGER, TARGET, PUBLIC :: ieofx ! - X EOF option -
  INTEGER, TARGET, PUBLIC :: ieofy ! - Y EOF option -
!
  INTEGER, PUBLIC :: ieofx_bk ! - backup X EOF option -
  INTEGER, PUBLIC :: nxo      ! - optimal number of X EOF modes -
  INTEGER, PUBLIC :: nyo      ! - optimal number of Y EOF modes -
  INTEGER, PUBLIC :: nco      ! - optimal number of CCA modes -
  INTEGER, PUBLIC :: mxe      ! - maximum number of X EOF modes -
  INTEGER, PUBLIC :: mye      ! - maximum number of Y EOF modes -
  INTEGER, PUBLIC :: mcc      ! - maximum number of CCA modes -
  INTEGER, PUBLIC :: nxe      ! - number of X EOF modes -
  INTEGER, PUBLIC :: nye      ! - number of Y EOF modes -
  INTEGER, PUBLIC :: ncc      ! - number of CCA modes -
  INTEGER, PUBLIC :: npx      ! - number of non-zero X eigenvalues -
  INTEGER, PUBLIC :: npy      ! - number of non-zero Y eigenvalues -
  INTEGER, PUBLIC :: ncu      ! - number of canonical modes to calculate -
  INTEGER, PUBLIC :: iex      ! - current X EOF mode -
  INTEGER, PUBLIC :: iey      ! - current Y EOF mode -
  INTEGER, PUBLIC :: iec      ! - current CCA mode -
  INTEGER, PUBLIC :: icco     ! - perfect canonical correlation option flag (0 = continue; 1 = avoid) -
!
! Real scalars
  REAL(KIND=rp), PUBLIC :: cancor ! - canonical correlation -
!
CONTAINS
!
!
 SUBROUTINE init_pcs ()
!
! Initialises PC settings
!
! Executable Statements
!
! Reset calculation options
  ieofx=1
  ieofy=1
  ieofx_bk=1
  icco=0
!
! Reset current modes
  iex=1
  iey=1
  iec=1
!
  RETURN
 END SUBROUTINE init_pcs
!
!
!
 FUNCTION eofx_opts()
!
! Calls function to set X EOF options
!
! Modules
  USE settings, ONLY: nx,nt,lcw
!
! Function type
  INTEGER :: eofx_opts
!
! Executable Statements
!
! Prompt for EOF options
  eofx_opts=eof_opts('X',nx,nt,lcw,nxe,mxe)
  ieofx_bk=ieofx
!
  RETURN
 END FUNCTION eofx_opts
!
!
!
 FUNCTION eofy_opts()
!
! Calls function to set Y EOF options
!
! Modules
  USE settings, ONLY: ny,nt,lcw
!
! Function type
  INTEGER :: eofy_opts
!
! Executable Statements
!
! Prompt for EOF options
  eofy_opts=eof_opts('Y',ny,nt,lcw,nye,mye)
!
  RETURN
 END FUNCTION eofy_opts
!
!
!
 FUNCTION eof_opts(cxy,nv,nt,lcw,ne,me)
!
! Sets EOF options
!
! Modules
  USE analysis, ONLY: reset
  USE numbers,  ONLY: ihuge
!
! Function type
  INTEGER :: eof_opts
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nv  ! - number of variables -
  INTEGER, INTENT(IN) :: nt  ! - number of time steps -
  INTEGER, INTENT(IN) :: lcw ! - length of cross-vlidation window -
!
  CHARACTER(LEN=*), INTENT(IN) :: cxy ! - X/Y variable flag -
!
! Input/output scalars
  INTEGER, INTENT(INOUT) :: ne ! - minimum number of modes -
  INTEGER, INTENT(INOUT) :: me ! - maximum number of modes -
!
! Locals
!
! Local scalars
  INTEGER :: n      ! - nt-lcw-1 -
  INTEGER :: mm     ! - maximum number of modes -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MIN
!
! Executable Statements
!
! Calculate maximum number of modes
  eof_opts=2
  n=nt-lcw-1
  IF (nv>0) THEN
     IF (n>1) THEN
        mm=MIN(n,nv)
     ELSE
        mm=nv
     END IF
  ELSE
     IF (n>1) THEN
        mm=n
     ELSE
        mm=ihuge
     END IF
  END IF
  IF (mm==1) THEN
     ne=1
     me=1
     RETURN
  END IF
!
! Check for completed calculations
  IF (reset('Changing the modes settings')==1) RETURN
!
! Prompt for EOF options
  WRITE (UNIT=*,FMT='(A)')
  WRITE (UNIT=*,FMT='(A)') cxy//' modes options'
1 WRITE (UNIT=*,FMT='(3A)',ADVANCE='no') 'Minimum number of ',cxy,' modes: '
  READ (UNIT=*,FMT=*,ERR=1) ne
  IF ((ne<1).OR.(ne>mm)) GOTO 1
  IF (ne==mm) THEN
     me=mm
     RETURN
  END IF
2 WRITE (UNIT=*,FMT='(3A)',ADVANCE='no') 'Maximum number of ',cxy,' modes: '
  READ (UNIT=*,FMT=*,ERR=2) me
  IF ((me<ne).OR.(me>mm)) GOTO 2
!
  RETURN
 END FUNCTION eof_opts
!
!
!
 FUNCTION advanced_eof(cxy,ieof)
!
! Sets EOF options
!
! Modules
  USE CPT_constants, ONLY: npo
  USE labels,        ONLY: cg_pccos
!
! Function type
  INTEGER :: advanced_eof
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: cxy ! - X/Y variable flag -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ieof ! - modes calculation option -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - principal components calculation option -
!
! Executable Statements
!
! Prompt for modes calculation options
  WRITE (UNIT=*,FMT='(A)')
1 WRITE (UNIT=*,FMT='(A)') cxy//' modes calculation options'
  DO i=1,npo
     WRITE (UNIT=*,FMT='(A)') i,'.  ',cg_pccos(i)
  END DO
  READ (UNIT=*,FMT=*,ERR=1) ieof
  IF ((ieof<1).OR.(ieof>npo)) GOTO 1
  advanced_eof=0
!
  RETURN
 END FUNCTION advanced_eof
!
!
!
 FUNCTION cca_opts()
!
! Sets CCA options
!
! Modules
  USE analysis, ONLY: reset
  USE maths,    ONLY: magnitude
!
! Function type
  INTEGER :: cca_opts
!
! Locals
!
! Local scalars
  INTEGER :: mm ! - maximum number of modes -
!
  CHARACTER(LEN=8) :: fmt ! - format statement -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MIN
!
! Executable Statements
!
! Calculate maximum number of modes
  cca_opts=0
  mm=MIN(mxe,mye)
  IF (mm==1) THEN
     ncc=1
     mcc=1
     RETURN
  END IF
!
! Check for completed calculations
  IF (reset('Changing the CCA settings')==1) RETURN
!
! Get number of CCA modes
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=fmt,FMT='(A,I1,A)') '(A,I',magnitude(mm),',A)'
  WRITE (UNIT=*,FMT=fmt) 'CCA modes (maximum = ',mm,')'
1 WRITE (UNIT=*,FMT='(A)',ADVANCE='no') 'Minimum number of CCA modes: '
  READ (UNIT=*,FMT=*,ERR=1)  ncc
  IF ((ncc<1).OR.(ncc>mm)) GOTO 1
  IF (ncc==mm) THEN
     mcc=mm
     RETURN
  END IF
2 WRITE (UNIT=*,FMT='(A)',ADVANCE='no') 'Maximum number of CCA modes: '
  READ (UNIT=*,FMT=*,ERR=2)  mcc
  IF ((mcc<ncc).OR.(mcc>mm)) GOTO 2
!
  RETURN
 END FUNCTION cca_opts
!
!
!
 FUNCTION gcm_opts()
!
! Sets EOF options
!
! Modules
  USE analysis, ONLY: reset
  USE settings, ONLY: igcms,intp
!
! Function type
  INTEGER :: gcm_opts
!
! Executable Statements
!
  gcm_opts=0
!
! Check for completed calculations
  IF (reset('Changing the GCM standardization options')==1) RETURN
!
! Prompt for GCM options
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)',ADVANCE='yes') 'GCM standardization options'
1 WRITE (UNIT=*,FMT='(A)') 'Select Model Grid Method:'
  WRITE (UNIT=*,FMT='(A)',ADVANCE='yes') '1. Interpolate '
  WRITE (UNIT=*,FMT='(A)',ADVANCE='yes') '2. Nearest gridpoint'
  READ (UNIT=*,FMT=*,ERR=1) intp 
  IF ((intp<1).OR.(intp>2)) GOTO 1
  IF (intp==2) intp=0
2 WRITE (UNIT=*,FMT='(A)',ADVANCE='yes') 'Select Model Climatology Method: '
  WRITE (UNIT=*,FMT='(A)',ADVANCE='yes') '1. No correction '
  WRITE (UNIT=*,FMT='(A)',ADVANCE='yes') '2. Correct mean biases '
  WRITE (UNIT=*,FMT='(A)',ADVANCE='yes') '3. Correct mean and variance biases '
  WRITE (UNIT=*,FMT='(A)',ADVANCE='yes') '4. Correct for skill'
  READ (UNIT=*,FMT=*,ERR=2) igcms 
  IF ((igcms<1).OR.(igcms>4)) GOTO 2
!
  RETURN
 END FUNCTION gcm_opts
!
!
!
 FUNCTION check_pcs()
!
! Checks PC settings
!
! On exit:
!    ifail =  0 All checks passed
!    ifail =  1 nxe > nt-lcw-1, nxe reset to nt-lcw-1
!    ifail =  2 nxe > mxa, nxe reset to mxa
!    ifail =  3 mxe > nxe, mxe reset to nxe
!    ifail =  4 mxe > nt-lcw-1, mxe reset to nt-lcw-1
!    ifail =  5 mxe > mxa, mxe reset to mxa
!    ifail =  6 nye > nt-lcw-1, nye reset to nt-lcw-1
!    ifail =  7 nye > mya, nye reset to mya
!    ifail =  8 mye > nye, mye reset to nye
!    ifail =  9 mye > nt-lcw-1, mye reset to nt-lcw-1
!    ifail = 10 mye > mya, mye reset to mya
!    ifail = 11 ncc > nxe, ncc reset to nxe
!    ifail = 12 ncc > nye, ncc reset to nye
!    ifail = 13 mcc < ncc, mcc reset to ncc
!    ifail = 14 mcc > mxe, mcc reset to mxe
!    ifail = 15 mcc > mye, mcc reset to mye
!    ifail = 16 mxe+mye > nt
!
! Modules
  USE analysis, ONLY: ianal
  USE settings, ONLY: nt,mxa,mya
!
! Function type
  INTEGER :: check_pcs
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
!
! Executable Statements
!
! Check that number of modes is not greater than number of variables or number of cases
  IF (ianal<3) THEN
     CALL check_ne (mxa,nt,nxe,mxe,ifail)
     IF (ifail/=0) THEN
        check_pcs=ifail
        RETURN
     END IF
     IF (ianal==1) THEN
        CALL check_ne (mya,nt,nye,mye,ifail)
        IF (ifail/=0) THEN
           check_pcs=ifail+5
           RETURN
        END IF
!
! Check that number of CCA modes is not greater than number of EOF modes
        CALL check_nc (nxe,mxe,nye,mye,ncc,mcc,ifail)
        IF (ifail/=0) THEN
           check_pcs=ifail+10
           RETURN
        END IF
!
! Check that combined number of modes is not greater than number of cases
        CALL check_nes (nt,mxe,mye,ifail)
        IF (ifail/=0) THEN
           check_pcs=16
           RETURN
        END IF
     END IF
  END IF
!
  check_pcs=0
  RETURN
 END FUNCTION check_pcs
!
!
!
 SUBROUTINE check_ne (nv,nt,ne,me,ifail)
!
! Checks that number of EOF modes is not greater than number of cases-1 or variables
! and that maximum number of modes is not less than minimum number.
!
! On exit:
!    ifail = 0 All checks passed
!    ifail = 1 ne > nt-lcw-1, ne reset to nt-lcw-1
!    ifail = 2 ne > nv, ne reset to nv
!    ifail = 3 me > ne, me reset to ne
!    ifail = 4 me > nt-lcw-1, me reset to nt-lcw-1
!    ifail = 5 me > nv, me reset to nv
!
! Modules
  USE settings, ONLY: lcw
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nv ! - number of variables -
  INTEGER, INTENT(IN) :: nt ! - number of cases -
!
! Input/output scalars
  INTEGER, INTENT(INOUT) :: ne ! - minimum number of modes -
  INTEGER, INTENT(INOUT) :: me ! - maximum number of modes -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
  INTEGER :: lvt ! - minimum of nv and nt -
  INTEGER :: n   ! - number of cross-validation cases - 1  -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MIN
!
! Executable Statements
!
! Set maximum number of modes
  n=nt-lcw-1
  lvt=MIN(nv,n)
!
! Force minimum number of modes to be no more than number of variables or number of cases
  IF (ne>lvt) THEN
     ne=lvt
     IF (lvt==n) THEN
        ifail=1
     ELSE
        ifail=2
     END IF
     RETURN
  END IF
!
! Force maximum number of modes to be at least the minimum number
  IF (me<ne) THEN
     me=ne
     ifail=3
     RETURN
  END IF
!
! Force maximum number of modes to be no more than number of variables or number of cases
  IF (me>lvt) THEN
     me=lvt
     IF (lvt==n) THEN
        ifail=4
     ELSE
        ifail=5
     END IF
     RETURN
  END IF
  ifail=0
!
  RETURN
 END SUBROUTINE check_ne
!
!
!
 SUBROUTINE check_nes (nt,mxe,mye,ifail)
!
! Checks that combined number of EOF modes is not greater than number of cases.
!
! On exit:
!    ifail = 0 All checks passed
!    ifail = 1 mxe+mye > nt
!
! Modules
  USE settings, ONLY: lcw
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nt  ! - number of cases -
  INTEGER, INTENT(IN) :: mxe ! - minimum number of X modes -
  INTEGER, INTENT(IN) :: mye ! - maximum number of Y modes -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
  INTEGER :: n ! - number of cross-validation cases  -
  INTEGER :: m ! - combined number of EOF modes  -
!
! Executable Statements
!
! Identify maximum number of combined modes
  n=nt-lcw
  m=mxe+mye
!
! Warn if number of modes is too large
  IF ((m>n).AND.(icco==1)) THEN
     ifail=1
  ELSE
     ifail=0
  END IF
!
  RETURN
 END SUBROUTINE check_nes
!
!
!
 SUBROUTINE check_nc (nxe,mxe,nye,mye,ncc,mcc,ifail)
!
! Checks that number of CCA modes is not greater than number of EOF modes
! and that maximum number of modes is not less than minimum number
!
! On exit:
!    ifail = 0 All checks passed
!    ifail = 1 nxe > nxe, ncc reset to nxe
!    ifail = 2 ncc > nye, ncc reset to nye
!    ifail = 3 mcc < ncc, mcc reset to ncc
!    ifail = 4 mcc > mxe, mcc reset to mxe
!    ifail = 5 mcc > mye, mcc reset to mye
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nxe ! - minimum number of X EOF modes -
  INTEGER, INTENT(IN) :: nye ! - minimum number of Y EOF modes -
  INTEGER, INTENT(IN) :: mxe ! - maximum number of X EOF modes -
  INTEGER, INTENT(IN) :: mye ! - maximum number of Y EOF modes -
!
! Input/output scalars
  INTEGER, INTENT(INOUT) :: ncc ! - number of CCA modes -
  INTEGER, INTENT(INOUT) :: mcc ! - maximum number of CCA modes -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
  INTEGER :: lxy ! - minimum of nxe and nye -
!
! Executable Statements
!
! Force minimum number of CCA modes to be no more than minimum numbers of EOF modes
  IF (nxe<=nye) THEN
     lxy=nxe
     ifail=1
  ELSE
     lxy=nye
     ifail=2
  END IF
  IF (ncc>lxy) THEN
     ncc=lxy
     RETURN
  END IF
!
! Force maximum number of CCA modes to be at least the minimum number
  IF (mcc<ncc) THEN
     mcc=ncc
     ifail=3
     RETURN
  END IF
!
! Force maximum number of CCA modes to be no more than smallest of the maximum numbers of EOF modes
  IF (mxe<=mye) THEN
     lxy=mxe
     ifail=4
  ELSE
     lxy=mye
     ifail=5
  END IF
  IF (mcc>lxy) THEN
     mcc=lxy
     RETURN
  END IF
  ifail=0
!
  RETURN
 END SUBROUTINE check_nc
END MODULE pcs
