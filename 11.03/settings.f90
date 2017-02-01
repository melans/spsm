! $Id: settings.f90 1245 2011-03-04 21:14:47Z simon $
MODULE settings
!
! Modules
  USE CPT_constants, ONLY: nev,nstd
  USE numbers,       ONLY: rp
!
! Implicit declarations
  IMPLICIT NONE
!
! Arrays
!
! Integer arrays
  INTEGER, PUBLIC :: istds(nstd) ! - standardization options flags -
  INTEGER, PUBLIC :: ievo(nev)   ! - error-variance options flags -
!
! Scalars
!
! Integer scalars
  INTEGER, TARGET, PUBLIC :: nx ! - total number of X variables -
  INTEGER, TARGET, PUBLIC :: ny ! - total number of Y variables -
  INTEGER, TARGET, PUBLIC :: nz ! - total number of Z variables -
!
  INTEGER, POINTER, PUBLIC :: nv ! - total number of variables -
!
  INTEGER, PUBLIC :: nt        ! - number of training cases -
  INTEGER, PUBLIC :: nu        ! - number of used training cases -
  INTEGER, PUBLIC :: nt_old    ! - previous number of training cases -
  INTEGER, PUBLIC :: nt1       ! - initial number of training cases -
  INTEGER, PUBLIC :: nu1       ! - initial number of used training cases -
  INTEGER, PUBLIC :: ntr       ! - number of retroactive verification cases -
  INTEGER, PUBLIC :: nur       ! - number of used retroactive verification cases -
  INTEGER, PUBLIC :: nret      ! - number of retroactive iterations -
  INTEGER, PUBLIC :: nf        ! - number of forecasts -
  INTEGER, PUBLIC :: nens      ! - number of ensemble members -
  INTEGER, PUBLIC :: m         ! - number of variables -
  INTEGER, PUBLIC :: mxa       ! - total number of available X variables -
  INTEGER, PUBLIC :: mya       ! - total number of available Y variables -
  INTEGER, PUBLIC :: mza       ! - total number of available Z variables -
  INTEGER, PUBLIC :: iretro    ! - retroactive forecast flag -
  INTEGER, PUBLIC :: nretro    ! - model update interval -
  INTEGER, PUBLIC :: ifcast    ! - operational forecast flag -
  INTEGER, PUBLIC :: ncv       ! - length of cross-validated training period -
  INTEGER, PUBLIC :: lcw       ! - length of cross-validation window -
  INTEGER, PUBLIC :: lcw_old   ! - previous length of cross-validation window -
  INTEGER, PUBLIC :: hcw       ! - half cross-validation window -
  INTEGER, PUBLIC :: lxt       ! - minimum of nxa and nu -
  INTEGER, PUBLIC :: lyt       ! - minimum of nya and nu -
  INTEGER, PUBLIC :: lxyt      ! - MIN(MAX(nxa,nya),nu) -
  INTEGER, PUBLIC :: lc1       ! - first dimension of ce -
  INTEGER, PUBLIC :: lr1       ! - first dimension of r -
  INTEGER, PUBLIC :: iv        ! - current series -
  INTEGER, PUBLIC :: ivf       ! - current series by field -
  INTEGER, PUBLIC :: iva       ! - current available series -
  INTEGER, PUBLIC :: ivfa      ! - current available series by field -
  INTEGER, PUBLIC :: igauss    ! - transform to gaussian flag -
  INTEGER, PUBLIC :: igauss_bk ! - backup transform to gaussian flag -
  INTEGER, PUBLIC :: istd      ! - standardization option -
  INTEGER, PUBLIC :: istdo     ! - original standardization option -
  INTEGER, PUBLIC :: igood     ! - goodness index flag -
  INTEGER, PUBLIC :: igcms     ! - GCM standardization option -
  INTEGER, PUBLIC :: intp      ! - interpolation option -
  INTEGER, PUBLIC :: izero     ! - zero-bound flag -
  INTEGER, PUBLIC :: isem      ! - sort ensemble members flag -
  INTEGER, PUBLIC :: ipval     ! - p-values calculation flag -
  INTEGER, PUBLIC :: iprec     ! - forecast precision -
  INTEGER, PUBLIC :: iodds     ! - odds ratio flag -
  INTEGER, PUBLIC :: lag       ! - forecast lag -
  INTEGER, PUBLIC :: iev       ! - error-variance flag -
  INTEGER, PUBLIC :: xfd_old   ! - backup X first year of interest -
  INTEGER, PUBLIC :: yfd_old   ! - backup Y first year of interest -
  INTEGER, PUBLIC :: isave     ! - change made to project settings flag -
  INTEGER, PUBLIC :: liwk      ! - integer workspace dimensions -
  INTEGER, PUBLIC :: lrwk      ! - real workspace dimensions -
!
! Real scalars
  REAL(KIND=rp), PUBLIC :: clf   ! - forecast confidence level -
  REAL(KIND=rp), PUBLIC :: dofr  ! - degrees of freedom for regression -
!
! Logical scalars
  LOGICAL, PUBLIC :: lnew ! - new settings flag -
!
CONTAINS
!
!
 SUBROUTINE odd_lcw (lcw,ifail)
!
! Forces length of cross-validation window to be odd
!
! Modules
  USE gui, ONLY: yesno
!
! Implicit declarations
  IMPLICIT NONE
!
! Arguments
!
! Input/output scalars
  INTEGER, INTENT(INOUT) :: lcw ! - length of cross-validation window -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error flag -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MOD
!
! Executable Statements
!
! Force lcw to be odd
  IF ((MOD(lcw,2)==0).AND.(lcw>0)) THEN
     lcw=lcw+1
     WRITE (UNIT=*,FMT='(A)') 'WARNING: Length of cross-validation window must be odd. Length has been increased.'
     WRITE (UNIT=*,FMT='(A)',ADVANCE='no') '         Length has been increased to:'
     WRITE (UNIT=*,FMT=*) lcw
     WRITE (UNIT=*,FMT=*) 'OK (Y/N)?'
     ifail=1-yesno()
  ELSE
     ifail=0
  END IF
!
  RETURN
 END SUBROUTINE odd_lcw
!
!
!
 FUNCTION record_change()
!
! Records a change to be saved in project file
!
! Function type
  INTEGER :: record_change
!
! Executable Statements
!
! Record change
  isave=1
  record_change=2
!
  RETURN
 END FUNCTION record_change
!
!
!
 FUNCTION set_zero()
!
! Toggles zero-bound option, and resets standardization option if necessary
!
! Modules
  USE errors, ONLY: error
!
! Function type
  INTEGER :: set_zero
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
!
! Executable Statements
!
! Toggle zero-bound option
  izero=1-izero
!
! Check for invalid standardization setting
  IF ((izero==0).AND.(istd==3)) THEN
     istd=0
     ifail=1
     CALL error ('set_zero',ifail)
  END IF
  set_zero=2
!
  RETURN
 END FUNCTION set_zero
!
!
!
 FUNCTION get_cv()
!
! Initialises settings for cross-validated analysis
!
! Function type
  INTEGER :: get_cv
!
! Executable Statements
!
! Set retroactive flag
  iretro=0
  nretro=1
  nt1=nt
  nret=1
  ntr=0
  nur=0
  get_cv=0
!
  RETURN
 END FUNCTION get_cv
!
!
!
 FUNCTION get_retro()
!
! Initialises settings for retroactive analysis
!
! Modules
  USE maths, ONLY: magnitude
  USE CPT_constants, ONLY: mnt
!
! Function type
  INTEGER :: get_retro
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
!
  CHARACTER(LEN=8) :: fmt ! - format statement -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC CEILING
  INTRINSIC REAL
  INTRINSIC MOD
!
! Executable Statements
!
! Calculate estimate of initial training period length
  IF (MOD(nt,2)==0) THEN
      nt1=nt/2
  ELSE
      nt1=nt/2+1
  END IF
!
! Prompt for initial training period length
  iretro=0
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') 'Retroactive forecasts'
  WRITE (UNIT=fmt,FMT='(A,I1,A)') '(A,I',magnitude(nt1),',A)'
1 WRITE (UNIT=*,FMT=fmt,ADVANCE='no') 'Length of initial training period (suggested value ',nt1,'): '
  READ (UNIT=*,FMT=*,ERR=1) nt1
  IF ((nt1<1).OR.(nt1>nt+1-mnt)) GOTO 1
  CALL check_nt1 (nt,lcw,nt1,ntr,ifail)
  IF (ifail/=0) GOTO 1
!
! Prompt for training period update interval
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') 'Retroactive forecasts'
2 WRITE (UNIT=*,FMT='(A)',ADVANCE='no') 'Training period update interval: '
  READ (UNIT=*,FMT=*,ERR=2) nretro
  IF (nretro<1) GOTO 2
  nret=CEILING((REAL(ntr)-0.5)/REAL(nretro))
  get_retro=0
!
  RETURN
 END FUNCTION get_retro
!
!
!
 SUBROUTINE check_nt1 (nt,lcw,nt1,ntr,ifail)
!
! Checks initial length of training period
!
! Modules
  USE CPT_constants, ONLY: mnt
  USE gui,           ONLY: yesno
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nt  ! - number of cases -
  INTEGER, INTENT(IN) :: lcw ! - length of cross-validation window -
!
! Input/output scalars
  INTEGER, INTENT(INOUT) :: nt1 ! - length of intial training period -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ntr   ! - number of cases in retroactive period -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Executable Statements
!
! Force nt1 to be at least lcw+mnt-1
  IF (nt1<lcw+mnt-1) THEN
     nt1=lcw+mnt-1
     WRITE (UNIT=*,FMT='(A,I1,A)') &
        'WARNING: Initial length of training period must be at least cross-validation window length + ',mnt-1,'.'
     WRITE (UNIT=*,FMT='(A)',ADVANCE='no') '         Initial length of training period has been reset to:'
     WRITE (UNIT=*,FMT=*) nt1
     WRITE (UNIT=*,FMT=*) 'OK (Y/N)?'
     ifail=1-yesno()
  ELSE
     ntr=nt-nt1
     nret=CEILING((REAL(ntr)-0.5)/REAL(nretro))
     iretro=1
     ifail=0
  END IF
!
  RETURN
 END SUBROUTINE check_nt1
!
!
!
 FUNCTION check_n(n,lcw,iretro,ifail)
!
! Checks that for sufficient number of cases and that number of cases is sufficiently more than length of cross-validation window.
!
! On exit:
!    check_n = 0 All checks passed
!    check_n = 1 n < mnt, and iretro=0
!    check_n = 2 n < 2*mnt, and iretro=1
!    check_n = 3 lcw > n-3, and iretro=0; lcw reset to n-3
!    check_n = 4 lcw > n-mnt*2, and iretro=1; lcw reset to n-mnt*2
!
! Modules
  USE CPT_constants, ONLY: mnt
!
! Function type
  INTEGER :: check_n
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n      ! - number of cases -
  INTEGER, INTENT(IN) :: iretro ! - retroactive flag -
!
! Input/output scalars
  INTEGER, INTENT(INOUT) :: lcw    ! - length of cross-validation window -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error flag -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MOD
!
! Executable Statements
!
! Determine whether there are sufficient data
  SELECT CASE (iretro)
   CASE (0) ! - cross-validation -
     IF (n<mnt) THEN
        ifail=mnt
        check_n=1
        RETURN
     END IF
   CASE (1) ! - retroactive -
     IF (n<2*mnt-1) THEN
        ifail=2*mnt-1
        check_n=2
        RETURN
     END IF
  END SELECT
!
! Force lcw to leave at least 3
  SELECT CASE (iretro)
   CASE (0) ! - cross-validation -
     IF (lcw>n-(mnt-1)) THEN
        lcw=n-(mnt-1)
        IF (MOD(lcw,2)==0) lcw=lcw-1
        ifail=0
        check_n=record_change()
        check_n=3
        RETURN
     END IF
   CASE (1) ! - retroactive -
     IF (lcw>n-2*(mnt-1)) THEN
        lcw=n-2*(mnt-1)
        IF (MOD(lcw,2)==0) lcw=lcw-1
        ifail=2*mnt-1
        check_n=record_change()
        check_n=4
        RETURN
     END IF
  END SELECT
  check_n=0
!
  RETURN
 END FUNCTION check_n
!
!
!
 FUNCTION set_nused()
!
! Adjusts indices based on data availability
!
! Modules
  USE arrays,        ONLY: kuse
  USE CPT_constants, ONLY: mnt
!
! Function type
  INTEGER :: set_nused
!
! Locals
!
! Local scalars
  INTEGER :: k  ! - case index -
  INTEGER :: kk ! - available case index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC COUNT
!
! Executable Statements
!
! Check retroactive settings
  IF (iretro==1) THEN
     nu1=nt1-COUNT(.NOT.kuse(1:nt1))
     IF (nu1>=lcw+mnt-1) THEN
        set_nused=0
     ELSE
        nu1=lcw+mnt-1
        kk=0
        DO k=1,nt
           IF (kuse(k)) THEN
              kk=kk+1
              IF (kk==nu1) THEN
                 nt1=k
                 EXIT
              END IF
           END IF
        END DO
        set_nused=1
     END IF
     nur=nu-nu1
  ELSE
     nu1=0
     set_nused=0
  END IF
!
  RETURN
 END FUNCTION set_nused
!
!
!
 SUBROUTINE check_it1 (isq,sdate,fdate,it1,ifail)
!
! Checks that first date is available.
!
! On exit:
!    ifail = 0 All checks passed
!    ifail = 1 fdate < sdate, fdate reset to sdate
!
! Modules
  USE time
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: isq ! - sequence -
!
  TYPE(date), INTENT(IN) :: sdate ! - first date available -
!
! Input/output scalars
  TYPE(date), INTENT(INOUT) :: fdate ! - first date of interest -
!
! Output scalars
  INTEGER, INTENT(OUT) :: it1   ! - index of first available date -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Executable Statements
!
! Force first date of interest not to be before first date available
  IF (fdate<sdate) THEN
     fdate=sdate
     ifail=1
  ELSE
     it1=date_diff(sdate,fdate,isq)+1
     ifail=0
  END IF
!
  RETURN
 END SUBROUTINE check_it1
!
!
!
 SUBROUTINE check_yr (it1,itf,ifail)
!
! Checks that first year is available
!
! On exit:
!    ifail = 0 All checks passed
!    ifail = 1 itf < it1, itf reset to it1
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: it1 ! - first year available -
!
! Input/output scalars
  INTEGER, INTENT(INOUT) :: itf ! - first year of interest -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error flag -
!
! Executable Statements
!
! Force first year of interest not to be before first year available
  IF (itf<it1) THEN
     itf=it1
     ifail=1
  ELSE
     ifail=0
  END IF
!
  RETURN
 END SUBROUTINE check_yr
!
!
!
 SUBROUTINE check_lag (xfdate,yfdate,xmdate,it1,isq,ifail)
!
! Warns of negative lags
!
! On exit:
!    ifail = 0 All checks passed
!    ifail = 1 negative lag
!    ifail = 2 large lag
!    ifail = 3 forecast and target periods do not match
!
! Modules
  USE time
  USE time_constants, ONLY: nmn,mdy
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: it1  ! - index of first X date of interest -
  INTEGER, INTENT(IN) :: isq ! - time sequencing -
!
  TYPE(date), INTENT(IN) :: xfdate ! - first X date of interest -
  TYPE(date), INTENT(IN) :: yfdate ! - first Y date of interest -
  TYPE(date), INTENT(IN) :: xmdate ! - first X model date -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
  INTEGER :: lag1 ! - lead-time -
  INTEGER :: lag2 ! - target period offset -
!
! Executable Statements
!
! Calculate lag
  IF (xmdate==0) THEN
     lag1=date_diff(xfdate,yfdate,isq)
     lag2=0
     lag=lag1
  ELSE
     lag1=date_diff(xmdate+(it1-1),yfdate,isq)
     lag2=date_diff(xfdate,yfdate,isq)
     lag=lag2
  END IF
!
! Check for negative lags
  IF (lag1<0) THEN
     ifail=1
!
! Check for large lags
  ELSE
     ifail=0
     SELECT CASE (isq)
      CASE (1) ! - yearly -
        IF (lag1>0) ifail=2
        IF (xmdate==0) THEN
           lag=yfdate%iyr-xfdate%iyr
        ELSE
           lag=yfdate%iyr-(xmdate%iyr+it1-1)
        END IF
      CASE (2) ! - monthly -
        IF (lag1>nmn) ifail=2
      CASE (3) ! - daily -
        IF (lag1>mdy) ifail=2
     END SELECT
  END IF
!
! Check for offset
  IF (ifail==0) THEN
     IF (lag2/=0) ifail=3
  END IF
!
  RETURN
 END SUBROUTINE check_lag
END MODULE settings
