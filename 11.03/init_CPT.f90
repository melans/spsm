!  $Id: init_CPT.f90 1245 2011-03-04 21:14:47Z simon $
SUBROUTINE init_cpt ()
!
! Modules
  USE analysis, ONLY: ianal
  USE errors,   ONLY: error,init_errors
  USE gui,      ONLY: init_chars
  USE iofiles,  ONLY: init_iofiles
  USE labels,   ONLY: init_labels
  USE maths,    ONLY: init_numbers
  USE user,     ONLY: get_user
  USE time,     ONLY: init_time
  USE version,  ONLY: cver,ver
!
! Implicit declarations
  IMPLICIT NONE
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! External routines
  EXTERNAL init_sets
!
! Executable Statements
!
! Initialise version
  WRITE (cver,'(A,F5.2)') 'Climate Predictability Tool, v. ',ver
!
! Initialise time
  CALL init_time ()
!
! Initialise numbers
  CALL init_numbers ()
!
! Initialise labels
  CALL init_labels (1,ifail)
  IF (ifail/=0) THEN
     CALL init_errors ()
     CALL error ('init_labels',ifail)
     STOP
  END IF
!
! Identify initialization file ("CPT.ini") 
  CALL get_user ()
!
! Initialise characters
  CALL init_chars ()
!
! Initialise settings
  ianal=0
  CALL init_sets ()
!
! Initialise output file descriptions
  CALL init_iofiles ()
!
  RETURN
END SUBROUTINE init_cpt
!
!
!
SUBROUTINE init_sets ()
!
! Modules
  USE analysis,      ONLY: iaction,ianal, &
                           analysis_flags
  USE arrays,        ONLY: init_arrays
  USE categories,    ONLY: iclim, &
                           init_climate,init_analogues
  USE errors,        ONLY: error
  USE fields,        ONLY: xfield,yfield,zfield,ifdx,ilfx,iffx,ifdy,ilfy,iffy,ilaty,ilngy, &
                           init_field,init_fields
  USE gui,           ONLY: cwtitle,irv,jcca,jpcr,jmlr,jgcm,jgauss
  USE iofiles,       ONLY: xfile,yfile,zfile, &
                           init_ifile,init_ofiles
  USE labels,        ONLY: cg_atypes_t
  USE missing,       ONLY: xmiss,ymiss
  USE pcs,           ONLY: init_pcs
  USE settings,      ONLY: ifcast,igauss,izero,isem,ipval,iodds,nf,nt,nt_old,nt1,nretro,nx,ny,nz,&
                           mxa,mya,mza,lnew
  USE statistics,    ONLY: iskills
  USE user,          ONLY: CPT_ini, &
                           read_ini
  USE version,       ONLY: cver
!
! Implicit declarations
  IMPLICIT NONE
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC TRIM
!
! Executable Statements
!
! Read customised defaults
  CALL read_ini (TRIM(CPT_ini))
!
! Array pointers
  CALL init_arrays ()
!
! Field pointers
  CALL init_fields ()
!
! X input file and field settings
  nx=0
  mxa=0
  CALL init_ifile (xfile)
  CALL init_field (xfield,0,(/xmiss/),ifail)
  IF (ifail/=0) GOTO 1
  xfile%igrid=0
  ifdx=1
  ilfx=1
  iffx=1
!
! Y input file and field settings
  ny=0
  mya=0
  CALL init_ifile (yfile)
  CALL init_field (yfield,0,(/ymiss/),ifail)
  IF (ifail/=0) GOTO 1
  yfile%igrid=0
  ifdy=1
  ilfy=1
  iffy=1
  ilaty=1
  ilngy=1
!
! Reset length of training period
  nt=0
  nt_old=0
!
! Forecasts data file and field settings
  nz=0
  mza=0
  nf=1
  CALL init_ifile (zfile)
  CALL init_field (zfield,0,(/xmiss/),ifail)
  IF (ifail/=0) GOTO 1
  zfile%igrid=0
!
! Initialise analysis settings
  CALL analysis_flags ('off')
  iaction=0
  ifcast=0
!
! Retroactive forecast settings
  nretro=1
  nt1=0
!
! Reset output-file settings
  CALL init_ofiles ()
!
! Current EOFs
  CALL init_pcs ()
!
! Score selections
  iskills(1)=1
  iskills(2:)=0
!
! Switch off climatological period
  iclim=0
  CALL init_climate ()
!
! Switch off analogue years
  CALL init_analogues ()
!
! Set retroactive verification flag
  irv=0
!
! Set transformation flag
  igauss=0
!
! Set zero-bound flag
  izero=0
!
! Set sort ensemble members flag
  isem=1
!
! Initialise odds flag
  iodds=0
!
! Initialise p-value calculation
  ipval=0
!
! Set view flags
  SELECT CASE (ianal)
   CASE (1)
     jcca=0
     jpcr=1
     jmlr=1
     jgcm=1
     jgauss=1
   CASE (2)
     jcca=1
     jpcr=0
     jmlr=1
     jgcm=1
     jgauss=1
   CASE (3)
     jcca=1
     jpcr=1
     jmlr=0
     jgcm=1
     jgauss=1
   CASE (4)
     jcca=1
     jpcr=1
     jmlr=1
     jgcm=0
     jgauss=0
   CASE DEFAULT
     jcca=1
     jpcr=1
     jmlr=1
     jgcm=1
     jgauss=1
  END SELECT
!
! New settings flag
  lnew=.true.
!
! Reset window caption
  SELECT CASE (ianal)
   CASE (0)
     cwtitle=cver
   CASE DEFAULT
     cwtitle=cver//' - '//cg_atypes_t(ianal)
  END SELECT
  RETURN
!
! Errors
1 CALL error ('init_sets',ifail)
  STOP
!
  RETURN
END SUBROUTINE init_sets
