MODULE fields
!
! Modules
  USE IO_constants, ONLY: ldat,lstn,lstr,lvar,lprd
  USE numbers,      ONLY: rp
  USE time
!
! Implicit declarations
  IMPLICIT NONE
!
! Derived type definitions
!
! - area -
  TYPE area
     REAL(KIND=rp) :: rltn ! - northern area limit -
     REAL(KIND=rp) :: rlts ! - southern area limit -
     REAL(KIND=rp) :: rlgw ! - western area limit -
     REAL(KIND=rp) :: rlge ! - eastern area limit -
  END TYPE area
!
! - domain -
  TYPE domain
     INTEGER :: nlts ! - number of latitudes in domain -
     INTEGER :: nlgs ! - number of longitudes in domain -
     INTEGER :: nlt1 ! - northern latitude domain limit index -
     INTEGER :: nlt2 ! - southern latitude domain limit index -
     INTEGER :: nlg1 ! - western longitude domain limit index -
     INTEGER :: nlg2 ! - eastern longitude domain limit index -
!
     TYPE(area) :: alim ! - area limits -
  END TYPE domain
!
! - level -
  TYPE level
     REAL(KIND=rp) :: hght    ! - height -
!
     CHARACTER(LEN=5) :: unit ! - units -
  END TYPE level
!
! - field -
  TYPE field
     INTEGER :: nlt               ! - total number of latitudes -
     INTEGER :: nlg               ! - total number of longitudes -
     INTEGER :: nv                ! - total number of variables -
     INTEGER :: nva               ! - total number of used variables -
     INTEGER :: member            ! - ensemble member -
!
     REAL(KIND=rp) :: rmiss       ! - missing value -
!
     CHARACTER(LEN= lvar) :: var  ! - field variable -
     CHARACTER(LEN= lvar) :: unit ! - field units -
     CHARACTER(LEN= lprd-4) :: cssn ! - field season -
!
     LOGICAL :: ln2s              ! - north to south latitudes flag -
!
     TYPE( level) :: z            ! - atmospheric level -
     TYPE(period) :: tdate        ! - target period -
     TYPE(  date) :: mdate        ! - date made ('start date' for model forecasts in Data Library) -
     TYPE(domain) :: region       ! - domain of interest -
  END TYPE field
!
! - old field -
  TYPE field_v10
     INTEGER :: nlt               ! - total number of latitudes -
     INTEGER :: nlg               ! - total number of longitudes -
     INTEGER :: nv                ! - total number of variables -
     INTEGER :: nva               ! - total number of used variables -
     INTEGER :: member            ! - ensemble member -
!
     CHARACTER(LEN= lvar) :: var  ! - field variable -
     CHARACTER(LEN= lvar) :: unit ! - field units -
     CHARACTER(LEN= lprd-4) :: cssn ! - field season -
!
     LOGICAL :: ls2n              ! - south to north (opposite of v11) latitudes flag -
!
     TYPE( level) :: z            ! - atmospheric level -
     TYPE(period) :: tdate        ! - period of first data -
     TYPE(  date) :: mdate        ! - date made ('start date' for model forecasts in Data Library) -
     TYPE(domain) :: region       ! - domain of interest -
  END TYPE field_v10
!
! Arrays
!
! Integer arrays
  INTEGER, POINTER, PUBLIC :: idomx(:,:) ! - X variables within domain -
  INTEGER, POINTER, PUBLIC :: idomy(:,:) ! - Y variables within domain -
  INTEGER, POINTER, PUBLIC :: idomz(:,:) ! - Z variables within domain -
!
! Real arrays
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: rlatn(:)  ! - nearest latitudes -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: rlngn(:)  ! - nearest longitudes -
!
  REAL(KIND=rp), POINTER, PUBLIC :: rlatx(:,:)  ! - X latitudes -
  REAL(KIND=rp), POINTER, PUBLIC :: rlaty(:,:)  ! - Y latitudes -
  REAL(KIND=rp), POINTER, PUBLIC :: rlatz(:,:)  ! - Z latitudes -
  REAL(KIND=rp), POINTER, PUBLIC :: rlngx(:,:)  ! - X longitudes -
  REAL(KIND=rp), POINTER, PUBLIC :: rlngy(:,:)  ! - Y longitudes -
  REAL(KIND=rp), POINTER, PUBLIC :: rlngz(:,:)  ! - Z longitudes -
  REAL(KIND=rp), POINTER, PUBLIC :: rlatdx(:,:) ! - X latitudes within domain -
  REAL(KIND=rp), POINTER, PUBLIC :: rlatdy(:,:) ! - Y latitudes within domain -
  REAL(KIND=rp), POINTER, PUBLIC :: rlatdz(:,:) ! - Z latitudes within domain -
  REAL(KIND=rp), POINTER, PUBLIC :: rlngdx(:,:) ! - X longitudes within domain -
  REAL(KIND=rp), POINTER, PUBLIC :: rlngdy(:,:) ! - Y longitudes within domain -
  REAL(KIND=rp), POINTER, PUBLIC :: rlngdz(:,:) ! - Z longitudes within domain-
!
! Character arrays
  CHARACTER(LEN=lstn), POINTER, PUBLIC :: cstnx(:,:)  ! - names of X stations -
  CHARACTER(LEN=lstn), POINTER, PUBLIC :: cstny(:,:)  ! - names of Y stations -
  CHARACTER(LEN=lstn), POINTER, PUBLIC :: cstnz(:,:)  ! - names of Z stations -
  CHARACTER(LEN=lstn), POINTER, PUBLIC :: cstndx(:,:) ! - names of X stations within domain -
  CHARACTER(LEN=lstn), POINTER, PUBLIC :: cstndy(:,:) ! - names of Y stations within domain -
  CHARACTER(LEN=lstn), POINTER, PUBLIC :: cstndz(:,:) ! - names of Z stations within domain -
!
! Derived type arrays
  TYPE(field), POINTER, PUBLIC :: xfield(:)  ! - X fields -
  TYPE(field), POINTER, PUBLIC :: yfield(:)  ! - Y fields -
  TYPE(field), POINTER, PUBLIC :: zfield(:)  ! - Z fields -
  TYPE(field), POINTER, PUBLIC :: tfield(:)  ! - temporary fields -
  TYPE(field), POINTER, PUBLIC :: bkfield(:) ! - backup  -
!
! Scalars
!
! Current field
  INTEGER, PUBLIC :: ifdx  ! - current X field -
  INTEGER, PUBLIC :: ifdy  ! - current Y field -
  INTEGER, PUBLIC :: ilfx  ! - current X lagged field -
  INTEGER, PUBLIC :: ilfy  ! - current Y lagged field -
  INTEGER, PUBLIC :: iffx  ! - current Y field / lagged field -
  INTEGER, PUBLIC :: iffy  ! - current Y field / lagged field -
  INTEGER, PUBLIC :: ilaty ! - current latitude -
  INTEGER, PUBLIC :: ilngy ! - current longitude -
!
! Character scalars
  CHARACTER(LEN=  30), PUBLIC :: coor  ! - coordinates -
  CHARACTER(LEN=lstn), PUBLIC :: cstnc ! - current station label -
  CHARACTER(LEN=lstr), PUBLIC :: dsdy  ! - Y data structure item -
!
  CHARACTER(LEN=  30), PRIVATE :: y_coors ! - Y-grid coordinates -
!
! Derived type scalars
  TYPE(area), PUBLIC :: xarea     ! - default X area -
  TYPE(area), PUBLIC :: yarea     ! - default Y area -
  TYPE(area), PUBLIC :: xarea_old ! - backup default X area -
  TYPE(area), PUBLIC :: yarea_old ! - backup default Y area -
!
! Interfaces
!
! Interface assignments
  INTERFACE ASSIGNMENT(=)
     MODULE PROCEDURE assign_area
  END INTERFACE
!
  INTERFACE ASSIGNMENT(=)
     MODULE PROCEDURE assign_domain
  END INTERFACE
!
! Interface Operators
  INTERFACE OPERATOR(/=)
     MODULE PROCEDURE areas_differ
  END INTERFACE
  INTERFACE OPERATOR(/=)
     MODULE PROCEDURE domains_differ
  END INTERFACE
  INTERFACE OPERATOR(/=)
     MODULE PROCEDURE fields_differ
  END INTERFACE
!
CONTAINS
!
!
 SUBROUTINE assign_area (a1,r)
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: r ! - assignment value -
!
! Output scalars
  TYPE(area), INTENT(OUT) :: a1 ! - area -
!
! Executable Statements
!
! Assign area
  a1%rltn=r ! - northern area limit -
  a1%rlts=r ! - southern area limit -
  a1%rlgw=r ! - western area limit -
  a1%rlge=r ! - eastern area limit -
!
  RETURN
 END SUBROUTINE assign_area
!
!
!
 SUBROUTINE assign_domain (d1,i)
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: i ! - assignment value -
!
! Output scalars
  TYPE(domain), INTENT(OUT) :: d1 ! - domain -
!
! Executable Statements
!
! Assign domain
  d1%nlts=i ! - number of latitudes in domain -
  d1%nlgs=i ! - number of longitudes in domain -
  d1%nlt1=i ! - northern latitude domain limit index -
  d1%nlt2=i ! - southern latitude domain limit index -
  d1%nlg1=i ! - western longitude domain limit index -
  d1%nlg2=i ! - eastern longitude domain limit index -
!
  d1%alim=REAL(i,KIND=rp) ! - area limits -
!
  RETURN
 END SUBROUTINE assign_domain
!
!
!
 FUNCTION areas_differ(aarea,barea)
!
! Function type
  LOGICAL :: areas_differ
!
! Arguments
!
! Input scalars
  TYPE(area), INTENT(IN) :: aarea ! - first area -
  TYPE(area), INTENT(IN) :: barea ! - second area -
!
! Executable Statements
!
! Compare area settings
  areas_differ=.true.
  IF (aarea%rltn/=barea%rltn) RETURN
  IF (aarea%rlts/=barea%rlts) RETURN
  IF (aarea%rlgw/=barea%rlgw) RETURN
  IF (aarea%rlge/=barea%rlge) RETURN
  areas_differ=.false.
!
  RETURN
 END FUNCTION areas_differ
!
!
!
 FUNCTION domains_differ(adomain,bdomain)
!
! Function type
  LOGICAL :: domains_differ
!
! Arguments
!
! Input scalars
  TYPE(domain), INTENT(IN) :: adomain ! - first domain -
  TYPE(domain), INTENT(IN) :: bdomain ! - second domain -
!
! Executable Statements
!
! Compare area settings
  domains_differ=.true.
  IF (adomain%nlts/=bdomain%nlts) RETURN
  IF (adomain%nlgs/=bdomain%nlgs) RETURN
  IF (adomain%nlt1/=bdomain%nlt1) RETURN
  IF (adomain%nlt2/=bdomain%nlt2) RETURN
  IF (adomain%nlg1/=bdomain%nlg1) RETURN
  IF (adomain%nlg2/=bdomain%nlg2) RETURN
  IF (adomain%alim/=bdomain%alim) RETURN
  domains_differ=.false.
!
  RETURN
 END FUNCTION domains_differ
!
!
!
 FUNCTION fields_differ(afield,bfield)
!
! Function type
  LOGICAL :: fields_differ
!
! Arguments
!
! Input scalars
  TYPE(field), INTENT(IN) :: afield(:) ! - first field -
  TYPE(field), INTENT(IN) :: bfield(:) ! - second field -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ANY
!
! Executable Statements
!
! Compare area settings
  fields_differ=.true.
  IF (ANY(afield(:)%nlt/=bfield(:)%nlt)) RETURN
  IF (ANY(afield(:)%nlg/=bfield(:)%nlg)) RETURN
  IF (ANY(afield(:)%nv/=bfield(:)%nv)) RETURN
  IF (ANY(afield(:)%var/=bfield(:)%var)) RETURN
  IF (ANY(afield(:)%unit/=bfield(:)%unit)) RETURN
  IF (ANY(afield(:)%cssn/=bfield(:)%cssn)) RETURN
  fields_differ=.false.
!
  RETURN
 END FUNCTION fields_differ
!
!
!
 SUBROUTINE init_fields ()
!
! Executable Statements
!
! Initialise pointers
  NULLIFY (idomx)
  NULLIFY (idomy)
  NULLIFY (idomz)
  NULLIFY (rlatx)
  NULLIFY (rlaty)
  NULLIFY (rlatz)
  NULLIFY (rlngx)
  NULLIFY (rlngy)
  NULLIFY (rlngz)
  NULLIFY (rlatdx)
  NULLIFY (rlatdy)
  NULLIFY (rlatdz)
  NULLIFY (rlngdx)
  NULLIFY (rlngdy)
  NULLIFY (rlngdz)
  NULLIFY (cstnx)
  NULLIFY (cstny)
  NULLIFY (cstnz)
  NULLIFY (cstndx)
  NULLIFY (cstndy)
  NULLIFY (cstndz)
  NULLIFY (xfield)
  NULLIFY (yfield)
  NULLIFY (zfield)
  NULLIFY (tfield)
  NULLIFY (bkfield)
!
  RETURN
 END SUBROUTINE init_fields
!
!
!
 SUBROUTINE init_field (afield,nfl,rmiss,ifail)
!
! Initialises a field
!
! Modules
  USE numbers, ONLY: zero
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nfl ! - number of fields / lagged fields -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: rmiss(:) ! - missing values flags -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Pointer arrays
  TYPE(field), POINTER :: afield(:) ! - input field -
!
! Locals
!
! Local scalars
  INTEGER :: l   ! - field index -
  INTEGER :: nfm ! - maximum number of fields -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ASSOCIATED
  INTRINSIC MAX
  INTRINSIC SIZE
!
! Executable Statements
!
! Initialise field
  IF (ASSOCIATED(afield)) THEN
     DEALLOCATE (afield)
     NULLIFY (afield)
  END IF
  nfm=MAX(1,nfl)
  ALLOCATE (afield(nfm),STAT=ifail)
  IF (ifail/=0) THEN
     ifail=1
     RETURN
  END IF
!
! Set field settings
  DO l=1,nfm
     afield(l)%nlt=2
     afield(l)%nlg=2
     afield(l)%nv=0
     afield(l)%nva=0
     afield(l)%member=0
!
     IF (SIZE(rmiss)==SIZE(afield)) THEN
        afield(l)%rmiss=rmiss(l)
     ELSE
        afield(l)%rmiss=rmiss(1)
     END IF
!
     afield(l)%var='undefined'
     afield(l)%unit='none'
     afield(l)%cssn='N/A'
!
!     afield(l)%tdate=0
!     afield(l)%tdate%sdate=0
     afield(l)%tdate%sdate%iyr=0
     afield(l)%tdate%sdate%imn=0
     afield(l)%tdate%sdate%idy=0
!     afield(l)%tdate%edate=0
     afield(l)%tdate%edate%iyr=0
     afield(l)%tdate%edate%imn=0
     afield(l)%tdate%edate%idy=0
!     afield(l)%mdate=0
     afield(l)%mdate%iyr=0
     afield(l)%mdate%imn=0
     afield(l)%mdate%idy=0
     afield(l)%z%hght=zero
     afield(l)%z%unit='none'
!
! Set initial domain limits
     afield(l)%region%alim=xarea
     afield(l)%region%nlt1=1
     afield(l)%region%nlt2=2
     afield(l)%region%nlts=2
     afield(l)%region%nlg1=1
     afield(l)%region%nlg2=2
     afield(l)%region%nlgs=2
     afield(l)%ln2s=.true.
  END DO
  ifail=0
!
  RETURN
 END SUBROUTINE init_field
!
!
!
 SUBROUTINE field_v10_to_v11 (afield_v10,afield_v11)
!
! Converts version 10 fields to version 11
!
! Arguments
!
! Input scalars
  TYPE(field_v10), INTENT(IN) :: afield_v10 ! - version 10 field -
!
! Output scalars
  TYPE(field), INTENT(OUT) :: afield_v11 ! - version 11 field -
!
! Executable Statements
!
! Convert field
  afield_v11%nlt=afield_v10%nlt        ! - total number of latitudes -
  afield_v11%nlg=afield_v10%nlg        ! - total number of longitudes -
  afield_v11%nv=afield_v10%nv          ! - total number of variables -
  afield_v11%nva=afield_v10%nva        ! - total number of used variables -
  afield_v11%member=afield_v10%member  ! - ensemble member -
  afield_v11%var=afield_v10%var        ! - field variable -
  afield_v11%unit=afield_v10%unit      ! - field units -
  afield_v11%cssn=afield_v10%cssn      ! - field season -
  afield_v11%ln2s=.NOT.afield_v10%ls2n ! - north to south latitudes flag -
  afield_v11%z=afield_v10%z            ! - atmospheric level -
  afield_v11%tdate=afield_v10%tdate    ! - target period -
  afield_v11%mdate=afield_v10%mdate    ! - date made -
  afield_v11%region=afield_v10%region  ! - domain of interest -
!
  RETURN
 END SUBROUTINE field_v10_to_v11
!
!
!
 RECURSIVE SUBROUTINE reset_grids (rlat,rlng,cstn,rlatd,rlngd,cstnd,idom,ifail, &
                                  igrid,nfl,nlt,nlg)
!
! Arguments
!
! Input scalars
! - optional input scalars -
  INTEGER, INTENT(IN), OPTIONAL :: igrid ! - data structure -
  INTEGER, INTENT(IN), OPTIONAL :: nfl   ! - number of fields / lagged fields -
!
! Input arrays
  INTEGER, INTENT(IN), OPTIONAL :: nlt(:) ! - number of latitudes -
  INTEGER, INTENT(IN), OPTIONAL :: nlg(:) ! - number of longitudes -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Pointer arrays
  INTEGER, POINTER :: idom(:,:) ! - variables within domain -
!
  REAL(KIND=rp), POINTER :: rlat(:,:)  ! - latitudes -
  REAL(KIND=rp), POINTER :: rlng(:,:)  ! - longitudes -
  REAL(KIND=rp), POINTER :: rlatd(:,:) ! - longitudes within domain -
  REAL(KIND=rp), POINTER :: rlngd(:,:) ! - longitudes within domain -
!
  CHARACTER(LEN=lstn), POINTER :: cstn(:,:)  ! - station labels -
  CHARACTER(LEN=lstn), POINTER :: cstnd(:,:) ! - station labels within domain -
!
! Locals
!
! Local scalars
  INTEGER :: ios ! - memory allocation status -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ASSOCIATED
  INTRINSIC MAX
  INTRINSIC MAXVAL
  INTRINSIC PRESENT
!
! Executable Statements
!
! Clear pointers
  IF (ASSOCIATED(idom)) THEN
     DEALLOCATE (idom)
     NULLIFY (idom)
  END IF
  IF (ASSOCIATED(cstnd)) THEN
     DEALLOCATE (cstnd)
     NULLIFY (cstnd)
  END IF
  IF (ASSOCIATED(rlngd)) THEN
     DEALLOCATE (rlngd)
     NULLIFY (rlngd)
  END IF
  IF (ASSOCIATED(rlatd)) THEN
     DEALLOCATE (rlatd)
     NULLIFY (rlatd)
  END IF
  IF (ASSOCIATED(cstn)) THEN
     DEALLOCATE (cstn)
     NULLIFY (cstn)
  END IF
  IF (ASSOCIATED(rlng)) THEN
     DEALLOCATE (rlng)
     NULLIFY (rlng)
  END IF
  IF (ASSOCIATED(rlat)) THEN
     DEALLOCATE (rlat)
     NULLIFY (rlat)
  END IF
!
! Reset
  IF (PRESENT(igrid)) THEN
     IF ((.NOT.PRESENT(nfl)).OR.(.NOT.(PRESENT(nlt)))) THEN
        ifail=1
        RETURN
     END IF
     SELECT CASE (igrid)
      CASE (1) ! - gridded data -
        IF (.NOT.PRESENT(nlg)) THEN
           ifail=1
           RETURN
        END IF
        ALLOCATE (rlat(MAXVAL(nlt(:)),nfl),STAT=ios)
        IF (ios/=0) GOTO 1
        ALLOCATE (rlng(MAXVAL(nlg(:)),nfl),STAT=ios)
        IF (ios/=0) GOTO 1
        ALLOCATE (rlatd(MAX(2,MAXVAL(nlt(:))),nfl),STAT=ios)
        IF (ios/=0) GOTO 1
        ALLOCATE (rlngd(MAX(2,MAXVAL(nlg(:))),nfl),STAT=ios)
        IF (ios/=0) GOTO 1
        ALLOCATE (idom(MAXVAL(nlg(:)),nfl),STAT=ios)
        IF (ios/=0) GOTO 1
      CASE (2) ! - station data -
        ALLOCATE (rlat(MAXVAL(nlt(:)),nfl),STAT=ios)
        IF (ios/=0) GOTO 1
        ALLOCATE (rlng(MAXVAL(nlt(:)),nfl),STAT=ios)
        IF (ios/=0) GOTO 1
        ALLOCATE (cstn(MAXVAL(nlt(:)),nfl),STAT=ios)
        IF (ios/=0) GOTO 1
        ALLOCATE (rlatd(MAXVAL(nlt(:)),nfl),STAT=ios)
        IF (ios/=0) GOTO 1
        ALLOCATE (rlngd(MAXVAL(nlt(:)),nfl),STAT=ios)
        IF (ios/=0) GOTO 1
        ALLOCATE (cstnd(MAXVAL(nlt(:)),nfl),STAT=ios)
        IF (ios/=0) GOTO 1
        ALLOCATE (idom(MAXVAL(nlt(:)),nfl),STAT=ios)
        IF (ios/=0) GOTO 1
      CASE (3) ! - unreferenced data -
        ALLOCATE (cstn(MAXVAL(nlt(:)),nfl),STAT=ios)
        IF (ios/=0) GOTO 1
        ALLOCATE (cstnd(MAXVAL(nlt(:)),nfl),STAT=ios)
        IF (ios/=0) GOTO 1
        ALLOCATE (idom(MAXVAL(nlt(:)),nfl),STAT=ios)
        IF (ios/=0) GOTO 1
     END SELECT
  END IF
!
  ifail=0
  RETURN
!
! Errors
1 CALL reset_grids(rlat,rlng,cstn,rlatd,rlngd,cstnd,idom,ifail)
  ifail=1
!
  RETURN
 END SUBROUTINE reset_grids
!
!
!
 SUBROUTINE update_grid (ivf,iff,igrid)
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: ivf   ! - index by field -
  INTEGER, INTENT(IN) :: iff   ! - field / lagged field index -
  INTEGER, INTENT(IN) :: igrid ! - field structure -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC TRIM
!
! Executable Statements
!
! Construct and add coordinate label for initial point
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(2A)',ADVANCE='no') TRIM(dsdy),': '
  SELECT CASE (igrid)
   CASE (1)
     y_coors=get_grid_coors()
     WRITE (UNIT=*,FMT='(A)') y_coors
   CASE (2)
     y_coors=get_stn_coors()
     cstnc=cstny(ivf,iff)
     WRITE (UNIT=*,FMT='(3A)') TRIM(cstnc),'  ',TRIM(y_coors)
   CASE (3)
     cstnc=cstny(ivf,iff)
     WRITE (UNIT=*,FMT='(A)') TRIM(cstnc)
  END SELECT
!
  RETURN
 END SUBROUTINE update_grid
!
!
!
 FUNCTION get_xarea()
!
! Modules
  USE iofiles,  ONLY: xfile,zfile
  USE settings, ONLY: nx,nz
!
! Function type
  INTEGER :: get_xarea
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
!
! Executable Statements
!
! Prompt for domain
  CALL get_area ('X',xfile%igrid,xfile%nfs,xfile%nls,xfield,rlatx,rlngx,cstnx,nx,rlatdx,rlngdx,cstndx,idomx,ifail)
  SELECT CASE (ifail)
   CASE (0)
     IF (zfile%lset) THEN
        zfield(:)%region=xfield(:)%region
        zfield(:)%nlt=xfield(:)%nlt
        zfield(:)%nlg=xfield(:)%nlg
        zfield(:)%nv=xfield(:)%nv
        zfield(:)%nva=xfield(:)%nva
        nz=nx
     END IF
     get_xarea=2
   CASE (1)
     get_xarea=3
  END SELECT
!
  RETURN
 END FUNCTION get_xarea
!
!
!
 FUNCTION get_yarea()
!
! Modules
  USE iofiles,  ONLY: yfile
  USE settings, ONLY: ny
!
! Function type
  INTEGER :: get_yarea
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
!
! Executable Statements
!
! Prompt for domain
  CALL get_area ('Y',yfile%igrid,yfile%nfs,yfile%nls,yfield,rlaty,rlngy,cstny,ny,rlatdy,rlngdy,cstndy,idomy,ifail)
  SELECT CASE (ifail)
   CASE (0)
     get_yarea=2
   CASE (1)
     get_yarea=3
  END SELECT
!
  RETURN
 END FUNCTION get_yarea
!
!
!
 SUBROUTINE get_area (cxy,igrid,nfs,nls,afield,rlat,rlng,cstn,nv,rlatd,rlngd,cstnd,idom,ifail)
!
! Prompts for area of interest and identifies domain indices
!
! Modules
  USE errors,   ONLY: error
  USE maths,    ONLY: get_cordn
  USE numbers,  ONLY: r90,r360
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: igrid ! - field structure -
  INTEGER, INTENT(IN) :: nfs   ! - number of fields -
  INTEGER, INTENT(IN) :: nls   ! - number of lagged fields -
!
  CHARACTER(LEN=1), INTENT(IN) :: cxy ! - X or Y variables flag -
!
! Output scalars
  INTEGER, INTENT(OUT) :: nv    ! - total number of variables -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, POINTER :: idom(:,:) ! - stations within domain -
!
  REAL(KIND=rp), POINTER :: rlat(:,:) ! - latitudes -
  REAL(KIND=rp), POINTER :: rlng(:,:) ! - longitudes -
!
  CHARACTER(LEN=*), POINTER :: cstn(:,:)  ! - station labels -
!
! Input/output arrays
  TYPE(field), POINTER :: afield(:) ! - field -
!
! Output arrays
  REAL(KIND=rp), POINTER :: rlatd(:,:) ! - latitudes within domain -
  REAL(KIND=rp), POINTER :: rlngd(:,:) ! - longitudes within domain -
!
  CHARACTER(LEN=*), POINTER :: cstnd(:,:) ! - station labels within domain -
!
! Locals
!
! Local scalars
  INTEGER :: l   ! - current field / lagged field -
!
  INTEGER :: ifd ! - field index -
  INTEGER :: ilf ! - lagged field index -
!
  REAL(KIND=rp) :: rlatn ! - northernmost data latitude -
  REAL(KIND=rp) :: rlats ! - southernmost data latitude -
  REAL(KIND=rp) :: rlngw ! - westernmost data longitude -
  REAL(KIND=rp) :: rlnge ! - easternmost data longitude -
!
  TYPE(area) :: dlim ! - data limit -
!
  TYPE(field), POINTER :: tmfield(:) ! - backup  -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC CEILING
  INTRINSIC FLOOR
  INTRINSIC MAXVAL
  INTRINSIC MINVAL
  INTRINSIC REAL
  INTRINSIC SIZE
  INTRINSIC SUM
!
! Executable Statements
!
! Backup area settings
  NULLIFY (tmfield)
  CALL init_field (tmfield,SIZE(afield),afield(:)%rmiss,ifail)
  IF (ifail/=0) RETURN
  tmfield(:)=afield(:)
!
! Repeat for each field
  get_domains: DO ifd=1,nfs
     DO ilf=1,nls
        l=(ifd-1)*nls+ilf
!
! Calculate domain limits
        rlatn=MAXVAL(rlat(1:afield(l)%nlt,l))
        rlats=MINVAL(rlat(1:afield(l)%nlt,l))
        rlngw=MINVAL(rlng(1:afield(l)%nlg,l))
        rlnge=MAXVAL(rlng(1:afield(l)%nlg,l))
!
! Calculate data limits
        dlim%rltn=REAL(CEILING(rlatn),KIND=rp)
        dlim%rlts=REAL(FLOOR(rlats),KIND=rp)
        dlim%rlgw=REAL(FLOOR(rlngw),KIND=rp)
        dlim%rlge=REAL(CEILING(rlnge),KIND=rp)
!
! Prompt for area of interest
     afield(l)%nv=0
     DO   !FIXME Windows has more stuff
        WRITE (UNIT=*,FMT='(A)') cxy//' domain selection'
        IF (nfs>1) THEN
           WRITE (UNIT=*,FMT='(A,I2,4A)') 'Please specify domain limits the ',&
                  ifd,get_cordn(ifd),' field (',TRIM(afield(ifd)%var),') '
           WRITE (UNIT=*,FMT='(A)') '(approximate data limits in brackets):'
        ELSE
           WRITE (UNIT=*,FMT='(A)') 'Please specify domain limits (approximate data limits in brackets):'
        END IF
        WRITE (UNIT=*,FMT='(A)') '(southern latitudes and western longitudes negative)'
! - northernmost latitude -
1       WRITE (UNIT=*,FMT='(A,I3,A)',ADVANCE='no') 'Northernmost latitude (',CEILING(rlatn),'): '
        READ (UNIT=*,FMT=*,ERR=1,END=1) afield(l)%region%alim%rltn
        IF ((afield(l)%region%alim%rltn<-r90).OR.(afield(l)%region%alim%rltn>r90)) GOTO 1
! - southernmost latitude -
2       WRITE (UNIT=*,FMT='(A,I3,A)',ADVANCE='no') 'Southernmost latitude (',FLOOR(rlats),'): '
        READ (UNIT=*,FMT=*,ERR=2,END=2) afield(l)%region%alim%rlts
        IF ((afield(l)%region%alim%rlts<-r90).OR.(afield(l)%region%alim%rlts>r90)) GOTO 2
! - westernmost longitude -
3       WRITE (UNIT=*,FMT='(A,I4,A)',ADVANCE='no') 'Westernmost longitude (',FLOOR(rlngw),'): '
        READ (UNIT=*,FMT=*,ERR=3,END=3) afield(l)%region%alim%rlgw
        IF ((afield(l)%region%alim%rlgw<-r360).OR.(afield(l)%region%alim%rlgw>r360)) GOTO 3
! - easternmost longitude -
4       WRITE (UNIT=*,FMT='(A,I4,A)',ADVANCE='no') 'Easternmost longitude (',CEILING(rlnge),'): '
        READ (UNIT=*,FMT=*,ERR=4,END=4) afield(l)%region%alim%rlge
        IF ((afield(l)%region%alim%rlge<-r360).OR.(afield(l)%region%alim%rlge>r360)) GOTO 4
!
! Check domain
           SELECT CASE (igrid)
            CASE (1)
              CALL check_grid_domain (afield(l)%nlt,afield(l)%nlg,rlat(:,l),rlng(:,l),afield(l)%region,afield(l)%nv, &
                   rlatd(:,l),rlngd(:,l),idom(:,l),ifail)
            CASE (2)
              CALL check_stn_domain (afield(l)%nlt,rlatn,rlats,afield(l)%region%alim,rlat(:,l),rlng(:,l),cstn(:,l), &
                   afield(l)%nv,rlatd(:,l),rlngd(:,l),cstnd(:,l),idom(:,l),ifail)
                   afield(l)%region%nlts=afield(l)%nv
                   afield(l)%region%nlgs=afield(l)%nv
           END SELECT
           IF (ifail==0) EXIT
           CALL correct_domain (ifail,rlatn,rlats,rlngw,rlnge,afield(l)%region%alim)
           CALL error ('get_area',ifail)
        END DO
     END DO
  END DO get_domains
!
! Update number of variables
  IF (ifail==0) THEN
     nv=SUM(afield(:)%nv)
  ELSE
     afield(:)=tmfield(:)
  END IF
!
  RETURN
 END SUBROUTINE get_area
!
!
!
  SUBROUTINE correct_domain (ifail,rlatn,rlats,rlngw,rlnge,alim)
!
! Attempts to correct domain
!
! Modules
  USE numbers, ONLY: r90,r360
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: ifail ! - error indicator -
!
  REAL(KIND=rp), INTENT(IN) :: rlatn ! - northernmost data latitude -
  REAL(KIND=rp), INTENT(IN) :: rlats ! - southernmost data latitude -
  REAL(KIND=rp), INTENT(IN) :: rlngw ! - westernmost data longitude -
  REAL(KIND=rp), INTENT(IN) :: rlnge ! - easternmost data longitude -
!
! Output scalars
  TYPE(area), INTENT(OUT) :: alim ! - domain -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC CEILING
  INTRINSIC FLOOR
  INTRINSIC MAX
  INTRINSIC MIN
  INTRINSIC REAL
!
! Executable Statements
!
! Attempt to correct domain
  SELECT CASE (ifail)
   CASE (1) ! - northernmost latitude south of southernmost -
     IF (rlatn>rlats) THEN
        alim%rltn=REAL(CEILING(rlatn),KIND=rp)
        alim%rlts=REAL(FLOOR(rlats),KIND=rp)
     ELSE
        alim%rltn=MIN(REAL(CEILING(rlatn)+1,KIND=rp),r90)
        alim%rlts=MAX(REAL(FLOOR(rlats)-1,KIND=rp),-r90)
     END IF
   CASE (2) ! - northernmost latitude south of southern limit of map -
     IF (rlatn>rlats) THEN
        alim%rltn=REAL(CEILING(rlatn),KIND=rp)
     ELSE
        alim%rltn=MIN(REAL(CEILING(rlatn)+1,KIND=rp),r90)
     END IF
   CASE (3) ! - southernmost latitude north of northern limit of map -
     IF (rlats<rlatn) THEN
        alim%rlts=REAL(FLOOR(rlats),KIND=rp)
     ELSE
        alim%rlts=MAX(REAL(FLOOR(rlats)-1,KIND=rp),-r90)
     END IF
   CASE (4) ! - westernmost longitude out of range -
     IF (rlngw<rlnge) THEN
        alim%rlgw=REAL(FLOOR(rlngw),KIND=rp)
        alim%rlge=REAL(CEILING(rlnge),KIND=rp)
     ELSE
        alim%rlgw=MAX(REAL(FLOOR(rlngw)-1,KIND=rp),-r360)
        alim%rlge=MIN(REAL(CEILING(rlnge)+1,KIND=rp),r360)
     END IF
    CASE (5) ! - no data within domain -
     IF (rlngw<rlnge) THEN
        alim%rlgw=REAL(FLOOR(rlngw),KIND=rp)
        alim%rlge=REAL(CEILING(rlnge),KIND=rp)
     ELSE
        alim%rlgw=MAX(REAL(FLOOR(rlngw)-1,KIND=rp),-r360)
        alim%rlge=MIN(REAL(CEILING(rlnge)+1,KIND=rp),r360)
     END IF
  END SELECT
!
  RETURN
 END SUBROUTINE correct_domain
!
!
!
 SUBROUTINE set_rrlng (nfs,region,nlg,rlng,rrlng)
!
! Sets realigned longitudes
!
! Modules
  USE numbers, ONLY: r360
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nfs ! - number of fields -
!
! Input arrays
  INTEGER, INTENT(IN) :: nlg(:) ! - number of longitudes -
!
  REAL(KIND=rp), INTENT(IN) :: rlng(:,:) ! - longitudes -
!
  TYPE(domain), INTENT(IN) :: region(:) ! - domain settings -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: rrlng(:,:) ! - realigned longitudes -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - number of grids to end -
  INTEGER :: l ! - field index -
!
! Executable Statements
!
! Realign longitudes
  DO l=1,nfs
     IF (region(l)%nlg1<=region(l)%nlg2) THEN
        rrlng(1:region(l)%nlgs,l)=rlng(region(l)%nlg1:region(l)%nlg2,l)
     ELSE
        i=nlg(l)+1-region(l)%nlg1
        rrlng(1:i,l)=rlng(region(l)%nlg1:nlg(l),l)
        rrlng(i+1:region(l)%nlgs,l)=rlng(1:region(l)%nlg2,l)+r360
     END IF
!
! Shift if too far east
     IF (rrlng(region(l)%nlgs,l)>r360) rrlng(1:region(l)%nlgs,l)=rrlng(1:region(l)%nlgs,l)-r360
!
! Supply additional longitudes if only one is used
     IF (nlg(l)==1) THEN
        IF (region(l)%nlg1<nlg(l)) THEN
           rrlng(2,l)=rlng(region(l)%nlg1+1,l)
        ELSE
           rrlng(2,l)=2*rlng(nlg(l),l)-rlng(nlg(l)-1,l)
        END IF
     END IF
  END DO
!
  RETURN
 END SUBROUTINE set_rrlng
!
!
!
 SUBROUTINE check_grid_domain (nlt,nlg,rlat,rlng,region,nv,rlatd,rlngd,idom,ifail)
!
! Modules
  USE numbers, ONLY: zero,r180,r360
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nlt ! - number of latitudes -
  INTEGER, INTENT(IN) :: nlg ! - number of longitudes -
!
! Input/output derived types
  TYPE(domain), INTENT(INOUT) :: region ! - domain -
!
! Output scalars
  INTEGER, INTENT(OUT) :: nv    ! - number of grids -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: rlat(:) ! - latitudes -
  REAL(KIND=rp), INTENT(IN) :: rlng(:) ! - longitudes -
!
! Output arrays
  INTEGER, INTENT(OUT) :: idom(:) ! - gridpoints within domain -
!
  REAL(KIND=rp), INTENT(OUT) :: rlatd(:) ! - latitudes within domain -
  REAL(KIND=rp), INTENT(OUT) :: rlngd(:) ! - longitudes within domain -
!
! Locals
!
! Local scalars
  INTEGER :: i  ! - latitude index -
  INTEGER :: j  ! - longitude index -
  INTEGER :: ii ! - current latitude -
  INTEGER :: jj ! - current longitude -
!
  REAL(KIND=rp) :: rrlgw ! - rescaled western domain limit -
  REAL(KIND=rp) :: rrlge ! - rescaled eastern domain limit -
  REAL(KIND=rp) :: dlng  ! - longitudinal resolution -
  REAL(KIND=rp) :: dmap  ! - longitudinal map extent -
  REAL(KIND=rp) :: rwadj ! - western longitudinal shift -
  REAL(KIND=rp) :: readj ! - eastern longitudinal shift -
!
! Executable Statements
!
! Reset longitudes limits
  IF (region%alim%rlgw>=rlng(1).AND.region%alim%rlgw<=rlng(nlg)) THEN
     rwadj=zero
  ELSE IF (region%alim%rlgw-r360>=rlng(1).AND.region%alim%rlgw-r360<=rlng(nlg)) THEN
     rwadj=-r360
  ELSE IF (region%alim%rlgw+r360>=rlng(1).AND.region%alim%rlgw+r360<=rlng(nlg)) THEN
     rwadj=r360
  ELSE IF (region%alim%rlgw<rlng(1).AND.region%alim%rlge>=rlng(1)) THEN
     rwadj=zero
  ELSE
     ifail=5
     RETURN
  END IF
  rrlgw=region%alim%rlgw+rwadj
  readj=rwadj
  DO
     IF (.NOT.region%alim%rlge+readj<rrlgw) EXIT
     readj=readj+r360
  END DO
  rrlge=region%alim%rlge+readj
!
! Identify indices
! - northernmost -
  IF (region%alim%rltn<region%alim%rlts) THEN
     ifail=1
     RETURN
  END IF
  northernmost: DO i=1,nlt
     IF (.NOT.region%alim%rltn<rlat(i)) THEN
        region%nlt1=i
        EXIT northernmost
     END IF
  END DO northernmost
  IF ((i>nlt).OR.(i<1)) THEN
     ifail=2
     RETURN
  END IF
! - southernmost -
  IF (region%alim%rlts>rlat(region%nlt1)) THEN
     ifail=3
     RETURN
  ELSE
     southernmost: DO i=nlt,region%nlt1,-1
        IF (.NOT.region%alim%rlts>rlat(i)) THEN
           region%nlt2=i
           EXIT southernmost
        END IF
     END DO southernmost
  END IF
! - westernmost -
  region%nlg1=nlg
  westernmost: DO j=1,nlg
     IF (.NOT.rrlgw>rlng(j)) THEN
        region%nlg1=j
        EXIT westernmost
     END IF
  END DO westernmost
! - easternmost -
  IF (rrlge/=rrlgw.AND.rrlge-rrlgw<r360) THEN
     easternmost: DO
        DO j=region%nlg1+1,nlg
           IF (rrlge+readj<rlng(j)) THEN
              region%nlg2=j-1
              EXIT easternmost
           END IF
        END DO
        readj=readj-r360
        DO j=1,region%nlg1
           IF (rrlge+readj<rlng(j)) THEN
              IF (j>1) THEN
                 region%nlg2=j-1
              ELSE
                 region%nlg2=nlg
              END IF
              EXIT easternmost
           END IF
        END DO
     END DO easternmost
  ELSE IF (region%nlg1>1) THEN
     region%nlg2=region%nlg1-1
  ELSE
     region%nlg2=nlg
  END IF
!
! Calculate number of grids
  region%nlts=region%nlt2+1-region%nlt1
  IF (region%nlg2>=region%nlg1) THEN
     region%nlgs=region%nlg2+1-region%nlg1
  ELSE
     region%nlgs=region%nlg2+nlg+1-region%nlg1
  END IF
  IF (region%nlg1/=1) THEN
     dlng=rlng(region%nlg1)-rlng(region%nlg1-1)
  ELSE IF ((.NOT.region%alim%rlge<rlng(1)).AND.region%alim%rlge>region%alim%rlgw) THEN
     dlng=rlng(2)-rlng(1)
  ELSE
     dlng=r360+rlng(1)-rlng(nlg)
  END IF
  IF (region%alim%rlge<zero) THEN
     dmap=region%alim%rlgw-region%alim%rlge
     IF (dmap>zero) THEN
        dmap=r360-dmap
     ELSE
        dmap=-dmap
     END IF
  ELSE
     dmap=region%alim%rlge-region%alim%rlgw
  END IF
  IF (region%nlgs==nlg.AND.dlng>dmap.AND.dmap>zero) THEN
     ifail=4
     RETURN
  END IF
  nv=region%nlgs*region%nlts
!
! Determine which latitudes are within the domain
  ii=0
  DO i=region%nlt1,region%nlt2
     ii=ii+1
     rlatd(ii)=rlat(i)
  END DO
!
! Determine which longitudes are within the domain
  jj=0
  IF (region%nlg1<=region%nlg2) THEN
     DO j=region%nlg1,region%nlg2
        jj=jj+1
        idom(jj)=j
        rlngd(jj)=rlng(j)
     END DO
  ELSE
     DO j=region%nlg1,nlg
        jj=jj+1
        idom(jj)=j
        rlngd(jj)=rlng(j)
     END DO
     ii=jj
     DO j=1,region%nlg2
        jj=jj+1
        idom(jj)=j
        rlngd(jj)=rlng(j)
     END DO
     check_rlng: DO
        IF (ANY(rlngd(1:region%nlgs-1)>rlngd(2:region%nlgs))) THEN
           DO j=2,region%nlgs
              IF (rlngd(j-1)>rlngd(j)) THEN
                 rlngd(j:region%nlgs)=rlngd(j:region%nlgs)+r360
                 EXIT
              END IF
           END DO
        ELSE IF (ANY(rlngd(1:region%nlgs)>r360)) THEN
           rlngd(1:region%nlgs)=rlngd(1:region%nlgs)-r360
        ELSE
           EXIT check_rlng
        END IF
     END DO check_rlng
  END IF
  IF (ANY(rlngd(1:region%nlgs)<-r180)) rlngd(1:region%nlgs)=rlngd(1:region%nlgs)+r360
  ifail=0
!
  RETURN
 END SUBROUTINE check_grid_domain
!
!
!
 SUBROUTINE check_stn_domain (mst,rlatn,rlats,alim,rlat,rlng,cstn,nst,rlatd,rlngd,cstnd,idom,ifail)
!
! Modules
  USE numbers, ONLY: r360
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: mst ! - total number of stations -
!
  REAL(KIND=rp), INTENT(IN) :: rlatn ! - northernmost data latitude -
  REAL(KIND=rp), INTENT(IN) :: rlats ! - southernmost data latitude -
!
  TYPE(area), INTENT(IN) :: alim ! - area limits -
!
! Output scalars
  INTEGER, INTENT(OUT) :: nst   ! - number of stations within domain -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: rlat(:) ! - latitudes -
  REAL(KIND=rp), INTENT(IN) :: rlng(:) ! - longitudes -
!
  CHARACTER(LEN=*), INTENT(IN) :: cstn(:) ! - station labels -
!
! Output arrays
  INTEGER, INTENT(OUT) :: idom(:) ! - stations within domain -
!
  REAL(KIND=rp), INTENT(OUT) :: rlatd(:) ! - latitudes within domain -
  REAL(KIND=rp), INTENT(OUT) :: rlngd(:) ! - longitudes within domain -
!
  CHARACTER(LEN=*), INTENT(OUT) :: cstnd(:) ! - station labels within domain -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - latitude index -
!
  REAL(KIND=rp) :: rrlge ! - rescaled eastern domain limit -
!
! Executable Statements
!
! Identify stations within domain
  nst=0
  IF (alim%rlgw<alim%rlge) THEN
     rrlge=alim%rlge
  ELSE
     rrlge=alim%rlge+r360
  END IF
  DO i=1,mst
     IF ((rlat(i)<=alim%rltn).AND.(rlat(i)>=alim%rlts).AND. &
       (((rlng(i)>=alim%rlgw).AND.(rlng(i)<=rrlge)).OR. &
        ((rlng(i)+r360>=alim%rlgw).AND.(rlng(i)+r360<=rrlge)).OR. &
        ((rlng(i)-r360>=alim%rlgw).AND.(rlng(i)-r360<=rrlge)))) THEN
        nst=nst+1
        rlatd(nst)=rlat(i)
        rlngd(nst)=rlng(i)
        cstnd(nst)=cstn(i)
        idom(nst)=i
     END IF
  END DO
!
! Diagnose lack of stations
  IF (nst>0) THEN
     ifail=0
  ELSE
     IF (alim%rltn<alim%rlts) THEN
        ifail=1
     ELSE IF (alim%rltn<rlats) THEN
        ifail=2
     ELSE IF (alim%rlts>rlatn) THEN
        ifail=3
     ELSE
        ifail=4
     END IF
  END IF
!
  RETURN
 END SUBROUTINE check_stn_domain
!
!
!
 SUBROUTINE full_domain (afield,idom)
!
! Assigns field settings for a complete domain
!
! Arguments
!
! Input/output scalars
  TYPE(field), INTENT(INOUT) :: afield ! - field settings -
!
! Output arrays
  INTEGER, INTENT(OUT) :: idom(:) ! - variable is within domain -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - variable index -
!
! Executable Statements
!
! Set domain limits
  afield%region%nlts=afield%nlt ! - number of latitudes in domain -
  afield%region%nlgs=afield%nlg ! - number of longitudes in domain -
  afield%region%nlt1=1          ! - northern latitude domain limit index -
  afield%region%nlt2=afield%nlt ! - southern latitude domain limit index -
  afield%region%nlg1=1          ! - western longitude domain limit index -
  afield%region%nlg2=afield%nlg ! - eastern longitude domain limit index -
!
! Indicate used variables
  DO i=1,afield%nv
     idom(i)=i
  END DO
!
  RETURN
 END SUBROUTINE full_domain
!
!
!
 FUNCTION get_grid_coors()
!
! Modules
  USE settings, ONLY: ivf
!
! Function type
  CHARACTER(LEN=15) :: get_grid_coors
!
! Locals
!
! Local scalars
  INTEGER :: ilt ! - latitude index -
  INTEGER :: jlg ! - longitude index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MOD
!
  CHARACTER(LEN=15),EXTERNAL :: make_coors
!
! Executable Statements
!
! Identify grid
  jlg=MOD(ivf,yfield(iffy)%region%nlgs)
  ilt=ivf/yfield(iffy)%region%nlgs
  IF (jlg==0) THEN
     jlg=yfield(iffy)%region%nlgs
  ELSE
     ilt=ilt+1
  END IF
!
! Construct coordinate label
  get_grid_coors=make_coors(rlatdy(ilt,iffy),rlngdy(jlg,iffy))
!
  RETURN
 END FUNCTION get_grid_coors
!
!
!
 FUNCTION get_stn_coors()
!
! Modules
  USE settings, ONLY: ivf
!
! Function type
  CHARACTER(LEN=15) :: get_stn_coors
!
  CHARACTER(LEN=15),EXTERNAL :: make_coors
!
! Executable Statements
!
! Construct coordinate label
  get_stn_coors=make_coors(rlatdy(ivf,iffy),rlngdy(ivf,iffy))
!
  RETURN
 END FUNCTION get_stn_coors
!
!
!
 FUNCTION make_map_coor(rlat,rlng)
!
! Modules
  USE numbers, ONLY: zero,r180,r360
!
! Function type
  CHARACTER(LEN=18) :: make_map_coor
!
!  CHARACTER(LEN=15),EXTERNAL :: make_coors
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: rlat ! - latitude -
  REAL(KIND=rp), INTENT(IN) :: rlng ! - longitude -
!
! Locals
!
! Local scalars
  REAL(KIND=rp) :: rlt   ! - latitude -
  REAL(KIND=rp) :: rlg   ! - longitude -
  REAL(KIND=rp) :: rrlng ! - realigned longitude -
!
  CHARACTER(LEN=1) :: clt ! - latitude -
  CHARACTER(LEN=1) :: clg ! - longitude -
!
! Executable Statements
!
! Realign longitude if necessary
  rrlng=rlng
  DO
     IF (rrlng<-r180) THEN
        rrlng=rrlng+r360
     ELSE IF (rrlng>r360) THEN
        rrlng=rrlng-r360
     ELSE
        EXIT
     END IF
  END DO
!
! Construct coordinate label
! - latitudes -
  IF (rlat>zero) THEN
     rlt=rlat
     clt='N'
  ELSE IF (rlat<zero) THEN
     rlt=-rlat
     clt='S'
  ELSE
     rlt=zero
     clt=' '
  END IF
  IF (rrlng>zero) THEN
     IF (rrlng>r180) THEN
        rlg=r360-rrlng
        clg='W'
     ELSE IF (rrlng<r180) THEN
        rlg=rrlng
        clg='E'
     ELSE
        rlg=r180
        clg=' '
     END IF
  ELSE IF (rrlng<zero) THEN
     rlg=-rlng
     clg='W'
  ELSE
     rlg=zero
     clg=' '
  END IF
  WRITE (UNIT=make_map_coor,FMT='(2(F8.2,A))') rlt,clt,rlg,clg
!
  RETURN
 END FUNCTION make_map_coor
!
!
!
 SUBROUTINE latitude_weight (nfl,nlt,region,rlat,iuse,nt,v)
!
! Weights gridded data by cosine of latitude
!
! Modules
  USE numbers, ONLY: three,pi,r180,r360
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nfl ! - number of fields / lagged fields -
  INTEGER, INTENT(IN) :: nt  ! - number of cases -
!
! Input arrays
  INTEGER, INTENT(IN) :: nlt(:)  ! - numbers of latitudes per field / lagged field -
  INTEGER, INTENT(IN) :: iuse(:) ! - used variables indices -
!
  REAL(KIND=rp), INTENT(IN) :: rlat(:,:) ! - latitudes -
!
  TYPE(domain), INTENT(IN) :: region(:) ! - domain settings -
!
! Input/output arrays
  REAL(KIND=rp), INTENT(INOUT) :: v(:,:) ! - data -
!
! Locals
!
! Local scalars
  INTEGER :: i  ! - latitude index -
  INTEGER :: j  ! - longitude index -
  INTEGER :: ij ! - latitude/longitude index -
  INTEGER :: ii ! - available series index -
  INTEGER :: l  ! - field index -
!
  REAL(KIND=rp) :: r1 ! - first latitude limit -
  REAL(KIND=rp) :: r2 ! - second latitude limit -
  REAL(KIND=rp) :: wt ! - latitude weighting -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ABS
  INTRINSIC COS
  INTRINSIC SIN
  INTRINSIC SQRT
!
! Executable Statements
!
! Set increment
  ii=1
  ij=0
  DO l=1,nfl
!
! Weight by cosine of latitude
     SELECT CASE (nlt(l))
      CASE (1) ! - no weighting if only one line of latitude -
        CONTINUE
      CASE (2) ! - simple cosine weighting if two lines of latitude -
        DO i=region(l)%nlt1,region(l)%nlt2
           wt=COS(rlat(i,l)*pi/r180)
           wt=SQRT(wt)
           DO j=1,region(l)%nlgs
              ij=ij+1
              IF (iuse(ii)==ij) THEN
                 v(ii,1:nt)=v(ii,1:nt)*wt
                 ii=ii+1
              END IF
           END DO
        END DO
      CASE (3:) ! - integrated cosine weighting if multiple lines of latitude -
        DO i=region(l)%nlt1,region(l)%nlt2
           IF (i==1) THEN
              r1=(three*rlat(1,l)-rlat(2,l))*pi/r360
              r2=(rlat(1,l)+rlat(2,l))*pi/r360
           ELSE IF (i<nlt(l)) THEN
              r1=(rlat(i,l)+rlat(i-1,l))*pi/r360
              r2=(rlat(i,l)+rlat(i+1,l))*pi/r360
           ELSE
              r1=(rlat(nlt(l),l)+rlat(nlt(l)-1,l))*pi/r360
              r2=(three*rlat(nlt(l),l)-rlat(nlt(l)-1,l))*pi/r360
           END IF
           wt=(SIN(r1)-SIN(r2))/(r1-r2)
           wt=SQRT(ABS(wt))
           DO j=1,region(l)%nlgs
              ij=ij+1
              IF (iuse(ii)==ij) THEN
                 v(ii,1:nt)=v(ii,1:nt)*wt
                 ii=ii+1
              END IF
           END DO
        END DO
     END SELECT
  END DO
!
  RETURN
 END SUBROUTINE latitude_weight
!
!
!
 FUNCTION check_ivf(iffy,ivfa)
!
! Modules
  USE arrays,   ONLY: iuse=>iusey
  USE iofiles,  ONLY: yfile
  USE settings, ONLY: iv,ivf,iva
!
! Function type
  INTEGER :: check_ivf
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iffy ! - current Y field / lagged field -
  INTEGER, INTENT(IN) :: ivfa ! - current available series by field / lagged field -
!
! Locals
!
! Local scalars
  INTEGER :: ivmin  ! - minimum available variable index for current field -
  INTEGER :: ivmax  ! - maximum available variable index for current field -
  INTEGER :: ivamin ! - minimum variable index for current field -
  INTEGER :: ivamax ! - maximum variable index for current field -
  INTEGER :: iv0    ! - index of zeroth variable for current field -
  INTEGER :: iva0   ! - index of zeroth available variable for current field -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC SUM
!
! Executable Statements
!
! Define limits for current field
  IF (iffy==1) THEN
     iv0=0
     iva0=0
  ELSE
     iv0=SUM(yfield(1:iffy-1)%nv)
     iva0=SUM(yfield(1:iffy-1)%nva)
  END IF
  ivamin=iva0+1
  ivamax=SUM(yfield(1:iffy)%nva)
  ivmin=iuse(ivamin)
  ivmax=iuse(ivamax)
!
! Identify next series
  iva=iva0+ivfa
  iv=iuse(iva)
  ivf=iv-iv0
!
! Update coordinates
  SELECT CASE (yfile%igrid)
   CASE (1)
     y_coors=get_grid_coors()
   CASE (2)
     y_coors=get_stn_coors()
     cstnc=cstny(ivf,iffy)
   CASE (3)
     cstnc=cstny(ivf,iffy)
  END SELECT
  check_ivf=2
!
  RETURN
 END FUNCTION check_ivf
!
!
!
 FUNCTION which_grid(xsp,ysp)
!
! Identifies grid for current point
!
! Modules
  USE numbers, ONLY: half,three,r360
!
! Function type
  INTEGER :: which_grid
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: xsp ! - longitude of selected point -
  REAL(KIND=rp), INTENT(IN) :: ysp ! - latitude of selected point -
!
! Locals
!
! Local scalars
  INTEGER :: i  ! - latitude index -
  INTEGER :: j  ! - longitude index -
  INTEGER :: ii ! - index of selected latitude -
  INTEGER :: jj ! - index of selected longitude -
!
  REAL(KIND=rp) :: rlt1 ! - south latitude of current grid -
  REAL(KIND=rp) :: rlt2 ! - north latitude of current grid -
  REAL(KIND=rp) :: rlg1 ! - west longitude of current grid -
  REAL(KIND=rp) :: rlg2 ! - east longitude of current grid -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC SUM
!
! Executable Statements
!
! Get grid of selected point
  which_grid=0
  ii=0
  DO i=yfield(iffy)%region%nlt1,yfield(iffy)%region%nlt2
     IF (i>1) THEN
        rlt1=(rlaty(i,iffy)+rlaty(i-1,iffy))*half
     ELSE
        rlt1=(three*rlaty(i,iffy)-rlaty(i+1,iffy))*half
     END IF
     IF (i<yfield(iffy)%nlt) THEN
        rlt2=(rlaty(i,iffy)+rlaty(i+1,iffy))*half
     ELSE
        rlt2=(three*rlaty(i,iffy)-rlaty(i-1,iffy))*half
     END IF
     IF (ysp>rlt1.AND.ysp<=rlt2) THEN
        ii=i
        EXIT
     END IF
  END DO
  IF (ii==0) RETURN
! - identify longitude -
  jj=0
  DO j=1,yfield(iffy)%region%nlgs
     IF (j>1) THEN
        rlg1=(rlngdy(j,iffy)+rlngdy(j-1,iffy))*half
     ELSE
        rlg1=rlngdy(1,iffy)-(rlngdy(2,iffy)-rlngy(1,iffy))*half
     END IF
     IF (j<yfield(iffy)%region%nlgs) THEN
        rlg2=(rlngdy(j,iffy)+rlngdy(j+1,iffy))*half
     ELSE IF (yfield(iffy)%region%nlgs>1) THEN
        rlg2=rlngdy(yfield(iffy)%region%nlgs,iffy)+(rlngdy(yfield(iffy)%region%nlgs,iffy)- &
             rlngdy(yfield(iffy)%region%nlgs-1,iffy))*half
     ELSE
        rlg2=2*rlngdy(1,iffy)-rlg1
     END IF
     IF (rlg2<rlg1) rlg2=rlg2+r360
     IF ((xsp>rlg1.AND.xsp<=rlg2).OR. &
         (xsp-r360>rlg1.AND.xsp-r360<=rlg2).OR. &
         (xsp+r360>rlg1.AND.xsp+r360<=rlg2)) THEN
        jj=j
        EXIT
     END IF
  END DO
  IF (jj==0) RETURN
!
! Determine grid
  which_grid=(ii-1)*yfield(iffy)%region%nlgs+jj
  IF (iffy>1) which_grid=which_grid+SUM(yfield(1:iffy-1)%nv)
!
  RETURN
 END FUNCTION which_grid
!
!
!
 FUNCTION which_station(xsp,ysp)
!
! Identifies station at current point
!
! Modules
  USE arrays,   ONLY: iusey
  USE numbers,  ONLY: zero,oneh,r360
!
! Function type
  INTEGER :: which_station
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: xsp ! - longitude of selected point -
  REAL(KIND=rp), INTENT(IN) :: ysp ! - latitude of selected point -
!
! Locals
!
! Local scalars
  INTEGER :: i  ! - station index -
  INTEGER :: ij ! - used station index -
  INTEGER :: iy ! - available station index -
  INTEGER :: i0 ! - offset -
  INTEGER :: j0 ! - offset -
!
  REAL(KIND=rp) :: rlt1 ! - slightly south latitude of current point -
  REAL(KIND=rp) :: rlt2 ! - slightly north latitude of current point -
  REAL(KIND=rp) :: rlg1 ! - slightly west longitude of current point -
  REAL(KIND=rp) :: rlg2 ! - slightly east longitude of current point -
  REAL(KIND=rp) :: dlng ! - longitudinal extent of domain -
  REAL(KIND=rp) :: tol  ! - tolerance distance for near miss -
  REAL(KIND=rp) :: radj ! - longitude adjustment -
  REAL(KIND=rp) :: d    ! - distance from point to station -
  REAL(KIND=rp) :: dmin ! - distance from point to nearest station -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC SUM
  INTRINSIC MIN
!
! Executable Statements
!
! Set tolerance distance for near miss
  which_station=0
  dlng=yfield(iffy)%region%alim%rlge-yfield(iffy)%region%alim%rlgw
  IF (dlng<zero) dlng=dlng-r360
  tol=MIN(yfield(iffy)%region%alim%rltn-yfield(iffy)%region%alim%rlts,dlng)/oneh
!
! Get station at selected point
  IF (iffy==1) THEN
     i0=0
     j0=0
  ELSE
     i0=SUM(yfield(1:iffy-1)%nva)
     j0=SUM(yfield(1:iffy-1)%nv)
  END IF
  iy=0
  DO i=1,yfield(iffy)%nva
     ij=iusey(i+i0)-j0
     rlt1=rlatdy(ij,iffy)-tol
     rlt2=rlatdy(ij,iffy)+tol
     rlg1=rlngdy(ij,iffy)-tol
     rlg2=rlngdy(ij,iffy)+tol
     IF (ysp>rlt1.AND.ysp<rlt2) THEN
        IF (xsp>rlg1.AND.xsp<rlg2) THEN
           radj=0
        ELSE IF (xsp>rlg1-r360.AND.xsp<rlg2-r360) THEN
           radj=-r360
        ELSE IF (xsp>rlg1+r360.AND.xsp<rlg2+r360) THEN
           radj=r360
        ELSE
           CYCLE
        END IF
        IF (iy==0) THEN
           iy=i
           dmin=gcd(ysp,xsp,rlatdy(ij,iffy)+radj,rlngdy(ij,iffy)+radj)
        ELSE ! - find nearest station if two stations are near the selected point -
           d=gcd(ysp,xsp,rlatdy(ij,iffy)+radj,rlngdy(ij,iffy)+radj)
           IF (d<dmin) THEN
              iy=i           
              dmin=d
           END IF
        END IF
     END IF
  END DO
  IF (iy==0) RETURN
!
! Determine station
  IF (iffy>1) iy=iy+SUM(yfield(1:iffy-1)%nva)
  which_station=iusey(iy)
!
  RETURN
 END FUNCTION which_station
!
!
!
 FUNCTION which_index(xsp)
!
! Identifies index at current point
!
! Function type
  INTEGER :: which_index
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: xsp ! - longitude of selected point -
!
! Locals
!
! Local scalars
  INTEGER :: iy ! - current used index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC SUM
  INTRINSIC CEILING
!
! Executable Statements
!
! Get index at selected point
  iy=CEILING(xsp)
  IF ((iy<1).OR.(iy>yfield(iffy)%nv)) THEN
     which_index=0
     RETURN
  END IF
!
! Determine index
  IF (iffy==1) iy=iy+SUM(yfield(1:iffy-1)%nv)
  which_index=iy
!
  RETURN
 END FUNCTION which_index
!
!
!
 SUBROUTINE get_nearest_grids (iygrid,intp,prog,dprog)
!
! Identifies nearest grid boxes. Returns grid references
!
! Modules
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iygrid ! - Y data structure -
  INTEGER, INTENT(IN) :: intp   ! - interpolation option -
!
! Input output scalars
  REAL(KIND=rp), INTENT(INOUT) :: prog  ! - progress meter -
  REAL(KIND=rp), INTENT(INOUT) :: dprog ! - progress increment -
!
! Locals
!
! Local scalars
  INTEGER :: i     ! - latitude index -
  INTEGER :: j     ! - longitude index -
  INTEGER :: ii    ! - latitude index -
  INTEGER :: ij    ! - station index -
  INTEGER :: imin1 ! - nearest latitude -
  INTEGER :: imin2 ! - second nearest latitude -
  INTEGER :: jmin1 ! - nearest longitude -
  INTEGER :: jmin2 ! - second nearest longitude -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SIGN
!
! Executable Statements
!
! Identify nearest gridpoints
  SELECT CASE (iygrid)
   CASE (1)
     dprog=dprog*yfield(1)%region%nlts*yfield(1)%region%nlgs/(yfield(1)%region%nlts+yfield(1)%region%nlgs)
! - latitudes -
     DO i=yfield(1)%region%nlt1,yfield(1)%region%nlt2
        ii=i+1-yfield(1)%region%nlt1
        CALL get_nearest_lat (rlaty(i,1),xfield(1)%nlt,xfield(1)%region%nlt1,xfield(1)%region%nlt2,rlatx(:,1),imin1,imin2)
! - interpolate -
        rlatn(ii)=REAL(imin1,KIND=rp)
        IF (imin1>0) THEN
           IF ((intp==1).AND.(imin1/=imin2)) THEN
              rlatn(ii)=rlatn(ii)+frac_lat(rlaty(i,1),rlatx(imin1,1),rlatx(imin2,1))*REAL(SIGN(1,imin2-imin1),KIND=rp)
           END IF
        ELSE
        END IF
        prog=prog+dprog
     END DO
! - longitudes -
     DO j=1,yfield(1)%region%nlgs
        CALL get_nearest_lng (rlngdy(j,1),xfield(1)%nlg,xfield(1)%region%nlg1,xfield(1)%region%nlg2,rlngx(:,1),jmin1,jmin2)
! - interpolate -
        rlngn(j)=REAL(jmin1,KIND=rp)
        IF ((intp==1).AND.(jmin1/=jmin2)) THEN
           rlngn(j)=rlngn(j)+frac_lng(rlngdy(j,1),rlngdx(jmin1,1),rlngdx(jmin2,1))*REAL(SIGN(1,jmin2-jmin1),KIND=rp)
        END IF
        prog=prog+dprog
     END DO
     dprog=dprog*(yfield(1)%region%nlts+yfield(1)%region%nlgs)/(yfield(1)%region%nlts*yfield(1)%region%nlgs)
!
! Identify nearest gridpoints to stations
   CASE (2)
     DO ij=1,yfield(1)%nva
! - latitudes -
        CALL get_nearest_lat (rlatdy(ij,1),xfield(1)%nlt,xfield(1)%region%nlt1,xfield(1)%region%nlt2,rlatx(:,1),imin1,imin2)
! - longitudes -
        CALL get_nearest_lng (rlngdy(ij,1),xfield(1)%nlg,xfield(1)%region%nlg1,xfield(1)%region%nlg2,rlngx(:,1),jmin1,jmin2)
! - interpolate -
        rlatn(ij)=REAL(imin1,KIND=rp)
        rlngn(ij)=REAL(jmin1,KIND=rp)
        IF ((imin1>0).AND.(jmin1>0)) THEN
           IF (intp==1) THEN
              IF (imin1/=imin2) THEN
                 rlatn(ij)=rlatn(ij)+frac_lat(rlatdy(ij,1),rlatx(imin1,1),rlatx(imin2,1))*REAL(SIGN(1,imin2-imin1),KIND=rp)
              END IF
              IF (jmin1/=jmin2) THEN
                 rlngn(ij)=rlngn(ij)+frac_lng(rlngdy(ij,1),rlngx(jmin1,1),rlngx(jmin2,1))*REAL(SIGN(1,jmin2-jmin1),KIND=rp)
              END IF
           END IF
        END IF
        prog=prog+dprog
     END DO
  END SELECT
!
  RETURN
!
 CONTAINS
!
!
  SUBROUTINE get_nearest_lat (rlt,nlt,nlt1,nlt2,rlts,imin1,imin2)
!
! Identifies nearest two latitudes
!
! Modules
  USE numbers, ONLY: zero,r180
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nlt  ! - number of latitudes -
  INTEGER, INTENT(IN) :: nlt1 ! - northernmost latitude -
  INTEGER, INTENT(IN) :: nlt2 ! - southernmost latitude -
!
  REAL(KIND=rp), INTENT(IN) :: rlt ! - latitude -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: rlts(:) ! - latitudes -
!
! Output scalars
  INTEGER, INTENT(OUT) :: imin1 ! - nearest latitude -
  INTEGER, INTENT(OUT) :: imin2 ! - second nearest latitude -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - latitude index -
!
  REAL(KIND=rp) :: d     ! - distance -
  REAL(KIND=rp) :: dmin1 ! - minimum distance -
  REAL(KIND=rp) :: dmin2 ! - second minimum distance -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ABS
!
! Executable Statements
!
! Find nearest latitudes
  dmin1=r180
  dmin2=r180
  imin1=1
  imin2=1
  get_lat: DO i=1,nlt
     d=ABS(rlt-rlts(i))
     IF (d==zero) THEN
        imin1=i
        imin2=i
        EXIT get_lat
     END IF
     IF (d<dmin1) THEN
        dmin2=dmin1
        imin2=imin1
        dmin1=d
        imin1=i
     ELSE IF (d<dmin2) THEN
        dmin2=d
        imin2=i
     ELSE
        EXIT get_lat
     END IF
  END DO get_lat
!
! Check whether latitude is within the domain
  IF ((imin1>=nlt1).AND.(imin1<=nlt2).AND.(imin2>=nlt1).AND.(imin2<=nlt2)) THEN
     imin1=imin1+1-nlt1
     imin2=imin2+1-nlt1
  ELSE
     imin1=-1
     imin2=-1
  END IF
!
  RETURN
  END SUBROUTINE get_nearest_lat
!
!
!
  SUBROUTINE get_nearest_lng (rlg,nlg,nlg1,nlg2,rlgs,jmin1,jmin2)
!
! Identifies nearest two longitudes
!
! Modules
  USE numbers, ONLY: zero,r180,r360
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nlg  ! - number of longitudes -
  INTEGER, INTENT(IN) :: nlg1 ! - westernmost longitude -
  INTEGER, INTENT(IN) :: nlg2 ! - easternmost longitude -
!
  REAL(KIND=rp), INTENT(IN) :: rlg ! - longitude -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: rlgs(:) ! - longitudes -
!
! Output scalars
  INTEGER, INTENT(OUT) :: jmin1 ! - nearest longitude -
  INTEGER, INTENT(OUT) :: jmin2 ! - second nearest longitude -
!
! Locals
!
! Local scalars
  INTEGER :: j    ! - longitude index -
!
  REAL(KIND=rp) :: d     ! - distance -
  REAL(KIND=rp) :: dmin1 ! - minimum distance -
  REAL(KIND=rp) :: dmin2 ! - second minimum distance -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ABS
!
! Executable Statements
!
! Find nearest longitudes
  dmin1=r180
  dmin2=r180
  jmin1=0
  jmin2=0
  DO j=1,nlg
     d=ABS(rlg-rlgs(j))
     DO
        IF (d<r360) EXIT
        d=d-r360
     END DO
     IF (d>r180) d=r360-d
     IF (d==zero) THEN
        jmin1=j
        jmin2=j
        RETURN
     END IF
     IF (d<dmin1) THEN
        dmin2=dmin1
        jmin2=jmin1
        dmin1=d
        jmin1=j
     ELSE IF (d<dmin2) THEN
        dmin2=d
        jmin2=j
     END IF
  END DO
!
! Check whether longitude is within the domain
  IF (nlg1<nlg2) THEN
     IF ((jmin1>=nlg1).AND.(jmin1<=nlg2).AND.(jmin2>=nlg1).AND.(jmin2<=nlg2)) THEN
        jmin1=jmin1+1-nlg1
        jmin2=jmin2+1-nlg1
     ELSE
        jmin1=-1
        jmin2=-1
     END IF
  ELSE
     IF (((jmin1>=nlg1).OR.(jmin1<=nlg2)).AND.((jmin2>=nlg1).OR.(jmin2<=nlg2))) THEN
        IF (jmin1>=nlg1) THEN
           jmin1=jmin1+1-nlg1
        ELSE
           jmin1=jmin1+nlg+1-nlg1
        END IF
        IF (jmin2>=nlg1) THEN
           jmin2=jmin2+1-nlg1
        ELSE
           jmin2=jmin2+nlg+1-nlg1
        END IF
     ELSE
        jmin1=-1
        jmin2=-1
     END IF
  END IF
!
  RETURN
  END SUBROUTINE get_nearest_lng
!
!
!
  FUNCTION frac_lat(c1,c2,c3)
!
! Calculates proportional distance from c1 to c2 compared to the distance c2 to c3
!
! Function result
  REAL(KIND=rp) :: frac_lat
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: c1 ! - first coordinate -
  REAL(KIND=rp), INTENT(IN) :: c2 ! - second coordinate -
  REAL(KIND=rp), INTENT(IN) :: c3 ! - third coordinate -
!
! Locals
!
! Local scalars
  REAL(KIND=rp) :: d12 ! - distance from c1 to c2 -
  REAL(KIND=rp) :: d13 ! - distance from c1 to c3 -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ABS
!
! Executable Statements
!
! Check for identical locations
  d12=ABS(c1-c2)
  d13=ABS(c3-c1)
!
! Calculate proportional distance
  frac_lat=d12/(d12+d13)
!
  RETURN
  END FUNCTION frac_lat
!
!
!
  FUNCTION frac_lng(c1,c2,c3)
!
! Calculates proportional distance from c1 to c2 compared to the distance c2 to c3
!
! Modules
  USE numbers, ONLY: r180,r360
!
! Function result
  REAL(KIND=rp) :: frac_lng
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: c1 ! - first coordinate -
  REAL(KIND=rp), INTENT(IN) :: c2 ! - second coordinate -
  REAL(KIND=rp), INTENT(IN) :: c3 ! - third coordinate -
!
! Locals
!
! Local scalars
  REAL(KIND=rp) :: d12 ! - distance from c1 to c2 -
  REAL(KIND=rp) :: d13 ! - distance from c1 to c3 -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ABS
!
! Executable Statements
!
! Check for identical locations
  d12=ABS(c2-c1)
  DO
     IF (d12<=r180) EXIT
     d12=ABS(r360-d12)
  END DO
  d13=ABS(c1-c3)
  DO
     IF (d13<=r180) EXIT
     d13=ABS(r360-d13)
  END DO
!
! Calculate proportional distance
  frac_lng=d12/(d12+d13)
!
  RETURN
  END FUNCTION frac_lng
 END SUBROUTINE get_nearest_grids
!
!
!
 SUBROUTINE get_interpolated (iygrid,intp,iusey,iusex,nu,x,xiny)
!
! Identifies nearest grid boxes. Returns grid references
!
! Modules
  USE numbers, ONLY: zero,one
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iygrid ! - Y data structure -
  INTEGER, INTENT(IN) :: intp   ! - interpolation option -
  INTEGER, INTENT(IN) :: nu     ! - number of used cases -
!
! Input arrays
  INTEGER, INTENT(IN) :: iusey(:) ! - indices of used Y variables -
  INTEGER, INTENT(IN) :: iusex(:) ! - indices of used X variables -
!
  REAL(KIND=rp), INTENT(IN) :: x(:,:) ! - X data -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: xiny(:,:) ! - interpolated X data -
!
! Locals
!
! Local scalars
  INTEGER :: i   ! - latitude index -
  INTEGER :: j   ! - longitude index -
  INTEGER :: i1  ! - first latitude index -
  INTEGER :: i2  ! - second latitude index -
  INTEGER :: j1  ! - first longitude index -
  INTEGER :: j2  ! - second longitude index -
  INTEGER :: ij  ! - station index -
  INTEGER :: ij1 ! - first location index -
  INTEGER :: ij2 ! - second location index -
  INTEGER :: ij3 ! - third location index -
  INTEGER :: ij4 ! - fourth location index -
!
  REAL(KIND=rp) :: wti1 ! - first latitude weight -
  REAL(KIND=rp) :: wti2 ! - second longitude weight -
  REAL(KIND=rp) :: wtj1 ! - first latitude weight -
  REAL(KIND=rp) :: wtj2 ! - second longitude weight -
  REAL(KIND=rp) :: wt1  ! - first gridpoint weight -
  REAL(KIND=rp) :: wt2  ! - second gridpoint weight -
  REAL(KIND=rp) :: wt3  ! - third gridpoint weight -
  REAL(KIND=rp) :: wt4  ! - fourth gridpoint weight -
  REAL(KIND=rp) :: swt  ! - sum of weights -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC CEILING
  INTRINSIC FLOOR
  INTRINSIC NINT
  INTRINSIC REAL
!
! Executable Statements
!
! Select nearest gridpoints
  SELECT CASE (intp)
   CASE (0)
     SELECT CASE (iygrid)
      CASE (1)
        ij=1
        DO i=1,yfield(1)%region%nlts
           i1=NINT(rlatn(i))
           DO j=1,yfield(1)%region%nlgs
              IF (iusey(ij)/=(i-1)*yfield(1)%region%nlgs+j) CYCLE
              j1=NINT(rlngn(j))
              IF ((i1>0).AND.(j1>0)) THEN
                 ij1=check_iv(i1,j1,xfield(1)%region%nlgs,iusex)
              ELSE
                 ij1=0
              END IF
              IF (ij1>0) THEN
                 xiny(ij,1:nu)=x(ij1,1:nu)
              ELSE
                 xiny(ij,1:nu)=zero
              END IF
              ij=ij+1
           END DO
        END DO
      CASE (2)
        DO ij=1,yfield(1)%nva
           i1=NINT(rlatn(ij))
           j1=NINT(rlngn(ij))
           IF ((i1>0).AND.(j1>0)) THEN
              ij1=check_iv(i1,j1,xfield(1)%region%nlgs,iusex)
           ELSE
              ij1=0
           END IF
           IF (ij1>0) THEN
              xiny(ij,1:nu)=x(ij1,1:nu)
           ELSE
              xiny(ij,1:nu)=zero
           END IF
        END DO
     END SELECT
!
! Interpolate
   CASE (1)
     SELECT CASE (iygrid)
! - interpolate to grid -
      CASE (1)
        ij=1
        DO i=1,yfield(1)%region%nlts
           i1=FLOOR(rlatn(i))
           i2=CEILING(rlatn(i))
           wti1=REAL(i2,KIND=rp)-rlatn(i)
           wti2=one-wti1
           DO j=1,yfield(1)%region%nlgs
              IF (iusey(ij)/=(i-1)*yfield(1)%region%nlgs+j) CYCLE
              j1=FLOOR(rlngn(j))
              j2=CEILING(rlngn(j))
              wtj1=REAL(j2,KIND=rp)-rlngn(j)
              wtj2=one-wti1
              IF (j1<1) j1=xfield(1)%region%nlgs
              IF (j2>xfield(1)%region%nlgs) j2=1
              ij1=check_iv(i1,j1,xfield(1)%region%nlgs,iusex)
              IF (ij1>0) THEN
                 wt1=wti1*wtj1
              ELSE
                 wt1=zero
              END IF
              ij2=check_iv(i1,j2,xfield(1)%region%nlgs,iusex)
              IF (ij1>0) THEN
                 wt2=wti1*wtj2
              ELSE
                 wt2=zero
              END IF
              ij3=check_iv(i2,j1,xfield(1)%region%nlgs,iusex)
              IF (ij1>0) THEN
                 wt3=wti2*wtj1
              ELSE
                 wt3=zero
              END IF
              ij4=check_iv(i2,j2,xfield(1)%region%nlgs,iusex)
              IF (ij1>0) THEN
                 wt4=wti2*wtj2
              ELSE
                 wt4=zero
              END IF
              swt=wt1+wt2+wt3+wt4
              IF (swt>zero) THEN
                 xiny(ij,1:nu)=(x(ij1,1:nu)*wt1+x(ij2,1:nu)*wt2+x(ij3,1:nu)*wt3+x(ij4,1:nu)*wt4)/swt
              ELSE
                 xiny(ij,1:nu)=zero
              END IF
              ij=ij+1
           END DO
        END DO
! - interpolate to stations -
      CASE (2)
        DO ij=1,yfield(1)%nva
           i1=FLOOR(rlatn(ij))
           i2=CEILING(rlatn(ij))
           wti1=REAL(i2,KIND=rp)-rlatn(ij)
           wti2=one-wti1
           j1=FLOOR(rlngn(ij))
           j2=CEILING(rlngn(ij))
           wtj1=REAL(j2,KIND=rp)-rlatn(ij)
           wtj2=one-wti1
           IF (j1<1) j1=xfield(1)%region%nlgs
           IF (j2>xfield(1)%region%nlgs) j2=1
           ij1=check_iv(i1,j1,xfield(1)%region%nlgs,iusex)
           IF (ij1>0) THEN
              wt1=wti1*wtj1
           ELSE
              wt1=zero
           END IF
           ij2=check_iv(i1,j2,xfield(1)%region%nlgs,iusex)
           IF (ij2>0) THEN
              wt2=wti1*wtj2
           ELSE
              wt2=zero
           END IF
           ij3=check_iv(i2,j1,xfield(1)%region%nlgs,iusex)
           IF (ij3>0) THEN
              wt3=wti2*wtj1
           ELSE
              wt3=zero
           END IF
           ij4=check_iv(i2,j2,xfield(1)%region%nlgs,iusex)
           IF (ij4>0) THEN
              wt4=wti2*wtj2
           ELSE
              wt4=zero
           END IF
           swt=wt1+wt2+wt3+wt4
           IF (swt>zero) THEN
              xiny(ij,1:nu)=(x(ij1,1:nu)*wt1+x(ij2,1:nu)*wt2+x(ij3,1:nu)*wt3+x(ij4,1:nu)*wt4)/swt
           ELSE
              xiny(ij,1:nu)=zero
           END IF
        END DO
     END SELECT
  END SELECT
!
  RETURN
!
 CONTAINS
!
!
  FUNCTION check_iv(i,j,nlg,iuse)
!
! Function type
  INTEGER :: check_iv
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: i   ! - latitude index -
  INTEGER, INTENT(IN) :: j   ! - longitude index -
  INTEGER, INTENT(IN) :: nlg ! - number of longitudes -
!
! Input arrays
  INTEGER, INTENT(IN) :: iuse(:) ! - indices of used variables -
!
! Locals
!
! Local scalars
  INTEGER :: ij ! - location index -
  INTEGER :: iv ! - variable index -
!
! Functions and Subroutines
!
! Executable Statements
!
! Identify used variable
  iv=(i-1)*nlg+j
  DO ij=1,iv
     IF (iuse(ij)==iv) THEN
        check_iv=ij
        EXIT
     ELSE IF (iuse(ij)>iv) THEN
        check_iv=0
        EXIT
     END IF
  END DO
!
  RETURN
  END FUNCTION check_iv
 END SUBROUTINE get_interpolated
!
!
!
 FUNCTION gcd(dlt1,dlg1,dlt2,dlg2)
!
! Calculates great circle distance (m) between 2 points
!
! Modules
  USE numbers, ONLY: zero,pi,r180,re
!
! Function result
  REAL(KIND=rp) :: gcd
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: dlt1 ! - latitude of first point (in degrees) -
  REAL(KIND=rp), INTENT(IN) :: dlg1 ! - longitude of first point (in degrees) -
  REAL(KIND=rp), INTENT(IN) :: dlt2 ! - latitude of second point (in degrees) -
  REAL(KIND=rp), INTENT(IN) :: dlg2 ! - longitude of second point (in degrees) -
!
! Locals
!
! Local scalars
  REAL(KIND=rp) :: rlt1 ! - latitude of first point (in radians) -
  REAL(KIND=rp) :: rlt2 ! - latitude of second point (in radians) -
  REAL(KIND=rp) :: rlgd ! - longitude difference (radians) -
  REAL(KIND=rp) :: rnum ! - numerator of ATAN2 -
  REAL(KIND=rp) :: rden ! - denominator of ATAN2 -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ATAN2
  INTRINSIC COS
  INTRINSIC SIN
  INTRINSIC SQRT
!
! Executable Statements
!
! Check for identical locations
  IF ((dlt1==dlt2).AND.(dlg1==dlg2)) THEN
     gcd=zero
     RETURN
  END IF
!
! Convert latitudes to radians
  rlt1=dlt1*pi/r180
  rlt2=dlt2*pi/r180
!
! Calculate longitude difference in radians
  rlgd=(dlg1-dlg2)*pi/r180
!
! Calculate distance
  rnum=SQRT((COS(rlt2)*SIN(rlgd))**2 + (COS(rlt1)*SIN(rlt2) - SIN(rlt1)*COS(rlt2)*COS(rlgd))**2)
  rden=SIN(rlt1)*SIN(rlt2) + COS(rlt1)*COS(rlt2)*COS(rlgd)
  gcd=ATAN2(rnum,rden)
  gcd=gcd*re
!
  RETURN
 END FUNCTION gcd
END MODULE fields
