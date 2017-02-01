! $Id: tables.f90 1221 2011-03-03 15:32:00Z simon $
MODULE tables
!
! Modules
  USE CPT_constants, ONLY: ng
  USE numbers,       ONLY: rp
!
! Implicit declarations
  IMPLICIT NONE
!
! Arrays
!
! Integer arrays
  INTEGER, PRIVATE :: ctbl(ng,ng) ! - contingency table -
!
CONTAINS
!
!
 FUNCTION table_cv()
!
! Function type
  INTEGER :: table_cv
!
! Executable Statements
!
! Cross-validated contingency tables
  table_cv=table('Cross-validated contingency tables',calc_cvtable)
!
  RETURN
 END FUNCTION table_cv
!
!
!
 FUNCTION table_ra()
!
! Function type
  INTEGER :: table_ra
!
! Executable Statements
!
! Retroactive contingency tables
  table_ra=table('Retroactive contingency tables',calc_rtable)
!
  RETURN
 END FUNCTION table_ra
!
!
!
 FUNCTION table(title,calc_table)
!
! Modules
  USE fields,   ONLY: yfield,iffy, &
                      check_ivf,update_grid
  USE iofiles,  ONLY: yfile
  USE settings, ONLY: ivf,ivfa
!
! Function type
  INTEGER :: table
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: title ! - window title -
!
! Procedure arguments
  INTEGER, EXTERNAL :: calc_table ! - table calculation function -
!
! Executable Statements
!
! Construct and add coordinate label for current point
  DO iffy=1,yfile%nfs*yfile%nls
     DO ivfa=1,yfield(iffy)%nva
        WRITE (UNIT=*,FMT=*)
        WRITE (UNIT=*,FMT='(A)') title
        table=check_ivf(iffy,ivfa)
        CALL update_grid (ivf,iffy,yfile%igrid)
! 
! Print tables
        table=calc_table()
        CALL write_table ()
     END DO
  END DO
  table=0
!
  RETURN
 END FUNCTION table
!
!
!
 SUBROUTINE write_table ()
!
! Modules
  USE fields,       ONLY: iffy, &
                          update_grid
  USE iofiles,      ONLY: yfile
  USE labels,       ONLY: cg_all,cg_cat_a,cg_fcast
  USE numbers,      ONLY: zero
  USE settings,     ONLY: ivf
!
! Locals
!
! Local arrays
  INTEGER :: orf(ng) ! - observed relative frequencies -
  INTEGER :: frf(ng) ! - forecast relative frequencies -
!
! Local scalars
  INTEGER :: j    ! - indices -
  INTEGER :: itot ! - number of cases -
!
  REAL(KIND=rp) :: df  ! - number of cases -
  REAL(KIND=rp) :: tot ! - number of forecasts in category -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC NINT
  INTRINSIC REAL
  INTRINSIC SUM
!
! Executable Statements
!
! Construct and add coordinate label for current point
  CALL update_grid (ivf,iffy,yfile%igrid)
!
! Print tables
  DO j=1,ng
     orf(j)=SUM(ctbl(j,:))
     frf(j)=SUM(ctbl(:,j))
  END DO
  itot=SUM(orf(:))
  df=REAL(itot,KIND=rp)
! - print frequency table -
  WRITE (UNIT=*,FMT='(A)') ' '
  WRITE (UNIT=*,FMT='(A)') '                     ',cg_fcast
  WRITE (UNIT=*,FMT='(A)') ' '
  WRITE (UNIT=*,FMT='(13X,3(5X,A),A)') (cg_cat_a(j),j=1,ng),'   Total'
  WRITE (UNIT=*,FMT='(A)') ' '
  WRITE (UNIT=*,FMT='(2A,3I6,I8)') '            ',cg_cat_a(3),(ctbl(3,j),j=1,ng),orf(3)
  WRITE (UNIT=*,FMT='(2A,3I6,I8)') ' Observed   ',cg_cat_a(2),(ctbl(2,j),j=1,ng),orf(2)
  WRITE (UNIT=*,FMT='(2A,3I6,I8)') '            ',cg_cat_a(1),(ctbl(1,j),j=1,ng),orf(1)
  WRITE (UNIT=*,FMT='(A)') ' '
  WRITE (UNIT=*,FMT='(A,3I6,I8)') '        Total',(frf(j),j=1,ng),itot
  WRITE (UNIT=*,FMT='(A)') ' '
! - calculate observed relative frequencies -
  DO j=1,ng
     orf(j)=NINT(REAL(100*orf(j),KIND=rp)/df)
     frf(j)=NINT(REAL(100*frf(j),KIND=rp)/df)
     tot=REAL(SUM(ctbl(:,j)),KIND=rp)
     IF (tot>zero) ctbl(:,j)=NINT(REAL(100*ctbl(:,j),KIND=rp)/tot)
  END DO
! - print contingency table -
  WRITE (UNIT=*,FMT='(A)') ' '
  WRITE (UNIT=*,FMT='(A)') '                     ',cg_fcast
  WRITE (UNIT=*,FMT='(A)') ' '
  WRITE (UNIT=*,FMT='(13X,3(5X,A),2A)') (cg_cat_a(j),j=1,ng),'   ',ADJUSTR(cg_all)
  WRITE (UNIT=*,FMT='(A)') ' '
  WRITE (UNIT=*,FMT='(2A,3(I5,A),I7,A)') '            ',cg_cat_a(3),(ctbl(3,j),'%',j=1,ng),orf(3),'%'
  WRITE (UNIT=*,FMT='(2A,3(I5,A),I7,A)') ' Observed   ',cg_cat_a(2),(ctbl(2,j),'%',j=1,ng),orf(2),'%'
  WRITE (UNIT=*,FMT='(2A,3(I5,A),I7,A)') '            ',cg_cat_a(1),(ctbl(1,j),'%',j=1,ng),orf(1),'%'
  WRITE (UNIT=*,FMT='(A)') ' '
  WRITE (UNIT=*,FMT='(2A,3(I5,A),I7,A)') '        ',ADJUSTR(cg_all),(frf(j),'%',j=1,ng),100,'%'
  WRITE (UNIT=*,FMT='(A)') ' '
!
  RETURN
 END SUBROUTINE write_table
!
!
!
 FUNCTION calc_cvtable()
!
! Modules
  USE arrays,   ONLY: iobs,ifor
  USE settings, ONLY: iva,nu
!
! Function type
  INTEGER :: calc_cvtable
!
! Locals
!
! Local scalars
  INTEGER :: i ! - indices -
!
! Executable Statements
!
! Initialise table
  ctbl(:,:)=0
!
! Construct frequency table
  DO i=1,nu
     ctbl(iobs(iva,i),ifor(iva,i))=ctbl(iobs(iva,i),ifor(iva,i))+1
  END DO
  calc_cvtable=0
!
  RETURN
 END FUNCTION calc_cvtable
!
!
!
 FUNCTION calc_rtable()
!
! Modules
  USE arrays,   ONLY: irobs,irfor
  USE settings, ONLY: iva,ntr
!
! Function type
  INTEGER :: calc_rtable
!
! Locals
!
! Local scalars
  INTEGER :: i ! - indices -
!
! Executable Statements
!
! Initialise table
  ctbl(:,:)=0
!
! Construct frequency table
  DO i=1,ntr
     ctbl(irobs(iva,i),irfor(iva,i))=ctbl(irobs(iva,i),irfor(iva,i))+1
  END DO
  calc_rtable=0
!
  RETURN
 END FUNCTION calc_rtable
END MODULE tables
