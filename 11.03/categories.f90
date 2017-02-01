MODULE categories
!
! Modules
  USE CPT_constants, ONLY: ng,nts
  USE IO_constants,  ONLY: lprd
  USE numbers,       ONLY: rp
  USE time
!
! Implicit declarations
  IMPLICIT NONE
!
! Arrays
!
! Real arrays
  INTEGER, PUBLIC :: iay(nts) ! - analogue year indices -
!
  REAL(KIND=rp), PUBLIC :: pthr(nts) ! - percentile thresholds -
  REAL(KIND=rp), PUBLIC :: thr(nts)  ! - absolute thresholds -
!
  TYPE(date), PUBLIC :: anlg(nts)   ! - analogue years -
!
! Scalars
!
! Integer scalars
  INTEGER, PUBLIC :: iclim ! - settable climatological period flag -
  INTEGER, PUBLIC :: ithr  ! - threshold type -
!
! Derived types
!
! Derived type definitions
! - climatological period -
  TYPE clim_per
     TYPE(date) :: it1        ! - first date of climatology -
     TYPE(date) :: it2        ! - last date of climatology -
!
     INTEGER :: ic1           ! - index of first date of climatological period -
     INTEGER :: ic2           ! - index of last date of climatological period -
     INTEGER :: nc            ! - length of climatological period -
     INTEGER :: lsn           ! - length of season -
!
     CHARACTER(lprd) :: clim1 ! - first date of climatology -
     CHARACTER(lprd) :: clim2 ! - last date of climatology -
  END TYPE clim_per
!
! Interface operators
  INTERFACE OPERATOR(==)
     MODULE PROCEDURE same_clim_per
  END INTERFACE
  INTERFACE OPERATOR(/=)
     MODULE PROCEDURE diff_clim_per
  END INTERFACE
!
! Derived type scalars
  TYPE(clim_per), PUBLIC :: climate_per ! - climatological period -
  TYPE(clim_per), PUBLIC :: climate_bak ! - backup climatological period -
!
CONTAINS
!
!
 FUNCTION same_clim_per(cp1,cp2)
!
! Tests whether climate periods are identical
!
! Function type
  LOGICAL same_clim_per
!
! Arguments
!
! Input scalars
  TYPE(clim_per), INTENT(IN) :: cp1 ! - first climatological period -
  TYPE(clim_per), INTENT(IN) :: cp2 ! - second climatological period -
!
! Executable Statements
!
! Test whether climate periods are identical
  IF ((cp1%it1==cp2%it1).AND.(cp1%it2==cp2%it2)) THEN
     same_clim_per=.true.
  ELSE
     same_clim_per=.false.
  END IF
!
  RETURN
 END FUNCTION same_clim_per
!
!
!
 FUNCTION diff_clim_per(cp1,cp2)
!
! Tests whether climate periods are different
!
! Function type
  LOGICAL diff_clim_per
!
! Arguments
!
! Input scalars
  TYPE(clim_per), INTENT(IN) :: cp1 ! - first climatological period -
  TYPE(clim_per), INTENT(IN) :: cp2 ! - second climatological period -
!
! Executable Statements
!
! Test whether climate periods are different
  IF ((cp1%it1==cp2%it1).AND.(cp1%it2==cp2%it2)) THEN
     diff_clim_per=.false.
  ELSE
     diff_clim_per=.true.
  END IF
!
  RETURN
 END FUNCTION diff_clim_per
!
!
!
 SUBROUTINE init_analogues ()
!
! Initialises analogue years
!
! Implicit declarations
  IMPLICIT NONE
!
! Locals
! 
! Local scalars
  INTEGER :: i ! index
!
! Executable Statements
!
! Initialize analogue years
  !anlg(:)=0 gfortran won't compile this
  DO i=1,nts
     anlg(i)=0
  END DO
  iay(:)=0
!
  RETURN
 END SUBROUTINE init_analogues
!
!
!
 FUNCTION check_analogues()
!
! Checks analogue years
!
! Modules
  USE analysis, ONLY: icalc
  USE arrays,   ONLY: kuse
  USE iofiles,  ONLY: yfile
  USE settings, ONLY: nt,nu
!
! Function type
  INTEGER :: check_analogues
!
! Locals
!
! Local scalars
  INTEGER :: j  ! - threshold index -
  INTEGER :: iy ! - year index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC COUNT
!
! Executable Statements
!
! Initialize analogue years
  check_analogues=0
  IF (icalc==1) THEN
     IF (.NOT.anlg(1)==0) THEN
        DO j=1,nts
           IF (anlg(j)<yfile%fdate) THEN
              iy=1
              DO
                 IF (kuse(iy)) EXIT
                 iy=iy+1
              END DO
              iay(j)=1
              anlg(j)=yfile%fdate+(iy-1)
              check_analogues=check_analogues+j
           ELSE IF (anlg(j)>yfile%fdate+(nt-1)) THEN
              iy=nt
              DO
                 IF (kuse(iy)) EXIT
                 iy=iy-1
              END DO
              iay(j)=nu
              anlg(j)=yfile%fdate+(iy-1)
              check_analogues=check_analogues+j
           ELSE
              iay(j)=date_diff(yfile%fdate,anlg(j),yfile%iseq)+1
              iay(j)=iay(j)-COUNT(.NOT.kuse(1:iay(j)))
           END IF
        END DO
     ELSE
        iy=0
        DO j=1,nts
           iy=iy+1
           DO
              IF (kuse(iy)) EXIT
              iy=iy+1
           END DO
           iay(j)=j
           anlg(j)=yfile%fdate+(iy-1)
        END DO
     END IF
  END IF
!
  RETURN
 END FUNCTION check_analogues
!
!
!
 FUNCTION tailoring()
!
! Modules
  USE analysis, ONLY: prog,dprog,icalc,ifc
  USE arrays,   ONLY: y,yhat,yret,yrpls,clim,ave,sdev,fcast,fpls, &
                      get_flag,restdize
  USE gui,      ONLY: irv
  USE labels,   ONLY: cg_done
  USE numbers,  ONLY: zero,one
  USE settings, ONLY: igauss,iretro,istd,istds,izero,mya,nu,nur,nf,nens, &
                      record_change
!
! Function type
  INTEGER :: tailoring
!
! Locals
!
! Localarrays
  INTEGER :: anlg_old(nts)  ! - backup analogue years -
!
  REAL(KIND=rp) :: pthr_old(nts) ! - backup percentile thresholds -
  REAL(KIND=rp) :: thr_old(nts)  ! - backup absolute thresholds -
!
! Local scalars
  INTEGER :: i        ! - ensemble member index -
  INTEGER :: istd_old ! - backup standardization option -
  INTEGER :: ithr_old ! - backup threshold type -
  INTEGER :: ifail    ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ALLOCATED
  INTRINSIC ANY
  INTRINSIC REAL
  INTRINSIC TRIM
!
! Executable Statements
!
! Backup settings
  tailoring=2
  istd_old=istd
  ithr_old=ithr
  thr_old=thr
  pthr_old(:)=pthr(:)
  anlg_old(:)=anlg(:)%iyr
!
! Convert upper threshold to right-tail probability
  pthr(2)=one-pthr(2)
!
! Prompt for threshold settings
  CALL threshold_opts (istd,istds,ithr,pthr(:),thr(:),izero,ifail)
!
! Confirm settings
  IF (ifail/=0) THEN
     istd=istd_old
     ithr=ithr_old
     thr=thr_old
     pthr(:)=pthr_old(:)
     RETURN
  END IF
  istd=get_flag(istds)-1
  pthr(2)=one-pthr(2)
  IF (istd/=istd_old) THEN
     CONTINUE
  ELSE IF ((ithr/=ithr_old).OR.(ANY(thr/=thr_old)).OR.(ANY(pthr(:)/=pthr_old(:))) &
           .OR.(ANY(anlg(:)%iyr/=anlg_old(:)))) THEN
       IF (ifc==2) ifc=1
  ELSE 
      RETURN
  END IF
  tailoring=record_change()
  IF (thr(1)>thr(2)) THEN
     thr_old=thr
     thr(1)=thr_old(2)
     thr(2)=thr_old(1)
  END IF
  IF (icalc==0) RETURN
  IF (ithr==3) ifail=check_analogues()
  WRITE (UNIT=*,FMT='(A)') 'Recalculating thresholds and categories ...'
  prog=zero
  dprog=one/REAL(3+mya*(iretro+1)+igauss,KIND=rp)
! - restandardise -
  IF (istd/=istd_old) THEN
     CALL restdize (mya,nu,y,ave,sdev,istd_old,istd)
     CALL restdize (mya,climate_per%nc,clim,ave,sdev,istd_old,istd)
     CALL restdize (mya,nu,yhat,ave,sdev,istd_old,istd)
     IF (iretro==1) THEN
        CALL restdize (mya,nur,yret,ave,sdev,istd_old,istd)
        DO i=1,2
           CALL restdize (mya,nur,yrpls(:,:,i),ave,sdev,istd_old,istd)
        END DO
     END IF
     IF (ALLOCATED(fcast)) THEN
        DO i=0,nens
           CALL restdize (mya,nf,fcast(:,:,i),ave,sdev,istd_old,istd)
        END DO
        DO i=1,2
           CALL restdize (mya,nf,fpls(:,:,i),ave,sdev,istd_old,istd)
        END DO
     END IF
  END IF
  prog=prog+dprog
! - redefine categories and thresholds -
  CALL set_cv_categories ()
  IF (iretro==1) CALL set_ra_categories ()
  SELECT CASE (ithr)
   CASE (1,2)
     CALL set_percentiles ()
     CALL set_thresholds (igauss)
   CASE (3)
     CALL set_thresholds (igauss)
     CALL set_percentiles ()
  END SELECT
  irv=0
  prog=one
  WRITE (UNIT=*,FMT='(A)') TRIM(cg_done)//'!'
!
  RETURN
!
 CONTAINS
!
!
  SUBROUTINE threshold_opts (istd,istds,ithr,pthr,thr,izero,ifail)
!
! Prompts for threshold settings
!
! Modules
  USE labels,  ONLY: cg_cat_l,cg_thr_l
  USE numbers, ONLY: zero,half
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: izero ! - zero-bound flag -
!
! Input/output scalars
  INTEGER, INTENT(INOUT) :: istd ! - standardization option -
  INTEGER, INTENT(INOUT) :: ithr ! - threshold type -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail  ! - error indicator -
!
! Input/output arrays
  INTEGER, INTENT(INOUT) :: istds(:) ! - standardization options flags -
!
  REAL(KIND=rp), INTENT(INOUT) :: pthr(:) ! - percentile thresholds -
  REAL(KIND=rp), INTENT(INOUT) :: thr(:)  ! - absolute thresholds -
!
! Locals
!
! Local scalars
  INTEGER :: j ! - threshold index -
!
! Executable Statements
!
! Identify standardization option
  istds(:)=0
!
1 WRITE (UNIT=*,FMT='(A)') 'Standardization option:'
  WRITE (UNIT=*,FMT='(A)') '1. No standardization'
  WRITE (UNIT=*,FMT='(A)') '2. Anomalies'
  WRITE (UNIT=*,FMT='(A)') '3. Standardized anomalies'
  IF (izero==1) WRITE (UNIT=*,FMT='(A)') '4. %s of average'
  READ (UNIT=*,FMT=*,ERR=1) istd
  IF ((istd<1).OR.(istd>3+izero)) GOTO 1
  istds(istd)=1
! 
! Prompt for threshold settings
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') 'Threshold Settings'
  WRITE (UNIT=*,FMT=*)
2 WRITE (UNIT=*,FMT='(A)') 'Threshold type'
  WRITE (UNIT=*,FMT='(A)') '1. Climatological probabilities'
  WRITE (UNIT=*,FMT='(A)') '2. Absolute thresholds'
  WRITE (UNIT=*,FMT='(A)') '3. Analogues'
  READ (UNIT=*,FMT=*,ERR=2) ithr
  IF ((ithr<1).OR.(ithr>3)) GOTO 2
  SELECT CASE (ithr)
   CASE (1)
3    WRITE (UNIT=*,FMT='(5X,2A)',ADVANCE='no') cg_cat_l(ng),': '
     READ (UNIT=*,FMT=*,ERR=3) pthr(2)
     IF ((pthr(2)<zero).OR.(pthr(2)>half)) GOTO 3
4    WRITE (UNIT=*,FMT='(5X,2A)',ADVANCE='no') cg_cat_l(1),': '
     READ (UNIT=*,FMT=*,ERR=4) pthr(1)
     IF ((pthr(1)<zero).OR.(pthr(1)>half)) GOTO 4
   CASE (2)
     ithr=0
     DO j=nts,1,-1
5       WRITE (UNIT=*,FMT='(5X,2A)',ADVANCE='no') cg_thr_l(j),': '
        READ (UNIT=*,FMT=*,ERR=5) thr(j)
     END DO
   CASE (3)
6    WRITE (UNIT=*,FMT='(5X,A)',ADVANCE='no') 'First analogue year:  '
     READ (UNIT=*,FMT=*,ERR=6) anlg(1)%iyr
7    WRITE (UNIT=*,FMT='(5X,A)',ADVANCE='no') 'Second analogue year: '
     READ (UNIT=*,FMT=*,ERR=7) anlg(2)%iyr
     IF (check_analogues()/=0) GOTO 6
  END SELECT
  ifail=0
!
  RETURN
  END SUBROUTINE threshold_opts
 END FUNCTION tailoring
!
!
!
 SUBROUTINE set_thresholds (igauss)
!
! Modules
  USE analysis, ONLY: prog,dprog
  USE arrays,   ONLY: tobs,pobs,clim,tobst,y, &
                      insertion_sort,quantile
  USE distribs, ONLY: normq
  USE errors,   ONLY: error
  USE numbers,  ONLY: zero
  USE settings, ONLY: mya
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: igauss ! - transform Y data flag -
!
! Locals
!
! Local scalars
  INTEGER :: i     ! - variable index -
  INTEGER :: j     ! - category index -
  INTEGER :: ifail ! - error indicator -
!
  REAL(KIND=rp) :: sp ! - cumulative probability -
!
! Executable Statements
!
! Set thresholds by percentile
  SELECT CASE (ithr)
   CASE (1)
     DO i=1,mya
        DO j=1,nts
           tobs(i,j)=quantile(clim(i,:),climate_per%nc,pthr(j))
        END DO
     END DO
!
! Set thresholds by absolute value
   CASE (2)
     DO i=1,mya
        tobs(i,:)=thr(:)
     END DO
!
! Set thresholds by analogues
   CASE (3)
     ifail=check_analogues()
     IF (ifail/=0) CALL error ('check_analogues',ifail)
     DO j=1,nts
        tobs(:,j)=y(:,iay(j))
     END DO
     DO i=1,mya
        CALL insertion_sort (tobs(i,:),nts,'a')
     END DO
  END SELECT
!
! Update progress meter
  prog=prog+dprog
!
! Transform thresholds
  SELECT CASE (igauss)
   CASE (0)
     tobst(:,:)=tobs(:,:)
   CASE (1)
     DO i=1,mya
        sp=zero
        DO j=1,nts
           sp=sp+pobs(i,j)
           tobst(i,j)=normq(sp)
        END DO
     END DO
!
! Update progress meter
     prog=prog+dprog
  END SELECT
!
  RETURN
 END SUBROUTINE set_thresholds
!
!
!
 SUBROUTINE set_percentiles ()
!
! Modules
  USE analysis, ONLY: prog,dprog
  USE arrays,   ONLY: pobs,tobs,tobst,clim, &
                      percentile
  USE distribs, ONLY: normq
  USE numbers,  ONLY: zero,one
  USE settings, ONLY: igauss,mya
!
! Locals
!
! Local scalars
  INTEGER :: i ! - variable index -
  INTEGER :: j ! - category index -
!
  REAL(KIND=rp) :: sp ! - cumulative probability -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
!
! Executable Statements
!
! Set percentiles
  dprog=dprog/REAL(mya,KIND=rp)
  SELECT CASE (ithr)
   CASE (1)
     DO i=1,mya
        pobs(i,1:nts)=pthr(:)
        pobs(i,ng)=one
        DO j=ng,2,-1
           pobs(i,j)=pobs(i,j)-pobs(i,j-1)
        END DO
        prog=prog+dprog
     END DO
!
! Set percentiles from absolute value
   CASE (2)
     DO i=1,mya
        DO j=1,nts
           pobs(i,j)=percentile(clim(i,:),climate_per%nc,thr(j))
        END DO
        pobs(i,ng)=one
        DO j=ng,2,-1
           pobs(i,j)=pobs(i,j)-pobs(i,j-1)
        END DO
        prog=prog+dprog
     END DO
!
! Set percentiles from analogues
   CASE (3)
     DO i=1,mya
        DO j=1,nts
           pobs(i,j)=percentile(clim(i,:),climate_per%nc,tobs(i,j))
        END DO
        prog=prog+dprog
     END DO
     IF (igauss==1) THEN
        DO i=1,mya
           sp=zero
           DO j=1,nts
              sp=sp+pobs(i,j)
              tobst(i,j)=normq(sp)
           END DO
           pobs(i,ng)=one
           DO j=ng,2,-1
              pobs(i,j)=pobs(i,j)-pobs(i,j-1)
           END DO
!
! Update progress meter
           prog=prog+dprog
        END DO
     END IF
     prog=prog+dprog
  END SELECT
  dprog=dprog*REAL(mya,KIND=rp)
!
  RETURN
 END SUBROUTINE set_percentiles
!
!
!
 SUBROUTINE set_cv_categories ()
!
! Calculate cross-validated categories
!
! Modules
  USE analysis, ONLY: prog,dprog
  USE arrays,   ONLY: y,yhat,iobs,ifor,dwk, &
                      insertion_sort
  USE errors,   ONLY: error
  USE settings, ONLY: ncv,nu,mya,hcw
!
! Locals
!
! Local scalars
  INTEGER :: i     ! - case index -
  INTEGER :: j     ! - threshold index -
  INTEGER :: k     ! - time index -
  INTEGER :: i1,i2 ! - cross-validation indices -
  INTEGER :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
!
! Executable Statements
!
! Set categories by percentile
  SELECT CASE (ithr)
   CASE (1)
     dprog=dprog/REAL(nu*mya,KIND=rp)
! - cross-validate -
     i1=hcw
     i2=hcw+ncv-1
     DO k=1,nu
        i1=i1+1
        IF (i1==nu+1) i1=1
        i2=i2+1
        IF (i2==nu+1) i2=1
        DO i=1,mya
           IF (i1<i2) THEN
              dwk(1:ncv)=y(i,i1:i2)
           ELSE
              dwk(1:i2)=y(i,1:i2)
              dwk(i2+1:ncv)=y(i,i1:nu)
           END IF
           CALL insertion_sort (dwk,ncv,'a')
! - categorise observations -
           CALL categorise_per (1,nts,y(i,k:k),pthr(:),ncv,dwk(:),iobs(i,k:k))
! - categorise forecasts -
           CALL categorise_per (1,nts,yhat(i,k:k),pthr(:),ncv,dwk(:),ifor(i,k:k))
! - update progress meter -
           prog=prog+dprog
        END DO
     END DO
     dprog=dprog*REAL(nu*mya,KIND=rp)
!
! Set categories by absolute value
   CASE (2)
     dprog=dprog/REAL(mya,KIND=rp)
     DO i=1,mya
        CALL categorise_abs (nu,nts,y(i,:),thr(:),iobs(i,:))    ! - categorise observations -
        CALL categorise_abs (nu,nts,yhat(i,:),thr(:),ifor(i,:)) ! - categorise forecasts -
        prog=prog+dprog
     END DO
     dprog=dprog*REAL(mya,KIND=rp)
!
! Set categories by analogues
   CASE (3)
     ifail=check_analogues()
     IF (ifail/=0) CALL error ('check_analogues',ifail)
     dprog=dprog/REAL(mya,KIND=rp)
     DO i=1,mya
        DO j=1,nts
           dwk(j)=y(i,iay(j))
        END DO
        CALL insertion_sort (dwk(:),nts,'a')
        CALL categorise_abs (nu,nts,y(i,:),dwk(:),iobs(i,:))    ! - categorise observations -
        CALL categorise_abs (nu,nts,yhat(i,:),dwk(:),ifor(i,:)) ! - categorise forecasts -
        prog=prog+dprog
     END DO
     dprog=dprog*REAL(mya,KIND=rp)
  END SELECT
!
  RETURN
 END SUBROUTINE set_cv_categories
!
!
!
 SUBROUTINE set_ra_categories ()
!
! Calculate retroactive categories
!
! Modules
  USE analysis, ONLY: prog,dprog
  USE arrays,   ONLY: y,yret,irobs,irfor,rclim,kuse,dwk, &
                      insertion_sort
  USE errors,   ONLY: error 
  USE numbers,  ONLY: zero,one
  USE settings, ONLY: nt,nt1,nu,nur,nu1,nret,mya,nretro
!
! Locals
!
! Local scalars
  INTEGER :: i     ! - grid / station index -
  INTEGER :: j     ! - threshold index -
  INTEGER :: k     ! - case index -
  INTEGER :: irl   ! - index of last retroactive training-period date -
  INTEGER :: ir1   ! - index of first retroactive date -
  INTEGER :: irn   ! - index of last retroactive date -
  INTEGER :: ir0   ! - index of first new date in retroactive training period -
  INTEGER :: iru   ! - index of last new date in retroactive training period -
  INTEGER :: nr    ! - total number of retroaoctive steps in current cycle -
  INTEGER :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC COUNT
  INTRINSIC MIN
  INTRINSIC REAL
!
! Executable Statements
!
! Set categories by percentile
  SELECT CASE (ithr)
   CASE (1)
     dprog=dprog/REAL(nret*mya,KIND=rp)
     DO i=1,mya
        ir1=1
        irn=0
        ir0=1
        iru=nu1
        retro: DO irl=nt1,nt-1,nretro
           irn=irn+COUNT(kuse(irl+1:MIN(irl+nretro,nt)))
           nr=irn+1-ir1
           IF (nr>0) THEN
              dwk(ir0:iru)=y(i,ir0:iru)
              CALL insertion_sort (dwk,iru,'a')

! - categorise observations -
              CALL categorise_per (nr,nts,y(i,nu1+ir1:nu1+irn),pthr(:),iru,dwk,irobs(i,ir1:irn))
! - categorise forecasts -
              CALL categorise_per (nr,nts,yret(i,ir1:irn),pthr(:),iru,dwk,irfor(i,ir1:irn))
           END IF
! - update indices -
           ir1=irn+1
           ir0=iru+1
           iru=nu1+irn
! - update progress meter -
           prog=prog+dprog
        END DO retro
     END DO
     dprog=dprog*REAL(nret*mya,KIND=rp)
!
! Set categories by absolute value
   CASE (2)
     dprog=dprog/REAL(mya,KIND=rp)
     irl=nu1+1
     DO i=1,mya
        CALL categorise_abs (nur,nts,y(i,irl:nu),thr,irobs(i,:)) ! - categorise observations -
        CALL categorise_abs (nur,nts,yret(i,:),thr,irfor(i,:))   ! - categorise forecasts -
        prog=prog+dprog
     END DO
     dprog=dprog*REAL(mya,KIND=rp)
!
! Set categories by analogues
   CASE (3)
     ifail=check_analogues()
     IF (ifail/=0) CALL error ('check_analogues',ifail)
     dprog=dprog/REAL(mya,KIND=rp)
     irl=nu1+1
     DO i=1,mya
        DO j=1,nts
           dwk(j)=y(i,iay(j))
        END DO
        CALL insertion_sort (dwk(:),nts,'a')
        CALL categorise_abs (nur,nts,y(i,irl:nu),dwk(:),irobs(i,:)) ! - categorise observations -
        CALL categorise_abs (nur,nts,yret(i,:),dwk(:),irfor(i,:))   ! - categorise forecasts -
        prog=prog+dprog
     END DO
     dprog=dprog*REAL(mya,KIND=rp)
  END SELECT
!
! Recalculate climatologies
  rclim(0:,:)=zero
  DO i=1,mya
     DO k=1,nur
        rclim(i,irobs(i,k))=rclim(i,irobs(i,k))+one
     END DO
     rclim(i,:)=rclim(i,:)/REAL(nur,KIND=rp)
     rclim(0,:)=rclim(0,:)+rclim(i,:)
  END DO
  rclim(0,:)=rclim(0,:)/REAL(mya,KIND=rp)
!
  RETURN
 END SUBROUTINE set_ra_categories
!
!
!
 SUBROUTINE categorise_abs (n,nts,x,t,icat)
!
! Categorises data given threshold values
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n   ! - number of cases -
  INTEGER, INTENT(IN) :: nts ! - number of thresholds -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:) ! - data to be categorised -
  REAL(KIND=rp), INTENT(IN) :: t(:) ! - absolute thresholds -
!
! Output arrays
  INTEGER, INTENT(OUT) :: icat(:) ! - categories -
!
! Locals
!
! Local scalars
  INTEGER :: k ! - case index -
  INTEGER :: j ! - category index -
!
! Executable Statements
!
! Determine category
  DO k=1,n
     icat(k)=1
     DO j=1,nts
        IF (t(j)>x(k)) EXIT
        icat(k)=icat(k)+1
     END DO
  END DO
!
  RETURN
 END SUBROUTINE categorise_abs
!
!
!
 SUBROUTINE categorise_per (n,nts,x,p,nc,c,icat)
!
! Categorises data given percentile thresholds
!
! Modules
  USE arrays, ONLY: quantile
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n   ! - number of cases -
  INTEGER, INTENT(IN) :: nts ! - number of thresholds -
  INTEGER, INTENT(IN) :: nc  ! - length of reference climatology -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:) ! - data to be categorised -
  REAL(KIND=rp), INTENT(IN) :: p(:) ! - percentiles -
  REAL(KIND=rp), INTENT(IN) :: c(:) ! - reference climatology (sorted in ascending order) -
!
! Output arrays
  INTEGER, INTENT(OUT) :: icat(:) ! - categories -
!
! Locals
!
! Local arrays
  REAL(KIND=rp) :: t(nts)
!
! Local scalars
  INTEGER :: j ! - category index -
!
! Executable Statements
!
! Set thresholds
  DO j=1,nts
     t(j)=quantile(c,nc,p(j))
  END DO
!
! Determine category
  CALL categorise_abs (n,nts,x,t,icat)
!
  RETURN
 END SUBROUTINE categorise_per
!
!
!
 SUBROUTINE init_climate ()
!
! Initialises climatological period
!
! Executable Statements
!
! Initialise climatological period
  climate_per%it1=0     ! - first date of climatology -
  climate_per%it2=0     ! - last date of climatology -
  climate_per%ic1=0     ! - index of first date of climatological period -
  climate_per%ic2=0     ! - index of last date of climatological period -
  climate_per%nc=0      ! - length of climatological period -
  climate_per%lsn=0     ! - length of season -
  climate_per%clim1=' ' ! - first date of climatology -
  climate_per%clim2=' ' ! - last date of climatology -
!
  RETURN
 END SUBROUTINE init_climate
!
!
!
 FUNCTION climatology()
!
! Redefines climatological period for forecasts
!
! Modules
  USE analysis, ONLY: prog,dprog,icalc,ifc
  USE arrays,   ONLY: y,clim,ave,sdev
  USE iofiles,  ONLY: yfile
  USE labels,   ONLY: cg_climate,cg_done
  USE numbers,  ONLY: zero,one
  USE settings, ONLY: igauss,nt,mya
!
! Function type
  INTEGER :: climatology
!
! Locals
!
! Local scalars
  INTEGER :: iy2 ! - year in two-digit format -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MOD
  INTRINSIC REAL
  INTRINSIC TRIM
!
! Executable Statements
!
! Set default climatology if necesssary
  climatology=2
  climate_bak=climate_per
  IF (climate_per%it1==0) climate_per%it1=yfile%fdate
  IF (climate_per%it2==0) climate_per%it2=yfile%fdate+(nt-1)
!
! Prompt for climatological period
  WRITE (UNIT=*,FMT=*)
!  WRITE (UNIT=*,FMT='(A)') 'Climatological Period'
  WRITE (UNIT=*,FMT='(A)') TRIM(cg_climate)
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') 'Please specify climatological period:'
  IF (yfile%period1%edate%iyr/=yfile%period1%sdate%iyr) THEN
     WRITE (UNIT=*,FMT='(A)') 'Note that the year should be for the first month of the season.'
     iy2=MOD(yfile%fdate%iyr+1,100)
     WRITE (UNIT=*,FMT='(3A,I4,A,I2.2,A,I4)') 'For example, for ',yfile%cssn,' ',yfile%fdate%iyr,'/',iy2,' enter ',yfile%fdate%iyr
  END IF
  WRITE (UNIT=*,FMT='(A,2(I4,A))') '(Data limits are ',yfile%fdate%iyr,' to ',yfile%fdate%iyr+(nt-1),')'
1 WRITE (UNIT=*,FMT='(A)',ADVANCE='no') 'First year: '
  READ (UNIT=*,FMT=*,ERR=1) climate_per%it1%iyr
  IF ((climate_per%it1%iyr<yfile%fdate%iyr).OR.(climate_per%it1%iyr>yfile%fdate%iyr+(nt-1))) GOTO 1
2 WRITE (UNIT=*,FMT='(A)',ADVANCE='no') 'Last year: '
  READ (UNIT=*,FMT=*,ERR=2) climate_per%it2%iyr
  IF ((climate_per%it2%iyr<climate_per%it1%iyr).OR.(climate_per%it2%iyr>yfile%fdate%iyr+(nt-1))) GOTO 2
! 
! Reset climatological period
  IF (climate_per/=climate_bak) THEN
! - set unused month and day for yearly sequencing -
     IF (yfile%iseq==1) THEN
        climate_per%it1%imn=yfile%period1%sdate%imn
        climate_per%it1%idy=0
        climate_per%it2%imn=yfile%period1%sdate%imn
        climate_per%it2%idy=0
     END IF
!
! Recalculate climatology
     IF (icalc==0) RETURN
     prog=zero
     SELECT CASE (ithr)
      CASE (1,3)
        dprog=one/REAL(3+igauss,KIND=rp)
      CASE (2)
        dprog=one/REAL(3+igauss-1,KIND=rp)
     END SELECT
     WRITE (UNIT=*,FMT=*)
     WRITE (UNIT=*,FMT='(A)') 'Calculating climatologies ...'
     CALL set_climate ()
     CALL calc_climate (.true.,mya,y,climate_per,ave,sdev,clim=clim)
!
! Calculate thresholds
     SELECT CASE (ithr)
      CASE (1)
        CALL set_thresholds (igauss)
      CASE (2)
        CALL set_percentiles ()
        CALL set_thresholds (igauss)
      CASE (3)
        CALL set_percentiles ()
     END SELECT
     ifc=1
     prog=one
     WRITE (UNIT=*,FMT='(A)') TRIM(cg_done)//'!' 
!
! Restore old settings
  ELSE
     climate_per=climate_bak
  END IF
!
  RETURN
 END FUNCTION climatology
!
!
!
 FUNCTION check_climate()
!
! Checks that end of climatological period is later than beginning
!
! Modules
  USE errors,   ONLY: error
  USE iofiles,  ONLY: yfile
  USE settings, ONLY: nt
!
! Function type
  INTEGER :: check_climate
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
!
! Executable Statements
!
! Check for valid settings
  IF (climate_per%it2<=climate_per%it1) THEN
     climate_per%it2=yfile%period1%sdate+(yfile%it1+nt-2)
     ifail=1
     CALL error ('check_climate',ifail, &
          i_arg1=yfile%iseq)
     check_climate=2
  ELSE
     check_climate=-1
  END IF
!
  RETURN
 END FUNCTION check_climate
!
!
!
 SUBROUTINE check_climates ()
!
! Checks validity of climate period
!
! Modules
  USE iofiles,  ONLY: yfile
  USE settings, ONLY: nt
!
! Executable Statements
!
! Check for valid settings
  IF (climate_per%nc<=1) THEN
     climate_per%it1=yfile%fdate
     climate_per%it2=yfile%fdate+(nt-1)
  ELSE
! - identify time indices -
     IF (date_diff(yfile%fdate,climate_per%it1,yfile%iseq)<0) climate_per%it1=yfile%fdate
     IF (climate_per%ic2>nt) climate_per%it2=yfile%fdate+(nt-1)
  END IF
  CALL set_climate ()
  IF (climate_per%nc<=1) THEN
     climate_per%it1=yfile%fdate
     climate_per%it2=yfile%fdate+(nt-1)
     CALL set_climate ()
  END IF
!
  RETURN
 END SUBROUTINE check_climates
!
!
!
 SUBROUTINE set_climate ()
!
! Sets climatological parameters given date ranges
!
! Modules
  USE arrays,  ONLY: kuse
  USE iofiles, ONLY: yfile
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC COUNT
!
! Executables Statements
!
! !dentify year indices
  climate_per%ic1=date_diff(yfile%fdate,climate_per%it1,yfile%iseq)+1
  climate_per%ic1=climate_per%ic1-COUNT(.NOT.kuse(1:climate_per%ic1))
  IF (climate_per%ic1==0) climate_per%ic1=1
  climate_per%ic2=date_diff(yfile%fdate,climate_per%it2,yfile%iseq)+1
  climate_per%ic2=climate_per%ic2-COUNT(.NOT.kuse(1:climate_per%ic2))
  climate_per%nc=climate_per%ic2+1-climate_per%ic1
!
! Calculate length of season
  SELECT CASE (iseq)
   CASE (1,2)
     climate_per%lsn=date_diff(yfile%period1%sdate,yfile%period1%edate,2)+1
   CASE (3)
     climate_per%lsn=date_diff(yfile%period1%sdate,yfile%period1%edate,3)+1
  END SELECT
!
! Construct climatological period labels
  climate_per%clim1=get_cdate(climate_per%it1,climate_per%lsn,2)
  climate_per%clim2=get_cdate(climate_per%it2,climate_per%lsn,2)
!
  RETURN
 END SUBROUTINE set_climate
!
!
!
 SUBROUTINE calc_climate (lstd,m,y,climate_per,ybar,ystd,clim)
!
! Calculates mean and variance for a climate period
!
! Modules
  USE analysis, ONLY: prog,dprog
  USE arrays,   ONLY: yhat,yret,yrpls, &
                      insertion_sort,stdize,ustdize
  USE numbers,  ONLY: zero,one
  USE settings, ONLY: istd,iretro,nu,nur
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: m ! - number of variables -
!
  LOGICAL, INTENT(IN) :: lstd ! - recalculate climatology flag -
!
  TYPE(clim_per), INTENT(IN) :: climate_per ! - climatological period -
!
! Input/output arrays
  REAL(KIND=rp), INTENT(INOUT) :: y(:,:) ! - data -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: ybar(:) ! - climatological mean -
  REAL(KIND=rp), INTENT(OUT) :: ystd(:) ! - climatological standard deviation -
!
  REAL(KIND=rp), INTENT(OUT), OPTIONAL :: clim(:,:) ! - climatological (sorted) data -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - variable index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SQRT
  INTRINSIC SUM
!
! Executable Statements
!
! Unstandardise, if requested
  IF ((lstd).AND.(istd/=0)) THEN
     CALL ustdize (m,nu,y,ybar,ystd,istd)
     CALL ustdize (m,nu,yhat,ybar,ystd,istd)
     IF (iretro==1) THEN
        CALL ustdize (m,nur,yret,ybar,ystd,istd)
        CALL ustdize (m,nur,yrpls(:,:,1),ybar,ystd,istd)
        CALL ustdize (m,nur,yrpls(:,:,2),ybar,ystd,istd)
     END IF
  END IF
!
! Calculate climatological mean
  dprog=dprog/REAL(m,KIND=rp)
  DO i=1,m
     ybar(i)=SUM(y(i,climate_per%ic1:climate_per%ic2))/REAL(climate_per%nc,KIND=rp)
!
! Calculate climatological standard deviation
     IF (climate_per%nc>1) THEN
        ystd(i)=SUM((y(i,climate_per%ic1:climate_per%ic2)-ybar(i))**2)/REAL(climate_per%nc-1,KIND=rp)
        IF (ystd(i)>zero) THEN
           ystd(i)=SQRT(ystd(i))
        ELSE
           ystd(i)=one
        END IF
     ELSE
        ystd(i)=one
     END IF
!
! Create climatology
     clim(i,1:climate_per%nc)=y(i,climate_per%ic1:climate_per%ic2)
     CALL insertion_sort (clim(i,:),climate_per%nc,'a')
!
! Update progress meter
     prog=prog+dprog
  END DO
  dprog=dprog*REAL(m,KIND=rp)
!
! Restandardise, if requested
  IF ((lstd).AND.(istd/=0)) THEN
     CALL stdize (m,nu,y,ybar,ystd,istd)
     CALL stdize (m,climate_per%nc,clim,ybar,ystd,istd)
     CALL stdize (m,nu,yhat,ybar,ystd,istd)
     IF (iretro==1) THEN
        CALL stdize (m,nur,yret,ybar,ystd,istd)
        CALL stdize (m,nur,yrpls(:,:,1),ybar,ystd,istd)
        CALL stdize (m,nur,yrpls(:,:,2),ybar,ystd,istd)
     END IF
  END IF
!
  RETURN
 END SUBROUTINE calc_climate
!
!
!
 SUBROUTINE calc_probs (igauss,nt,nf,ng,nx,ny,y,yhat,xvp,fcast,tobst,pobs,clf,fps,odds,oddr,fpls, &
            nens,fens,nc,clim,pev)
!
! Calculates forecast probabilities and prediction intervals
!
! Modules
  USE analysis, ONLY: ianal,prog,dprog
  USE arrays,   ONLY: ave,sdev,dwk                      
  USE distribs, ONLY: gaussian_inv,studnt,studnt2
  USE numbers,  ONLY: zero,eps,one,two,oneh
  USE settings, ONLY: izero,istd,dofr
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: igauss ! - transform to gaussian flag -
  INTEGER, INTENT(IN) :: nt     ! - number of cases in training period -
  INTEGER, INTENT(IN) :: nf     ! - number of forecasts -
  INTEGER, INTENT(IN) :: ng     ! - number of categories -
  INTEGER, INTENT(IN) :: nx     ! - number of predictors -
  INTEGER, INTENT(IN) :: ny     ! - number of predictands -
!
  INTEGER, INTENT(IN), OPTIONAL :: nc   ! - number of years in climatology -
  INTEGER, INTENT(IN), OPTIONAL :: nens ! - number of ensemble members -
!
  REAL(KIND=rp), INTENT(IN) :: clf ! - forecast confidence level -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: y(:,:)     ! - training period data -
  REAL(KIND=rp), INTENT(IN) :: yhat(:,:)  ! - estimated values -
  REAL(KIND=rp), INTENT(IN) :: xvp(:,:)   ! - predictors over forecast period -
  REAL(KIND=rp), INTENT(IN) :: fcast(:,:) ! - forecast values -
  REAL(KIND=rp), INTENT(IN) :: tobst(:,:) ! - thresholds -
  REAL(KIND=rp), INTENT(IN) :: pobs(:,:)  ! - climatological percentile thresholds -
!
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: clim(:,:) ! - climatology -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: fps(:,:,:)  ! - forecast probabilities -
  REAL(KIND=rp), INTENT(OUT) :: odds(:,:,:) ! - odds -
  REAL(KIND=rp), INTENT(OUT) :: oddr(:,:,:) ! - odds relative to climatology -
  REAL(KIND=rp), INTENT(OUT) :: fpls(:,:,:) ! - prediction limits -
!
  REAL(KIND=rp), INTENT(OUT), OPTIONAL :: fens(:,:,:) ! - ensemble forecasts -
  REAL(KIND=rp), INTENT(OUT), OPTIONAL :: pev(:,:)    ! - prediction error standard deviations -
!
! Locals
!
! Local scalars
  INTEGER :: i  ! - variable index -
  INTEGER :: j  ! - category index -
  INTEGER :: k  ! - forecast index -
  INTEGER :: l  ! - category index -
  INTEGER :: ii ! - variable index -
  INTEGER :: ie ! - ensemble member index -
!
  REAL(KIND=rp) :: t     ! - Student's t-deviate -
  REAL(KIND=rp) :: t0    ! - lower tail Student's t-deviate -
  REAL(KIND=rp) :: p     ! - lower tail Student's t-deviate -
  REAL(KIND=rp) :: s2    ! - estimated mean squared error -
  REAL(KIND=rp) :: psd   ! - prediction error standard deviation -
  REAL(KIND=rp) :: szero ! - standardized zero -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC PRESENT
  INTRINSIC REAL
  INTRINSIC SQRT
  INTRINSIC SUM
!
! Executable Statements
!
! Calculate critical t statistic
  dofr=REAL(nt-nx-1,KIND=rp)
  IF (dofr>zero) THEN
     t0=studnt2(one-clf/oneh,dofr)
  ELSE
     t0=zero
  END IF
!
! Calculate goodness of fit statistics
  dprog=dprog/REAL(ny,KIND=rp)
  ii=1
  DO i=1,ny
     IF (dofr>zero) s2=SUM((y(i,1:nt)-yhat(i,1:nt))**2)/dofr
!
! Calculate forecast probabilities
     DO k=1,nf
        IF (dofr>zero) THEN
           psd=SQRT(s2*(one+xvp(ii,k)))
        ELSE
           psd=zero
        END IF
        IF (psd>eps) THEN
           DO j=1,ng-1
              t=(tobst(i,j)-fcast(i,k))/psd
              fps(i,k,j+1)=studnt(t,dofr)*oneh
           END DO
           fps(i,k,1)=oneh
           DO j=1,ng-1
              fps(i,k,j)=fps(i,k,j)-fps(i,k,j+1)
           END DO
        ELSE
           l=1
           DO j=1,ng-1
              IF (fcast(i,k)>tobst(i,j)) THEN
                 l=l+1
              ELSE
                 EXIT
              END IF
           END DO
           fps(i,k,l)=oneh
        END IF
!
! Calculate odds
        DO j=1,ng
           IF ((fps(i,k,j)>zero).AND.(fps(i,k,j)<oneh)) THEN
              odds(i,k,j)=fps(i,k,j)/(oneh-fps(i,k,j))
           ELSE
              odds(i,k,j)=-one
           END IF
!
! Calculate odds relative to climatology
           IF ((pobs(i,j)>zero).AND.(pobs(i,j)<one)) THEN
              oddr(i,k,j)=odds(i,k,j)/(pobs(i,j)/(one-pobs(i,j)))
           ELSE
              oddr(i,k,j)=-one
           END IF
        END DO
!
! Calculate confidence limits
        t=t0*psd
        fpls(i,k,1)=fcast(i,k)-t
        fpls(i,k,2)=fcast(i,k)+t
        IF (PRESENT(pev)) pev(i,k)=psd
        IF (PRESENT(nens)) THEN
           DO ie=nens/2+1,nens
              p=one-REAL(ie,KIND=rp)/REAL(nens+1,KIND=rp)
              IF (dofr>zero) THEN
                 t=studnt2(two*p,dofr)*psd
              ELSE
                 t=zero
              END IF
              fens(i,k,ie)=fcast(i,k)+t
              fens(i,k,nens+1-ie)=fcast(i,k)-t
           END DO
        END IF
!
! Update progress meter
        prog=prog+dprog
     END DO
     IF (ianal==4) ii=ii+1
  END DO
!
! Transform if necessary
  IF (igauss==1) THEN
     DO i=1,ny
        dwk(1:nc)=clim(i,1:nc)
        DO ie=1,2
           CALL gaussian_inv (nf,fpls(i,:,ie),nt,dwk(1:nc))
        END DO
        IF (PRESENT(fens)) THEN
           DO ie=1,nens
              CALL gaussian_inv (nf,fens(i,:,ie),nt,dwk(1:nc))
           END DO
        END IF
        prog=prog+dprog
     END DO
  END IF
  dprog=dprog*REAL(ny,KIND=rp)
!
! Apply zero-bound
  IF (izero==1) THEN
     SELECT CASE (istd)
      CASE (0,3)
        WHERE (fpls(:,:,:)<zero) fpls(:,:,:)=zero
        IF (PRESENT(fens)) THEN
           WHERE (fens(:,:,:)<zero) fens(:,:,:)=zero
        END IF
      CASE (1)
        DO i=1,ny
           szero=-ave(i)
           WHERE (fpls(:,:,:)<szero) fpls(:,:,:)=szero
           IF (PRESENT(fens)) THEN
              WHERE (fens(:,:,:)<szero) fens(:,:,:)=zero
           END IF
        END DO
      CASE (2)
        DO i=1,ny
           szero=-ave(i)/sdev(i)
           WHERE (fpls(:,:,:)<szero) fpls(:,:,:)=szero
           IF (PRESENT(fens)) THEN
              WHERE (fens(:,:,:)<szero) fens(:,:,:)=zero
           END IF
        END DO
     END SELECT
     prog=prog+dprog
  END IF
!
  RETURN
 END SUBROUTINE calc_probs
END MODULE categories
