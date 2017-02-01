! $Id: pcr.f90 1215 2011-02-25 21:30:20Z simon $
MODULE pcr
!
! Modules
  USE analysis, ONLY: prog,dprog
  USE numbers,  ONLY: rp
  USE time
!
! Implicit declarations
  IMPLICIT NONE
!
CONTAINS
!
!
 FUNCTION perform_pcr()
!
! Performs PCR.
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Memory allocation problem
!    ifail =  2 Calculation error
!    ifail = -1 Terminated
!
! Modules
  USE analysis,      ONLY: ianal,nopt, &
                           init_analysis1,init_analysis2,init_results,num_calcs
  USE arrays,        ONLY: x,y,clim,ave,sdev,svx,eofx,tsx,yhat,yret,yrpls,bz,b,b0,rfps,rodds,roddr, &
                           yopt,yt,yhatt,yrett,tobst,pobs,xvp,xm,ym,kuse,                           &
                           stdize
  USE categories,    ONLY: ithr,        &
                           climate_per, &
                           set_cv_categories,set_ra_categories,set_thresholds,set_percentiles,calc_climate,calc_probs
  USE CPT_constants, ONLY: ng,nts
  USE fields,        ONLY: xfield,yfield
  USE IO_constants,  ONLY: lprd
  USE iofiles,       ONLY: yfile
  USE labels,        ONLY: cg_to_l,cg_tperiod
  USE numbers,       ONLY: zero,one
  USE pcs,           ONLY: ieofx,mxe,nxe,nxo,npx
  USE settings,      ONLY: iretro,nretro,igauss,izero,istd,iev,nu,nu1,nur,nt,nt1,nret,mxa,mya,lcw,ncv,clf
!
! Function type
  INTEGER :: perform_pcr
!
! Locals
!
! Local scalars
  INTEGER :: irl   ! - index of last retroactive training-period year -
  INTEGER :: ir1   ! - index of first retroactive year -
  INTEGER :: irn   ! - index of last retroactive year -
  INTEGER :: iru   ! - index of last new year in retroactive training period -
  INTEGER :: nr    ! - total number of retroaoctive steps in current cycle -
  INTEGER :: iopt  ! - optimization indicator -
  INTEGER :: ifail ! - error indicator -
!
  CHARACTER(LEN=lprd) :: cprd1 ! - period -
  CHARACTER(LEN=lprd) :: cprd2 ! - period -
  CHARACTER(LEN=lprd) :: cprd3 ! - period -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MATMUL
  INTRINSIC MAX
  INTRINSIC MAXVAL
  INTRINSIC MIN
  INTRINSIC REAL
  INTRINSIC TRIM
!
! Executable Statements
!
! Initialise progress meter
  perform_pcr=-1
  prog=zero
!
! Calculate number of optimization steps
  SELECT CASE (ianal)
   CASE (2) ! - PCR -
     nopt=mxe+1-nxe
   CASE (3) ! - MLR -
     nopt=1
  END SELECT
!
! Initialise analysis
  IF (init_analysis1(ng,nts,nopt)/=0) THEN
     perform_pcr=1
     RETURN
  END IF
!
! Calculate optimization settings
  SELECT CASE (ianal)
   CASE (2) ! - PCR -
     IF (nopt==1) THEN
        nopt=0
        iopt=0
        nxo=nxe
     ELSE
        iopt=1
     END IF
   CASE (3) ! - MLR -
     nopt=0
     iopt=0
     nxe=MIN(mxa,nu-1)
     mxe=nxe
  END SELECT
!
! Estimate number of calculations
  IF (iev/=3) THEN
     dprog=one/REAL(num_calcs(ianal,iretro,igauss,izero,0,mya,nu,nret,nretro,nu1,nur,iopt,ithr),KIND=rp)
  ELSE
     dprog=one/REAL(num_calcs(ianal,iretro,igauss,izero,1,mya,nu,nret,nretro,nu1,nur,iopt,ithr),KIND=rp)
  END IF
!
! Calculate climatologies
  WRITE (UNIT=*,FMT='(A)') 'Calculating climatologies and thresholds ...'
  CALL calc_climate (.false.,mya,y,climate_per,ave,sdev,clim=clim)
!
! Standardise
  CALL stdize (mya,nu,y,ave,sdev,istd)
  CALL stdize (mya,climate_per%nc,clim,ave,sdev,istd)
!
! Calculate thresholds
  SELECT CASE (ithr)
   CASE (1,2)
     CALL set_percentiles ()
     CALL set_thresholds (igauss)
   CASE (3)
     CALL set_thresholds (0)
     CALL set_percentiles ()
  END SELECT
!
! Perform retroactive calculations
  cprd1=get_cdate(yfile%period1+(yfile%it1-1),2)
! - optimise model for successive training periods -
  IF (iretro==1) THEN
     WRITE (UNIT=*,FMT='(A)') 'Calculating retroactive forecasts ...'
     ir1=1
     irn=0
     iru=nu1
     retro: DO irl=nt1,nt-1,nretro
        irn=irn+COUNT(kuse(irl+1:MIN(irl+nretro,nt)))
        nr=irn+1-ir1
        IF (nr>0) THEN
           ncv=iru-lcw
           cprd2=get_cdate(yfile%period1+(yfile%it1+irl-2),2)
           WRITE (UNIT=*,FMT='(A)') TRIM(cg_tperiod)//': '//TRIM(cprd1)//' '//TRIM(cg_to_l)//' '//TRIM(cprd2)
           WRITE (UNIT=*,FMT='(A)') ' '
           IF (nopt>0) THEN
              SELECT CASE (igauss)
               CASE (0)
                 CALL cv_pcr (iru,ncv,mxa,x,mya,y,ieofx,nxe,mxe,yhat=yhat,nxo=nxo)
               CASE (1)
                 CALL cv_pcr (iru,ncv,mxa,x,mya,y,ieofx,nxe,mxe,yhatt=yhat,nxo=nxo)
              END SELECT
           ELSE
              SELECT CASE (igauss)
               CASE (0)
                 CALL cv_pcr (iru,ncv,mxa,x,mya,y,ieofx,nxe,mxe,yhat=yhat)
               CASE (1)
                 CALL cv_pcr (iru,ncv,mxa,x,mya,y,ieofx,nxe,mxe,yhatt=yhat)
              END SELECT
              IF (ianal==3) nxo=nxe
           END IF
! - construct full model -
           SELECT CASE (igauss)
            CASE (0)
              CALL full_pcr (iru,mxa,x,mya,y,ieofx,nxo,svx,eofx,tsx,bz,b,npx,ifail)
            CASE (1)
              CALL full_pcr (iru,mxa,x,mya,y,ieofx,nxo,svx,eofx,tsx,bz,b,npx,ifail,yt=yopt(:,:,2))
           END SELECT
           IF (ifail/=0) THEN
              perform_pcr=2
              RETURN
           END IF
           IF (nxo>npx) nxo=npx
! - produce retroactive forecast -
           cprd3=get_cdate(yfile%period1+(yfile%it1+irl-1),2)
           IF (nr>1) THEN
              cprd2=get_cdate(yfile%period1+(yfile%it1+irl+nr-2),2)
              WRITE (UNIT=*,FMT='(A)') &
                    'Calculating retroactive forecasts for '//TRIM(cprd3)//' - '//TRIM(cprd2)
           ELSE
              WRITE (UNIT=*,FMT='(A,I4,A)') 'Calculating retroactive forecasts for '//TRIM(cprd3)
           END IF
           SELECT CASE (igauss)
            CASE (0)
              CALL pcr_predict (nr,mxa,mya,ieofx,x(1:mxa,nu1+ir1:nu1+irn),b,yret(:,ir1:irn), &
                   nt=iru,nxe=nxo,xvp=xvp(1,ir1:irn))
            CASE (1)
              CALL pcr_predict (nr,mxa,mya,ieofx,x(1:mxa,nu1+ir1:nu1+irn),b,yret(:,ir1:irn), &
                   nt=iru,nxe=nxo,xvp=xvp(1,ir1:irn),nc=iru,clim=y(:,1:iru),fcastt=yrett(:,ir1:irn))
           END SELECT
           IF (iev==3) CALL pcr_predict (iru,mxa,mya,ieofx,x(1:mxa,1:iru),b,yhat(:,:))
! - calculate probabilities -
           SELECT CASE (igauss)
            CASE (0)
              CALL calc_probs (igauss,iru,nr,ng,nxo,mya,y,yhat,xvp(1:1,ir1:irn),yret(:,ir1:irn),tobst, &
                   pobs,clf,rfps(:,ir1:irn,:),rodds(:,ir1:irn,:),roddr(:,ir1:irn,:),yrpls(:,ir1:irn,:))
            CASE (1)
              CALL calc_probs (igauss,iru,nr,ng,nxo,mya,yopt(:,:,2),yhat,xvp(1:1,ir1:irn),yrett(:,ir1:irn),   &
                   tobst,pobs,clf,rfps(:,ir1:irn,:),rodds(:,ir1:irn,:),roddr(:,ir1:irn,:),yrpls(:,ir1:irn,:), &
                   nc=iru,clim=y(1:mya,1:iru))
           END SELECT
           WRITE (UNIT=*,FMT='(A)') ' '
        END IF
! - update indices -
        iru=nu1+irn
        ir1=irn+1
     END DO retro
  END IF
!
! Optimise PCR using full training period
  IF (ianal==3) nxo=nxe
  cprd2=get_cdate(yfile%period1+(yfile%it1+nt-2),2)
  ncv=nu-lcw
  IF (nopt>0) THEN
     WRITE (UNIT=*,FMT='(A)') 'Optimizing cross-validated performance ...'
     WRITE (UNIT=*,FMT='(A)') TRIM(cg_tperiod)//': '//TRIM(cprd1)//' '//TRIM(cg_to_l)//' '//TRIM(cprd2)
     WRITE (UNIT=*,FMT='(A)') ' '
     SELECT CASE (igauss)
      CASE (0)
        CALL cv_pcr (nu,ncv,mxa,x,mya,y,ieofx,nxe,mxe,yhat=yhat,nxo=nxo)
      CASE (1)
        CALL cv_pcr (nu,ncv,mxa,x,mya,y,ieofx,nxe,mxe,yhat=yhat,yhatt=yhatt,nxo=nxo)
     END SELECT
!
! Cross-validate optimal / chosen model
  ELSE
     WRITE (UNIT=*,FMT='(A)') 'Cross-validating model ...'
     SELECT CASE (igauss)
      CASE (0)
        CALL cv_pcr (nu,ncv,mxa,x,mya,y,ieofx,nxo,nxo,yhat=yhat)
      CASE (1)
        CALL cv_pcr (nu,ncv,mxa,x,mya,y,ieofx,nxo,nxo,yhat=yhat,yhatt=yhatt)
     END SELECT
  END IF
!
! Allocate additional memory
  IF (init_analysis2()/=0) THEN
     perform_pcr=1
     RETURN
  END IF
!
! Fit model using all data
  WRITE (UNIT=*,FMT='(A)') &
     'Constructing model using full training period ('//TRIM(cprd1)//' '//TRIM(cg_to_l)//' '//TRIM(cprd2)//') ...'
  SELECT CASE (igauss)
   CASE (0)
     CALL full_pcr (nu,mxa,x,mya,y,ieofx,nxo,svx,eofx,tsx,bz,b,npx,ifail)
   CASE (1)
     CALL full_pcr (nu,mxa,x,mya,y,ieofx,nxo,svx,eofx,tsx,bz,b,npx,ifail,yt=yt)
  END SELECT
  IF (ifail/=0) THEN
     perform_pcr=2
     RETURN
  END IF
!
! Calculate regression constant for MLR
  IF (ianal==3) b0(1:mya)=ym(1:mya)-MATMUL(xm(1:mxa),b(1:mxa,1:mya))
!
! Allocate additional memory
  IF (init_results(MAX(MAXVAL(xfield(:)%nlt),MAXVAL(yfield(:)%nlt)),MAX(MAXVAL(xfield(:)%nlg),MAXVAL(yfield(:)%nlg)),ng)/=0) THEN
     perform_pcr=1
     RETURN
  END IF
!
! Determine forecast categories
  WRITE (UNIT=*,FMT='(A)') 'Identifying categories ...'
  dprog=(one-prog)/REAL(1+iretro,KIND=rp) ! - reset progress to minimise floating point errors -
  CALL set_cv_categories ()
  IF (iretro==1) CALL set_ra_categories ()
  perform_pcr=0
!
  RETURN
 END FUNCTION perform_pcr
!
!
!
 SUBROUTINE full_pcr (nt,nx,x,ny,y,ieofx,nxe,svx,eofx,tsx,bz,b,npx,ifail,yt)
!
! Performs PCR using all data
!
! Modules
  USE arrays,     ONLY: iusex,xm,ym,xsd,ysd,xc,yc,dwk,iwk, &
                        stdize
  USE distribs,   ONLY: gaussian
  USE fields,     ONLY: xfield,rlatx, &
                        latitude_weight
  USE iofiles,    ONLY: xfile
  USE lapack,     ONLY: dgesdd
  USE numbers,    ONLY: zero,four,tol
  USE settings,   ONLY: igauss,lxt,lrwk
  USE statistics, ONLY: moments
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nt    ! - number of cases -
  INTEGER, INTENT(IN) :: nx    ! - number of x spatial points -
  INTEGER, INTENT(IN) :: ny    ! - number of y spatial points -
  INTEGER, INTENT(IN) :: nxe   ! - number of X EOF modes -
  INTEGER, INTENT(IN) :: ieofx ! - X EOF option -
!
! Output scalars
  INTEGER, INTENT(OUT) :: npx   ! - number of positive eigenvalues -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:,:) ! - explanatory variables -
  REAL(KIND=rp), INTENT(IN) :: y(:,:) ! - response variables -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: svx(:)    ! - singular values of x -
  REAL(KIND=rp), INTENT(OUT) :: eofx(:,:) ! - x EOF patterns -
  REAL(KIND=rp), INTENT(OUT) :: tsx(:,:)  ! - time-series of x EOFs (transposed) -
  REAL(KIND=rp), INTENT(OUT) :: bz(:,:)   ! - principal component regression coefficients -
  REAL(KIND=rp), INTENT(OUT) :: b(:,:)    ! - regression coefficients -
!
  REAL(KIND=rp), INTENT(OUT), OPTIONAL :: yt(:,:) ! - transformed response variables -
!
! Locals
!
! Local scalars
  INTEGER :: i  ! - predictor/predictand index -
  INTEGER :: k  ! - case index -
  INTEGER :: ie ! - EOF mode index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ABS
  INTRINSIC COUNT
  INTRINSIC MATMUL
  INTRINSIC MAXVAL
  INTRINSIC MIN
  INTRINSIC MINVAL
  INTRINSIC PRESENT
  INTRINSIC TRANSPOSE
!
! Executable Statements
!
! Adjust progress increment
  dprog=dprog/four
!
! Backup data
  ifail=0
  xc(1:nx,1:nt)=x(1:nx,1:nt)
  SELECT CASE (igauss)
   CASE (0)
     yc(1:ny,1:nt)=y(1:ny,1:nt)
   CASE (1)
     DO i=1,ny
        CALL gaussian (nt,y(i,1:nt),yc(i,1:nt))
     END DO
  END SELECT
!
! Copy transformed data
  IF (PRESENT(yt)) yt(:,1:nt)=yc(:,1:nt)
!
! Calculate means and standard deviations
  CALL moments (nx,nt,xc,xm,xsd)
  CALL moments (ny,nt,yc,ym,ysd)
!
! Calculate anomalies
  CALL stdize (nx,nt,xc,xm,xsd,3-ieofx)
  CALL stdize (ny,nt,yc,ym,ysd,1)
!
! Scale by latitude
  IF (xfile%igrid==1) CALL latitude_weight (xfile%nfs,xfield(:)%nlt,xfield(:)%region,rlatx(:,:),iusex,nt,xc(:,:))
!
! Update progress meter
  prog=prog+dprog
!
! Perform EOF prefiltering
  CALL dgesdd ('S',nx,nt,xc,nx,svx,eofx,nx,tsx,lxt,dwk,lrwk,iwk,ifail)
  IF (ifail/=0) RETURN
!
! Determine number of non-zero eigenvalues
  npx=COUNT(svx(1:MIN(lxt,nt-1))>zero)
  IF (npx>2) THEN
     IF (svx(npx)*svx(npx-2)/svx(npx-1)**2<tol) npx=npx-1 ! - check for probable rounding errors -
  END IF
!
! Rescale loadings
! - rescale loadings by latitude -
  IF (xfile%igrid==1) CALL latitude_weight (xfile%nfs,xfield(:)%nlt,xfield(:)%region,rlatx(:,:),iusex,npx,eofx(:,:))
! - ensure that largest absolute loading is positive -
  DO ie=1,npx
     IF (ABS(MAXVAL(eofx(1:nx,ie)))<ABS(MINVAL(eofx(1:nx,ie)))) THEN
        eofx(1:nx,ie)=-eofx(1:nx,ie)
        tsx(ie,1:nt)=-tsx(ie,1:nt)
     END IF
  END DO
  IF (npx<lxt) THEN
     svx(npx+1:lxt)=zero
     tsx(npx+1:lxt,1:nt)=zero
     eofx(1:nx,npx+1:lxt)=zero
  END IF
!
! Update progress meter
  prog=prog+dprog
!
! Compute regression coefficients
! - rescale EOF time series -
  DO k=1,nt
     DO ie=1,npx
        tsx(ie,k)=tsx(ie,k)/svx(ie)
     END DO
  END DO
! - principal component regression coefficients -
  bz(1:nxe,1:ny)=MATMUL(tsx(1:nxe,1:nt),TRANSPOSE(yc(1:ny,1:nt)))
! - update progress meter -
  prog=prog+dprog
! - regression coefficients -
  b(1:nx,1:ny)=MATMUL(eofx(1:nx,1:nxe),bz(1:nxe,1:ny))
! - rescale EOF time series -
  DO k=1,nt
     DO ie=1,npx
        tsx(ie,k)=tsx(ie,k)*svx(ie)**2
     END DO
  END DO
! - update progress meter -
  prog=prog+dprog
!
! Reset progress increment
  dprog=dprog*four
!
  RETURN
 END SUBROUTINE full_pcr
!
!
!
 SUBROUTINE cv_pcr (nt,ncv,nx,x,ny,y,ieofx,nxe,mxe,yhat,yhatt,nxo)
!
! Performs cross-validated PCR
!
! Modules
  USE analysis,   ONLY: nopt
  USE arrays,     ONLY: iusex,svx,eofx,tsx,bz,b,ave,sdev,xm,ym,xsd,xc,yc,yopt,dwk,iwk, &
                        stdize
  USE distribs,   ONLY: gaussian,gaussian_inv
  USE fields,     ONLY: xfield,rlatx, &
                        latitude_weight
  USE iofiles,    ONLY: xfile
  USE lapack,     ONLY: dgesdd
  USE numbers,    ONLY: zero,one,tol
  USE pcs,        ONLY: npx
  USE settings,   ONLY: igauss,izero,istd,igood,hcw,lxt,lrwk
  USE statistics, ONLY: goodness,moments
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nt    ! - number of cases -
  INTEGER, INTENT(IN) :: ncv   ! - length of cross-validated training period -
  INTEGER, INTENT(IN) :: nx    ! - number of x spatial points -
  INTEGER, INTENT(IN) :: ny    ! - number of y spatial points -
  INTEGER, INTENT(IN) :: nxe   ! - minimum number of X EOF modes -
  INTEGER, INTENT(IN) :: mxe   ! - maximum number of X EOF modes -
  INTEGER, INTENT(IN) :: ieofx ! - X EOF option -
!
! Output scalars
! - optional output scalars -
  INTEGER, INTENT(OUT), OPTIONAL :: nxo ! - optimal number of X EOF modes -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:,:) ! - explanatory variables -
  REAL(KIND=rp), INTENT(IN) :: y(:,:) ! - response variables -
!
! Output arrays
! - optional output arrays -
  REAL(KIND=rp), INTENT(OUT), OPTIONAL :: yhat(:,:)  ! - cross-validated hindcasts -
  REAL(KIND=rp), INTENT(OUT), OPTIONAL :: yhatt(:,:) ! - transformed cross-validated hindcasts -
!
! Locals
!
! Local scalars
  INTEGER :: it    ! - cross-validation time-step -
  INTEGER :: i     ! - predictor/predictand index -
  INTEGER :: k     ! - case index -
  INTEGER :: i1,i2 ! - cross-validation indices -
  INTEGER :: ixe   ! - X EOF mode index -
  INTEGER :: ixu   ! - used X EOF mode index -
  INTEGER :: iopt  ! - optimization step index -
  INTEGER :: jopt  ! - index of optimized settings -
  INTEGER :: ifail ! - error indicator -
!
  REAL(KIND=rp) :: gm    ! - goodness metric -
  REAL(KIND=rp) :: gmo   ! - optimal goodness metric -
  REAL(KIND=rp) :: szero ! - standardized zero -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC COUNT
  INTRINSIC MATMUL
  INTRINSIC MIN
  INTRINSIC PRESENT
  INTRINSIC TRANSPOSE
!
! Executable Statements
!
! Adjust progress meter
  IF (nopt>0) dprog=dprog/REAL(nopt,KIND=rp)
!
! Initialise cross-validation window
  i1=hcw
  i2=hcw+ncv-1
!
! Cross-validate
  time_step: DO it=1,nt
     i1=i1+1
     IF (i1==nt+1) i1=1
     i2=i2+1
     IF (i2==nt+1) i2=1
     IF (i1<i2) THEN
        xc(1:nx,1:ncv)=x(1:nx,i1:i2)
        yc(1:ny,1:ncv)=y(1:ny,i1:i2)
     ELSE
        xc(1:nx,1:i2)=x(1:nx,1:i2)
        xc(1:nx,i2+1:ncv)=x(1:nx,i1:nt)
        yc(1:ny,1:i2)=y(1:ny,1:i2)
        yc(1:ny,i2+1:ncv)=y(1:ny,i1:nt)
     END IF
!
! Transform
     IF (igauss==1) THEN
        DO i=1,ny
           dwk(1:ncv)=yc(i,1:ncv)
           CALL gaussian (ncv,dwk(1:ncv),yc(i,1:ncv))
        END DO
     END IF
!
! Calculate means and standard deviations
     CALL moments (nx,ncv,xc,xm,xsd)
     CALL moments (ny,ncv,yc,ym,dwk)
!
! Calculate anomalies
     CALL stdize (nx,ncv,xc,xm,xsd,3-ieofx)
     CALL stdize (ny,ncv,yc,ym,dwk,1)
!
! Scale by latitude
     IF (xfile%igrid==1) CALL latitude_weight (xfile%nfs,xfield(:)%nlt,xfield(:)%region,rlatx(:,:),iusex,ncv,xc(:,:))
!
! Perform EOF prefiltering
     CALL dgesdd ('S',nx,ncv,xc,nx,svx,eofx,nx,tsx,lxt,dwk,lrwk,iwk,ifail)
!
! Compute PCR
     IF (ifail==0) THEN
! - determine number of non-zero eigenvalues -
        npx=COUNT(svx(1:MIN(mxe,ncv-1))>zero)
! - check for probable rounding errors -
        IF (npx>2) THEN
           IF (svx(npx)*svx(npx-2)/svx(npx-1)**2<tol) npx=npx-1
        END IF
! - rescale loadings by latitude -
        IF (xfile%igrid==1) CALL latitude_weight (xfile%nfs,xfield(:)%nlt,xfield(:)%region,rlatx(:,:),iusex,npx,eofx(:,:))
! - rescale EOF time series -
        DO k=1,ncv
           DO ixe=1,npx
              tsx(ixe,k)=tsx(ixe,k)/svx(ixe)
           END DO
        END DO
        IF (npx<mxe) THEN
           svx(npx+1:mxe)=-one
           tsx(npx+1:mxe,1:nt)=zero
           eofx(1:nx,npx+1:mxe)=zero
        END IF
! - principal component regression coefficients -
        bz(1:mxe,1:ny)=MATMUL(tsx(1:mxe,1:ncv),TRANSPOSE(yc(1:ny,1:ncv)))
! - regression coefficients -
        iopt=0
        DO ixe=nxe,mxe
           ixu=MIN(ixe,npx)
           iopt=iopt+1
           b(1:nx,1:ny)=MATMUL(eofx(1:nx,1:ixu),bz(1:ixu,1:ny))
!
! Predict anomaly
           CALL pcr_predict (1,nx,ny,ieofx,x(1:nx,it:it),b,yopt(1:ny,it:it,iopt))
        END DO
!
! Supply mean if PCR failed
     ELSE
        iopt=0
        DO ixe=nxe,mxe
           iopt=iopt+1
           yopt(1:ny,it,iopt)=ym(1:ny)
        END DO
     END IF
  END DO time_step
!
! Adjust progress meter
  IF (nopt>0) dprog=dprog*REAL(nopt,KIND=rp)
!
! Calculate and print goodness metric
  IF (PRESENT(nxo)) THEN
     WRITE (UNIT=*,FMT='(A)') '               CURRENT                         OPTIMUM'
     WRITE (UNIT=*,FMT='(A)') ' '
     WRITE (UNIT=*,FMT='(A)') '      Number of Modes    Goodness      Number of Modes    Goodness'
     WRITE (UNIT=*,FMT='(A)') '                            Index                            Index'
     gmo=-one
     iopt=0
     DO ixe=nxe,mxe
        iopt=iopt+1
        SELECT CASE (igauss)
         CASE (0)
           gm=goodness(igood,nt,ny,yopt(:,:,iopt),y)
         CASE (1)
           DO i=1,ny
              CALL gaussian (nt,y(i,:),yc(i,:))
           END DO
           gm=goodness(igood,nt,ny,yopt(:,:,iopt),yc)
        END SELECT
        IF (gm>gmo) THEN
           jopt=iopt
           gmo=gm
           nxo=ixe
        END IF
        WRITE (UNIT=*,FMT='(2(10X,I5,F18.3))') ixe,gm,nxo,gmo
     END DO
     prog=prog+dprog
  ELSE
     jopt=1
     gm=goodness(igood,nt,ny,yopt(:,:,jopt),y)
     WRITE (UNIT=*,FMT='(A,F18.3)') 'Goodness index: ',gm
  END IF
  IF (PRESENT(yhatt)) yhatt(:,1:nt)=yopt(:,1:nt,jopt)
  IF (PRESENT(yhat)) THEN
     yhat(:,1:nt)=yopt(:,1:nt,jopt)
!
! Transform
     IF (igauss==1) THEN
        i1=hcw
        i2=hcw+ncv-1
        DO it=1,nt
           i1=i1+1
           IF (i1==nt+1) i1=1
           i2=i2+1
           IF (i2==nt+1) i2=1
           IF (i1<i2) THEN
              yc(1:ny,1:ncv)=y(1:ny,i1:i2)
           ELSE
              yc(1:ny,1:i2)=y(1:ny,1:i2)
              yc(1:ny,i2+1:ncv)=y(1:ny,i1:nt)
           END IF
           iopt=0
           DO i=1,ny
              CALL gaussian_inv (1,yhat(i,it:it),ncv,yc(i,1:ncv))
           END DO
        END DO
        prog=prog+dprog
     END IF
!
! Apply zero-bound
     IF (izero==1) THEN
        SELECT CASE (istd)
         CASE (0,3)
           WHERE (yhat(:,1:nt)<zero) yhat(:,1:nt)=zero
         CASE (1)
           DO i=1,ny
              szero=-ave(i)
              WHERE (yhat(i,1:nt)<szero) yhat(i,1:nt)=szero
           END DO
         CASE (2)
           DO i=1,ny
              szero=-ave(i)/sdev(i)
              WHERE (yhat(i,1:nt)<szero) yhat(i,1:nt)=szero
           END DO
        END SELECT
        prog=prog+dprog
     END IF
  END IF
!
  RETURN
 END SUBROUTINE cv_pcr
!
!
!
 SUBROUTINE pcr_predict (nf,nx,ny,ieofx,x,b,fcast,nt,nxe,xvp,xhat,nc,clim,fcastt)
!
! Calculates predictions given new predictor values
!
! Modules
  USE arrays,   ONLY: svx,eofx,ave,sdev,xm,xsd,ym,xc,dwk, &
                      stdize
  USE distribs, ONLY: gaussian_inv
  USE numbers,  ONLY: zero,one
  USE pcs,      ONLY: mxe
  USE settings, ONLY: igauss,izero,istd
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nf    ! - number of forecasts -
  INTEGER, INTENT(IN) :: nx    ! - number of X variables -
  INTEGER, INTENT(IN) :: ny    ! - number of Y variables -
  INTEGER, INTENT(IN) :: ieofx ! - Y EOF option -
!
  INTEGER, INTENT(IN), OPTIONAL :: nt  ! - number of cases in training period -
  INTEGER, INTENT(IN), OPTIONAL :: nxe ! - number of X EOF modes -
  INTEGER, INTENT(IN), OPTIONAL :: nc  ! - number of cases in climatology -
!
! Arrays,
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:,:) ! - new predictor values -
  REAL(KIND=rp), INTENT(IN) :: b(:,:) ! - regression coefficients -
!
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: clim(:,:) ! - climatological data for transformation -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: fcast(:,:) ! - forecast values -
!
  REAL(KIND=rp), INTENT(OUT), OPTIONAL :: xvp(:)      ! - predictors over forecast period -
  REAL(KIND=rp), INTENT(OUT), OPTIONAL :: xhat(:,:)   ! - predictor time scores -
  REAL(KIND=rp), INTENT(OUT), OPTIONAL :: fcastt(:,:) ! - transformed forecast values -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - EOF mode index -
  INTEGER :: k ! - case index -
!
  REAL(KIND=rp) :: df    ! - number of cases -
  REAL(KIND=rp) :: szero ! - standardized zero -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MATMUL
  INTRINSIC PRESENT
  INTRINSIC REAL
  INTRINSIC SUM
!
! Executable Statements
!
! Standardize predictors
  xc(1:nx,1:nf)=x(1:nx,1:nf)
  CALL stdize (nx,nf,xc,xm,xsd,3-ieofx)
!
! Predict anomalies
  IF (PRESENT(xvp)) df=REAL(nt,KIND=rp)
  DO k=1,nf
     fcast(1:ny,k)=MATMUL(xc(1:nx,k),b(1:nx,1:ny))+ym(1:ny)
!
! Calculate prediction error variance
     IF (PRESENT(xvp)) THEN
        xvp(k)=one/df
        DO i=1,nxe
           xvp(k)=xvp(k)+(SUM(eofx(:,i)*xc(:,k))/svx(i))**2
        END DO
     END IF
!
! Project predictors onto EOFs
     IF (PRESENT(xhat)) THEN
        DO i=1,mxe
           xhat(i,k)=SUM(eofx(:,i)*xc(:,k))/svx(i)
        END DO
! - rescale EOFs (for output) -
        xhat(1:mxe,k)=xhat(1:mxe,k)*svx(1:mxe)
     END IF
!
! Update progress meter
     prog=prog+dprog
  END DO
!
! Transform
! - transform from gaussian -
  IF ((PRESENT(clim)).AND.(PRESENT(nc))) THEN
     IF (igauss==1) THEN
        IF (PRESENT(fcastt)) fcastt(:,1:nf)=fcast(:,1:nf)
        dprog=dprog/REAL(ny,KIND=rp)
        DO i=1,ny
           dwk(1:nc)=clim(i,1:nc)
           CALL gaussian_inv (nf,fcast(i,1:nf),nc,dwk(1:nc))
           prog=prog+dprog
        END DO
        dprog=dprog*REAL(ny,KIND=rp)
     END IF
  END IF
! - apply zero-transform -
  IF (izero==1) THEN
     SELECT CASE (istd)
      CASE (0,3)
        WHERE (fcast(:,1:nf)<zero) fcast(:,1:nf)=zero
      CASE (1)
        DO i=1,ny
           szero=-ave(i)
           WHERE (fcast(i,1:nf)<szero) fcast(i,1:nf)=szero
        END DO
      CASE (2)
        DO i=1,ny
           szero=-ave(i)/sdev(i)
           WHERE (fcast(i,1:nf)<szero) fcast(i,1:nf)=szero
        END DO
     END SELECT
  END IF
!
  RETURN
 END SUBROUTINE pcr_predict
END MODULE pcr
