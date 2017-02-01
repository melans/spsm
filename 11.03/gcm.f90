!$Id: gcm.f90 1215 2011-02-25 21:30:20Z simon $
MODULE gcm
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
 FUNCTION perform_gcm()
!
! Performs GCM gridpoint comparisons.
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Memory allocation problem
!    ifail =  2 Calculation error
!    ifail = -1 Terminated
!
! Modules
  USE analysis,      ONLY: ianal, &
                           init_analysis1,init_analysis2,init_results,num_calcs
  USE arrays,        ONLY: x,y,clim,ave,sdev,iusex,iusey,xiny,yhat,yret,yrpls,b0,b1=>b,rfps,rodds,roddr,tobst,pobs,xvp,kuse, &
                           convert_units,stdize
  USE categories,    ONLY: ithr,        &
                           climate_per, &
                           set_cv_categories,set_ra_categories,set_thresholds,set_percentiles,calc_climate,calc_probs
  USE CPT_constants, ONLY: ng,nts
  USE gui,           ONLY: print_warning
  USE fields,        ONLY: xfield,yfield, &
                           get_nearest_grids,get_interpolated
  USE IO_constants,  ONLY: lprd
  USE iofiles,       ONLY: yfile
  USE labels,        ONLY: cg_to_l,cg_tperiod
  USE numbers,       ONLY: zero,one
  USE settings,      ONLY: iretro,nretro,izero,istd,iev,intp,nu,nu1,nur,nt,nt1,nret,mya,lcw,ncv,clf
!
! Function type
  INTEGER :: perform_gcm
!
! Locals
!
! Local scalars
  INTEGER :: irl   ! - index of last retroactive training-period year -
  INTEGER :: ir1   ! - index of first retroactive year -
  INTEGER :: irn   ! - index of last retroactive year -
  INTEGER :: iru   ! - index of last new year in retroactive training period -
  INTEGER :: nr    ! - total number of retroaoctive steps in current cycle -
  INTEGER :: nlt   ! - number of latitudes -
  INTEGER :: nlg   ! - number of longitudes -
  INTEGER :: ifail ! - error indicator -
!
  CHARACTER(LEN=lprd) :: cprd1 ! - period -
  CHARACTER(LEN=lprd) :: cprd2 ! - period -
  CHARACTER(LEN=lprd) :: cprd3 ! - period -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MAX
  INTRINSIC MAXVAL
  INTRINSIC MIN
  INTRINSIC REAL
  INTRINSIC TRIM
!
! Executable Statements
!
! Initialise progress meter
  perform_gcm=-1
  prog=zero
!
! Check for consistent units
  IF (TRIM(xfield(1)%var)==TRIM(yfield(1)%var)) THEN
     IF ((TRIM(xfield(1)%unit)/='none').AND.(TRIM(yfield(1)%unit)/='none')) THEN
        IF (TRIM(xfield(1)%unit)/=TRIM(yfield(1)%unit)) THEN
           CALL convert_units (xfield(1)%unit,yfield(1)%unit,x,ifail)
           IF (ifail/=0) THEN
              CALL print_warning ('Unable to convert from '//TRIM(xfield(1)%unit)//' to '//TRIM(yfield(1)%unit))
           END IF
        END IF
     END IF
  END IF
!
! Initialise analysis
  SELECT CASE (yfile%igrid)
   CASE (1)
     nlt=yfield(1)%region%nlts
     nlg=yfield(1)%region%nlgs
   CASE (2)
     nlt=yfield(1)%nva
     nlg=yfield(1)%nva
  END SELECT
  IF (init_analysis1(ng,nts,1,nlt=nlt,nlg=nlg)/=0) THEN
     perform_gcm=1
     RETURN
  END IF
!
! Estimate number of calculations
  IF (iev/=3) THEN
     dprog=one/REAL(num_calcs(ianal,iretro,0,izero,0,mya,nu,nret,nretro,nu1,nur,0,ithr),KIND=rp)
  ELSE
     dprog=one/REAL(num_calcs(ianal,iretro,0,izero,1,mya,nu,nret,nretro,nu1,nur,0,ithr),KIND=rp)
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
     CALL set_thresholds (0)
   CASE (3)
     CALL set_thresholds (0)
     CALL set_percentiles ()
  END SELECT
!
! Identify nearest grids
  CALL get_nearest_grids (yfile%igrid,intp,prog,dprog)
!
! Calculate interpolated values
  CALL get_interpolated (yfile%igrid,intp,iusey,iusex,nu,x,xiny)
!
! Perform retroactive calculations
  cprd1=get_cdate(yfile%period1+(yfile%it1-1),2)
! - cross-validate model for successive training periods -
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
           CALL cv_gcm (iru,ncv,mya,xiny,y,yhat=yhat)
! - construct full model -
           CALL full_gcm (iru,mya,xiny,y,b0,b1(:,1))
! - produce retroactive forecast -
           cprd3=get_cdate(yfile%period1+(yfile%it1+irl-1),2)
           IF (nr>1) THEN
              cprd2=get_cdate(yfile%period1+(yfile%it1+irl+nr-2),2)
              WRITE (UNIT=*,FMT='(A)') &
                    'Calculating retroactive forecasts for '//TRIM(cprd3)//' - '//TRIM(cprd2)
           ELSE
              WRITE (UNIT=*,FMT='(A,I4,A)') 'Calculating retroactive forecasts for '//TRIM(cprd3)
           END IF
           CALL gcm_predict (nr,mya,xiny(1:mya,nu1+ir1:nu1+irn),b0,b1(:,1),yret(:,ir1:irn), &
                nt=iru,xvp=xvp(:,ir1:irn))
           IF (iev==3) CALL gcm_predict (iru,mya,xiny(1:mya,1:iru),b0,b1(:,1),yhat(:,:))
! - calculate probabilities -
           CALL calc_probs (0,iru,nr,ng,1,mya,y,yhat,xvp(:,ir1:irn),yret(:,ir1:irn),tobst, &
                pobs,clf,rfps(:,ir1:irn,:),rodds(:,ir1:irn,:),roddr(:,ir1:irn,:),yrpls(:,ir1:irn,:))
           WRITE (UNIT=*,FMT='(A)') ' '
        END IF
! - update indices -
        iru=nu1+irn
        ir1=irn+1
     END DO retro
  END IF
  cprd2=get_cdate(yfile%period1+(yfile%it1+nt-2),2)
  ncv=nu-lcw
!
! Cross-validate model
  WRITE (UNIT=*,FMT='(A)') 'Cross-validating model ...'
  CALL cv_gcm (nu,ncv,mya,xiny,y,yhat=yhat)
!
! Allocate additional memory
  IF (init_analysis2()/=0) THEN
     perform_gcm=1
     RETURN
  END IF
!
! Fit model using all data
  WRITE (UNIT=*,FMT='(4A)') &
     'Constructing model using full training period (',TRIM(cprd1)//' ',&
     TRIM(cg_to_l)//' ',TRIM(cprd2)//') ...'
  CALL full_gcm (nu,mya,xiny,y,b0,b1(:,1))
!
! Allocate additional memory
  IF (init_results(MAX(MAXVAL(xfield(:)%nlt),MAXVAL(yfield(:)%nlt)),MAX(MAXVAL(xfield(:)%nlg),MAXVAL(yfield(:)%nlg)),ng)/=0) THEN
     perform_gcm=1
     RETURN
  END IF
!
! Determine forecast categories
  WRITE (UNIT=*,FMT='(A)') 'Identifying categories ...'
  dprog=(one-prog)/REAL(mya*(1+iretro),KIND=rp)
  CALL set_cv_categories ()
  IF (iretro==1) CALL set_ra_categories ()
  perform_gcm=0
!
  RETURN
 END FUNCTION perform_gcm
!
!
!
 SUBROUTINE full_gcm (nt,ny,x,y,b0,b1)
!
! Performs GCM using all data
!
! Modules
  USE analysis,   ONLY: prog,dprog
  USE arrays,     ONLY: xm,ym,xsd,ysd
  USE numbers,    ONLY: zero,eps,one
  USE settings,   ONLY: igcms
  USE statistics, ONLY: moments,get_regr
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nt ! - number of cases -
  INTEGER, INTENT(IN) :: ny ! - number of y spatial points -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:,:)   ! - explanatory variables -
  REAL(KIND=rp), INTENT(IN) :: y(:,:)   ! - response variables -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: b0(:) ! - regression constants -
  REAL(KIND=rp), INTENT(OUT) :: b1(:) ! - regression coefficients -
!
! Locals
!
! Local scalars
  INTEGER :: i  ! - location index -
!
! Executable Statements
!
! Calculate means and standard deviations
  CALL moments (ny,nt,x,xm,xsd)
  CALL moments (ny,nt,y,ym,ysd)
!
! Compute regression coefficients
  SELECT CASE (igcms)
   CASE (0) ! - no correction -
     b0(:)=zero
     b1(:)=one
   CASE (1) ! - correct mean biases -
     b1(:)=one
     b0(:)=ym(:)-xm(:)
   CASE (2) ! - correct mean and variance biases -
     DO i=1,ny
        IF (xsd(i)>eps) THEN
           b1(i)=ysd(i)/xsd(i)
        ELSE
           b1(i)=zero
        END IF
        b0(i)=ym(i)-xm(i)*b1(i)
     END DO
   CASE (3) ! - correct for skill -
     DO i=1,ny
        CALL get_regr (nt,x(i,:),y(i,:),b0(i),b1(i))
     END DO
  END SELECT
  prog=prog+dprog
!
  RETURN
 END SUBROUTINE full_gcm
!
!
!
 SUBROUTINE cv_gcm (nt,ncv,nv,x,y,yhat)
!
! Performs cross-validated GCM
!
! Modules
  USE analysis,   ONLY: prog,dprog
  USE arrays,     ONLY: xm,ym,xsd,ysd,xc,yc,ave,sdev,b0,b1=>b
  USE numbers,    ONLY: zero,eps
  USE settings,   ONLY: igcms,istd,izero,igood,hcw
  USE statistics, ONLY: goodness,moments,get_regr
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nt  ! - number of cases -
  INTEGER, INTENT(IN) :: ncv ! - length of cross-validated training period -
  INTEGER, INTENT(IN) :: nv  ! - number of y spatial points -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:,:) ! - explanatory variables -
  REAL(KIND=rp), INTENT(IN) :: y(:,:) ! - response variables -
!
! Output arrays
! - optional output arrays -
  REAL(KIND=rp), INTENT(OUT), OPTIONAL :: yhat(:,:) ! - cross-validated hindcasts -
!
! Locals
!
! Local scalars
  INTEGER :: it    ! - cross-validation time-step -
  INTEGER :: i     ! - predictor/predictand index -
  INTEGER :: i1,i2 ! - cross-validation indices -
!
  REAL(KIND=rp) :: gm    ! - goodness metric -
  REAL(KIND=rp) :: szero ! - standardized zero -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC PRESENT
!
! Executable Statements
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
        xc(1:nv,1:ncv)=x(1:nv,i1:i2)
        yc(1:nv,1:ncv)=y(1:nv,i1:i2)
     ELSE
        xc(1:nv,1:i2)=x(1:nv,1:i2)
        xc(1:nv,i2+1:ncv)=x(1:nv,i1:nt)
        yc(1:nv,1:i2)=y(1:nv,1:i2)
        yc(1:nv,i2+1:ncv)=y(1:nv,i1:nt)
     END IF
!
! Calculate means and standard deviations
     CALL moments (nv,ncv,xc,xm,xsd)
     CALL moments (nv,ncv,yc,ym,ysd)
!
! Compute regression coefficients
     SELECT CASE (igcms)
      CASE (0) ! - no correction -
        yhat(1:nv,it)=x(1:nv,it)
      CASE (1) ! - correct mean biases -
        yhat(1:nv,it)=x(1:nv,it)+ym(1:nv)-xm(1:nv)
      CASE (2) ! - correct mean and variance biases -
        DO i=1,nv
           IF (xsd(i)>eps) THEN
              yhat(i,it)=(x(i,it)-xm(i))*ysd(i)/xsd(i)+ym(i)
           ELSE
              yhat(i,it)=ym(i)
           END IF
        END DO
      CASE (3) ! - correct for skill -
        DO i=1,nv
           CALL get_regr (ncv,xc(i,1:ncv),yc(i,1:ncv),b0(i),b1(i,1))
           yhat(i,it)=x(i,it)*b1(i,1)+b0(i)
        END DO
     END SELECT
  END DO time_step
!
! Calculate and print goodness metric
  gm=goodness(igood,nt,nv,yhat(:,:),y(:,:))
  WRITE (UNIT=*,FMT='(A,F18.3)') 'Goodness index: ',gm
!
! Apply zero-bound
  IF (PRESENT(yhat)) THEN
     IF (izero==1) THEN
        SELECT CASE (istd)
         CASE (0,3)
           WHERE (yhat(:,1:nt)<zero) yhat(:,1:nt)=zero
         CASE (1)
           DO i=1,nv
              szero=-ave(i)
              WHERE (yhat(i,1:nt)<szero) yhat(i,1:nt)=szero
           END DO
         CASE (2)
           DO i=1,nv
              szero=-ave(i)/sdev(i)
              WHERE (yhat(i,1:nt)<szero) yhat(i,1:nt)=szero
           END DO
        END SELECT
        prog=prog+dprog
     END IF
  END IF
!
  RETURN
 END SUBROUTINE cv_gcm
!
!
!
 SUBROUTINE gcm_predict (nf,ny,x,b0,b1,fcast,nt,xvp)
!
! Calculates predictions given new predictor values
!
! Modules
  USE arrays,   ONLY: ave,sdev,xm,xsd
  USE numbers,  ONLY: zero,one
  USE settings, ONLY: izero,istd
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nf ! - number of forecasts -
  INTEGER, INTENT(IN) :: ny ! - number of Y variables -
!
  INTEGER, INTENT(IN), OPTIONAL :: nt ! - number of cases in training period -
!
! Arrays,
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:,:) ! - new predictor values -
  REAL(KIND=rp), INTENT(IN) :: b0(:)  ! - regression constants -
  REAL(KIND=rp), INTENT(IN) :: b1(:)  ! - regression coefficients -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: fcast(:,:) ! - forecast values -
!
  REAL(KIND=rp), INTENT(OUT), OPTIONAL :: xvp(:,:) ! - predictors over forecast period -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - variable index -
  INTEGER :: k ! - case index -
!
  REAL(KIND=rp) :: df    ! - number of cases -
  REAL(KIND=rp) :: szero ! - standardized zero -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC PRESENT
  INTRINSIC REAL
!
! Executable Statements
!
! Predict anomalies
  IF (PRESENT(xvp)) df=REAL(nt,KIND=rp)
  DO k=1,nf
     fcast(1:ny,k)=b0(1:ny)+b1(1:ny)*x(1:ny,k)
!
! Calculate prediction error variance
     IF (PRESENT(xvp)) THEN
        DO i=1,ny
           IF (xsd(i)>zero) THEN
              xvp(i,k)=((one+(x(i,k)-xm(i))**2)/xsd(i))/df
           ELSE
              xvp(i,k)=zero
           END IF
        END DO
     END IF
!
! Update progress meter
     prog=prog+dprog
  END DO
!
! Apply zero-transform
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
 END SUBROUTINE gcm_predict
END MODULE gcm
