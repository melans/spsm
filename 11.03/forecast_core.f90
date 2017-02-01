! $Id: forecast_core.f90 1215 2011-02-25 21:30:20Z simon $
MODULE forecast_core
!
! Modules
  USE fields,   ONLY: field,domain,area
  USE iofiles,  ONLY: ifile
  USE numbers,  ONLY: rp
  USE time
!
! Implicit declarations
  IMPLICIT NONE
!
! Scalars
!
! Integer scalars
  INTEGER, PRIVATE :: ifm  ! - current forecast map -
  INTEGER, PRIVATE :: ifmy ! - year of current forecast map -
!
CONTAINS
!
!
 FUNCTION init_fcast()
!
! Initialises memory for forecasting
!
! Modules
  USE analysis,      ONLY: prog,dprog
  USE arrays,        ONLY: yfit,fcast,pev,fpls,z,iusex,fps,odds,oddr,xhat, &
                           xc,fcastt,xvp,kfuse
  USE CPT_constants, ONLY: ng
  USE data_input,    ONLY: read_grid,read_stns,read_unrf,num_read
  USE fields,        ONLY: zfield,rlatz,rlngz,rrlatz,rrlngz,cstnz
  USE iofiles,       ONLY: zfile
  USE missing,       ONLY: immx,xmiss, &
                           non_missingz
  USE numbers,       ONLY: zero,one
  USE pcs,           ONLY: mxe
  USE settings,      ONLY: igauss,nu,nf,nens,nx,mxa,mya,mza
!
! Function type
  INTEGER :: init_fcast
!
! Locals
!
! Local scalars
  INTEGER :: ios   ! - memory allocation flag -
  INTEGER :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MAX
  INTRINSIC SUM
  INTRINSIC TRIM
!
! Executable Statements
!
! Initialise progress meter
  prog=zero
  dprog=one/num_read(zfile%igrid,nf,SUM(zfield(:)%nlt),zfile%nfs,zfile%nls,zfile%it1)
!
! Allocate workspace
  ifail=1
! - forecast data -
  ALLOCATE (z(nx,nf),STAT=ios)
  IF (ios/=0) GOTO 1
! - copy of x -
  ALLOCATE (xc(mxa,MAX(nu,nf)),STAT=ios)
  IF (ios/=0) GOTO 1
! - fitted values -
  ALLOCATE (yfit(mya,nu),STAT=ios)
  IF (ios/=0) GOTO 1
! - forecasts -
  ALLOCATE (fcast(mya,nf,0:nens),STAT=ios)
  IF (ios/=0) GOTO 1
! - prediction error variance -
  ALLOCATE (pev(mya,nf),STAT=ios)
  IF (ios/=0) GOTO 1
! - forecast prediction limits -
  ALLOCATE (fpls(mya,nf,2),STAT=ios)
  IF (ios/=0) GOTO 1
! - forecast probabilities -
  ALLOCATE (fps(mya,nf,ng),STAT=ios)
  IF (ios/=0) GOTO 1
! - odds -
  ALLOCATE (odds(mya,nf,ng),STAT=ios)
  IF (ios/=0) GOTO 1
! - odds relative to climatology -
  ALLOCATE (oddr(mya,nf,ng),STAT=ios)
  IF (ios/=0) GOTO 1
! - predictors over forecast period -
  ALLOCATE (xvp(nf),STAT=ios)
  IF (ios/=0) GOTO 1
! - projections onto X EOFs -
  ALLOCATE (xhat(mxe,nf),STAT=ios)
  IF (ios/=0) GOTO 1
! - transformed forecasts -
  IF (igauss==1) THEN
     ALLOCATE (fcastt(mya,nf),STAT=ios)
     IF (ios/=0) GOTO 1
  END IF
! - used forecast flag -
  ALLOCATE (kfuse(nf),STAT=ios)
  IF (ios/=0) GOTO 1
!
! Read forecast data
  WRITE (UNIT=*,FMT='(A)') 'Reading '//TRIM(zfile%ffile)//' ...'
  SELECT CASE (zfile%igrid)
   CASE (1)
     CALL read_grid (zfile,zfield,nf,z,rlatz,rlngz,rrlatz,rrlngz,ifail)
   CASE (2)
     CALL read_stns (zfile,zfield,nf,z,cstnz,rlatz,rlngz,rrlatz,rrlngz,ifail)
   CASE (3)
     CALL read_unrf (zfile,zfield(1)%nv,nf,z,cstnz(:,1),ifail)
  END SELECT
  IF (ifail>0) THEN
     ifail=2
     GOTO 1
  END IF
!
! Replace missing values
  mza=mxa
  CALL non_missingz (mza,nf,z,immx,xmiss,iusex)
  kfuse(:)=.true.
!
1 init_fcast=ifail
  prog=one
!
  RETURN
 END FUNCTION init_fcast
!
!
!
 FUNCTION get_fcast_opts()
!
! Modules
  USE analysis, ONLY: ifc
  USE numbers,  ONLY: zero,oneh
  USE settings, ONLY: iretro,iev,iodds,iprec,clf,nens
!
! Function type
  INTEGER :: get_fcast_opts
!
! Executable Statements
!
! Prompt for forecast settings
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') 'Forecast Settings'
  WRITE (UNIT=*,FMT=*)
! - prediction interval -
  WRITE (UNIT=*,FMT=*)
1 WRITE (UNIT=*,FMT='(A)') 'Prediction interval:'
  WRITE (UNIT=*,FMT='(A)',ADVANCE='no') 'Confidence level (%): '
  READ (UNIT=*,FMT=*,ERR=1) clf
  IF (.NOT.((clf>zero).AND.(clf<oneh))) GOTO 1
! - precision -
2 WRITE (UNIT=*,FMT='(A)') 'Precision: '
  WRITE (UNIT=*,FMT='(A)',ADVANCE='no') 'Number of ensemble members: '
  READ (UNIT=*,FMT=*,ERR=2) nens
  IF (nens<0) GOTO 2
! - error variance -
3 WRITE (UNIT=*,FMT='(A)') 'Error variance:'
  WRITE (UNIT=*,FMT='(A)') '1. Cross-validated error variance'
  IF (iretro==1) WRITE (UNIT=*,FMT='(A)') '2. Retroactive error variance'
  WRITE (UNIT=*,FMT='(A)') '3. Fitted error variance'
  READ (UNIT=*,FMT=*,ERR=3) iev
  IF ((iev<1).OR.(iev>3)) GOTO 3
  IF ((iretro==1).AND.(iev==2)) GOTO 3
! - odds ratio -
4 WRITE (UNIT=*,FMT='(A)') 'Odds: '
  WRITE (UNIT=*,FMT='(A)') '0. Odds'
  WRITE (UNIT=*,FMT='(A)') '1. Odds relative to climatology'
  READ (UNIT=*,FMT=*,ERR=4) iodds
  IF ((iodds<0).OR.(iodds>1)) GOTO 4
! - precision -
5 WRITE (UNIT=*,FMT='(A)') 'Precision: '
  WRITE (UNIT=*,FMT='(A)',ADVANCE='no') 'Number of decimal places (maximum is 9): '
  READ (UNIT=*,FMT=*,ERR=5) iprec
  IF ((iprec<0).OR.(iprec>8)) GOTO 5
!
! Reset forecast flag
  IF (ifc==2) ifc=1
  get_fcast_opts=0
!
  RETURN
 END FUNCTION get_fcast_opts
!
!
!
 FUNCTION get_fcst_file()
!
! Modules
  USE analysis,       ONLY: ifc, &
                            close_fcast
  USE data_input,     ONLY: get_zfile
  USE fields,         ONLY: xfield,zfield,tfield
  USE iofiles,        ONLY: zfile,bkfile, &
                            init_ifile
  USE settings,       ONLY: ifcast
!
! Function type
  INTEGER :: get_fcst_file
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ASSOCIATED
  INTRINSIC SIZE
!
! Executable Statements
!
! Check for changes
  get_fcst_file=get_zfile()
  IF ((get_fcst_file==2).AND.(zfile%lset)) THEN
     IF (zfile%ffile==bkfile%ffile) THEN
        ifc=-1
     ELSE
        ifc=0
     END IF
! - reset if cancelled -
  ELSE
     zfield(:)=tfield(:)
     IF (get_fcst_file==2) THEN
        ifc=2
     ELSE
        CALL init_ifile (zfile)
        IF (ASSOCIATED(tfield)) NULLIFY (tfield)
        ALLOCATE (zfield(SIZE(xfield)))
        zfield(:)=xfield(:)
        ifc=0
     END IF
  END IF
  IF (ifc==0) ifc=close_fcast()
  IF (zfile%lset) ifcast=1
  get_fcst_file=0
!
  RETURN
 END FUNCTION get_fcst_file
!
!
!
 FUNCTION calc_fcast()
!
! Modules
  USE analysis,      ONLY: ianal,prog,dprog
  USE arrays,        ONLY: x,y,yhat,yret,yfit,fcast,pev,fpls,z,xhat,b,fps,odds,oddr, &
                           yt,yhatt,yrett,fcastt,tobst,pobs,xvp,iusey,ave,sdev,      &
                           restdize
  USE categories,    ONLY: ithr, &
                           calc_probs,check_analogues
  USE cca,           ONLY: cca_predict
  USE CPT_constants, ONLY: ng
  USE errors,        ONLY: error
  USE numbers,       ONLY: zero,one
  USE pcr,           ONLY: pcr_predict
  USE pcs,           ONLY: ieofx,ieofy,nxo,nyo,nco
  USE settings,      ONLY: igauss,izero,iev,istd,istdo,nu,nur,nu1,nf,nens,mxa,mya,clf
!
! Function type
  INTEGER :: calc_fcast
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
!
! Executable Statements
!
! Initialise progress meter
  calc_fcast=-1
  prog=zero
  dprog=REAL(2*(nf+igauss+izero),KIND=rp)
  IF (iev==3) dprog=dprog+REAL(nu+izero,KIND=rp)
  dprog=one/dprog
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') 'Calculating forecasts ...'
!
! Calculate fitted values
  IF (iev==3) THEN
     SELECT CASE (ianal)
      CASE (1) ! - CCA -
        CALL cca_predict (nu,mxa,mya,nxo,nyo,nco,ieofx,x,ieofy,yfit)
      CASE (2,3) ! - PCR/MLR -
        CALL pcr_predict (nu,mxa,mya,ieofx,x,b,yfit)
     END SELECT
  END IF
!
! Predict anomaly
  SELECT CASE (ianal)
   CASE (1) ! - CCA -
     SELECT CASE (igauss)
      CASE (0)
        CALL cca_predict (nf,mxa,mya,nxo,nyo,nco,ieofx,z,ieofy,fcast(:,:,0), &
             nt=nu,xvp=xvp,xhat=xhat)
      CASE (1)
        CALL cca_predict (nf,mxa,mya,nxo,nyo,nco,ieofx,z,ieofy,fcast(:,:,0), &
             nt=nu,xvp=xvp,xhat=xhat,nc=nu,clim=y(1:mya,1:nu),fcastt=fcastt)
     END SELECT
   CASE (2,3) ! - PCR/MLR -
     SELECT CASE (igauss)
      CASE (0)
        CALL pcr_predict (nf,mxa,mya,ieofx,z,b,fcast(:,:,0), &
             nt=nu,nxe=nxo,xvp=xvp,xhat=xhat)
      CASE (1)
        CALL pcr_predict (nf,mxa,mya,ieofx,z,b,fcast(:,:,0), &
             nt=nu,nxe=nxo,xvp=xvp,xhat=xhat,nc=nu,clim=y(1:mya,1:nu),fcastt=fcastt)
     END SELECT
  END SELECT
!
! Calculate probabilities
  IF (ithr==3) THEN
     ifail=check_analogues()
     IF (ifail/=0) CALL error ('check_analogues',ifail,.true.)
  END IF
  SELECT CASE (igauss)
   CASE (0)
     SELECT CASE (iev)
      CASE (1)
        CALL calc_probs (nu,nf,ng,nxo,mya,y,yhat,xvp,fcast(:,:,0),tobst,pobs,clf,fps,odds,oddr,fpls, &
             nens=nens,fens=fcast(:,:,1:),pev=pev)
      CASE (2)
        CALL calc_probs (nur,nf,ng,nxo,mya,y(:,nu1+1:nu),yret,xvp,fcast(:,:,0),tobst,pobs,clf,fps,odds,oddr,fpls, &
             nens=nens,fens=fcast(:,:,1:),pev=pev)
      CASE (3)
        CALL calc_probs (nu,nf,ng,nxo,mya,y,yfit,xvp,fcast(:,:,0),tobst,pobs,clf,fps,odds,oddr,fpls, &
             nens=nens,fens=fcast(:,:,1:),pev=pev)
     END SELECT
   CASE (1)
     SELECT CASE (iev)
      CASE (1)
        CALL calc_probs (nu,nf,ng,nxo,mya,yt,yhatt,xvp,fcastt,tobst,pobs,clf,fps,odds,oddr,fpls, &
             nens=nens,fens=fcast(:,:,1:),nc=nu,clim=y(1:mya,1:nu),pev=pev)
      CASE (2)
        CALL calc_probs (nur,nf,ng,nxo,mya,yt(:,nu1+1:nu),yrett,xvp,fcastt,tobst,pobs,clf,fps,odds,oddr,fpls, &
             nens=nens,fens=fcast(:,:,1:),nc=nu,clim=y(1:mya,1:nu),pev=pev)
      CASE (3)
        CALL calc_probs (nu,nf,ng,nxo,mya,yt,yfit,xvp,fcastt,tobst,pobs,clf,fps,odds,oddr,fpls, &
             nens=nens,fens=fcast(:,:,1:),nc=nu,clim=y(1:mya,1:nu),pev=pev)
     END SELECT
  END SELECT
!
  prog=one
  WRITE (UNIT=*,FMT='(A)') 'Done!'
  calc_fcast=0
!
  RETURN
 END FUNCTION calc_fcast
!
!
!
 SUBROUTINE change_fcast()
!
! Modules
  USE analysis, ONLY: ifc
  USE iofiles,  ONLY: zfile
!
! Executable Statements
!
! Check for changes
  IF (zfile%lset) ifc=-1
!
  RETURN
 END SUBROUTINE change_fcast
!
!
!
 FUNCTION get_forecast()
!
! Modules
  USE analysis, ONLY: ifc, &
                      close_analysis,close_fcast
  USE arrays,   ONLY: fps
  USE errors,   ONLY: error
  USE iofiles,  ONLY: zfile
!
! Function type
  INTEGER :: get_forecast
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ALLOCATED
!
! Executable Statements
!
! Initialise forecasts if necessary
  IF (.NOT.ALLOCATED(fps)) ifc=0
!
! Calculate forecasts if necessary
! - clear memory -
  IF (ifc==-1) ifail=close_fcast()
! - initialise -
  IF (ifc<=0) THEN
     ifail=init_fcast()
     SELECT CASE (ifail)
      CASE (0)
        CONTINUE
      CASE (3,4)
        CALL error ('init_fcast',ifail,.true., &
             i_arg1=zfile%iseq)
      CASE DEFAULT
        GOTO 1
     END SELECT
  END IF
! - calculate forecasts -
  IF (ifc<=1) THEN
     ifail=calc_fcast()
     IF (ifail/=0) GOTO 1
  END IF
  ifc=2
  ifail=0
!
! Errors
1 SELECT CASE (ifail)
   CASE (0)
     get_forecast=0
     RETURN
   CASE (-1)
     get_forecast=close_analysis()
     RETURN
   CASE (1)
     CALL error ('get_forecast',ifail,.false.)
     get_forecast=close_fcast()
   CASE (2)
     get_forecast=close_fcast()
  END SELECT
  get_forecast=1
!
  RETURN
 END FUNCTION get_forecast
!
!
!
 FUNCTION fcst_fser()
!
! Modules
  USE fields,   ONLY: yfield,ilfy,ilaty,ilngy,&
                      check_ivf,update_grid
  USE iofiles,  ONLY: yfile
  USE settings, ONLY: ivf,ivfa
  USE analysis, ONLY: ifc
!
! Function type
  INTEGER :: fcst_fser
!
! Executable Statements
!
! Check whether forecasts require initialization
  IF (get_forecast()/=0) RETURN
!
! Check for need to update forecasts
  IF (ifc==1) THEN
     ifc=calc_fcast()
     ifc=2
  END IF

  WRITE (UNIT=*,FMT='(A)') 'after calc_fcast ...';

  fcst_fser=0
!
  RETURN
 END FUNCTION fcst_fser
!
!
!
 FUNCTION fcst_fens()
!
! Modules
  USE fields,   ONLY: yfield,ilfy, &
                      check_ivf,update_grid
  USE iofiles,  ONLY: yfile
  USE settings, ONLY: ivf,ivfa
!
! Function type
  INTEGER :: fcst_fens
!
! Executable Statements
!
! Check whether forecasts require initialization
  IF (get_forecast()/=0) RETURN
! 
! Print forecasts
  DO ilfy=1,yfile%nfs*yfile%nls
     DO ivfa=1,yfield(ilfy)%nva
        fcst_fens=check_ivf()
        CALL update_grid (ivf,ilfy,yfile%igrid)
        fcst_fens=update_fens()
     END DO
  END DO
  fcst_fens=0
!
  RETURN
 END FUNCTION fcst_fens
!
!
!
 FUNCTION update_fens()
!
! Modules
  USE analysis,     ONLY: ifc
  USE arrays,       ONLY: fcast
  USE IO_constants, ONLY: lprd
  USE iofiles,      ONLY: yfile,zfile
  USE settings,     ONLY: iva,ivf,lag,nf,nens,iprec
  USE time,         ONLY: get_cdate
!
! Function type
  INTEGER :: update_fens
!
! Locals
!
! Local scalars
  INTEGER :: i ! - ensemble member index  -
  INTEGER :: k ! - forecast index -
!
  CHARACTER(LEN=lprd) :: cout ! - output field -
  CHARACTER(LEN=  16) :: fmt  ! - format statement -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
!
! Executable Statements
!
! Check for need to update forecasts
  IF (ifc==1) THEN
     ifc=calc_fcast()
     ifc=2
  END IF
!
! Indicate series
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') 'Forecasts'
!
! Update forecasts
  WRITE (UNIT=fmt,FMT='(A,I1,A)') '(I4,F10.3,F11.',iprec,')'
  DO k=1,nf
     cout=get_cdate(zfile%fdate+(lag+k-1),2)
     WRITE (UNIT=*,FMT='(A)') cout
     WRITE (UNIT=*,FMT='(A)') 'Member  Quantile   Forecast'
     WRITE (UNIT=*,FMT=*)
     DO i=1,nens
        WRITE (UNIT=*,FMT=fmt) i,REAL(i,KIND=rp)/REAL(nens+1,KIND=rp),fcast(iva,k,i)
     END DO
     WRITE (UNIT=*,FMT=*)
  END DO
!
  update_fens=0
!
  RETURN
 END FUNCTION update_fens
!
! 
! 
 FUNCTION fcst_pexc()
!
! Modules
  USE fields,   ONLY: yfield,ilfy, &
                      check_ivf,update_grid
  USE iofiles,  ONLY: yfile
  USE settings, ONLY: ivf,ivfa
!
! Function type
  INTEGER :: fcst_pexc
!
! Executable Statements
!
! Check whether forecasts require initialization
  IF (get_forecast()/=0) RETURN
! 
! Print forecasts
  DO ilfy=1,yfile%nfs*yfile%nls
     DO ivfa=1,yfield(ilfy)%nva
        fcst_pexc=check_ivf()
        CALL update_grid (ivf,ilfy,yfile%igrid)
        fcst_pexc=update_pexc()
     END DO
  END DO
  fcst_pexc=0
!
  RETURN
 END FUNCTION fcst_pexc
!
!
!
 FUNCTION update_pexc()
!
! Modules
  USE analysis,      ONLY: dprog
  USE arrays,        ONLY: y,fcast,clim,ave,sdev,tobs,f,e,c,pev, &
                           yt,fcastt,dwk, &
                           insertion_sort,percentile,quantile
  USE categories,    ONLY: climate_per
  USE CPT_constants, ONLY: cstds,nts,nep
  USE distribs,      ONLY: normq,normq_inv,studnt
  USE numbers,       ONLY: zero,one,oneh
  USE settings,      ONLY: igauss,istd,iva,nu,dofr
!
! Function type
  INTEGER :: update_pexc
!
! Locals
!
! Local scalars
  INTEGER :: i ! - probability of exceedance index -
!
  REAL(KIND=rp) :: x    ! - x-value -
  REAL(KIND=rp) :: xinc ! - x-value increment -
  REAL(KIND=rp) :: xmin ! - x minimum -
  REAL(KIND=rp) :: df   ! - number of cases -
  REAL(KIND=rp) :: vbar ! - mean -
  REAL(KIND=rp) :: vsd  ! - standard deviation -
! 
! Functions and Subroutines
!  
! Intrinsic functions
  INTRINSIC COUNT
  INTRINSIC REAL
  INTRINSIC SQRT
!
! Executable Statements
!
! Allocate workspace
  dprog=zero
! - forecast data -
  ALLOCATE (f(0:nep))
  ALLOCATE (e(0:nep))
  ALLOCATE (c(0:nep))
!
! Determine mean and variance
  SELECT CASE (istd)
   CASE (0) ! - no standardization -
     vbar=ave(iva)
     vsd=sdev(iva)
   CASE (1) ! - anomalies -
     vbar=zero
     vsd=sdev(iva)
   CASE (2) ! - standardization -
     vbar=zero
     vsd=one
   CASE (3) ! - % of average -
     vbar=oneh
     vsd=oneh*sdev(iva)/ave(iva)
  END SELECT
!
! Calculate probabilities of exceedance
  CALL init_exceed (xmin,xinc,clim(iva,1),clim(iva,climate_per%nc))
  df=REAL(climate_per%nc,KIND=rp)
  x=xmin
  e(0)=REAL(100*COUNT(clim(iva,1:climate_per%nc)>x),KIND=rp)/df
  SELECT CASE (igauss)
   CASE (0)
     f(0)=oneh*studnt((x-fcast(iva,ifm,0))/pev(iva,ifm),dofr)
     c(0)=oneh-oneh*normq_inv((x-vbar)/vsd)
     DO i=1,nep
        x=(xmin+REAL(i,KIND=rp)*xinc)
        c(i)=oneh-oneh*normq_inv((x-vbar)/vsd)
        e(i)=REAL(100*COUNT(clim(iva,1:climate_per%nc)>x),KIND=rp)/df
        f(i)=oneh*studnt((x-fcast(iva,ifm,0))/pev(iva,ifm),dofr)
     END DO
   CASE (1)
     IF (climate_per%nc<nu) THEN
        dwk(1:nu)=y(iva,1:nu)
        CALL insertion_sort (dwk,nu,'a')
     ELSE
        dwk(1:nu)=clim(iva,1:nu)
     END IF
     x=percentile(dwk(1:nu),nu,x)
     c(0)=oneh-oneh*x
     x=normq(x)
     f(0)=oneh*studnt((x-fcastt(iva,ifm))/pev(iva,ifm),dofr)
     DO i=1,nep
        x=(xmin+REAL(i,KIND=rp)*xinc)
        e(i)=REAL(100*COUNT(clim(iva,1:climate_per%nc)>x),KIND=rp)/df
        x=percentile(clim(iva,1:climate_per%nc),climate_per%nc,x)
        c(i)=oneh-oneh*x
        x=normq(x)
        f(i)=oneh*studnt((x-fcastt(iva,ifm))/pev(iva,ifm),dofr)
     END DO
  END SELECT
!
! Print probabilities of exceedance
  WRITE (UNIT=*,FMT=*) TRIM(cstds(istd+1))
  WRITE (UNIT=*,FMT=*) 'Probabilities of exceedance:'
  WRITE (UNIT=*,FMT=*) '     Given forecast   Empirical   Climatological'
  DO i=1,nep
     WRITE (UNIT=*,FMT=*) i,f(i),e(i),c(i)
  END DO
!
! Free workspace
  DEALLOCATE (c)
  DEALLOCATE (e)
  DEALLOCATE (f)
  update_pexc=1
!
  RETURN
!
 CONTAINS
!
!
  SUBROUTINE init_exceed (vmin,vinc,vlow,vhgh)
! 
! Calculates appropriate limits given data
!
! Modules
  USE numbers, ONLY: zero,half,one,two,four,five,ten
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: vlow ! - minimum -
  REAL(KIND=rp), INTENT(IN) :: vhgh ! - maximum -
!
! Output scalars
  REAL(KIND=rp), INTENT(OUT) :: vmin ! - minimum -
  REAL(KIND=rp), INTENT(OUT) :: vinc ! - increment -
! 
! Locals
!
! Local scalars
  REAL(KIND=rp) :: arange ! - axis range -
  REAL(KIND=rp) :: vscale ! - scaling -
  REAL(KIND=rp) :: vmax   ! - maximum -
  REAL(KIND=rp) :: vtick  ! - tick mark increment -
  REAL(KIND=rp) :: a      ! - absolute value -
  REAL(KIND=rp) :: r      ! - ratio -
  REAL(KIND=rp) :: r_old  ! - old ratio -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ABS
  INTRINSIC MOD
  INTRINSIC NINT
  INTRINSIC PRESENT
  INTRINSIC REAL
!
! Executable Statements
!
! Get preliminary estimates for axis limits
  IF (ABS(vlow)<ABS(vhgh)) THEN
     vmin=rescaled(vlow,'l',vscale)
     vmax=rescaled(vhgh,'u',vscale)
  ELSE
     vmax=rescaled(vhgh,'u',vscale)
     vmin=rescaled(vlow,'l',vscale)
  END IF
  IF (.NOT.(vmin<vmax.OR.vmin>vmax)) THEN
     vmin=vmin-one
     vmax=vmax+one
  END IF
  vmin=vmin*vscale
  vmax=vmax*vscale
!
! Reset if x=0 is nearby
  r=-one
! - extremes both negative -
1 IF ((vlow<zero).AND.(vhgh<zero)) THEN
     arange=vhgh-vlow
     IF ((arange>vlow).OR.(-vhgh/arange<0.2_rp)) vmax=zero
! - extremes both positive -
  ELSE IF (vlow>zero) THEN
     arange=vhgh-vlow
     IF ((arange>vhgh).OR.(vlow/arange<0.2_rp)) vmin=zero
  END IF
!
! Define appropriate tick-mark interval
  a=ABS(vmax-vmin)
  vtick=one
  IF (a>zero) THEN
     rescale1: DO
        IF (a<=one) THEN
           vtick=vtick/ten
           a=a*ten
        ELSE IF (a>=oneh) THEN
           vtick=vtick*oneh
           a=a/oneh
        END IF
        IF (a<=four) THEN
           vtick=vtick/two
           a=a*two
        ELSE IF (a>=ten) THEN
           vtick=vtick*five
           a=a/five
        ELSE
           EXIT rescale1
        END IF
     END DO rescale1
  END IF
!
! Compare axis range to data range, and reduce axis range if data range is too small
  r_old=r
  r=ABS(((vhgh-vlow)*vscale)/(vmax-vmin))
  IF ((r<half).AND.(r>zero).AND.(r/=r_old)) THEN
     IF (vhgh>vlow) THEN
        DO
           IF (vmax-vtick>vhgh*vscale) THEN
              vmax=vmax-vtick
           ELSE
              EXIT
           END IF
        END DO
        DO
           IF (vmin+vtick<vlow*vscale) THEN
              vmin=vmin+vtick
           ELSE
              EXIT
           END IF
        END DO
     ELSE
        DO
           IF (vmax+vtick<vhgh*vscale) THEN
              vmax=vmax+vtick
           ELSE
              EXIT
           END IF
        END DO
        DO
           IF (vmin-vtick>vlow*vscale) THEN
              vmin=vmin-vtick
           ELSE
              EXIT
           END IF
        END DO
     END IF
     GOTO 1
  END IF
!
  vinc=(vmax-vmin)/(REAL(nep,KIND=rp)*vscale)
  vmin=vmin/vscale
!
  RETURN
  END SUBROUTINE init_exceed
!
!
!
  FUNCTION rescaled (z,ext,zscale)
!
! Modules
  USE numbers, ONLY: zero,onetth,one,oneh,onet
!
! Function type 
  REAL(KIND=rp) :: rescaled
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: z ! - value -
! 
  CHARACTER(LEN=1), INTENT(IN) :: ext ! - tail -
!
! Output scalars 
  REAL(KIND=rp), INTENT(OUT) :: zscale ! - scaling factor -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - rounded absolute value -
!
  REAL(KIND=rp) :: a ! - absolute value -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MOD
  INTRINSIC REAL
  INTRINSIC SIGN
!
! Executable Statements
!
! Rescale
  a=ABS(z)
  zscale=one
  IF (a>zero) THEN
     rescale2: DO
        IF (a<one) THEN
           zscale=zscale*oneh
           a=a*oneh
        ELSE IF (a>onet) THEN
           zscale=zscale*onetth
           a=a*onetth
        ELSE
           EXIT rescale2
        END IF
     END DO rescale2
  END IF
!
! Define appropriate limit
  i=INT(a)
  IF (((z<zero).AND.(ext=='l')).OR.((z>zero).AND.(ext=='u'))) THEN
     i=i+1
  ELSE IF (((z>zero).AND.(ext=='l')).OR.((z<zero).AND.(ext=='u'))) THEN
     i=i-1
  END IF
  SELECT CASE (i)
   CASE (0:15)
     CONTINUE
   CASE (16:40)
     IF (MOD(i,5)/=0) THEN
        IF (((z<zero).AND.(ext=='l')).OR.((z>zero).AND.(ext=='u'))) THEN
           i=i+5-MOD(i,5)
        ELSE
           i=i-MOD(i,5)
        END IF
     END IF
   CASE DEFAULT
     IF (MOD(i,10)/=0) THEN
        IF (((z<zero).AND.(ext=='l')).OR.((z>zero).AND.(ext=='u'))) THEN
           i=i+10-MOD(i,10)
        ELSE
           i=i-MOD(i,10)
        END IF
     END IF
  END SELECT
  rescaled=REAL(i,KIND=rp)/zscale
  rescaled=SIGN(rescaled,z)
!
  RETURN
  END FUNCTION rescaled
 END FUNCTION update_pexc
!
!
!
 FUNCTION fcst_fval()
!
! Modules
  USE iofiles,  ONLY: zfile
  USE settings, ONLY: lag,nf
!
! Function type
  INTEGER :: fcst_fval
!
! Executable Statements
!
! Check whether forecasts require initialization
  IF (get_forecast()/=0) RETURN
! 
! Print results
  DO ifm=1,nf
     ifmy=zfile%fdate%iyr+lag+ifm-1
     fcst_fval=update_fval()
  END DO
!
  RETURN
 END FUNCTION fcst_fval
!
!
!
 FUNCTION update_fval()
!
! Modules
  USE arrays,        ONLY: iuse=>iusey,fcast,fpls
  USE categories,    ONLY: climate_per
  USE CPT_constants, ONLY: dsdu
  USE fields,        ONLY: yfield,ilfy,rlaty,rlngy,cstny, &
                           make_map_coor
  USE iofiles,       ONLY: yfile
  USE settings,      ONLY: iprec
!
! Function type
  INTEGER :: update_fval
!
! Locals
!
! Local scalars
  INTEGER :: i      ! - latitude index -
  INTEGER :: j      ! - longitude index -
  INTEGER :: ij,ija ! - indices -
  INTEGER :: ilat   ! - latitude index -
  INTEGER :: jlng   ! - longitude index -
!
  CHARACTER(LEN=18) :: coors ! - coordinates -
  CHARACTER(LEN=12) :: fmt   ! - format statement -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC TRIM
!
! Executable Statements
!
! Check whether forecasts require initialization
  IF (get_forecast()/=0) RETURN
!
! Print forecasts
  WRITE (UNIT=fmt,FMT='(A,I1,A)') '(A,3F11.',iprec,')'
! - climatological period -
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(4A)') TRIM(climate_per%clim1),' - ',TRIM(climate_per%clim2),' climatology'
! - gridded data -
  SELECT CASE (yfile%igrid)
   CASE (1)
     WRITE (UNIT=*,FMT='(A)') '     Lat.    Long.   Forecast      Lower      Upper'
     WRITE (UNIT=*,FMT=*)
     ij=0
     ija=1
     IF (ilfy>1) THEN
        ij=SUM(yfield(1:ilfy-1)%nv)
        ija=SUM(yfield(1:ilfy-1)%nva)+1
     END IF
     DO i=1,yfield(ilfy)%region%nlts
        IF (.NOT.yfield(ilfy)%linvert) THEN
           ilat=yfield(ilfy)%region%nlt1+i-1
        ELSE
           ilat=yfield(ilfy)%region%nlt2+i-1
        END IF
        DO j=1,yfield(ilfy)%region%nlgs
           jlng=yfield(ilfy)%region%nlg1+j-1
           IF (jlng>yfield(ilfy)%nlg) jlng=jlng-yfield(ilfy)%nlg
           ij=ij+1
           IF (iuse(ija)==ij) THEN
              coors=make_map_coor(rlaty(ilat,ilfy),rlngy(jlng,ilfy))
              WRITE (UNIT=*,FMT=fmt) coors,fcast(ija,ifm,0),fpls(ija,ifm,1),fpls(ija,ifm,2)
              ija=ija+1
           END IF
        END DO
     END DO
! - station and unreferenced data -
   CASE (2,3)
     WRITE (UNIT=*,FMT='(2A)') dsdu(yfile%igrid),'          Forecast      Lower      Upper'
     WRITE (UNIT=*,FMT=*)
     IF (ilfy==1) THEN
        ija=0
     ELSE
        ija=SUM(yfield(1:ilfy-1)%nva)
     END IF
     DO j=1,yfield(ilfy)%nva
        ija=ija+1
        WRITE (UNIT=*,FMT=fmt) cstny(iuse(ija),ilfy),fcast(ija,ifm,0),fpls(ija,ifm,1),fpls(ija,ifm,2)
     END DO
  END SELECT
  update_fval=0
!
  RETURN
 END FUNCTION update_fval
!
!
!
 FUNCTION fcst_fps()
!
! Modules
  USE iofiles,  ONLY: zfile
  USE settings, ONLY: lag,nf
!
! Function type
  INTEGER :: fcst_fps
!
! Executable Statements
!
! Check whether forecasts require initialization
  IF (get_forecast()/=0) RETURN
! 
! Print results
  DO ifm=1,nf
     ifmy=zfile%fdate%iyr+lag+ifm-1
     fcst_fps=update_fps()
  END DO
!
  RETURN
 END FUNCTION fcst_fps
!
!
!
 FUNCTION update_fps()
!
! Modules
  USE arrays,        ONLY: iuse=>iusey,fps
  USE categories,    ONLY: climate_per
  USE CPT_constants, ONLY: ng,dsdu
  USE fields,        ONLY: yfield,ilfy,rlaty,rlngy,cstny, &
                           make_map_coor
  USE iofiles,       ONLY: yfile
!
! Function type
  INTEGER :: update_fps
!
! Locals
!
! Local scalars
  INTEGER :: i      ! - latitude index -
  INTEGER :: j      ! - longitude index -
  INTEGER :: ij,ija ! - indices -
  INTEGER :: k      ! - category index -
  INTEGER :: ilat   ! - latitude index -
  INTEGER :: jlng   ! - longitude index -
!
  CHARACTER(LEN=18) :: coors ! - coordinates -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ADJUSTR
  INTRINSIC NINT
  INTRINSIC TRIM
!
! Executable Statements
!
! Print forecasts
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(4A)') TRIM(climate_per%clim1),' - ',TRIM(climate_per%clim2),' climatology'
! - gridded data -
  SELECT CASE (yfile%igrid)
   CASE (1)
     WRITE (UNIT=*,FMT='(A)') '     Lat.    Long.  Below Normal  Above'
     WRITE (UNIT=*,FMT=*)
     ij=0
     ija=1
     IF (ilfy>1) THEN
        ij=SUM(yfield(1:ilfy-1)%nv)
        ija=SUM(yfield(1:ilfy-1)%nva)+1
     END IF
     DO i=1,yfield(ilfy)%region%nlts
        IF (.NOT.yfield(ilfy)%linvert) THEN
           ilat=yfield(ilfy)%region%nlt1+i-1
        ELSE
           ilat=yfield(ilfy)%region%nlt2+i-1
        END IF
        DO j=1,yfield(ilfy)%region%nlgs
           jlng=yfield(ilfy)%region%nlg1+j-1
           IF (jlng>yfield(ilfy)%nlg) jlng=jlng-yfield(ilfy)%nlg
           ij=(i-1)*yfield(ilfy)%region%nlgs+j
           IF (iuse(ija)==ij) THEN
              coors=make_map_coor(rlaty(ilat,ilfy),rlngy(jlng,ilfy))
              WRITE (UNIT=*,FMT='(A,3(I6,A))') coors,(NINT(fps(ija,ifm,k)),'%',k=1,ng)
              ija=ija+1
           END IF
        END DO
     END DO
! - station and unreferenced data -
   CASE (2,3)
     WRITE (UNIT=*,FMT='(2A)') ADJUSTR(dsdu(yfile%igrid)),'        Below Normal  Above'
     WRITE (UNIT=*,FMT=*)
     IF (ilfy==1) THEN
        ija=0
     ELSE
        ija=SUM(yfield(1:ilfy-1)%nva)
     END IF
     DO j=1,yfield(ilfy)%nva
        ija=ija+1
        WRITE (UNIT=*,FMT='(A,3(I6,A))') cstny(iuse(j),ilfy),(NINT(fps(ija,ifm,k)),'%',k=1,ng)
     END DO
  END SELECT
  update_fps=0
!
  RETURN
 END FUNCTION update_fps
END MODULE forecast_core
