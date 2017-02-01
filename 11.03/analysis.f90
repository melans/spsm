! $Id: analysis.f90 1226 2011-03-03 16:31:29Z simon $
MODULE analysis
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
  INTEGER, PUBLIC :: ianal=0 ! - analysis identifier (1=CCA, 2=PCR, 3=MLR) -
  INTEGER, PUBLIC :: ianaln  ! - intended new analysis identifier (1=CCA, 2=PCR, 3=MLR) -
  INTEGER, PUBLIC :: iaction ! - ready for calculation flag -
  INTEGER, PUBLIC :: icalc   ! - calculation completed flag -
  INTEGER, PUBLIC :: jcalc   ! - calculation uncompleted flag -
  INTEGER, PUBLIC :: ifc     ! - recalculate forecast flag (0=allocate and forecast; 1=forecast; 2=do nothing; -1=deallocate first ) -
  INTEGER, PUBLIC :: nopt    ! - total number of optimization steps -
!
! Real scalars
  REAL(KIND=rp), PUBLIC :: prog  ! - progress meter -
  REAL(KIND=rp), PUBLIC :: dprog ! - incremental progress -
!
CONTAINS
!
!
 SUBROUTINE analysis_flags (onoff)
!
! Switches on or off calculation flags 
!
! Modules
  USE numbers,  ONLY: zero,one
  USE settings, ONLY: iretro
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: onoff
!
! Functions and Subroutines
!
! Intrinsic routines
  INTRINSIC RANDOM_SEED
!
! Executable Statements
!
! Set flags on
  SELECT CASE (onoff)
   CASE ('on','ON','On')
     icalc=0   ! - calculation completed flag -
     ifc=0     ! - forecast calculation flag -
     prog=zero ! - progress meter -
!
! - initialise random number generator -
     CALL RANDOM_SEED ()
!
! Set flags to indicate end of calculation
   CASE ('end','END','End')
     prog=one ! - progress meter -
     icalc=1  ! - calculation completed flag -
     jcalc=0  ! - calculation uncompleted flag -
!
! Set all flags off
   CASE ('off','OFF','Off')
     icalc=0  ! - calculation completed flag -
     jcalc=1  ! - calculation uncompleted flag -
     iretro=0 ! - retroactive calculation flag -
  END SELECT
!
  RETURN
 END SUBROUTINE analysis_flags
!
!
!
 FUNCTION reset(msg)
!
! Clears memory for recalculation
!
! Modules
  USE numbers,  ONLY: zero
  USE settings, ONLY: mxa,mya
!
! Function type
  INTEGER :: reset
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: msg ! - message -
!
! Executable Statements
!
! Check for reset
  IF (icalc==1) THEN
     PRINT *, msg//' clears the current results.'
     reset=1
     reset=close_analysis()
!
! Reset numbers of used variables
     mxa=0
     mya=0
!
! Reset progress meter
     prog=zero
  ELSE
     reset=2
  END IF
!
  RETURN
 END FUNCTION reset
!
!
!
 FUNCTION init_analysis1(ng,nts,nopt,nlt,nlg,mxe,mye,mcc)
!
! Initialises memory and settings to perform EOF prefiltering and model fitting
!
! Modules
  USE arrays,   ONLY: clim,ave,sdev,yhat,yopt,yret,yrpls,rfps,rodds,roddr,svx,eofx,tsx,svy,eofy,tsy,bz,b,b0,mu,r,s, &
                      tobs,pobs,iusey,xm,ym,xsd,ysd,xvp,prjc,yhatt,yrett,tobst,ce,xiny,xc,yc,yopt,iwk,dwk
  USE fields,   ONLY: yfield,ifdx,ifdy,ilfx,ilfy,iffx,iffy,ilaty,ilngy,rlatn,rlngn
  USE settings, ONLY: igauss,iretro,nu,nur,mxa,mya,iv,iva,ivf,ivfa,hcw,lcw,lc1,lr1,lxyt,lxt,lyt,liwk,lrwk
!
! Function type
  INTEGER :: init_analysis1
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: ng   ! - number of categories -
  INTEGER, INTENT(IN) :: nts  ! - number of thresholds -
  INTEGER, INTENT(IN) :: nopt ! - number of optimization steps -
!
! - optional input scalars -
  INTEGER, INTENT(IN), OPTIONAL :: nlt ! - number of latitudes -
  INTEGER, INTENT(IN), OPTIONAL :: nlg ! - number of longitudes -
  INTEGER, INTENT(IN), OPTIONAL :: mxe ! - number of X EOF modes upper bound -
  INTEGER, INTENT(IN), OPTIONAL :: mye ! - number of Y EOF modes upper bound -
  INTEGER, INTENT(IN), OPTIONAL :: mcc ! - number of CCA modes upper bound -
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - memory allocation status -
  INTEGER :: mxyt  ! - maximum of mxa, mya, and nu -
  INTEGER :: mxt   ! - maximum of mxa, and nu -
!
! Functions and Subroutines
!
! Intrinsic routines
  INTRINSIC ALLOCATED
  INTRINSIC MIN
!
! Executable Statements
!
! Free existing memory
  IF (ALLOCATED(dwk)) DEALLOCATE (dwk)
!
! Allocate additional memory
! - climatological data -
  ALLOCATE (clim(mya,nu),STAT=ifail)
  IF (ifail/=0) GOTO 1
! - climatological averages -
  ALLOCATE (ave(mya),STAT=ifail)
  IF (ifail/=0) GOTO 1
! - climatological standard deviations -
  ALLOCATE (sdev(mya),STAT=ifail)
  IF (ifail/=0) GOTO 1
! - means -
  SELECT CASE (ianal)
   CASE (1:3)
     ALLOCATE (xm(mxa),STAT=ifail)
   CASE (4)
     ALLOCATE (xm(mya),STAT=ifail)
  END SELECT
  IF (ifail/=0) GOTO 1
  ALLOCATE (ym(mya),STAT=ifail)
  IF (ifail/=0) GOTO 1
! - standard deviations -
  SELECT CASE (ianal)
   CASE (1:3)
     ALLOCATE (xsd(mxa),STAT=ifail)
   CASE (4)
     ALLOCATE (xsd(mya),STAT=ifail)
  END SELECT
  IF (ifail/=0) GOTO 1
  ALLOCATE (ysd(mya),STAT=ifail)
  IF (ifail/=0) GOTO 1
! - cross-validated predictions -
  ALLOCATE (yhat(mya,nu),STAT=ifail)
  IF (ifail/=0) GOTO 1
! - cross-validated transformed predictions -
  IF (igauss==1) THEN
     ALLOCATE (yhatt(mya,nu),STAT=ifail)
     IF (ifail/=0) GOTO 1
  END IF
! - retroactive predictions -
  IF (iretro==1) THEN
     ALLOCATE (yret(mya,nur),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - transformed retroactive predictions -
     IF (igauss==1) THEN
        ALLOCATE (yrett(mya,nur),STAT=ifail)
        IF (ifail/=0) GOTO 1
     END IF
! - retroactive prediction limits -
     ALLOCATE (yrpls(mya,nur,2),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - retroactive forecast probabilities -
     ALLOCATE (rfps(mya,nur,ng),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - retroactive odds -
     ALLOCATE (rodds(mya,nur,ng),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - retroactive odds relative to climatology -
     ALLOCATE (roddr(mya,nur,ng),STAT=ifail)
     IF (ifail/=0) GOTO 1
  END IF
! - independent variables singular vectors -
  lxt=MIN(mxa,nu)
  IF (ianal<=3) THEN
     ALLOCATE (svx(lxt),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - independent variables EOF patterns -
     ALLOCATE (eofx(mxa,lxt),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - independent variables EOF scores -
     ALLOCATE (tsx(lxt,nu),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - dependent variables singular vectors -
  END IF
  SELECT CASE (ianal)
   CASE (1)
     lyt=MIN(mya,nu)
     lxyt=MIN(MAX(mxa,mya),nu)
     ALLOCATE (svy(lyt),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - dependent variables EOF patterns -
     ALLOCATE (eofy(mya,lyt),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - dependent variables EOF scores -
     ALLOCATE (tsy(lyt,nu),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - canonical correlations -
     ALLOCATE (mu(lxyt),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - canonical Y EOF weights -
     lr1=mye
     ALLOCATE (r(lr1,lxyt),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - canonical X EOF weights -
     ALLOCATE (s(lxyt,mxe),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - projections onto CCA modes -
     ALLOCATE (prjc(mcc),STAT=ifail)
     IF (ifail/=0) GOTO 1
   CASE (2,3)
! - principal component regression coefficients -
     ALLOCATE (bz(lxt,mya),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - regression coefficients -
     ALLOCATE (b(mxa,mya),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - regression constants -
     IF (ianal==3) THEN
        ALLOCATE (b0(mya),STAT=ifail)
        IF (ifail/=0) GOTO 1
     END IF
   CASE (4)
! - interpolated X values -
     ALLOCATE (xiny(mya,nu),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - regression coefficients -
     ALLOCATE (b(mya,1),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - regression constants -
     ALLOCATE (b0(mya),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - nearest latitudes -
     ALLOCATE (rlatn(nlt),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - nearest longitudes -
     ALLOCATE (rlngn(nlg),STAT=ifail)
     IF (ifail/=0) GOTO 1
  END SELECT
! - absolute thresholds -
  ALLOCATE (tobs(mya,nts),STAT=ifail)
  IF (ifail/=0) GOTO 1
! - transformed absolute thresholds -
  ALLOCATE (tobst(mya,nts),STAT=ifail)
  IF (ifail/=0) GOTO 1
! - percentiles / climatological probabilities -
  ALLOCATE (pobs(mya,ng),STAT=ifail)
  IF (ifail/=0) GOTO 1
! - copy of training period independent data -
  SELECT CASE (ianal)
   CASE (1:3)
     ALLOCATE (xc(mxa,nu),STAT=ifail)
   CASE (4)
     ALLOCATE (xc(mya,nu),STAT=ifail)
  END SELECT
  IF (ifail/=0) GOTO 1
! - copy of training period dependent data -
  ALLOCATE (yc(mya,nu),STAT=ifail)
  IF (ifail/=0) GOTO 1
! - prediction error variance for retroactive forecasts -
  IF (iretro==1) THEN
     SELECT CASE (ianal)
      CASE (1:3)
        ALLOCATE (xvp(1,nur),STAT=ifail)
      CASE (4)
        ALLOCATE (xvp(mya,nur),STAT=ifail)
     END SELECT
     IF (ifail/=0) GOTO 1
  END IF
! - EOF cross-correlations -
  SELECT CASE (ianal)
   CASE (1)
     lc1=mye
     ALLOCATE (ce(lc1,mxe),STAT=ifail)
     IF (ifail/=0) GOTO 1
!
! Calculate required workspace
     mxyt=MAX(mxa,mya,nu)
     lrwk=3*lxyt+MAX(mxyt,4*lxyt*(lxyt+1))
     liwk=8*lxyt
   CASE (2,3)
     mxt=MAX(mxa,nu)
     lrwk=MAX(3*lxt+MAX(mxt,4*lxt*(lxt+1)),mya)
     liwk=8*lxt
   CASE (4)
     mxt=MAX(mya,nu)
     lrwk=MAX(3*lxt+MAX(mxt,4*lxt*(lxt+1)),mya)
     liwk=8*lxt
  END SELECT
!
! Allocate workspace
  ALLOCATE (iwk(liwk),STAT=ifail)
  IF (ifail/=0) GOTO 1
  ALLOCATE (dwk(lrwk),STAT=ifail)
  IF (ifail/=0) GOTO 1
! - hindcasts for optimization -
  SELECT CASE (igauss)
   CASE (0)
     ALLOCATE (yopt(mya,nu,nopt),STAT=ifail)
   CASE (1)
     ALLOCATE (yopt(mya,nu,MAX(2,nopt)),STAT=ifail)
  END SELECT
  IF (ifail/=0) GOTO 1
!
! Calculate half cross-validation window
  hcw=(lcw+1)/2
!
! Identify initial grid point
  iva=1
  iv=iusey(1)
  ivfa=iva
  ivf=iv
!
! Set initial field
  iffx=1
  ifdx=1
  ilfx=1
  iffy=1
  ifdy=1
  ilfy=1
  ilaty=1+(ivf-1)/yfield(1)%region%nlgs
  ilngy=ivf-(ilaty-1)*yfield(1)%region%nlgs
!
  init_analysis1=0
  RETURN
!
! Errors
1 init_analysis1=1
!
  RETURN
 END FUNCTION init_analysis1
!
!
!
 FUNCTION init_analysis2(nco)
!
! Initialises additional memory for analysis
!
! Modules
  USE arrays,   ONLY: hx_map,hx_ser,hy_map,hy_ser, &
                      yopt,yt
  USE settings, ONLY: igauss,nu,mxa,mya
!
! Function type
  INTEGER :: init_analysis2
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN), OPTIONAL :: nco ! - optimal number of CCA modes -
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - memory allocation status -
!
! Functions and Subroutines
!
! Intrinsic routines
  INTRINSIC ALLOCATED
  INTRINSIC PRESENT
!
! Executable Statements
!
! Deallocate unneeded workspace
  IF (ALLOCATED(yopt)) DEALLOCATE (yopt)
!
! Allocate additional memory
! - transformed response data -
  IF (igauss==1) THEN
     ALLOCATE (yt(mya,nu),STAT=ifail)
     IF (ifail/=0) GOTO 1
  END IF
!
! Allocate additional memory for CCA maps
  IF (PRESENT(nco)) THEN
! - X homogeneous maps -
     ALLOCATE (hx_map(mxa,nco),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - Y homogeneous maps -
     ALLOCATE (hy_map(mya,nco),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - X homogeneous maps time series -
     ALLOCATE (hx_ser(nco,nu),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - Y homogeneous maps time series -
     ALLOCATE (hy_ser(nco,nu),STAT=ifail)
     IF (ifail/=0) GOTO 1
  END IF
!
  init_analysis2=0
  RETURN
!
! Errors
1 init_analysis2=1
!
  RETURN
 END FUNCTION init_analysis2
!
!
!
 FUNCTION init_results(nlt,nlg,ng)
!
! Initialises memory for results
!
! Modules
  USE arrays,     ONLY: iobs,ifor,irobs,irfor,rclim, &
                        rclim,xvp,ce,xc,yc,iwk,swk,dwk
  USE settings,   ONLY: iretro,nu,nur,nx,ny,mya
!
! Function type
  INTEGER :: init_results
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nlt ! - maximum number of latitudes -
  INTEGER, INTENT(IN) :: nlg ! - maximum number of longitudes -
  INTEGER, INTENT(IN) :: ng  ! - number of categories -
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - memory allocation status -
!
! Functions and Subroutines
!
! Intrinsic routines
  INTRINSIC ALLOCATED
  INTRINSIC MAX
!
! Executable Statements
!
! Deallocate unneeded workspace
  IF (ALLOCATED(dwk))  DEALLOCATE (dwk)
  IF (ALLOCATED(iwk))  DEALLOCATE (iwk)
  IF (ALLOCATED(ce))   DEALLOCATE (ce)
  IF (ALLOCATED(xvp))  DEALLOCATE (xvp)
  IF (ALLOCATED(yc))   DEALLOCATE (yc)
  IF (ALLOCATED(xc))   DEALLOCATE (xc)
!
! Allocate additional memory
! - observed categories -
  ALLOCATE (iobs(mya,nu),STAT=ifail)
  IF (ifail/=0) GOTO 1
! - hindcast categories -
  ALLOCATE (ifor(mya,nu),STAT=ifail)
  IF (ifail/=0) GOTO 1
! - retroactive observed categories -
  IF (iretro==1) THEN
     ALLOCATE (irobs(mya,nur),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - retroactive forecast categories -
     ALLOCATE (irfor(mya,nur),STAT=ifail)
     IF (ifail/=0) GOTO 1
! - retroactive climatologies -
     ALLOCATE (rclim(0:mya,ng),STAT=ifail)
     IF (ifail/=0) GOTO 1
  END IF
! - workspace -
  ALLOCATE (swk(MAX(nx,ny,nlg,nlt)),STAT=ifail)
  IF (ifail/=0) GOTO 1
  ALLOCATE (dwk(MAX(nu,nx,ny,nlg,nlt)),STAT=ifail)
  IF (ifail/=0) GOTO 1
!
  init_results=0
  RETURN
!
! Errors
1 init_results=1
!
  RETURN
 END FUNCTION init_results
!
!
!
 FUNCTION num_calcs(ianal,iretro,igauss,izero,ifit,mya,nt,nret,nretro,nt1,ntr,iopt,ithr)
!
! Calculates total number of calculations to perform for progress meter
!
! Function type
  INTEGER :: num_calcs
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: ianal  ! - analysis method -
  INTEGER, INTENT(IN) :: iretro ! - retroactive forecast flag -
  INTEGER, INTENT(IN) :: igauss ! - transform to gaussian flag -
  INTEGER, INTENT(IN) :: izero  ! - zero-bound flag -
  INTEGER, INTENT(IN) :: ifit   ! - calculate fitted values flag -
  INTEGER, INTENT(IN) :: mya    ! - number of available gridpoints -
  INTEGER, INTENT(IN) :: nt     ! - number of cases in training period -
  INTEGER, INTENT(IN) :: nret   ! - number of retroactive iterations -
  INTEGER, INTENT(IN) :: nretro ! - model update interval -
  INTEGER, INTENT(IN) :: nt1    ! - initial number of training cases -
  INTEGER, INTENT(IN) :: ntr    ! - number of cases in retroactive period -
  INTEGER, INTENT(IN) :: iopt   ! - optimization indicator -
  INTEGER, INTENT(IN) :: ithr   ! - threshold type -
!
! Locals
!
! Local scalars
  INTEGER :: ncvr ! - total number of retroactive cross-validations -
!
! Executable Statements
!
! Calculate number of retroactive cross-validations
  ncvr=nt1*nret+nret*(nret-1)*nretro/2
!
! Calculate number of calculations to perform
  num_calcs=1                                                 ! - calc_climate -
  num_calcs=num_calcs+1                                       ! - set_percentiles -
  num_calcs=num_calcs+1+igauss                                ! - set_thresholds -
  SELECT CASE (ianal)
   CASE (1:3)
     IF (iretro==1) THEN
        num_calcs=num_calcs+ncvr+nret*(iopt+(1-igauss)*izero) ! - cv_analysis -
        num_calcs=num_calcs+nret                              ! - full_analysis -
        num_calcs=num_calcs+ntr+nret*(igauss+izero)           ! - analysis_prediction -
        num_calcs=num_calcs+ifit*ncvr                         ! - fit_analysis -
        num_calcs=num_calcs+ntr+nret*(igauss+izero)           ! - calc_probs -
     END IF
     num_calcs=num_calcs+nt+iopt+igauss+izero                 ! - cv_analysis -
     num_calcs=num_calcs+1                                    ! - full_analysis -
   CASE (4)
     num_calcs=num_calcs+mya                                  ! - get_nearest_grids -
     num_calcs=num_calcs+mya                                  ! - get_interpolated -
  END SELECT
  SELECT CASE (ithr)
   CASE (1,3)
     num_calcs=num_calcs+1                                    ! - set_cv_categories -
     IF (iretro==1) num_calcs=num_calcs+1                     ! - set_ra_categories -
   CASE (2)
     num_calcs=num_calcs+mya                                  ! - set_cv_categories -
     IF (iretro==1) num_calcs=num_calcs+mya                   ! - set_ra_categories -
  END SELECT
!
  RETURN
 END FUNCTION num_calcs
!
!
!
 FUNCTION close_analysis()
!
! Frees memory allocated for analysis
!
! Modules
  USE arrays,   ONLY: x,y,iusex,iusey,kuse,yhat,yret,yrpls,svx,eofx,tsx,svy,eofy,tsy,mu,r,s,hx_map,hy_map,hx_ser,hy_ser,bz,b,b0,   &
                      xiny,clim,ave,sdev,rfps,rodds,roddr,iobs,ifor,irobs,irfor,rclim,tobs,pobs,hit,far,hits,fars,cump,eir,skills, &
                      pvalues,yopt,yt,yhatt,yrett,xm,ym,xsd,ysd,xvp,prjc,rnko,rnkf,tobst,ce,xc,yc,iwk,swk,dwk
  USE fields,   ONLY: rlatn,rlngn
  USE settings, ONLY: iretro
!
! Function type
  INTEGER :: close_analysis
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ALLOCATED
!
! Executable Statements
!
! Reset retroactive flag
  iretro=0
!
! Free memory
  close_analysis=close_fcast()
  IF (ALLOCATED(pvalues)) DEALLOCATE (pvalues)
  IF (ALLOCATED(skills))  DEALLOCATE (skills)
  IF (ALLOCATED(eir))     DEALLOCATE (eir)
  IF (ALLOCATED(cump))    DEALLOCATE (cump)
  IF (ALLOCATED(fars))    DEALLOCATE (fars)
  IF (ALLOCATED(hits))    DEALLOCATE (hits)
  IF (ALLOCATED(far))     DEALLOCATE (far)
  IF (ALLOCATED(hit))     DEALLOCATE (hit)
  IF (ALLOCATED(rnkf))    DEALLOCATE (rnkf)
  IF (ALLOCATED(rnko))    DEALLOCATE (rnko)
  IF (ALLOCATED(dwk))     DEALLOCATE (dwk)
  IF (ALLOCATED(swk))     DEALLOCATE (swk)
  IF (ALLOCATED(iwk))     DEALLOCATE (iwk)
  IF (ALLOCATED(rclim))   DEALLOCATE (rclim)
  IF (ALLOCATED(irfor))   DEALLOCATE (irfor)
  IF (ALLOCATED(irobs))   DEALLOCATE (irobs)
  IF (ALLOCATED(ifor))    DEALLOCATE (ifor)
  IF (ALLOCATED(iobs))    DEALLOCATE (iobs)
  IF (ALLOCATED(yt))      DEALLOCATE (yt)
  IF (ianal==1) THEN
     IF (ALLOCATED(hy_ser)) DEALLOCATE (hy_ser)
     IF (ALLOCATED(hx_ser)) DEALLOCATE (hx_ser)
     IF (ALLOCATED(hy_map)) DEALLOCATE (hy_map)
     IF (ALLOCATED(hx_map)) DEALLOCATE (hx_map)
  END IF
  IF (ALLOCATED(yopt))    DEALLOCATE (yopt)
  IF (ALLOCATED(ce))      DEALLOCATE (ce)
  IF (ALLOCATED(xvp))     DEALLOCATE (xvp)
  IF (ALLOCATED(yc))      DEALLOCATE (yc)
  IF (ALLOCATED(xc))      DEALLOCATE (xc)
  IF (ALLOCATED(pobs))    DEALLOCATE (pobs)
  IF (ALLOCATED(tobst))   DEALLOCATE (tobst)
  IF (ALLOCATED(tobs))    DEALLOCATE (tobs)
  SELECT CASE (ianal)
   CASE (1)
     IF (ALLOCATED(prjc))   DEALLOCATE (prjc)
     IF (ALLOCATED(s))      DEALLOCATE (s)
     IF (ALLOCATED(r))      DEALLOCATE (r)
     IF (ALLOCATED(mu))     DEALLOCATE (mu)
     IF (ALLOCATED(tsy))    DEALLOCATE (tsy)
     IF (ALLOCATED(svy))    DEALLOCATE (svy)
     IF (ALLOCATED(eofy))   DEALLOCATE (eofy)
   CASE (2,3)
     IF (ALLOCATED(b0))     DEALLOCATE (b0)
     IF (ALLOCATED(b))      DEALLOCATE (b)
     IF (ALLOCATED(bz))     DEALLOCATE (bz)
   CASE (4)
     IF (ALLOCATED(rlngn))  DEALLOCATE (rlngn)
     IF (ALLOCATED(rlatn))  DEALLOCATE (rlatn)
     IF (ALLOCATED(b0))     DEALLOCATE (b0)
     IF (ALLOCATED(b))      DEALLOCATE (b)
     IF (ALLOCATED(xiny))   DEALLOCATE (xiny)
  END SELECT
  IF (ALLOCATED(tsx))     DEALLOCATE (tsx)
  IF (ALLOCATED(eofx))    DEALLOCATE (eofx)
  IF (ALLOCATED(svx))     DEALLOCATE (svx)
  IF (ALLOCATED(roddr))   DEALLOCATE (roddr)
  IF (ALLOCATED(rodds))   DEALLOCATE (rodds)
  IF (ALLOCATED(rfps))    DEALLOCATE (rfps)
  IF (ALLOCATED(yrpls))   DEALLOCATE (yrpls)
  IF (ALLOCATED(yret))    DEALLOCATE (yret)
  IF (ALLOCATED(yrett))   DEALLOCATE (yrett)
  IF (ALLOCATED(yhatt))   DEALLOCATE (yhatt)
  IF (ALLOCATED(yhat))    DEALLOCATE (yhat)
  IF (ALLOCATED(ysd))     DEALLOCATE (ysd)
  IF (ALLOCATED(xsd))     DEALLOCATE (xsd)
  IF (ALLOCATED(ym))      DEALLOCATE (ym)
  IF (ALLOCATED(xm))      DEALLOCATE (xm)
  IF (ALLOCATED(sdev))    DEALLOCATE (sdev)
  IF (ALLOCATED(ave))     DEALLOCATE (ave)
  IF (ALLOCATED(clim))    DEALLOCATE (clim)
  IF (ALLOCATED(kuse))    DEALLOCATE (kuse)
  IF (ALLOCATED(iusey))   DEALLOCATE (iusey)
  IF (ALLOCATED(iusex))   DEALLOCATE (iusex)
  IF (ALLOCATED(y))       DEALLOCATE (y)
  IF (ALLOCATED(x))       DEALLOCATE (x)
!
! Reset calculation flags
  CALL analysis_flags ('off')
  close_analysis=0
!
  RETURN
 END FUNCTION close_analysis
!
!
!
 FUNCTION close_fcast()
!
! Frees memory allocated for forecasts
!
! Modules
  USE arrays, ONLY: yfit,fcast,pev,fpls,z,ziny,fps,odds,oddr,xhat, &
                    xc,fcastt,xvp,kfuse
!
! Function type
  INTEGER :: close_fcast
!
! Executable Statements
!
! Free memory
  close_fcast=0
  IF (ALLOCATED(kfuse))  DEALLOCATE (kfuse)
  IF (ALLOCATED(fcastt)) DEALLOCATE (fcastt)
  IF (ALLOCATED(xhat))   DEALLOCATE (xhat)
  IF (ALLOCATED(xvp))    DEALLOCATE (xvp)
  IF (ALLOCATED(oddr))   DEALLOCATE (oddr)
  IF (ALLOCATED(odds))   DEALLOCATE (odds)
  IF (ALLOCATED(fps))    DEALLOCATE (fps)
  IF (ALLOCATED(fpls))   DEALLOCATE (fpls)
  IF (ALLOCATED(pev))    DEALLOCATE (pev)
  IF (ALLOCATED(fcast))  DEALLOCATE (fcast)
  IF (ALLOCATED(yfit))   DEALLOCATE (yfit)
  IF (ALLOCATED(xc))     DEALLOCATE (xc)
  IF (ALLOCATED(ziny))   DEALLOCATE (ziny)
  IF (ALLOCATED(z))      DEALLOCATE (z)
!
  RETURN
 END FUNCTION close_fcast
END MODULE analysis
