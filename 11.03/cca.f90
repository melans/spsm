! $Id: cca.f90 1215 2011-02-25 21:30:20Z simon $
MODULE cca
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
 FUNCTION perform_cca()
!
! Performs CCA.
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
  USE arrays,        ONLY: x,y,clim,ave,sdev,svx,eofx,tsx,svy,eofy,tsy,mu,r,s,yhat,yret,yrpls,rfps,rodds,roddr, &
                           hx_map,hy_map,hx_ser,hy_ser,yopt,yt,yhatt,yrett,tobst,pobs,xvp,kuse,                 &
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
  USE pcs,           ONLY: ieofx,ieofy,iey,iec,mxe,mye,mcc,nxe,nye,ncc,nxo,nyo,nco,npx,npy,ncu
  USE settings,      ONLY: iretro,nretro,igauss,izero,istd,iev,nu,nu1,nur,nt,nt1,nret,mxa,mya,lcw,ncv,clf
!
! Function type
  INTEGER :: perform_cca
!
! Locals
!
! Local scalars
  INTEGER :: i     ! - index -
  INTEGER :: irl   ! - index of last retroactive training-period date -
  INTEGER :: ir1   ! - index of first retroactive date -
  INTEGER :: irn   ! - index of last retroactive date -
  INTEGER :: iru   ! - index of last new date in retroactive training period -
  INTEGER :: ix    ! - X EOF index -
  INTEGER :: iy    ! - Y EOF index -
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
  INTRINSIC MAX
  INTRINSIC MAXVAL
  INTRINSIC MIN
  INTRINSIC REAL
  INTRINSIC TRIM
!
! Executable Statements
!
! Initialise progress meter
  perform_cca=-1
  prog=zero
!
! Calculate number of optimization steps
  nopt=0
  DO ix=nxe,mxe
     DO iy=nye,mye
        DO i=ncc,MIN(mcc,ix,iy)
           nopt=nopt+1
        END DO
     END DO
  END DO
!
! Initialise analysis
  IF (init_analysis1(ng,nts,nopt,mxe=mxe,mye=mye,mcc=mcc)/=0) THEN
     perform_cca=1
     RETURN
  END IF
!
! Calculate optimization settings
  IF (nopt==1) THEN
     nopt=0
     iopt=0
     nxo=nxe
     nyo=nye
     nco=ncc
  ELSE
     iopt=1
  END IF
  iey=1
  iec=1
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
! Standardize
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
                 CALL cv_cca (iru,ncv,mxa,x,mya,y,ieofx,nxe,mxe,ieofy,nye,mye,ncc,mcc,yhat=yhat,nxo=nxo,nyo=nyo,nco=nco)
               CASE (1)
                 CALL cv_cca (iru,ncv,mxa,x,mya,y,ieofx,nxe,mxe,ieofy,nye,mye,ncc,mcc,yhatt=yhat,nxo=nxo,nyo=nyo,nco=nco)
              END SELECT
           ELSE
              SELECT CASE (igauss)
               CASE (0)
                 CALL cv_cca (iru,ncv,mxa,x,mya,y,ieofx,nxe,mxe,ieofy,nye,mye,ncc,mcc,yhat=yhat)
               CASE (1)
                 CALL cv_cca (iru,ncv,mxa,x,mya,y,ieofx,nxe,mxe,ieofy,nye,mye,ncc,mcc,yhatt=yhat)
              END SELECT
           END IF
! - construct full model -
           SELECT CASE (igauss)
            CASE (0)
              CALL full_cca (iru,mxa,x,mya,y,ieofx,nxo,svx,eofx,tsx,ieofy,nyo,svy,eofy,tsy,nco,mu,r,s,ifail)
            CASE (1)
              CALL full_cca (iru,mxa,x,mya,y,ieofx,nxo,svx,eofx,tsx,ieofy,nyo,svy,eofy,tsy,nco,mu,r,s,ifail,yt=yopt(:,:,2))
           END SELECT
           IF (ifail/=0) THEN
              perform_cca=2
              RETURN
           END IF
           IF (npx<nxo) nxo=npx
           IF (npy<nyo) nyo=npy
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
              CALL cca_predict (nr,mxa,mya,nxo,nyo,nco,ieofx,x(1:mxa,nu1+ir1:nu1+irn),ieofy,yret(:,ir1:irn), &
                   nt=iru,xvp=xvp(1,ir1:irn))
            CASE (1)
              CALL cca_predict (nr,mxa,mya,nxo,nyo,nco,ieofx,x(1:mxa,nu1+ir1:nu1+irn),ieofy,yret(:,ir1:irn), &
                   nt=iru,xvp=xvp(1,ir1:irn),nc=iru,clim=y(1:mya,1:iru),fcastt=yrett(:,ir1:irn))
           END SELECT
           IF (iev==3) CALL cca_predict (iru,mxa,mya,nxo,nyo,nco,ieofx,x(1:mxa,1:iru),ieofy,yhat(:,:))
! - calculate probabilities -
           SELECT CASE (igauss)
            CASE (0)
              CALL calc_probs (igauss,iru,nr,ng,nxo,mya,y,yhat,xvp(:,ir1:irn),yret(:,ir1:irn),tobst, &
                   pobs,clf,rfps(:,ir1:irn,:),rodds(:,ir1:irn,:),roddr(:,ir1:irn,:),yrpls(:,ir1:irn,:))
            CASE (1)
              CALL calc_probs (igauss,iru,nr,ng,nxo,mya,yopt(:,:,2),yhat,xvp(:,ir1:irn),yrett(:,ir1:irn),     &
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
! Optimise CCA using full training period
  cprd2=get_cdate(yfile%period1+(yfile%it1+nt-2),2)
  ncv=nu-lcw
  IF (nopt>0) THEN
     WRITE (UNIT=*,FMT='(A)') 'Optimizing cross-validated performance ...'
     WRITE (UNIT=*,FMT='(A)') TRIM(cg_tperiod)//': '//TRIM(cprd1)//' '//TRIM(cg_to_l)//' '//TRIM(cprd2)
     WRITE (UNIT=*,FMT='(A)') ' '
     SELECT CASE (igauss)
      CASE (0)
        CALL cv_cca (nu,ncv,mxa,x,mya,y,ieofx,nxe,mxe,ieofy,nye,mye,ncc,mcc,yhat=yhat,nxo=nxo,nyo=nyo,nco=nco)
      CASE (1)
        CALL cv_cca (nu,ncv,mxa,x,mya,y,ieofx,nxe,mxe,ieofy,nye,mye,ncc,mcc,yhat=yhat,yhatt=yhatt,nxo=nxo,nyo=nyo,nco=nco)
     END SELECT
!
! Cross-validate optimal / chosen model
  ELSE
     WRITE (UNIT=*,FMT='(A)') 'Cross-validating model ...'
     SELECT CASE (igauss)
      CASE (0)
        CALL cv_cca (nu,ncv,mxa,x,mya,y,ieofx,nxo,nxo,ieofy,nyo,nyo,nco,nco,yhat=yhat)
      CASE (1)
        CALL cv_cca (nu,ncv,mxa,x,mya,y,ieofx,nxo,nxo,ieofy,nyo,nyo,nco,nco,yhat=yhat,yhatt=yhatt)
     END SELECT
  END IF
!
! Allocate additional memory
  ncu=MIN(nxo,nyo,mcc)
  IF (init_analysis2(ncu)/=0) THEN
     perform_cca=1
     RETURN
  END IF
!
! Fit model using all data
  WRITE (UNIT=*,FMT='(A)') &
     'Constructing model using full training period ('//TRIM(cprd1)//' '//TRIM(cg_to_l)//' '//TRIM(cprd2)//') ...'
  SELECT CASE (igauss)
   CASE (0)
     CALL full_cca (nu,mxa,x,mya,y,ieofx,nxo,svx,eofx,tsx,ieofy,nyo,svy,eofy,tsy,ncu,mu,r,s,ifail, &
          hx_map=hx_map,hy_map=hy_map,hx_ser=hx_ser,hy_ser=hy_ser)
   CASE (1)
     CALL full_cca (nu,mxa,x,mya,y,ieofx,nxo,svx,eofx,tsx,ieofy,nyo,svy,eofy,tsy,ncu,mu,r,s,ifail, &
          yt=yt,hx_map=hx_map,hy_map=hy_map,hx_ser=hx_ser,hy_ser=hy_ser)
  END SELECT
  IF (ifail/=0) THEN
     perform_cca=2
     RETURN
  END IF
!
! Allocate additional memory
  IF (init_results(MAX(MAXVAL(xfield(:)%nlt),MAXVAL(yfield(:)%nlt)),MAX(MAXVAL(xfield(:)%nlg),MAXVAL(yfield(:)%nlg)),ng)/=0) THEN
     perform_cca=1
     RETURN
  END IF
!
! Determine forecast categories
  WRITE (UNIT=*,FMT='(A)') 'Identifying categories ...'
  dprog=(one-prog)/REAL(1+iretro,KIND=rp) ! - reset progress to minimise floating point errors -
  CALL set_cv_categories ()
  IF (iretro==1) CALL set_ra_categories ()
  perform_cca=0
!
  RETURN
 END FUNCTION perform_cca
!
!
!
 SUBROUTINE full_cca (nt,nx,x,ny,y,ieofx,nxe,svx,eofx,tsx,ieofy,nye,svy,eofy,tsy,ncc,mu,r,s,ifail, &
            yt,hx_map,hy_map,hx_ser,hy_ser)
!
! Performs CCA using all data
!
! Modules
  USE arrays,     ONLY: iusex,iusey,xm,ym,xsd,ysd,xc,yc,ce,dwk,iwk, &
                        stdize
  USE distribs,   ONLY: gaussian
  USE fields,     ONLY: xfield,yfield,rlatx,rlaty, &
                        latitude_weight
  USE iofiles,    ONLY: xfile,yfile
  USE lapack,     ONLY: dgesdd
  USE numbers,    ONLY: zero,five,seven,tol
  USE pcs,        ONLY: npx,npy
  USE settings,   ONLY: igauss,lxt,lyt,lxyt,lc1,lr1,lrwk
  USE statistics, ONLY: moments
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nt    ! - number of cases -
  INTEGER, INTENT(IN) :: nx    ! - number of x spatial points -
  INTEGER, INTENT(IN) :: ny    ! - number of y spatial points -
  INTEGER, INTENT(IN) :: nxe   ! - number of X EOF modes -
  INTEGER, INTENT(IN) :: nye   ! - number of Y EOF modes -
  INTEGER, INTENT(IN) :: ncc   ! - number of CCA modes -
  INTEGER, INTENT(IN) :: ieofx ! - X EOF option -
  INTEGER, INTENT(IN) :: ieofy ! - Y EOF option -
!
! Output scalars
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
  REAL(KIND=rp), INTENT(OUT) :: svy(:)    ! - singular values of y -
  REAL(KIND=rp), INTENT(OUT) :: eofy(:,:) ! - y EOF patterns -
  REAL(KIND=rp), INTENT(OUT) :: tsy(:,:)  ! - time-series of y EOFs (transposed) -
  REAL(KIND=rp), INTENT(OUT) :: mu(:)     ! - canonical correlations -
  REAL(KIND=rp), INTENT(OUT) :: r(:,:)    ! - canonical Y EOF weights -
  REAL(KIND=rp), INTENT(OUT) :: s(:,:)    ! - canonical X EOF weights (transposed) -
!
! - optional output arrays -
  REAL(KIND=rp), INTENT(OUT), OPTIONAL :: yt(:,:)     ! - transformed response variables -
  REAL(KIND=rp), INTENT(OUT), OPTIONAL :: hx_map(:,:) ! - X homogeneous covariance maps -
  REAL(KIND=rp), INTENT(OUT), OPTIONAL :: hy_map(:,:) ! - Y homogeneous covariance maps -
  REAL(KIND=rp), INTENT(OUT), OPTIONAL :: hx_ser(:,:) ! - X homogeneous covariance maps time series -
  REAL(KIND=rp), INTENT(OUT), OPTIONAL :: hy_ser(:,:) ! - Y homogeneous covariance maps time series -
!
! Locals
!
! Local scalars
  INTEGER :: i  ! - current variable -
  INTEGER :: k  ! - case index -
  INTEGER :: ie ! - EOF mode index -
!
  REAL(KIND=rp) :: padj ! - progress increment adjustment -
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
  IF (PRESENT(hx_map)) THEN
     padj=seven
  ELSE
     padj=five
  END IF
  dprog=dprog/padj
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
! Calculate anomalies according to PCA type
  CALL stdize (nx,nt,xc,xm,xsd,3-ieofx)
  CALL stdize (ny,nt,yc,ym,ysd,3-ieofy)
!
! Scale by latitude
  IF (xfile%igrid==1) CALL latitude_weight (xfile%nfs,xfield(:)%nlt,xfield(:)%region,rlatx(:,:),iusex,nt,xc(:,:))
  IF (yfile%igrid==1) CALL latitude_weight (yfile%nfs,yfield(:)%nlt,yfield(:)%region,rlaty(:,:),iusey,nt,yc(:,:))
!
! Update progress meter
  prog=prog+dprog
!
! Perform X EOF prefiltering
  CALL dgesdd ('S',nx,nt,xc,nx,svx,eofx,nx,tsx,lxt,dwk,lrwk,iwk,ifail)
  IF (ifail/=0) RETURN
! - determine number of non-zero eigenvalues -
  npx=COUNT(svx(1:MIN(lxt,nt-1))>zero)
  IF (npx>2) THEN
     IF (svx(npx)*svx(npx-2)/svx(npx-1)**2<tol) npx=npx-1 ! - check for probable rounding errors -
  END IF
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
! - update progress meter -
  prog=prog+dprog
!
! Perform Y EOF prefiltering
  CALL dgesdd ('S',ny,nt,yc,ny,svy,eofy,ny,tsy,lyt,dwk,lrwk,iwk,ifail)
  IF (ifail/=0) RETURN
! - determine number of non-zero eigenvalues -
  npy=COUNT(svy(1:MIN(lyt,nt-1))>zero)
  IF (npy>2) THEN
     IF (svy(npy)*svy(npy-2)/svy(npy-1)**2<tol) npy=npy-1 ! - check for probable rounding errors -
  END IF
! - rescale loadings by latitude -
  IF (yfile%igrid==1) CALL latitude_weight (yfile%nfs,yfield(:)%nlt,yfield(:)%region,rlaty(:,:),iusey,npy,eofy(:,:))
! - ensure that largest absolute loading is positive -
  DO ie=1,npy
     IF (ABS(MAXVAL(eofy(1:ny,ie)))<ABS(MINVAL(eofy(1:ny,ie)))) THEN
        eofy(1:ny,ie)=-eofy(1:ny,ie)
        tsy(ie,1:nt)=-tsy(ie,1:nt)
     END IF
  END DO
  IF (npy<lyt) THEN
     svy(npy+1:lyt)=zero
     tsy(npy+1:lyt,1:nt)=zero
     eofy(1:ny,npy+1:lyt)=zero
  END IF
! - update progress meter -
  prog=prog+dprog
!
! Compute CCA
  ce(1:nye,1:nxe)=MATMUL(tsy(1:nye,1:nt),TRANSPOSE(tsx(1:nxe,1:nt)))
  CALL dgesdd ('S',nye,nxe,ce,lc1,mu,r,lr1,s,lxyt,dwk,lrwk,iwk,ifail)
  IF (ifail/=0) RETURN
! - update progress meter -
  prog=prog+dprog
!
! Compute homogeneous covariance maps
! - X maps -
  IF (PRESENT(hx_map)) THEN
     CALL hcov_maps (nx,nt,nxe,ncc,svx,eofx,tsx,s,hx_map,hx_ser,dwk(1:nxe*ncc))
! - update progress meter -
     prog=prog+dprog
  END IF
! - Y maps -
  IF (PRESENT(hy_map)) THEN
     CALL hcov_maps (ny,nt,nye,ncc,svy,eofy,tsy,TRANSPOSE(r),hy_map,hy_ser,dwk(1:nye*ncc))
! - update progress meter -
     prog=prog+dprog
  END IF
!
! Rescale EOF time series
! - X EOFs -
  DO k=1,nt
     DO ie=1,npx
        tsx(ie,k)=tsx(ie,k)*svx(ie)
     END DO
  END DO
! - Y EOFs -
  DO k=1,nt
     DO ie=1,npy
        tsy(ie,k)=tsy(ie,k)*svy(ie)
     END DO
  END DO
! - update progress meter -
  prog=prog+dprog
!
! Reset progress increment
  dprog=dprog*padj
!
  RETURN
 END SUBROUTINE full_cca
!
!
!
 SUBROUTINE hcov_maps (nv,nt,ne,nc,sv,eof,ts,rs,hmap,hser,rwk)
!
! Constructs CCA maps
!
! Arguments
!
! Inpur scalars
  INTEGER, INTENT(IN) :: nv ! - number of variables -
  INTEGER, INTENT(IN) :: nt ! - number of cases -
  INTEGER, INTENT(IN) :: ne ! - number of EOF modes -
  INTEGER, INTENT(IN) :: nc ! - number of CCA modes -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: sv(:)    ! - singular values -
  REAL(KIND=rp), INTENT(IN) :: eof(:,:) ! - spatial loadings -
  REAL(KIND=rp), INTENT(IN) :: ts(:,:)  ! - temporal scores -
  REAL(KIND=rp), INTENT(IN) :: rs(:,:)  ! - canonical EOF weights -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: hmap(:,:) ! - homogeneous covariance maps -
  REAL(KIND=rp), INTENT(OUT) :: hser(:,:) ! - homogeneous covariance map time series -
!
! Workspace
!
! Workspace arrays
  REAL(KIND=rp) :: rwk(:) ! - real workspace -
!
! Locals
!
! Local scalars
  INTEGER :: i1,i2 ! - indices -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MATMUL
  INTRINSIC RESHAPE
!
! Executable Statements
!
! Calculate homogeneous covariance maps
  DO i2=1,nc
     DO i1=1,ne
        rwk((i2-1)*ne+i1)=sv(i1)*rs(i2,i1)
     END DO
  END DO
  hmap(1:nv,1:nc)=MATMUL(eof(1:nv,1:ne),RESHAPE(rwk(1:ne*nc),(/ne,nc/)))
!
! Calculate corresponding time series
  hser(1:nc,1:nt)=MATMUL(rs(1:nc,1:ne),ts(1:ne,1:nt))
!
  RETURN
 END SUBROUTINE hcov_maps
!
!
!
 SUBROUTINE cv_cca (nt,ncv,nx,x,ny,y,ieofx,nxe,mxe,ieofy,nye,mye,ncc,mcc,yhat,yhatt,nxo,nyo,nco)
!
! Performs cross-validated CCA
!
! Modules
  USE analysis,   ONLY: nopt
  USE arrays,     ONLY: iusex,iusey,svx,eofx,tsx,svy,eofy,tsy,mu,r,s,ave,sdev,xm,ym,xsd,ysd,xc,yc,ce,yopt,dwk,iwk, &
                        stdize
  USE distribs,   ONLY: gaussian,gaussian_inv
  USE fields,     ONLY: xfield,yfield,rlatx,rlaty, &
                        latitude_weight
  USE iofiles,    ONLY: xfile,yfile
  USE lapack,     ONLY: dgesdd
  USE numbers,    ONLY: zero,one,tol
  USE pcs,        ONLY: npx,npy,icco
  USE settings,   ONLY: igauss,izero,istd,igood,hcw,lxt,lyt,lxyt,lc1,lr1,lrwk
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
  INTEGER, INTENT(IN) :: nye   ! - minimum number of Y EOF modes -
  INTEGER, INTENT(IN) :: mye   ! - maximum number of Y EOF modes -
  INTEGER, INTENT(IN) :: ncc   ! - minimum number of CCA modes -
  INTEGER, INTENT(IN) :: mcc   ! - maximum number of CCA modes -
  INTEGER, INTENT(IN) :: ieofx ! - X EOF option -
  INTEGER, INTENT(IN) :: ieofy ! - Y EOF option -
!
! Output scalars
! - optional output scalars -
  INTEGER, INTENT(OUT), OPTIONAL :: nxo ! - optimal number of X EOF modes -
  INTEGER, INTENT(OUT), OPTIONAL :: nyo ! - optimal number of Y EOF modes -
  INTEGER, INTENT(OUT), OPTIONAL :: nco ! - optimal number of CCA modes -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:,:) ! - explanatory variables -
  REAL(KIND=rp), INTENT(IN) :: y(:,:) ! - response variables -
!
! Output arrays
! - optional output scalars -
  REAL(KIND=rp), INTENT(OUT), OPTIONAL :: yhat(:,:)  ! - cross-validated hindcasts -
  REAL(KIND=rp), INTENT(OUT), OPTIONAL :: yhatt(:,:) ! - transformed cross-validated hindcasts -
!
! Locals
!
! Local scalars
  INTEGER :: it    ! - cross-validation time-step -
  INTEGER :: i     ! - predictor/predictand index -
  INTEGER :: i1,i2 ! - cross-validation indices -
  INTEGER :: ixe   ! - X EOF mode index -
  INTEGER :: iye   ! - Y EOF mode index -
  INTEGER :: icc   ! - current number of CCA modes -
  INTEGER :: ixu   ! - used X EOF mode index -
  INTEGER :: iyu   ! - used Y EOF mode index -
  INTEGER :: icu   ! - current number of used CCA modes -
  INTEGER :: iopt  ! - optimization step index -
  INTEGER :: jopt  ! - index of optimized settings -
  INTEGER :: nprg  ! - progress meter adjustment -
  INTEGER :: ifail ! - error indicator -
!
  REAL(KIND=rp) :: gm    ! - goodness metric -
  REAL(KIND=rp) :: gmo   ! - optimal goodness metric -
  REAL(KIND=rp) :: szero ! - standardized zero -
!
! Local arrays
  LOGICAL :: lopt(mxe*mye*mcc) ! - optimized flag -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ADJUSTR
  INTRINSIC ANY
  INTRINSIC COUNT
  INTRINSIC MATMUL
  INTRINSIC MAX
  INTRINSIC MIN
  INTRINSIC PRESENT
  INTRINSIC REAL
  INTRINSIC TRANSPOSE
!
! Executable Statements
!
! Adjust progress meter
  nprg=MAX(1,nopt)*(1+izero)
  IF (nprg>1) dprog=dprog/REAL(nprg,KIND=rp)
!
! Set optimization checks
  lopt(:)=.true.
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
     CALL moments (ny,ncv,yc,ym,ysd)
!
! Calculate anomalies according to PC type
     CALL stdize (nx,ncv,xc,xm,xsd,3-ieofx)
     CALL stdize (ny,ncv,yc,ym,ysd,3-ieofy)
!
! Scale by latitude
     IF (xfile%igrid==1) CALL latitude_weight (xfile%nfs,xfield(:)%nlt,xfield(:)%region,rlatx(:,:),iusex,ncv,xc(:,:))
     IF (yfile%igrid==1) CALL latitude_weight (yfile%nfs,yfield(:)%nlt,yfield(:)%region,rlaty(:,:),iusey,ncv,yc(:,:))
!
! Perform EOF prefiltering
! - X variables -
     CALL dgesdd ('S',nx,ncv,xc,nx,svx,eofx,nx,tsx,lxt,dwk,lrwk,iwk,ifail)
! - Y variables -
     IF (ifail==0) CALL dgesdd ('S',ny,ncv,yc,ny,svy,eofy,ny,tsy,lyt,dwk,lrwk,iwk,ifail)
!
! Determine number of non-zero eigenvalues
     iopt=0
     IF (ifail==0) THEN
        npx=COUNT(svx(1:MIN(mxe,ncv-1))>zero)
        npy=COUNT(svy(1:MIN(mye,ncv-1))>zero)
! - check for near-zero eigenvalues -
        IF (npx>2) THEN
           IF (svx(npx)*svx(npx-2)/svx(npx-1)**2<tol) npx=npx-1
        END IF
        IF (npy>2) THEN
           IF (svy(npy)*svy(npy-2)/svy(npy-1)**2<tol) npy=npy-1
        END IF
! - rescale time scores -
        IF (npx<mxe) THEN
           svx(npx+1:mxe)=zero
           tsx(npx+1:mxe,1:ncv)=zero
           eofx(1:nx,npx+1:mxe)=zero
        END IF
        IF (npy<mye) THEN
           svy(npy+1:mye)=zero
           tsy(npy+1:mye,1:ncv)=zero
           eofy(1:ny,npy+1:mye)=zero
        END IF
!
! Rescale by latitude
        IF (xfile%igrid==1) CALL latitude_weight (xfile%nfs,xfield(:)%nlt,xfield(:)%region,rlatx(:,:),iusex,npx,eofx(:,:))
        IF (yfile%igrid==1) CALL latitude_weight (yfile%nfs,yfield(:)%nlt,yfield(:)%region,rlaty(:,:),iusey,npy,eofy(:,:))
!
! Compute CCA
        DO ixe=nxe,mxe
           ixu=MIN(ixe,npx)
           DO iye=nye,mye
              iyu=MIN(iye,npy)
              ce(1:iyu,1:ixu)=MATMUL(tsy(1:iyu,1:ncv),TRANSPOSE(tsx(1:ixu,1:ncv)))
              CALL dgesdd ('S',iyu,ixu,ce,lc1,mu,r,lr1,s,lxyt,dwk,lrwk,iwk,ifail)
!
! Check for over-fitting
              IF ((icco==0).AND.(ANY(mu(1:MIN(ixu,iyu))>one-tol))) THEN
                 DO icc=ncc,MIN(mcc,ixe,iye)
                    iopt=iopt+1
                    lopt(iopt)=.false.
                    yopt(1:ny,it,iopt)=ym(1:ny)
                 END DO
                 CYCLE
              ELSE
!
! Predict anomaly
                 DO icc=ncc,MIN(mcc,ixe,iye)
                    icu=MIN(icc,ixu,iyu)
                    iopt=iopt+1
                    IF ((icco==1).OR.(lopt(iopt))) THEN
                       CALL cca_predict (1,nx,ny,ixu,iyu,icu,ieofx,x(1:nx,it:it),ieofy,yopt(1:ny,it:it,iopt))
                    ELSE
                       yopt(1:ny,it,iopt)=ym(1:ny)
                    END IF
                 END DO
              END IF
           END DO
        END DO
!
! Supply mean if CCA failed
     ELSE
        DO ixe=nxe,mxe
           DO iye=nye,mye
              DO icc=ncc,MIN(mcc,ixe,iye)
                 iopt=iopt+1
                 yopt(1:ny,it,iopt)=ym(1:ny)
                 prog=prog+dprog
              END DO
           END DO
        END DO
     END IF
  END DO time_step
!
! Adjust progress meter
  IF (nprg>0) dprog=dprog*REAL(nprg,KIND=rp)
!
! Calculate and print goodness metric
  IF (((PRESENT(nxo)).OR.(PRESENT(nyo)).OR.(PRESENT(nco))).AND.(ANY(lopt))) THEN
     WRITE (UNIT=*,FMT='(A)') '               CURRENT                         OPTIMUM'
     WRITE (UNIT=*,FMT='(A)') ' '
     WRITE (UNIT=*,FMT='(A)') '      Number of Modes    Goodness      Number of Modes    Goodness'
     WRITE (UNIT=*,FMT='(A)') '         X    Y   CCA       Index         X    Y   CCA       Index'
     WRITE (UNIT=*,FMT='(A)') ' '
     gmo=-one
     iopt=0
     jopt=1
     nxo=nxe
     nyo=nye
     nco=ncc
     DO ixe=nxe,mxe
        DO iye=nye,mye
           DO icc=ncc,MIN(mcc,ixe,iye)
              iopt=iopt+1
              IF (lopt(iopt)) THEN
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
                    nyo=iye
                    nco=icc
                 END IF
                 WRITE (UNIT=*,FMT='(2(5X,2I5,I6,F12.3))') ixe,iye,icc,gm,nxo,nyo,nco,gmo
              ELSE
                 WRITE (UNIT=*,FMT='(5X,2I5,I6,A12,5X,2I5,I6,F12.3)') ixe,iye,icc,ADJUSTR('N/A'),nxo,nyo,nco,gmo
              END IF
           END DO
        END DO
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
 END SUBROUTINE cv_cca
!
!
!
 SUBROUTINE cca_predict (nf,nx,ny,nxe,nye,ncc,ieofx,x,ieofy,fcast,nt,xvp,xhat,nc,clim,fcastt)
!
! Calculates predictions given new predictor values
!
! Modules
  USE arrays,   ONLY: svx,eofx,svy,eofy,mu,r,s,ave,sdev,xm,xsd,ym,ysd,xc,prjc,dwk, &
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
  INTEGER, INTENT(IN) :: nxe   ! - number of X EOF modes -
  INTEGER, INTENT(IN) :: nye   ! - number of Y EOF modes -
  INTEGER, INTENT(IN) :: ncc   ! - number of CCA modes -
  INTEGER, INTENT(IN) :: ieofx ! - X EOF option -
  INTEGER, INTENT(IN) :: ieofy ! - Y EOF option -
!
  INTEGER, INTENT(IN), OPTIONAL :: nt  ! - number of cases in training period -
  INTEGER, INTENT(IN), OPTIONAL :: nc  ! - number of cases in climatology -
!
! Arrays,
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:,:) ! - new predictor values -
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
  INTRINSIC TRANSPOSE
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
!
! Project X anomalies onto the X EOFs
     dwk(1:nxe)=MATMUL(TRANSPOSE(eofx(1:nx,1:nxe)),xc(1:nx,k))
     IF (PRESENT(xhat)) THEN
        xhat(1:nxe,k)=dwk(1:nxe)
        IF (nxe<mxe) THEN
           DO i=nxe+1,mxe
              xhat(i,k)=SUM(eofx(:,i)*xc(:,k))
           END DO
        END IF
     END IF
! - standardise to unit variance by scaling by inverse of singular values -
     dwk(1:nxe)=dwk(1:nxe)/svx(1:nxe)
!
! Project X EOFs onto the CCA modes
     prjc(1:ncc)=MATMUL(s(1:ncc,1:nxe),dwk(1:nxe))
     IF (PRESENT(xvp)) THEN
        xvp(k)=one/df+SUM(prjc(1:ncc))**2
     END IF
! - scale by singular values -
     prjc(1:ncc)=prjc(1:ncc)*mu(1:ncc)
!
! Project CCA modes back onto the Y EOF modes
     dwk(1:nye)=MATMUL(r(1:nye,1:ncc),prjc(1:ncc))
! - scale by singular values -
     dwk(1:nye)=dwk(1:nye)*svy(1:nye)
!
! Project Y EOF modes back onto the Y anomalies
     fcast(1:ny,k)=MATMUL(eofy(1:ny,1:nye),dwk(1:nye))
!
! Rescale
     SELECT CASE (ieofy)
      CASE (1)
        fcast(1:ny,k)=fcast(1:ny,k)*ysd(1:ny)+ym(1:ny)
      CASE (2)
        fcast(1:ny,k)=fcast(1:ny,k)+ym(1:ny)
     END SELECT
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
     prog=prog+dprog
  END IF
!
  RETURN
 END SUBROUTINE cca_predict
END MODULE cca
