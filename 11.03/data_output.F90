!$Id: data_output.F90 1238 2011-03-04 16:25:04Z simon $
MODULE data_output
!
! Modules
  USE numbers, ONLY: rp=>dp
  USE time
!
! Implicit declarations
  IMPLICIT NONE
!
CONTAINS
!
!
 FUNCTION save_data()
!
! Modules
  USE analysis, ONLY: ianal,ifc
  USE iofiles,  ONLY: xefile,yefile,xlfile,ylfile,xsfile,ysfile,xmfile,ymfile,xtfile,ytfile,ccfile, &
                      rbfile,pbfile,yhfile,yrfile,rpfile,rlfile,xofile,xifile,yofile,               &
                      fpfile,fofile,fefile,fvfile,fsfile,flfile,fxfile,                             &
                      geto_gen
  USE settings, ONLY: iretro
!
! Function type
  INTEGER :: save_data
!
! Locals
!
! Local scalars
  INTEGER :: iw  ! - selected file -
  INTEGER :: ios ! - IO status -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC TRIM
!
! Executable Statements
!
! Get output files
  DO
     WRITE (UNIT=*,FMT=*)
     WRITE (UNIT=*,FMT='(A)') 'Output files:'
!
! Input data
     WRITE (UNIT=*,FMT=*)
     WRITE (UNIT=*,FMT='(A)') 'Input Data'
     WRITE (UNIT=*,FMT='(I2,2A)') xofile%nfile,'. ',TRIM(xofile%desc)
     IF (ianal==4) THEN
        WRITE (UNIT=*,FMT='(I2,2A)') xifile%nfile,'. ',TRIM(xifile%desc)
     END IF
     WRITE (UNIT=*,FMT='(I2,2A)') yofile%nfile,'. ',TRIM(yofile%desc)
!
! Historical predictions
     WRITE (UNIT=*,FMT=*)
     WRITE (UNIT=*,FMT='(A)') 'Historical Predictions'
     WRITE (UNIT=*,FMT='(I2,2A)') yhfile%nfile,'. ',TRIM(yhfile%desc)
     IF (iretro==1) THEN
        WRITE (UNIT=*,FMT='(I2,2A)') yrfile%nfile,'. ',TRIM(yrfile%desc)
        WRITE (UNIT=*,FMT='(I2,2A)') rpfile%nfile,'. ',TRIM(rpfile%desc)
        WRITE (UNIT=*,FMT='(I2,2A)') rlfile%nfile,'. ',TRIM(rlfile%desc)
     END IF
!
! EOF prefiltering
     IF (ianal<=2) THEN
        WRITE (UNIT=*,FMT=*)
        WRITE (UNIT=*,FMT='(A)') 'EOF Prefiltering'
! - X EOF files -
        WRITE (UNIT=*,FMT='(A)') 'X variable EOF files'
        WRITE (UNIT=*,FMT='(I2,2A)') xefile%nfile,'. ',TRIM(xefile%desc)
        WRITE (UNIT=*,FMT='(I2,2A)') xlfile%nfile,'. ',TRIM(xlfile%desc)
        WRITE (UNIT=*,FMT='(I2,2A)') xsfile%nfile,'. ',TRIM(xsfile%desc)
     END IF
! - Y EOF files -
     SELECT CASE (ianal)
      CASE (1)
        WRITE (UNIT=*,FMT='(A)') 'Y variable EOF files'
        WRITE (UNIT=*,FMT='(I2,2A)') yefile%nfile,'. ',TRIM(yefile%desc)
        WRITE (UNIT=*,FMT='(I2,2A)') ylfile%nfile,'. ',TRIM(ylfile%desc)
        WRITE (UNIT=*,FMT='(I2,2A)') ysfile%nfile,'. ',TRIM(ysfile%desc)
!
! CCA results
        WRITE (UNIT=*,FMT=*)
        WRITE (UNIT=*,FMT='(A)') 'CCA Results'
! - canonical correlations -
        WRITE (UNIT=*,FMT='(A)') 'Canonical correlations'
        WRITE (UNIT=*,FMT='(I2,2A)') ccfile%nfile,'. ',TRIM(ccfile%desc)
! - X homogeneous covariance maps -
        WRITE (UNIT=*,FMT='(A)') 'X homogeneous covariance maps'
        WRITE (UNIT=*,FMT='(I2,2A)') xmfile%nfile,'. ',TRIM(xmfile%desc)
        WRITE (UNIT=*,FMT='(I2,2A)') xtfile%nfile,'. ',TRIM(xtfile%desc)
! - Y homogeneous covariance maps -
        WRITE (UNIT=*,FMT='(A)') 'Y homogeneous covariance maps'
        WRITE (UNIT=*,FMT='(I2,2A)') ymfile%nfile,'. ',TRIM(ymfile%desc)
        WRITE (UNIT=*,FMT='(I2,2A)') ytfile%nfile,'. ',TRIM(ytfile%desc)
!
! PCR results
      CASE (2)
        WRITE (UNIT=*,FMT=*)
        WRITE (UNIT=*,FMT='(A)') 'PCR Results'
! - regression files -
        WRITE (UNIT=*,FMT='(A)') 'Regression files'
        WRITE (UNIT=*,FMT='(I2,2A)') rbfile%nfile,'. ',TRIM(rbfile%desc)
        WRITE (UNIT=*,FMT='(I2,2A)') pbfile%nfile,'. ',TRIM(pbfile%desc)
!
! MLR results
      CASE (3)
        WRITE (UNIT=*,FMT=*)
        WRITE (UNIT=*,FMT='(A)') 'MLR Results'
! - regression files -
        WRITE (UNIT=*,FMT='(A)') 'Regression files'
        WRITE (UNIT=*,FMT='(I2,2A)') rbfile%nfile,'. ',TRIM(rbfile%desc)
     END SELECT
!
! Get forecast output files
     IF (ifc==2) THEN
        WRITE (UNIT=*,FMT=*)
        WRITE (UNIT=*,FMT='(A)') 'Forecasts'
        WRITE (UNIT=*,FMT='(A)') 'Probabilistic forecasts'
        WRITE (UNIT=*,FMT='(I2,2A)') fpfile%nfile,'. ',TRIM(fpfile%desc)
        WRITE (UNIT=*,FMT='(I2,2A)') fofile%nfile,'. ',TRIM(fofile%desc)
        WRITE (UNIT=*,FMT=*)
        WRITE (UNIT=*,FMT='(A)') 'Deterministic forecasts'
        WRITE (UNIT=*,FMT='(I2,2A)') fvfile%nfile,'. ',TRIM(fvfile%desc)
        WRITE (UNIT=*,FMT='(I2,2A)') fsfile%nfile,'. ',TRIM(fsfile%desc)
        WRITE (UNIT=*,FMT='(I2,2A)') fefile%nfile,'. ',TRIM(fefile%desc)
        WRITE (UNIT=*,FMT=*)
        WRITE (UNIT=*,FMT='(A)') 'Confidence limits'
        WRITE (UNIT=*,FMT='(I2,2A)') flfile%nfile,'. ',TRIM(flfile%desc)
        WRITE (UNIT=*,FMT=*)
        WRITE (UNIT=*,FMT='(A)') 'Predictors'
        WRITE (UNIT=*,FMT='(I2,2A)') fxfile%nfile,'. ',TRIM(fxfile%desc)
     END IF
!
! Cancel
     WRITE (UNIT=*,FMT=*)
     WRITE (UNIT=*,FMT='(A)') ' 0. Exit'
!
! Identify file
     READ (UNIT=*,FMT=*,IOSTAT=ios) iw
     IF (ios/=0) CYCLE
     IF ( (ianal/=4 ).and. (iw==2) ) CYCLE
     IF (iw==0) THEN
        EXIT
     ELSE IF (iw==xofile%nfile) THEN ! - X input data output file -
        CALL geto_gen (xofile)
     ELSE IF (iw==xifile%nfile) THEN ! - interpolated X output file -
        CALL geto_gen (xifile)
     ELSE IF (iw==yofile%nfile) THEN ! - Y input data output file -
        CALL geto_gen (yofile)
     ELSE IF (iw==yhfile%nfile) THEN ! - cross-validated predictions output file -
        CALL geto_gen (yhfile)
     ELSE IF (iw==yrfile%nfile) THEN ! - retroactive predictions output file -
        IF (iretro/=1) CYCLE
        CALL geto_gen (yrfile)
     ELSE IF (iw==rpfile%nfile) THEN ! - retroactive forecast probabilities output file -
        IF (iretro/=1) CYCLE
        CALL geto_gen (rpfile)
     ELSE IF (iw==rlfile%nfile) THEN ! - retroactive lower prediction limits output file -
        IF (iretro/=1) CYCLE
        CALL geto_gen (rlfile)
     ELSE IF (iw==xefile%nfile) THEN ! - X eigenvalues output file -
        CALL geto_gen (xefile)
     ELSE IF (iw==xlfile%nfile) THEN ! - X spatial loadings output file -
        CALL geto_gen (xlfile)
     ELSE IF (iw==xsfile%nfile) THEN ! - X temporal scores output file -
        CALL geto_gen (xsfile)
     ELSE IF (iw==yefile%nfile) THEN ! - Y eigenvalues output file -
        IF (ianal==2) CYCLE
        CALL geto_gen (yefile)
     ELSE IF (iw==ylfile%nfile) THEN ! - Y spatial loadings output file -
        IF (ianal==2) CYCLE
        CALL geto_gen (ylfile)
     ELSE IF (iw==ysfile%nfile) THEN ! - Y temporal scores output file -
        IF (ianal==2) CYCLE
        CALL geto_gen (ysfile)
     ELSE IF (iw==ccfile%nfile) THEN ! - canonical correlations output file -
        IF (ianal==2) CYCLE
        CALL geto_gen (ccfile)
     ELSE IF (iw==xmfile%nfile) THEN ! - X homogeneous covariance maps output file -
        IF (ianal==2) CYCLE
        CALL geto_gen (xmfile)
     ELSE IF (iw==xtfile%nfile) THEN ! - X homogeneous covariance maps time series output file -
        IF (ianal==2) CYCLE
        CALL geto_gen (xtfile)
     ELSE IF (iw==ymfile%nfile) THEN ! - Y homogeneous covariance maps output file -
        IF (ianal==2) CYCLE
        CALL geto_gen (ymfile)
     ELSE IF (iw==ytfile%nfile) THEN ! - Y homogeneous covariance maps time series output file -
        IF (ianal==2) CYCLE
        CALL geto_gen (ytfile)
     ELSE IF (iw==rbfile%nfile) THEN ! - regression coefficients output file -
        IF (ianal==1) CYCLE
        CALL geto_gen (rbfile)
     ELSE IF (iw==pbfile%nfile) THEN ! - PC regression coefficients output file -
        IF (ianal==1) CYCLE
        CALL geto_gen (pbfile)
     ELSE IF (iw==fpfile%nfile) THEN ! - forecast probabilities output file -
        IF (ifc/=2) CYCLE
        CALL geto_gen (fpfile)
     ELSE IF (iw==fofile%nfile) THEN ! - forecast odds output file -
        IF (ifc/=2) CYCLE
        CALL geto_gen (fofile)
     ELSE IF (iw==fvfile%nfile) THEN ! - forecasts output file -
        IF (ifc/=2) CYCLE
        CALL geto_gen (fvfile)
     ELSE IF (iw==fsfile%nfile) THEN ! - forecast ensembles output file -
        IF (ifc/=2) CYCLE
        CALL geto_gen (fsfile)
     ELSE IF (iw==fefile%nfile) THEN ! - prediction error variances output file -
        IF (ifc/=2) CYCLE
        CALL geto_gen (fefile)
     ELSE IF (iw==flfile%nfile) THEN ! - prediction limits output file -
        IF (ifc/=2) CYCLE
        CALL geto_gen (flfile)
     ELSE IF (iw==fxfile%nfile) THEN ! - predictor time scores output file -
        IF (ifc/=2) CYCLE
        CALL geto_gen (fxfile)
      ELSE
        CYCLE
     END IF
!
! Save requested results
     CALL write_results ()
  END DO
  save_data=0
!
  RETURN
 END FUNCTION save_data
!
!
!
 FUNCTION save_rocs()
!
! Modules
  USE iofiles, ONLY: rrfile, &
                     geto_gen
!
! Function type
  INTEGER :: save_rocs
!
! Executable Statements
!
! Get output file
  CALL geto_gen (rrfile)
!
! Save requested results
  CALL write_results ()
  save_rocs=0
!
  RETURN
 END FUNCTION save_rocs
!
!
!
 FUNCTION save_roc()
!
! Modules
  USE iofiles, ONLY: rofile, &
                     geto_gen
!
! Function type
  INTEGER :: save_roc
!
! Executable Statements
!
! Get output file
  CALL geto_gen (rofile)
!
! Save requested results
  CALL write_results ()
  save_roc=0
!
  RETURN
 END FUNCTION save_roc
!
!
!
 FUNCTION save_rel()
!
! Modules
  USE iofiles, ONLY: atfile, &
                     geto_gen
!
! Function type
  INTEGER :: save_rel
!
! Executable Statements
!
! Get output file
  CALL geto_gen (atfile)
!
! Save requested results
  CALL write_results ()
  save_rel=0
!
  RETURN
 END FUNCTION save_rel
!
!
!
 FUNCTION save_wrlt()
!
! Modules
  USE iofiles, ONLY: wrfile, &
                     geto_gen
!
! Function type
  INTEGER :: save_wrlt
!
! Executable Statements
!
! Get output file
  CALL geto_gen (wrfile)
!
! Save requested results
  CALL write_results()
  save_wrlt=0
!
  RETURN
 END FUNCTION save_wrlt
!
!
!
 FUNCTION save_skill()
!
! Modules
  USE iofiles,    ONLY: skfile,pvfile, &
                        geto_gen
  USE settings,   ONLY: ipval
!
! Function type
  INTEGER :: save_skill
!
! Executable Statements
!
! Open window
!
! Get output file
  CALL geto_gen (skfile)
  IF (ipval==1) THEN
     CALL geto_gen(pvfile)
  END IF

! Save requested results
  CALL write_results ()
  save_skill=0
!
  RETURN
 END FUNCTION save_skill
!
!
!
 SUBROUTINE write_results ()
!
! Modules
  USE analysis,      ONLY: prog,dprog,ianal
  USE arrays,        ONLY: x,iusex,xiny,y,iusey,kuse,kfuse,yhat,yret,yrpls,rfps,svx,svy,eofx,eofy,tsx,tsy,mu,hx_map,hy_map,hx_ser, &
                           hy_ser,b,bz,b0,skills,pvalues,hit,far,hits,fars,afp,orf,ifq,cump,eir,fcast,pev,fpls,fps,odds,oddr,xhat
  USE categories,    ONLY: ng
  USE CPT_constants, ONLY: nb,nwr
  USE errors,        ONLY: error
  USE fields,        ONLY: xfield,rlatdx,rlngdx,cstndx, &
                           yfield,rlatdy,rlngdy,cstndy
  USE IO_constants,  ONLY: iout,ffmts,lstn,lvar
  USE iofiles,       ONLY: xfile,yfile,zfile,                                &
                           xofile,xifile,yofile,                             &
                           yhfile,yrfile,rpfile,rlfile,                      &
                           xefile,xlfile,xsfile,xmfile,xtfile,               &
                           yefile,ylfile,ysfile,ymfile,ytfile,               &
                           ccfile,rbfile,pbfile,                             &
                           skfile,pvfile,rofile,rrfile,atfile,wrfile,        &
                           fpfile,fofile,fvfile,fsfile,fefile,flfile,fxfile, &
                           file_reset,files_reset
  USE labels,        ONLY: cg_done,cg_skill_t,cg_wrlts_t
  USE numbers,       ONLY: zero,one
  USE pcs,           ONLY: mxe,mye,mcc,nco
  USE settings,      ONLY: nx,ny,mya,nt,nt1,ntr,nf,nens,lxt,lyt,iodds,clf
  USE statistics,    ONLY: roca,rocas,iskill
!
! Locals
!
! Local scalars
  INTEGER :: nsave ! - number of files to save -
  INTEGER :: it0   ! - time 0 -
  INTEGER :: ifail ! - error flag -
!
  CHARACTER(LEN=lvar+1) :: cout ! - output field -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC RESHAPE
  INTRINSIC TRANSPOSE
  INTRINSIC TRIM
!
! Executable Statements
!
! Update console
  prog=zero
  WRITE (UNIT=*,FMT=*)
! - count number of files to save -
  nsave=0
  IF (xofile%lset) nsave=nsave+1
  IF (xifile%lset) nsave=nsave+1
  IF (yofile%lset) nsave=nsave+1
  IF (yhfile%lset) nsave=nsave+1
  IF (yrfile%lset) nsave=nsave+1
  IF (rpfile%lset) nsave=nsave+1
  IF (rlfile%lset) nsave=nsave+1
  IF (xefile%lset) nsave=nsave+1
  IF (yefile%lset) nsave=nsave+1
  IF (xlfile%lset) nsave=nsave+1
  IF (ylfile%lset) nsave=nsave+1
  IF (xsfile%lset) nsave=nsave+1
  IF (ysfile%lset) nsave=nsave+1
  IF (ccfile%lset) nsave=nsave+1
  IF (xmfile%lset) nsave=nsave+1
  IF (ymfile%lset) nsave=nsave+1
  IF (xtfile%lset) nsave=nsave+1
  IF (ytfile%lset) nsave=nsave+1
  IF (rbfile%lset) nsave=nsave+1
  IF (pbfile%lset) nsave=nsave+1
  IF (fpfile%lset) nsave=nsave+1
  IF (fofile%lset) nsave=nsave+1
  IF (fvfile%lset) nsave=nsave+1
  IF (fsfile%lset) nsave=nsave+1
  IF (fefile%lset) nsave=nsave+1
  IF (flfile%lset) nsave=nsave+1
  IF (fxfile%lset) nsave=nsave+1
  IF (skfile%lset) nsave=nsave+1
  IF (pvfile%lset) nsave=nsave+1
  IF (rofile%lset) nsave=nsave+1
  IF (rrfile%lset) nsave=nsave+1
  IF (atfile%lset) nsave=nsave+1
  IF (wrfile%lset) nsave=nsave+1
  IF (nsave==0) THEN
     WRITE (UNIT=*,FMT='(A)') 'No files to save.'
     prog=one
     CALL files_reset()
     RETURN
  END IF
  dprog=one/REAL(nsave,KIND=rp)
!
! Save input data with missing values estimated
! - X input data -
  IF (xofile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(xofile%ffile)
     CALL open_output (iout,xofile,xfile%nfs,ifail)
     IF (ifail==0) THEN
        SELECT CASE (xfile%igrid)
         CASE (1)
           CALL write_grid (iout,xofile,xfield,xfile%nfs,xfile%nls,nt,nx,x,iusex,kuse,rlatdx,rlngdx,ifail, &
                it0=xfile%it1-1)
         CASE (2)
           CALL write_stns (iout,xofile,xfield,xfile%nfs,xfile%nls,xfile%it1-1,nt,nx,x,iusex,kuse,rlatdx,rlngdx,cstndx,ifail)
         CASE (3)
           CALL write_unrf (iout,xofile,xfield,xfile%nfs,xfile%nls,xfile%it1-1,nt,nx,x,iusex,kuse,cstndx,ifail)
        END SELECT
     END IF
     CLOSE (UNIT=iout)
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(xofile%desc),c_arg2=TRIM(xofile%ffile))
     CALL file_reset (xofile%lset,xofile%fname)
     prog=prog+dprog
  END IF
! - interpolated X data -
  IF (xifile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(xifile%ffile)
     CALL open_output (iout,xifile,xfile%nfs,ifail)
     IF (ifail==0) THEN
        SELECT CASE (yfile%igrid)
         CASE (1)
           CALL write_grid (iout,xifile,yfield,yfile%nfs,yfile%nls,nt,ny,xiny,iusey,kuse,rlatdy,rlngdy,ifail, &
                it0=xfile%it1-1)
         CASE (2)
           CALL write_stns (iout,xifile,yfield,yfile%nfs,yfile%nls,xfile%it1-1,nt,ny,xiny,iusey,kuse,rlatdy,rlngdy,cstndy,ifail)
         CASE (3)
           CALL write_unrf (iout,xifile,yfield,yfile%nfs,yfile%nls,xfile%it1-1,nt,ny,xiny,iusey,kuse,cstndy,ifail)
        END SELECT
     END IF
     CLOSE (UNIT=iout)
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(xifile%desc),c_arg2=TRIM(xifile%ffile))
     CALL file_reset (xifile%lset,xifile%fname)
     prog=prog+dprog
  END IF
! - Y input data -
  IF (yofile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(yofile%ffile)
     CALL open_output (iout,yofile,yfile%nfs,ifail)
     IF (ifail==0) THEN
        SELECT CASE (yfile%igrid)
         CASE (1)
           CALL write_grid (iout,yofile,yfield,yfile%nfs,yfile%nls,nt,ny,y,iusey,kuse,rlatdy,rlngdy,ifail, &
                it0=yfile%it1-1)
         CASE (2)
           CALL write_stns (iout,yofile,yfield,yfile%nfs,yfile%nls,yfile%it1-1,nt,ny,y,iusey,kuse,rlatdy,rlngdy,cstndy,ifail)
         CASE (3)
           CALL write_unrf (iout,yofile,yfield,yfile%nfs,yfile%nls,yfile%it1-1,nt,ny,y,iusey,kuse,cstndy,ifail)
        END SELECT
     END IF
     CLOSE (UNIT=iout)
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(yofile%desc),c_arg2=TRIM(yofile%ffile))
     CALL file_reset (yofile%lset,yofile%fname)
     prog=prog+dprog
  END IF
!
! Save predictions
! - cross-validated predictions -
  IF (yhfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(yhfile%ffile)
     CALL open_output (iout,yhfile,yfile%nfs,ifail)
     IF (ifail==0) THEN
        SELECT CASE (yfile%igrid)
         CASE (1)
           CALL write_grid (iout,yhfile,yfield,yfile%nfs,yfile%nls,nt,ny,yhat,iusey,kuse,rlatdy,rlngdy,ifail, &
                it0=yfile%it1-1)
         CASE (2)
           CALL write_stns (iout,yhfile,yfield,yfile%nfs,yfile%nls,yfile%it1-1,nt,ny,yhat,iusey,kuse,rlatdy,rlngdy,cstndy,ifail)
         CASE (3)
           CALL write_unrf (iout,yhfile,yfield,yfile%nfs,yfile%nls,yfile%it1-1,nt,ny,yhat,iusey,kuse,cstndy,ifail)
        END SELECT
     END IF
     CLOSE (UNIT=iout)
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(yhfile%desc),c_arg2=TRIM(yhfile%ffile))
     CALL file_reset (yhfile%lset,yhfile%fname)
     prog=prog+dprog
  END IF
! - retroactive predictions -
  IF (yrfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(yrfile%ffile)
     CALL open_output (iout,yrfile,yfile%nfs,ifail)
     IF (ifail==0) THEN
        SELECT CASE (yfile%igrid)
         CASE (1)
           CALL write_grid (iout,yrfile,yfield,yfile%nfs,yfile%nls,ntr,ny,yret,iusey,kuse(nt1+1:),rlatdy,rlngdy,ifail, &
                it0=yfile%it1+nt1-1)
         CASE (2)
           CALL write_stns (iout,yrfile,yfield,yfile%nfs,yfile%nls,yfile%it1+nt1-1,ntr,ny,yret,iusey,kuse(nt1+1:),rlatdy,rlngdy, &
                cstndy,ifail)
         CASE (3)
           CALL write_unrf (iout,yrfile,yfield,yfile%nfs,yfile%nls,yfile%it1+nt1-1,ntr,ny,yret,iusey,kuse(nt1+1:),cstndy,ifail)
        END SELECT
     END IF
     CLOSE (UNIT=iout)
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(yrfile%desc),c_arg2=TRIM(yrfile%ffile))
     CALL file_reset (yrfile%lset,yrfile%fname)
     prog=prog+dprog
  END IF
! - retroactive forecast probabilities -
  IF (rpfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(rpfile%ffile)
     CALL open_output (iout,rpfile,yfile%nfs*ng,ifail)
     IF (ifail==0) THEN
        SELECT CASE (yfile%igrid)
         CASE (1)
           CALL write_grids ('P',iout,rpfile,yfield,yfile%nfs,yfile%nls,yfile%it1+nt1-1,ntr,ny,ng,rfps(:,:,:), &
                iusey,kuse(nt1+1:),rlatdy,rlngdy,ifail)
         CASE (2)
           CALL write_stnss ('P',iout,rpfile,yfield,yfile%nfs,yfile%nls,yfile%it1+nt1-1,ntr,ny,ng,rfps(:,:,:), &
                iusey,kuse(nt1+1:),rlatdy,rlngdy,cstndy,ifail)
         CASE (3)
           CALL write_unrfs ('P',iout,rpfile,yfield,yfile%nfs,yfile%nls,yfile%it1+nt1-1,ntr,ny,ng,rfps(:,:,:), &
                iusey,kuse(nt1+1:),cstndy,ifail)
        END SELECT
     END IF
     CLOSE (UNIT=iout)
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(rpfile%desc),c_arg2=TRIM(rpfile%ffile))
     CALL file_reset (rpfile%lset,rpfile%fname)
     prog=prog+dprog
  END IF
!
! Save retroactive prediction limits
  IF (rlfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(rlfile%ffile)
     CALL open_output (iout,rlfile,yfile%nfs*2,ifail)
     IF (ifail==0) THEN
        SELECT CASE (yfile%igrid)
         CASE (1)
           CALL write_grids ('L',iout,rlfile,yfield,yfile%nfs,yfile%nls,yfile%it1+nt1-1,ntr,ny,2,yrpls(:,:,:), &
                iusey,kuse(nt1+1:),rlatdy,rlngdy,ifail,                                                        &
                cl=clf)
         CASE (2)
           CALL write_stnss ('L',iout,rlfile,yfield,yfile%nfs,yfile%nls,yfile%it1+nt1-1,ntr,ny,2,yrpls(:,:,:), &
                iusey,kuse(nt1+1:),rlatdy,rlngdy,cstndy,ifail,                                                  &
                cl=clf)
         CASE (3)
           CALL write_unrfs ('L',iout,rlfile,yfield,yfile%nfs,yfile%nls,yfile%it1+nt1-1,ntr,ny,2,yrpls(:,:,:), &
                iusey,kuse(nt1+1:),cstndy,ifail,                                                      &
                cl=clf)
        END SELECT
     END IF
     CLOSE (UNIT=iout)
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(rlfile%desc),c_arg2=TRIM(rlfile%ffile))
     CALL file_reset (rlfile%lset,rlfile%fname)
     prog=prog+dprog
  END IF
!
! Save eigenvalues
! - X eigenvalues -
  IF (xefile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(xefile%ffile)
     CALL open_output (iout,xefile,1,ifail)
     IF (ifail==0) THEN
        CALL write_eigs (iout,xefile,lxt,svx,nt)
        CLOSE (UNIT=iout)
     END IF
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(xefile%desc),c_arg2=TRIM(xefile%ffile))
     CALL file_reset (xefile%lset,xefile%fname)
     prog=prog+dprog
  END IF
! - Y eigenvalues -
  IF (yefile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(yefile%ffile)
     CALL open_output (iout,yefile,1,ifail)
     IF (ifail==0) THEN
        CALL write_eigs (iout,yefile,lyt,svy,nt)
        CLOSE (UNIT=iout)
     END IF
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(yefile%desc),c_arg2=TRIM(yefile%ffile))
     CALL file_reset (yefile%lset,yefile%fname)
     prog=prog+dprog
  END IF
!
! Save spatial loadings
! - X spatial loadings -
  IF (xlfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(xlfile%ffile)
     CALL open_output (iout,xlfile,xfile%nfs,ifail)
     IF (ifail==0) THEN
        SELECT CASE (xfile%igrid)
         CASE (1)
           CALL write_load (iout,xlfile,xfield,xfile%igrid,xfile%nfs,xfile%nls,mxe,nx,iusex,eofx,'X_loadings',ifail, &
                rlat=rlatdx,rlng=rlngdx)
         CASE (2)
           CALL write_load (iout,xlfile,xfield,xfile%igrid,xfile%nfs,xfile%nls,mxe,nx,iusex,eofx,'X_loadings',ifail, &
                rlat=rlatdx,rlng=rlngdx,cstn=cstndx)
         CASE (3)
           CALL write_load (iout,xlfile,xfield,xfile%igrid,xfile%nfs,xfile%nls,mxe,nx,iusex,eofx,'X_loadings',ifail, &
                cstn=cstndx)
        END SELECT
        CLOSE (UNIT=iout)
     END IF
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(xlfile%desc),c_arg2=TRIM(xlfile%ffile))
     CALL file_reset (xlfile%lset,xlfile%fname)
     prog=prog+dprog
  END IF
! - Y spatial loadings -
  IF (ylfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(ylfile%ffile)
     CALL open_output (iout,ylfile,yfile%nfs,ifail)
     IF (ifail==0) THEN
        SELECT CASE (yfile%igrid)
         CASE (1)
           CALL write_load (iout,ylfile,yfield,yfile%igrid,yfile%nfs,yfile%nls,mye,ny,iusey,eofy,'Y_loadings',ifail, &
                rlat=rlatdy,rlng=rlngdy)
         CASE (2)
           CALL write_load (iout,ylfile,yfield,yfile%igrid,yfile%nfs,yfile%nls,mye,ny,iusey,eofy,'Y_loadings',ifail, &
                rlat=rlatdy,rlng=rlngdy,cstn=cstndy)
         CASE (3)
           CALL write_load (iout,ylfile,yfield,yfile%igrid,yfile%nls,yfile%nls,mye,ny,iusey,eofy,'Y_loadings',ifail, &
                cstn=cstndy)
        END SELECT
        CLOSE (UNIT=iout)
     END IF
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(ylfile%desc),c_arg2=TRIM(ylfile%ffile))
     CALL file_reset (ylfile%lset,ylfile%fname)
     prog=prog+dprog
  END IF
!
! Save temporal scores
! - X scores -
  IF (xsfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(xsfile%ffile)
     CALL open_output (iout,xsfile,1,ifail)
     IF (ifail==0) THEN
        CALL write_scor (iout,'X_scores',xsfile,nt,xfile%it1-1,xfile%period1,mxe,kuse,xfield(1)%rmiss,tsx,ifail)
     END IF
     CLOSE (UNIT=iout)

     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(xsfile%desc),c_arg2=TRIM(xsfile%ffile))
     CALL file_reset (xsfile%lset,xsfile%fname)
     prog=prog+dprog
  END IF
! - Y scores -
  IF (ysfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(ysfile%ffile)
     CALL open_output (iout,ysfile,1,ifail)
     IF (ifail==0) THEN
        CALL write_scor (iout,'Y_scores',ysfile,nt,yfile%it1-1,yfile%period1,mye,kuse,yfield(1)%rmiss,tsy,ifail)
     END IF
     CLOSE (UNIT=iout)
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(ysfile%desc),c_arg2=TRIM(ysfile%ffile))
     CALL file_reset (ysfile%lset,ysfile%fname)
     prog=prog+dprog
  END IF
!
! Save CCA results
! - canonical correlations -
  IF (ccfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(ccfile%ffile)
     CALL open_output (iout,ccfile,1,ifail)
     IF (ifail==0) THEN
        CALL write_canc (iout,ccfile,mcc,mu,ifail)
        CLOSE (UNIT=iout)
     END IF
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(ccfile%desc),c_arg2=TRIM(ccfile%ffile))
     CALL file_reset (ccfile%lset,ccfile%fname)
     prog=prog+dprog
  END IF
! - X homogeneous maps -
  IF (xmfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(xmfile%ffile)
     CALL open_output (iout,xmfile,xfile%nfs,ifail)
     IF (ifail==0) THEN
        SELECT CASE (xfile%igrid)
         CASE (1)
           CALL write_load (iout,xmfile,xfield,xfile%igrid,xfile%nfs,xfile%nls,nco,nx,iusex,hx_map,'X_CCA_loadings',ifail, &
                rlat=rlatdx,rlng=rlngdx)
         CASE (2)
           CALL write_load (iout,xmfile,xfield,xfile%igrid,xfile%nfs,xfile%nls,nco,nx,iusex,hx_map,'X_CCA_loadings',ifail, &
                rlat=rlatdx,rlng=rlngdx,cstn=cstndx)
         CASE (3)
           CALL write_load (iout,xmfile,xfield,xfile%igrid,xfile%nfs,xfile%nls,nco,nx,iusex,hx_map,'X_CCA_loadings',ifail, &
                cstn=cstndx)
        END SELECT
        CLOSE (UNIT=iout)
     END IF
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(xmfile%desc),c_arg2=TRIM(xmfile%ffile))
     CALL file_reset (xmfile%lset,xmfile%fname)
     prog=prog+dprog
  END IF
! - Y homogeneous maps -
  IF (ymfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(ymfile%ffile)
     CALL open_output (iout,ymfile,yfile%nfs,ifail)
     IF (ifail==0) THEN
        SELECT CASE (yfile%igrid)
         CASE (1)
           CALL write_load (iout,ymfile,yfield,yfile%igrid,yfile%nfs,yfile%nls,nco,ny,iusey,hy_map,'Y_CCA_loadings',ifail, &
                rlat=rlatdy,rlng=rlngdy)
         CASE (2)
           CALL write_load (iout,ymfile,yfield,yfile%igrid,yfile%nfs,yfile%nls,nco,ny,iusey,hy_map,'Y_CCA_loadings',ifail, &
                rlat=rlatdy,rlng=rlngdy,cstn=cstndy)
         CASE (3)
           CALL write_load (iout,ymfile,yfield,yfile%igrid,yfile%nls,yfile%nls,nco,ny,iusey,hy_map,'Y_CCA_loadings',ifail, &
                cstn=cstndy)
        END SELECT
        CLOSE (UNIT=iout)
     END IF
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(ymfile%desc),c_arg2=TRIM(ymfile%ffile))
     CALL file_reset (ymfile%lset,ymfile%fname)
     prog=prog+dprog
  END IF
! - X homogeneous map series -
  IF (xtfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(xtfile%ffile)
     CALL open_output (iout,xtfile,1,ifail)
     IF (ifail==0) THEN
        CALL write_scor (iout,'X_CCA_scores',xtfile,nt,xfile%it1-1,xfile%period1,nco,kuse,xfield(1)%rmiss,hx_ser,ifail)
     END IF
     CLOSE (UNIT=iout)
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(xtfile%desc),c_arg2=TRIM(xtfile%ffile))
     CALL file_reset (xtfile%lset,xtfile%fname)
     prog=prog+dprog
  END IF
! - Y homogeneous map series -
  IF (ytfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(ytfile%ffile)
     CALL open_output (iout,ytfile,1,ifail)
     IF (ifail==0) THEN
        CALL write_scor (iout,'Y_CCA_scores',ytfile,nt,yfile%it1-1,yfile%period1,nco,kuse,yfield(1)%rmiss,hy_ser,ifail)
     END IF
     CLOSE (UNIT=iout)
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(ytfile%desc),c_arg2=TRIM(ytfile%ffile))
     CALL file_reset (ytfile%lset,ytfile%fname)
     prog=prog+dprog
  END IF
!
! Save regression coefficients
! - regression coefficients -
  IF (rbfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(rbfile%ffile)
     CALL open_output (iout,rbfile,xfile%nfs,ifail)
     IF (ifail==0) THEN
        SELECT CASE (xfile%igrid)
         CASE (1)
           SELECT CASE (yfile%igrid)
            CASE (1)
              CALL write_regr (iout,rbfile,xfile,xfield,nx,iusex,yfile,yfield,ny,iusey,b,ifail, &
                   rlatx=rlatdx,rlngx=rlngdx,rlaty=rlatdy,rlngy=rlngdy)
            CASE (2)
              CALL write_regr (iout,rbfile,xfile,xfield,nx,iusex,yfile,yfield,ny,iusey,b,ifail, &
                   rlatx=rlatdx,rlngx=rlngdx,rlaty=rlatdy,rlngy=rlngdy,cstny=cstndy)
            CASE (3)
              CALL write_regr (iout,rbfile,xfile,xfield,nx,iusex,yfile,yfield,ny,iusey,b,ifail, &
                   rlatx=rlatdx,rlngx=rlngdx,cstny=cstndy)
           END SELECT
         CASE (2)
           SELECT CASE (yfile%igrid)
            CASE (1)
              CALL write_regr (iout,rbfile,xfile,xfield,nx,iusex,yfile,yfield,ny,iusey,b,ifail, &
                   rlatx=rlatdx,rlngx=rlngdx,cstnx=cstndx,rlaty=rlatdy,rlngy=rlngdy)
            CASE (2)
              CALL write_regr (iout,rbfile,xfile,xfield,nx,iusex,yfile,yfield,ny,iusey,b,ifail, &
                   rlatx=rlatdx,rlngx=rlngdx,cstnx=cstndx,rlaty=rlatdy,rlngy=rlngdy,cstny=cstndy)
            CASE (3)
              CALL write_regr (iout,rbfile,xfile,xfield,nx,iusex,yfile,yfield,ny,iusey,b,ifail, &
                   rlatx=rlatdx,rlngx=rlngdx,cstnx=cstndx,cstny=cstndy)
           END SELECT
         CASE (3)
           SELECT CASE (yfile%igrid)
            CASE (1)
              SELECT CASE (ianal)
               CASE (2) ! - PCR -
                 CALL write_regr (iout,rbfile,xfile,xfield,nx,iusex,yfile,yfield,ny,iusey,b,ifail, &
                      cstnx=cstndx,rlaty=rlatdy,rlngy=rlngdy)
               CASE (3) ! - MLR -
                 CALL write_regr (iout,rbfile,xfile,xfield,nx,iusex,yfile,yfield,ny,iusey,b,ifail, &
                      cstnx=cstndx,rlaty=rlatdy,rlngy=rlngdy,b0=b0)
              END SELECT
            CASE (2)
              SELECT CASE (ianal)
               CASE (2) ! - PCR -
                 CALL write_regr (iout,rbfile,xfile,xfield,nx,iusex,yfile,yfield,ny,iusey,b,ifail, &
                      cstnx=cstndx,rlaty=rlatdy,rlngy=rlngdy,cstny=cstndy)
               CASE (3) ! - MLR -
                 CALL write_regr (iout,rbfile,xfile,xfield,nx,iusex,yfile,yfield,ny,iusey,b,ifail, &
                      cstnx=cstndx,rlaty=rlatdy,rlngy=rlngdy,cstny=cstndy,b0=b0)
              END SELECT
            CASE (3)
              SELECT CASE (ianal)
               CASE (2) ! - PCR -
                 CALL write_regr (iout,rbfile,xfile,xfield,nx,iusex,yfile,yfield,ny,iusey,b,ifail, &
                      cstnx=cstndx,cstny=cstndy)
               CASE (3) ! - MLR -
                 CALL write_regr (iout,rbfile,xfile,xfield,nx,iusex,yfile,yfield,ny,iusey,b,ifail, &
                      cstnx=cstndx,cstny=cstndy,b0=b0)
              END SELECT
           END SELECT
        END SELECT
        CLOSE (UNIT=iout)
     END IF
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(rbfile%desc),c_arg2=TRIM(rbfile%ffile))
     CALL file_reset (rbfile%lset,rbfile%fname)
     prog=prog+dprog
  END IF
! - PC regression coefficients -
  IF (pbfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(pbfile%ffile)
     CALL open_output (iout,pbfile,yfile%nfs,ifail)
     IF (ifail==0) THEN
        SELECT CASE (yfile%igrid)
         CASE (1)
           CALL write_load (iout,pbfile,yfield,yfile%igrid,yfile%nfs,yfile%nls,mxe,ny,iusey,TRANSPOSE(bz), &
                'coefficients',ifail,                                                                      &
                rlat=rlatdy,rlng=rlngdy)
         CASE (2)
           CALL write_load (iout,pbfile,yfield,yfile%igrid,yfile%nfs,yfile%nls,mxe,ny,iusey,TRANSPOSE(bz), &
                'coefficients',ifail,                                                                      &
                rlat=rlatdy,rlng=rlngdy,cstn=cstndy)
         CASE (3)
           CALL write_load (iout,pbfile,yfield,yfile%igrid,yfile%nfs,yfile%nls,mxe,ny,iusey,TRANSPOSE(bz), &
                'coefficients',ifail,                                                                      &
                cstn=cstndy)
        END SELECT
        CLOSE (UNIT=iout)
     END IF
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(pbfile%desc),c_arg2=TRIM(pbfile%ffile))
     CALL file_reset (pbfile%lset,pbfile%fname)
     prog=prog+dprog
  END IF
!
! Save forecast probabilities
  IF (fpfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(fpfile%ffile)
     CALL open_output (iout,fpfile,yfile%nfs*ng,ifail)
     it0=yfile%it1+date_diff(xfile%fdate,yfile%fdate,iseq)-date_diff(zfile%fdate,yfile%fdate,iseq)-1
     IF (ifail==0) THEN
        SELECT CASE (yfile%igrid)
         CASE (1)
           CALL write_grids ('P',iout,fpfile,yfield,yfile%nfs,yfile%nls,it0,nf,ny,ng,fps(:,:,:),iusey,kfuse,rlatdy,rlngdy,ifail)
         CASE (2)
           CALL write_stnss ('P',iout,fpfile,yfield,yfile%nfs,yfile%nls,it0,nf,ny,ng,fps(:,:,:),iusey,kfuse,rlatdy,rlngdy,cstndy, &
                ifail)
         CASE (3)
           CALL write_unrfs ('P',iout,fpfile,yfield,yfile%nfs,yfile%nls,it0,nf,ny,ng,fps(:,:,:),iusey,kfuse,cstndy,ifail)
        END SELECT
     END IF
     CLOSE (UNIT=iout)
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(fpfile%desc),c_arg2=TRIM(fpfile%ffile))
     CALL file_reset (fpfile%lset,fpfile%fname)
     prog=prog+dprog
  END IF
!
! Save forecast odds
  IF (fofile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(fofile%ffile)
     CALL open_output (iout,fofile,yfile%nfs*ng,ifail)
     it0=yfile%it1+date_diff(xfile%fdate,yfile%fdate,iseq)-date_diff(zfile%fdate,yfile%fdate,iseq)-1
     IF (ifail==0) THEN
        SELECT CASE (iodds)
         CASE (0)
           SELECT CASE (yfile%igrid)
            CASE (1)
              CALL write_grids ('O',iout,fofile,yfield,yfile%nfs,yfile%nls,it0,nf,ny,ng,odds(:,:,:), &
                   iusey,kfuse,rlatdy,rlngdy,ifail)
            CASE (2)
              CALL write_stnss ('O',iout,fofile,yfield,yfile%nfs,yfile%nls,it0,nf,ny,ng,odds(:,:,:), &
                   iusey,kfuse,rlatdy,rlngdy,cstndy,ifail)
            CASE (3)
              CALL write_unrfs ('O',iout,fofile,yfield,yfile%nfs,yfile%nls,it0,nf,ny,ng,odds(:,:,:), &
                   iusey,kfuse,cstndy,ifail)
           END SELECT
         CASE (1)
           SELECT CASE (yfile%igrid)
            CASE (1)
              CALL write_grids ('R',iout,fofile,yfield,yfile%nfs,yfile%nls,it0,nf,ny,ng,oddr(:,:,:), &
                   iusey,kfuse,rlatdy,rlngdy,ifail)
            CASE (2)
              CALL write_stnss ('R',iout,fofile,yfield,yfile%nfs,yfile%nls,it0,nf,ny,ng,oddr(:,:,:), &
                   iusey,kfuse,rlatdy,rlngdy,cstndy,ifail)
            CASE (3)
              CALL write_unrfs ('R',iout,fofile,yfield,yfile%nfs,yfile%nls,it0,nf,ny,ng,oddr(:,:,:), &
                   iusey,kfuse,cstndy,ifail)
           END SELECT
        END SELECT
     END IF
     CLOSE (UNIT=iout)
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(fofile%desc),c_arg2=TRIM(fofile%ffile))
     CALL file_reset (fofile%lset,fofile%fname)
     prog=prog+dprog
  END IF
!
! Save forecasts
  IF (fvfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(fvfile%ffile)
     CALL open_output (iout,fvfile,yfile%nfs,ifail)
     it0=yfile%it1+date_diff(xfile%fdate,yfile%fdate,iseq)-date_diff(zfile%fdate,yfile%fdate,iseq)-1
     IF (ifail==0) THEN
        SELECT CASE (yfile%igrid)
         CASE (1)
           CALL write_grid (iout,fvfile,yfield,yfile%nfs,yfile%nls,nf,ny,fcast(:,:,0),iusey,kfuse,rlatdy,rlngdy,ifail, &
                it0=it0)
         CASE (2)
           CALL write_stns (iout,fvfile,yfield,yfile%nfs,yfile%nls,it0,nf,ny,fcast(:,:,0),iusey,kfuse, &
                rlatdy,rlngdy,cstndy,ifail)
         CASE (3)
           CALL write_unrf (iout,fvfile,yfield,yfile%nfs,yfile%nls,it0,nf,ny,fcast(:,:,0),iusey,kfuse,cstndy,ifail)
        END SELECT
     END IF
     CLOSE (UNIT=iout)
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(fvfile%desc),c_arg2=TRIM(fvfile%ffile))
     CALL file_reset (fvfile%lset,fvfile%fname)
     prog=prog+dprog
  END IF
!
! Save forecast ensembles
  IF (fsfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(fsfile%ffile)
     CALL open_output (iout,fsfile,yfile%nfs*nens,ifail)
     it0=yfile%it1+date_diff(xfile%fdate,yfile%fdate,iseq)-date_diff(zfile%fdate,yfile%fdate,iseq)-1
     IF (ifail==0) THEN
        SELECT CASE (yfile%igrid)
         CASE (1)
           CALL write_grids ('E',iout,fsfile,yfield,yfile%nfs,yfile%nls,it0,nf,ny,nens,fcast(:,:,1:), &
                iusey,kfuse,rlatdy,rlngdy,ifail)
         CASE (2)
           CALL write_stnss ('E',iout,fsfile,yfield,yfile%nfs,yfile%nls,it0,nf,ny,nens,fcast(:,:,1:), &
                iusey,kfuse,rlatdy,rlngdy,cstndy,ifail)
         CASE (3)
           CALL write_unrfs ('E',iout,fsfile,yfield,yfile%nfs,yfile%nls,it0,nf,ny,nens,fcast(:,:,1:), &
                iusey,kfuse,cstndy,ifail)
        END SELECT
     END IF
     CLOSE (UNIT=iout)
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(fsfile%desc),c_arg2=TRIM(fsfile%ffile))
     CALL file_reset (fsfile%lset,fsfile%fname)
     prog=prog+dprog
  END IF
!
! Save prediction error variances
  IF (fefile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(fefile%ffile)
     CALL open_output (iout,fefile,yfile%nfs,ifail)
     it0=yfile%it1+date_diff(xfile%fdate,yfile%fdate,iseq)-date_diff(zfile%fdate,yfile%fdate,iseq)-1
     IF (ifail==0) THEN
        SELECT CASE (yfile%igrid)
         CASE (1)
           CALL write_grid (iout,fefile,yfield,yfile%nfs,yfile%nls,nf,ny,pev*pev,iusey,kfuse,rlatdy,rlngdy,ifail, &
                it0=it0)
         CASE (2)
           CALL write_stns (iout,fefile,yfield,yfile%nfs,yfile%nls,it0,nf,ny,pev*pev,iusey,kfuse,rlatdy,rlngdy,cstndy,ifail)
         CASE (3)
           CALL write_unrf (iout,fefile,yfield,yfile%nfs,yfile%nls,it0,nf,ny,pev*pev,iusey,kfuse,cstndy,ifail)
        END SELECT
     END IF
     CLOSE (UNIT=iout)
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(fefile%desc),c_arg2=TRIM(fefile%ffile))
     CALL file_reset (fefile%lset,fefile%fname)
     prog=prog+dprog
  END IF
!
! Save prediction limits
  IF (flfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(flfile%ffile)
     CALL open_output (iout,flfile,yfile%nfs*2,ifail)
     it0=yfile%it1+date_diff(xfile%fdate,yfile%fdate,iseq)-date_diff(zfile%fdate,yfile%fdate,iseq)-1
     IF (ifail==0) THEN
        SELECT CASE (yfile%igrid)
         CASE (1)
           CALL write_grids ('L',iout,flfile,yfield,yfile%nfs,yfile%nls,it0,nf,ny,2,fpls(:,:,:), &
                iusey,kfuse,rlatdy,rlngdy,ifail,                                                 &
                cl=clf)
         CASE (2)
           CALL write_stnss ('L',iout,flfile,yfield,yfile%nfs,yfile%nls,it0,nf,ny,2,fpls(:,:,:), &
                iusey,kfuse,rlatdy,rlngdy,cstndy,ifail,                                          &
                cl=clf)
         CASE (3)
           CALL write_unrfs ('L',iout,flfile,yfield,yfile%nfs,yfile%nls,it0,nf,ny,2,fpls(:,:,:), &
                iusey,kfuse,cstndy,ifail,                                                        &
                cl=clf)
        END SELECT
     END IF
     CLOSE (UNIT=iout)
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(flfile%desc),c_arg2=TRIM(flfile%ffile))
     CALL file_reset (flfile%lset,flfile%fname)
     prog=prog+dprog
  END IF
!
! Save predictor time scores
  IF (fxfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(fxfile%ffile)
     CALL open_output (iout,fxfile,1,ifail)
     it0=zfile%it1-1
     IF (ifail==0) THEN
        CALL write_scor (iout,'Predictor_scores',fxfile,nf,it0,zfile%period1,mxe,kfuse,xfield(1)%rmiss,xhat,ifail)
     END IF
     CLOSE (UNIT=iout)
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(fxfile%desc),c_arg2=TRIM(fxfile%ffile))
     CALL file_reset (fxfile%lset,fxfile%fname)
  END IF
!
! Save skill scores
  IF (skfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(skfile%ffile)
     CALL open_output (iout,skfile,yfile%nfs,ifail)
     IF (ifail==0) THEN
        SELECT CASE (yfile%igrid)
         CASE (1)
           CALL write_load (iout,skfile,yfield,yfile%igrid,yfile%nfs,yfile%nls,1,ny,iusey, &
                RESHAPE(skills,(/mya,1/)),cg_skill_t(iskill),ifail,                        &
                rlat=rlatdy,rlng=rlngdy)
         CASE (2)
           CALL write_load (iout,skfile,yfield,yfile%igrid,yfile%nfs,yfile%nls,1,ny,iusey, &
                RESHAPE(skills,(/mya,1/)),cg_skill_t(iskill),ifail,                        &
                rlat=rlatdy,rlng=rlngdy,cstn=cstndy)
         CASE (3)
           CALL write_load (iout,skfile,yfield,yfile%igrid,yfile%nfs,yfile%nls,1,ny,iusey, &
                RESHAPE(skills,(/mya,1/)),cg_skill_t(iskill),ifail,                        &
                cstn=cstndy)
        END SELECT
        CLOSE (UNIT=iout)
     END IF
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(skfile%desc),c_arg2=TRIM(skfile%ffile))
     CALL file_reset (skfile%lset,skfile%fname)
     prog=prog+dprog
  END IF
!
! Save p-values
  IF (pvfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(pvfile%ffile)
     CALL open_output (iout,pvfile,yfile%nfs,ifail)
     IF (ifail==0) THEN
        SELECT CASE (yfile%igrid)
         CASE (1)
           CALL write_load (iout,pvfile,yfield,yfile%igrid,yfile%nfs,yfile%nls,1,ny,iusey, &
                RESHAPE(pvalues,(/mya,1/)),'p-values',ifail,                               &
                rlat=rlatdy,rlng=rlngdy)
         CASE (2)
           CALL write_load (iout,pvfile,yfield,yfile%igrid,yfile%nfs,yfile%nls,1,ny,iusey, &
                RESHAPE(pvalues,(/mya,1/)),'p-values',ifail,                               &
                rlat=rlatdy,rlng=rlngdy,cstn=cstndy)
         CASE (3)
           CALL write_load (iout,pvfile,yfield,yfile%igrid,yfile%nfs,yfile%nls,1,ny,iusey, &
                RESHAPE(pvalues,(/mya,1/)),'p-values',ifail,                               &
                cstn=cstndy)
        END SELECT
        CLOSE (UNIT=iout)
     END IF
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(pvfile%desc),c_arg2=TRIM(pvfile%ffile))
     CALL file_reset (pvfile%lset,pvfile%fname)
     prog=prog+dprog
  END IF
!
! Save ROC results
! - individual point -
  IF (rofile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(rofile%ffile)
     CALL open_output (iout,rofile,1,ifail)
     IF (ifail==0) THEN
        CALL write_roc (iout,rofile,nt,2,hit,far,roca,ifail)
        CLOSE (UNIT=iout)
     END IF
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(rofile%desc),c_arg2=TRIM(rofile%ffile))
     CALL file_reset (rofile%lset,rofile%fname)
     prog=prog+dprog
  END IF
! - all points -
  IF (rrfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(rrfile%ffile)
     CALL open_output (iout,rrfile,1,ifail)
     IF (ifail==0) THEN
        CALL write_roc (iout,rrfile,nb,ng,hits,fars,rocas,ifail)
        CLOSE (UNIT=iout)
     END IF
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(rrfile%desc),c_arg2=TRIM(rrfile%ffile))
     CALL file_reset (rrfile%lset,rrfile%fname)
     prog=prog+dprog
  END IF
!
! Save attributes diagram
  IF (atfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(atfile%ffile)
     CALL open_output (iout,atfile,1,ifail)
     IF (ifail==0) THEN
        CALL write_rel (iout,atfile,ng,nb,afp,orf,ifq,ifail)
        CLOSE (UNIT=iout)
     END IF
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(atfile%desc),c_arg2=TRIM(atfile%ffile))
     CALL file_reset (atfile%lset,atfile%fname)
     prog=prog+dprog
  END IF
!
! Save weather roulette results
  IF (wrfile%lset) THEN
     WRITE (UNIT=*,FMT='(2A)') 'Saving: ',TRIM(wrfile%ffile)
     CALL open_output (iout,wrfile,1,ifail)
     IF (ifail==0) THEN
        cump(0:,2)=eir(0:,1)
        CALL write_unrf (iout,wrfile,yfield,1,1,ntr+1,yfile%it1+nt1-2,nwr,cump(0:,:),(/1,2/),(/.true.,kuse(nt1+1:)/), &
             RESHAPE(cg_wrlts_t(:),(/nwr,1/)),ifail)
     END IF
     CLOSE (UNIT=iout)
     IF (ifail/=0) CALL error ('write_results',ifail, &
                        c_arg1=TRIM(wrfile%desc),c_arg2=TRIM(wrfile%ffile))
     CALL file_reset (wrfile%lset,wrfile%fname)
     prog=prog+dprog
  END IF
!
! Update console
  prog=one
  WRITE (UNIT=*,FMT='(A)') TRIM(cg_done)//'!'
!
  RETURN
!
 CONTAINS
!
!
  SUBROUTINE write_grid (iout,afile,afield,nfs,nls,nt,nv,v,iuse,kuse,rlat,rlng,ifail, &
             it0)
!
! Outputs data in gridded format
!
! Modules
  USE fields,  ONLY: field
  USE iofiles, ONLY: ofile
  USE numbers, ONLY: r360
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iout ! - output unit number -
  INTEGER, INTENT(IN) :: nfs  ! - number of fields -
  INTEGER, INTENT(IN) :: nls  ! - number of lagged fields -
  INTEGER, INTENT(IN) :: nt   ! - number of cases -
  INTEGER, INTENT(IN) :: nv   ! - number of grids -
!
  TYPE(ofile), INTENT(IN) :: afile ! - output file -
!
! - optional input scalars -
  INTEGER, INTENT(IN), OPTIONAL :: it0 ! - time 0 -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: iuse(:) ! - used variables indices -
!
  REAL(KIND=rp), INTENT(IN) :: v(:,:)    ! - data -
  REAL(KIND=rp), INTENT(IN) :: rlat(:,:) ! - latitudes -
  REAL(KIND=rp), INTENT(IN) :: rlng(:,:) ! - longitudes -
!
  LOGICAL, INTENT(IN) :: kuse(:) ! - used cases flags -
!
  TYPE(field), INTENT(IN) :: afield(:) ! - field -
!
! Locals
!
! Local scalars
  INTEGER :: i   ! - latitude index -
  INTEGER :: j   ! - longitude index -
  INTEGER :: k   ! - time index -
  INTEGER :: l   ! - field / lagged field index -
  INTEGER :: ifd ! - field index -
  INTEGER :: ilf ! - lagged field index -
  INTEGER :: ii  ! - available series index -
  INTEGER :: ij  ! - current variable -
  INTEGER :: kk  ! - available time index -
!
  TYPE(date) :: mdate ! - start date -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ACHAR
  INTRINSIC ADJUSTL
  INTRINSIC PRESENT
  INTRINSIC TRIM
!
! Executable Statements
!
! Print gridded data
  SELECT CASE (TRIM(ffmts(afile%ffmt%ifmt)))
   CASE ('formatted')
     kk=0
     DO k=1,nt
        IF (kuse(k)) kk=kk+1
        ij=0
        ii=1
        DO ilf=1,nls
           DO ifd=1,nfs
              l=(ifd-1)*nls+ilf
              IF (afield(l)%mdate==0) THEN
                 mdate=0
              ELSE
                 IF (PRESENT(it0)) THEN
                    mdate=afield(l)%mdate+(it0+k-1)
                 ELSE
                    mdate=0
                 END IF
              END IF
              IF (PRESENT(it0)) THEN
                 CALL write_tag (iout,ifail,                                                             &
                                 cpt_field=TRIM(afield(l)%var),cpt_z=afield(l)%z,cpt_m=afield(l)%member, &
                                 cpt_s=mdate,cpt_t=afield(l)%tdate+(it0+k-1),                            &
                                 cpt_nrow=afield(l)%region%nlts,cpt_ncol=afield(l)%region%nlgs,          &
                                 cpt_row='Y',cpt_col='X',cpt_units=TRIM(afield(l)%unit),cpt_missing=afield(l)%rmiss)
              ELSE
                 CALL write_tag (iout,ifail,                                                    &
                                 cpt_field=TRIM(afield(l)%var),                                 &
                                 cpt_nrow=afield(l)%region%nlts,cpt_ncol=afield(l)%region%nlgs, &
                                 cpt_row='Y',cpt_col='X',cpt_units=TRIM(afield(l)%unit),cpt_missing=afield(l)%rmiss)
              END IF
              IF (ifail/=0) GOTO 1
              DO j=1,afield(l)%region%nlgs
                 IF (rlng(j,l)>r360) THEN
                    WRITE (cout,FMT=*) rlng(j,l)-r360
                 ELSE
                    WRITE (cout,FMT=*) rlng(j,l)
                 END IF
                 WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
              END DO
              WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
              DO i=1,afield(l)%region%nlts
                 WRITE (cout,FMT=*) rlat(i,l)
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) TRIM(ADJUSTL(cout))
                 DO j=1,afield(l)%region%nlgs
                    IF (kuse(k)) THEN
                       ij=ij+1
                       IF (iuse(ii)==ij) THEN
                          WRITE (cout,FMT=*) v(ii,kk)
                          ii=ii+1
                       ELSE
                          WRITE (cout,FMT=*) afield(l)%rmiss
                       END IF
                    ELSE
                       WRITE (cout,FMT=*) afield(l)%rmiss
                    END IF
                    WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
                 END DO
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
              END DO
           END DO
        END DO
     END DO
   CASE ('unformatted')
     CALL write_unform (iout,afile,nt,nv,v,iuse,kuse,afield(1)%rmiss,ifail)
     IF (ifail/=0) RETURN
  END SELECT
  ifail=0
!
  RETURN
!
! Error
1 ifail=3
!
  RETURN
  END SUBROUTINE write_grid
!
!
!
  SUBROUTINE write_stns (iout,afile,afield,nfs,nls,it0,nt,nv,v,iuse,kuse,rlat,rlng,cstn,ifail)
!
! Outputs data in station format
!
! Modules
  USE fields,  ONLY: field
  USE iofiles, ONLY: ofile
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iout ! - output unit number -
  INTEGER, INTENT(IN) :: nfs  ! - number of fields -
  INTEGER, INTENT(IN) :: nls  ! - number of lagged fields -
  INTEGER, INTENT(IN) :: it0  ! - time 0 -
  INTEGER, INTENT(IN) :: nt   ! - number of cases -
  INTEGER, INTENT(IN) :: nv   ! - number of stations -
!
  TYPE(ofile), INTENT(IN) :: afile  ! - output file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: iuse(:) ! - used variables indices -
!
  REAL(KIND=rp), INTENT(IN) :: v(:,:)    ! - data -
  REAL(KIND=rp), INTENT(IN) :: rlat(:,:) ! - latitudes -
  REAL(KIND=rp), INTENT(IN) :: rlng(:,:) ! - longitudes -
!
  CHARACTER(LEN=lstn), INTENT(IN) :: cstn(:,:) ! - station names -
!
  LOGICAL, INTENT(IN) :: kuse(:) ! - used cases flags -
!
  TYPE(field), INTENT(IN) :: afield(:) ! - field -
!
! Locals
!
! Local scalars
  INTEGER :: i   ! - station index -
  INTEGER :: k   ! - time index -
  INTEGER :: l   ! - field / lagged field index -
  INTEGER :: ifd ! - field index -
  INTEGER :: ilf ! - lagged field index -
  INTEGER :: ii  ! - available series index -
  INTEGER :: ij  ! - current variable -
  INTEGER :: kk  ! - available time index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ACHAR
  INTRINSIC ADJUSTL
  INTRINSIC TRIM
!
! Executable Statements
!
! Print tag line
  SELECT CASE (TRIM(ffmts(afile%ffmt%ifmt)))
   CASE ('formatted')
     DO ifd=1,nfs
        l=(ifd-1)*nls+1
        CALL write_tag (iout,ifail,                                                          &
                        cpt_field=TRIM(afield(l)%var),cpt_nrow=nt*nls,cpt_ncol=afield(l)%nv, &
                        cpt_row='T',cpt_col='station',cpt_units=TRIM(afield(l)%unit),cpt_missing=afield(l)%rmiss)
        IF (ifail/=0) GOTO 1
!
! Print station information
        DO i=1,afield(l)%nv
           WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cstn(i,l)))
        END DO
        WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
        WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) 'cpt:Y'
        DO i=1,afield(l)%nv
           WRITE (cout,FMT=*) rlat(i,l)
           WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
        END DO
        WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
        WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) 'cpt:X'
        DO i=1,afield(l)%nv
           WRITE (cout,FMT=*) rlng(i,l)
           WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
        END DO
        WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
!
! Print station data
        kk=0
        DO k=1,nt
           IF (kuse(k)) kk=kk+1
           IF (ifd==1) THEN
              ii=1
              ij=0
           ELSE
              ii=SUM(afield(1:l-1)%nva)+1
              ij=SUM(afield(1:l-1)%nv)
           END IF
           DO ilf=1,nls
              cout=get_cdate(afield(l)%tdate+(it0+k-1),1)
              WRITE (UNIT=iout,ADVANCE='no',FMT='(A)',ERR=1) TRIM(ADJUSTL(cout))
              DO i=1,afield(l)%nv
                 IF (kuse(k)) THEN
                    ij=ij+1
                    IF (iuse(ii)==ij) THEN
                       WRITE (cout,FMT=*) v(ii,kk)
                       ii=ii+1
                    ELSE
                       WRITE (cout,FMT=*) afield(l)%rmiss
                    END IF
                 ELSE
                    WRITE (cout,FMT=*) afield(l)%rmiss
                 END IF
                 WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
              END DO
              WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
           END DO
        END DO
     END DO
   CASE ('unformatted')
     CALL write_unform (iout,afile,nt,nv,v,iuse,kuse,afield(1)%rmiss,ifail)
     IF (ifail/=0) RETURN
  END SELECT
  ifail=0
!
  RETURN
!
! Error
1 ifail=3
!
  RETURN
  END SUBROUTINE write_stns
!
!
!
  SUBROUTINE write_unrf (iout,afile,afield,nfs,nls,it0,nt,nv,v,iuse,kuse,cstn,ifail, &
             b0)
!
! Outputs data in unreferenced format
!
! Modules
  USE fields,  ONLY: field
  USE iofiles, ONLY: ofile
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iout ! - output unit number -
  INTEGER, INTENT(IN) :: nfs  ! - number of fields -
  INTEGER, INTENT(IN) :: nls  ! - number of lagged fields -
  INTEGER, INTENT(IN) :: it0  ! - time 0 -
  INTEGER, INTENT(IN) :: nt   ! - number of cases -
  INTEGER, INTENT(IN) :: nv   ! - number of series -
!
  TYPE(ofile), INTENT(IN) :: afile ! - output file -
!
! - optional input scalars -
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: b0 ! - regression constant -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: iuse(:) ! - used variables indices -
!
  REAL(KIND=rp), INTENT(IN) :: v(:,:) ! - data -
!
  CHARACTER(LEN=*), INTENT(IN) :: cstn(:,:) ! - station names -
!
  LOGICAL, INTENT(IN) :: kuse(:) ! - used cases flags -
!
  TYPE(field), INTENT(IN) :: afield(:) ! - field -
!
! Locals
!
! Local scalars
  INTEGER :: i   ! - series index -
  INTEGER :: k   ! - time index -
  INTEGER :: l   ! - field / lagged field index -
  INTEGER :: ifd ! - field index -
  INTEGER :: ilf ! - lagged field index -
  INTEGER :: ii  ! - available series index -
  INTEGER :: ij  ! - current variable -
  INTEGER :: kk  ! - available time index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ACHAR
  INTRINSIC ADJUSTL
  INTRINSIC PRESENT
  INTRINSIC TRIM
!
! Executable Statements
!
! Print tag line
  SELECT CASE (TRIM(ffmts(afile%ffmt%ifmt)))
   CASE ('formatted')
     DO ifd=1,nfs
        l=(ifd-1)*nls+1
        CALL write_tag (iout,ifail,                                                          &
                        cpt_field=TRIM(afield(l)%var),cpt_nrow=nt*nls,cpt_ncol=afield(l)%nv, &
                        cpt_row='T',cpt_col='index',cpt_units=TRIM(afield(l)%unit),cpt_missing=afield(l)%rmiss)
        IF (ifail/=0) GOTO 1
!
! Print station information
        IF (PRESENT(b0)) WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),'Constant'
        DO i=1,afield(l)%nv
           WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cstn(i,l)))
        END DO
        WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
!
! Print unreferenced data
        kk=0
        DO k=1,nt
           IF (kuse(k)) kk=kk+1
           IF (ifd==1) THEN
              ii=1
              ij=0
           ELSE
              ii=SUM(afield(1:l-1)%nva)+1
              ij=SUM(afield(1:l-1)%nv)
           END IF
           DO ilf=1,nls
              cout=get_cdate(afield(l)%tdate+(it0+k-1),1)
              WRITE (UNIT=iout,ADVANCE='no',FMT='(A)',ERR=1) TRIM(ADJUSTL(cout))
              DO i=1,afield(l)%nv
                 IF (kuse(k)) THEN
                    ij=ij+1
                    IF (iuse(ii)==ij) THEN
                       WRITE (cout,FMT=*) v(ii,kk)
                       ii=ii+1
                    ELSE
                       WRITE (cout,FMT=*) afield(l)%rmiss
                    END IF
                 ELSE
                    WRITE (cout,FMT=*) afield(l)%rmiss
                 END IF
                 WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
              END DO
              WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
           END DO
        END DO
     END DO
   CASE ('unformatted')
     CALL write_unform (iout,afile,nt,nv,v,iuse,kuse,afield(1)%rmiss,ifail)
     IF (ifail/=0) RETURN
  END SELECT
  ifail=0
!
  RETURN
!
! Error
1 ifail=3
!
  RETURN
  END SUBROUTINE write_unrf
!
!
!
  SUBROUTINE write_grids (ctype,iout,afile,afield,nfs,nls,it0,nt,nv,n3,v,iuse,kuse,rlat,rlng,ifail, &
             cl)
!
! Outputs data in gridded format
!
! Modules
  USE fields,  ONLY: field
  USE iofiles, ONLY: ofile
  USE labels,  ONLY: cg_thr_l,cg_cat_a
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iout ! - output unit number -
  INTEGER, INTENT(IN) :: nfs  ! - number of fields -
  INTEGER, INTENT(IN) :: nls  ! - number of lagged fields -
  INTEGER, INTENT(IN) :: it0  ! - time 0 -
  INTEGER, INTENT(IN) :: nt   ! - number of cases -
  INTEGER, INTENT(IN) :: nv   ! - number of grids -
  INTEGER, INTENT(IN) :: n3   ! - third dimension -
!
  CHARACTER(LEN=1), INTENT(IN) :: ctype ! - output type -
!
  TYPE(ofile), INTENT(IN) :: afile ! - output file -
!
! - optional input scalars -
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: cl ! - confidence level -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: iuse(:) ! - used variables indices -
!
  REAL(KIND=rp), INTENT(IN) :: v(:,:,:)    ! - data -
  REAL(KIND=rp), INTENT(IN) :: rlat(:,:) ! - latitudes -
  REAL(KIND=rp), INTENT(IN) :: rlng(:,:) ! - longitudes -
!
  LOGICAL, INTENT(IN) :: kuse(:) ! - used cases flags -
!
  TYPE(field), INTENT(IN) :: afield(:) ! - field -
!
! Locals
!
! Local scalars
  INTEGER :: i   ! - latitude index -
  INTEGER :: j   ! - longitude index -
  INTEGER :: k   ! - time index -
  INTEGER :: l   ! - field / lagged field index -
  INTEGER :: ifd ! - field index -
  INTEGER :: ilf ! - lagged field index -
  INTEGER :: i3  ! - third dimension index -
  INTEGER :: ii  ! - available series index -
  INTEGER :: ij  ! - current variable -
  INTEGER :: kk  ! - available time index -
!
  TYPE(date) :: mdate ! - start date -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ACHAR
  INTRINSIC ADJUSTL
  INTRINSIC TRIM
!
! Executable Statements
!
! Print gridded data
  SELECT CASE (TRIM(ffmts(afile%ffmt%ifmt)))
   CASE ('formatted')
     kk=0
     DO k=1,nt
        IF (kuse(k)) kk=kk+1
        DO ilf=1,nls
           DO ifd=1,nfs
              l=(ifd-1)*nls+ilf
              IF (afield(l)%mdate==0) THEN
                 mdate=0
              ELSE
                 mdate=afield(l)%mdate+(it0+k-1)
              END IF
              DO i3=1,n3
                 SELECT CASE (ctype)
                  CASE ('E') ! - ensemble -
                    CALL write_tag (iout,ifail,                                                    &
                                    cpt_field=TRIM(afield(l)%var),cpt_z=afield(l)%z,cpt_m=i3,      &
                                    cpt_s=mdate,cpt_t=afield(l)%tdate+(it0+k-1),                   &
                                    cpt_nrow=afield(l)%region%nlts,cpt_ncol=afield(l)%region%nlgs, &
                                    cpt_row='Y',cpt_col='X',cpt_units=TRIM(afield(l)%unit),cpt_missing=afield(l)%rmiss)
                  CASE ('L') ! - prediction limits -
                    CALL write_tag (iout,ifail,                                                    &
                                    cpt_field=TRIM(afield(l)%var),cpt_z=afield(l)%z,               &
                                    cpt_m=afield(l)%member,cpt_clev=cl,cpt_limit=cg_thr_l(i3),     &
                                    cpt_s=mdate,cpt_t=afield(l)%tdate+(it0+k-1),                   &
                                    cpt_nrow=afield(l)%region%nlts,cpt_ncol=afield(l)%region%nlgs, &
                                    cpt_row='Y',cpt_col='X',cpt_units='%',cpt_missing=afield(l)%rmiss)
                  CASE ('O') ! - odds -
                    CALL write_tag (iout,ifail,                                                              &
                                    cpt_field=TRIM(afield(l)%var)//'_odds_'//cg_cat_a(i3),cpt_z=afield(l)%z, &
                                    cpt_m=afield(l)%member,cpt_s=mdate,cpt_t=afield(l)%tdate+(it0+k-1),      &
                                    cpt_nrow=afield(l)%region%nlts,cpt_ncol=afield(l)%region%nlgs,           &
                                    cpt_row='Y',cpt_col='X',cpt_missing=afield(l)%rmiss)
                  CASE ('R') ! - relative odds -
                    CALL write_tag (iout,ifail,                                                                 &
                                    cpt_field=TRIM(afield(l)%var)//'_relodds_'//cg_cat_a(i3),cpt_z=afield(l)%z, &
                                    cpt_m=afield(l)%member,cpt_s=mdate,cpt_t=afield(l)%tdate+(it0+k-1),         &
                                    cpt_nrow=afield(l)%region%nlts,cpt_ncol=afield(l)%region%nlgs,              &
                                    cpt_row='Y',cpt_col='X',cpt_missing=afield(l)%rmiss)
                  CASE ('P') ! - probabilities -
                    CALL write_tag (iout,ifail,                                                              &
                                    cpt_field=TRIM(afield(l)%var)//'_prob_'//cg_cat_a(i3),cpt_z=afield(l)%z, &
                                    cpt_m=afield(l)%member,cpt_s=mdate,cpt_t=afield(l)%tdate+(it0+k-1),      &
                                    cpt_nrow=afield(l)%region%nlts,cpt_ncol=afield(l)%region%nlgs,           &
                                    cpt_row='Y',cpt_col='X',cpt_units='%',cpt_missing=afield(l)%rmiss)
                 END SELECT
                 IF (ifail/=0) GOTO 1
                 DO j=1,afield(l)%region%nlgs
                    WRITE (cout,FMT=*) rlng(j,l)
                    WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
                 END DO
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
                 IF (l>1) THEN
                    ij=SUM(afield(1:l-1)%nv)
                    ii=SUM(afield(1:l-1)%nva)
                 ELSE
                    ij=0
                    ii=1
                 END IF
                 DO i=1,afield(l)%region%nlts
                    WRITE (cout,FMT=*) rlat(i,l)
                    WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) TRIM(ADJUSTL(cout))
                    DO j=1,afield(l)%region%nlgs
                       IF (kuse(k)) THEN
                          ij=ij+1
                          IF (iuse(ii)==ij) THEN
                             WRITE (cout,FMT=*) v(ii,kk,i3)
                             ii=ii+1
                          ELSE
                             WRITE (cout,FMT=*) afield(l)%rmiss
                          END IF
                       ELSE
                          WRITE (cout,FMT=*) afield(l)%rmiss
                       END IF
                       WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
                    END DO
                    WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
                 END DO
              END DO
           END DO
        END DO
     END DO
   CASE ('unformatted')
     DO i3=1,n3
        CALL write_unform (iout,afile,nt,nv,v(:,:,i3),iuse,kuse,afield(1)%rmiss,ifail)
        IF (ifail/=0) RETURN
     END DO
  END SELECT
  ifail=0
!
  RETURN
!
! Error
1 ifail=3
!
  RETURN
  END SUBROUTINE write_grids
!
!
!
  SUBROUTINE write_stnss (ctype,iout,afile,afield,nfs,nls,it0,nt,nv,n3,v,iuse,kuse,rlat,rlng,cstn,ifail, &
             cl)
!
! Outputs data in station format
!
! Modules
  USE fields,  ONLY: field
  USE iofiles, ONLY: ofile
  USE labels,  ONLY: cg_thr_l,cg_cat_a
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iout ! - output unit number -
  INTEGER, INTENT(IN) :: nfs  ! - number of fields -
  INTEGER, INTENT(IN) :: nls  ! - number of lagged fields -
  INTEGER, INTENT(IN) :: it0  ! - time 0 -
  INTEGER, INTENT(IN) :: nt   ! - number of cases -
  INTEGER, INTENT(IN) :: nv   ! - number of stations -
  INTEGER, INTENT(IN) :: n3   ! - third dimension -
!
  CHARACTER(LEN=1), INTENT(IN) :: ctype ! - output type -
!
  TYPE(ofile), INTENT(IN) :: afile  ! - output file -
!
! - optional input scalars -
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: cl ! - confidence level -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: iuse(:) ! - used variables indices -
!
  REAL(KIND=rp), INTENT(IN) :: v(:,:,:)  ! - data -
  REAL(KIND=rp), INTENT(IN) :: rlat(:,:) ! - latitudes -
  REAL(KIND=rp), INTENT(IN) :: rlng(:,:) ! - longitudes -
!
  CHARACTER(LEN=lstn), INTENT(IN) :: cstn(:,:) ! - station names -
!
  LOGICAL, INTENT(IN) :: kuse(:) ! - used cases flags -
!
  TYPE(field), INTENT(IN) :: afield(:) ! - field -
!
! Locals
!
! Local scalars
  INTEGER :: i   ! - station index -
  INTEGER :: k   ! - time index -
  INTEGER :: l   ! - field / lagged field index -
  INTEGER :: ifd ! - field index -
  INTEGER :: ilf ! - lagged field index -
  INTEGER :: i3  ! - third dimension index -
  INTEGER :: ii  ! - available series index -
  INTEGER :: ij  ! - current variable -
  INTEGER :: kk  ! - available time index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ACHAR
  INTRINSIC ADJUSTL
  INTRINSIC TRIM
!
! Executable Statements
!
! Print tag line
  SELECT CASE (TRIM(ffmts(afile%ffmt%ifmt)))
   CASE ('formatted')
     DO ifd=1,nfs
        l=(ifd-1)*nls+1
        DO i3=1,n3
           SELECT CASE (ctype)
            CASE ('E') ! - ensemble -
              CALL write_tag (iout,ifail,                                                          &
                              cpt_field=TRIM(afield(l)%var),cpt_m=i3,                              &
                              cpt_nrow=nt*nls,cpt_ncol=afield(l)%nv,cpt_row='T',cpt_col='station', &
                              cpt_units=TRIM(afield(l)%unit),cpt_missing=afield(l)%rmiss)
            CASE ('L') ! - prediction limits -
              CALL write_tag (iout,ifail,                                                          &
                              cpt_field=TRIM(afield(l)%var),                                       &
                              cpt_clev=cl,cpt_limit=cg_thr_l(i3),                                  &
                              cpt_nrow=nt*nls,cpt_ncol=afield(l)%nv,cpt_row='T',cpt_col='station', &
                              cpt_units=TRIM(afield(l)%unit),cpt_missing=afield(l)%rmiss)
            CASE ('O') ! - odds -
              CALL write_tag (iout,ifail,                                                          &
                              cpt_field=TRIM(afield(l)%var)//'_odds_'//cg_cat_a(i3),               &
                              cpt_nrow=nt*nls,cpt_ncol=afield(l)%nv,cpt_row='T',cpt_col='station', &
                              cpt_missing=afield(l)%rmiss)
            CASE ('R') ! - relative odds -
              CALL write_tag (iout,ifail,                                                          &
                              cpt_field=TRIM(afield(l)%var)//'_relodds_'//cg_cat_a(i3),            &
                              cpt_nrow=nt*nls,cpt_ncol=afield(l)%nv,cpt_row='T',cpt_col='station', &
                              cpt_missing=afield(l)%rmiss)
            CASE ('P') ! - probabilities -
              CALL write_tag (iout,ifail,                                                          &
                              cpt_field=TRIM(afield(l)%var)//'_prob_'//cg_cat_a(i3),               &
                              cpt_nrow=nt*nls,cpt_ncol=afield(l)%nv,cpt_row='T',cpt_col='station', &
                              cpt_units='%',cpt_missing=afield(l)%rmiss)
           END SELECT
           IF (ifail/=0) GOTO 1
!
! Print station information
           DO i=1,afield(l)%nv
              WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cstn(i,l)))
           END DO
           WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
           WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) 'cpt:Y'
           DO i=1,afield(l)%nv
              WRITE (cout,FMT=*) rlat(i,l)
              WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
           END DO
           WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
           WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) 'cpt:X'
           DO i=1,afield(l)%nv
              WRITE (cout,FMT=*) rlng(i,l)
              WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
           END DO
           WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
!
! Print station data
           kk=0
           DO k=1,nt
              IF (kuse(k)) kk=kk+1
              IF (ifd==1) THEN
                 ii=1
                 ij=0
              ELSE
                 ii=SUM(afield(1:l-1)%nva)+1
                 ij=SUM(afield(1:l-1)%nv)
              END IF
              DO ilf=1,nls
                 cout=get_cdate(afield(l)%tdate+(it0+k-1),1)
                 WRITE (UNIT=iout,ADVANCE='no',FMT='(A)',ERR=1) TRIM(ADJUSTL(cout))
                 DO i=1,afield(l)%nv
                    IF (kuse(k)) THEN
                       ij=ij+1
                       IF (iuse(ii)==ij) THEN
                          WRITE (cout,FMT=*) v(ii,kk,i3)
                          ii=ii+1
                       ELSE
                          WRITE (cout,FMT=*) afield(l)%rmiss
                       END IF
                    ELSE
                       WRITE (cout,FMT=*) afield(l)%rmiss
                    END IF
                    WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
                 END DO
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
              END DO
           END DO
        END DO
     END DO
   CASE ('unformatted')
     DO i3=1,n3
        CALL write_unform (iout,afile,nt,nv,v(:,:,i3),iuse,kuse,afield(1)%rmiss,ifail)
        IF (ifail/=0) RETURN
     END DO
  END SELECT
  ifail=0
!
  RETURN
!
! Error
1 ifail=3
!
  RETURN
  END SUBROUTINE write_stnss
!
!
!
  SUBROUTINE write_unrfs (ctype,iout,afile,afield,nfs,nls,it0,nt,nv,n3,v,iuse,kuse,cstn,ifail, &
             cl)
!
! Outputs data in unreferenced format
!
! Modules
  USE fields,  ONLY: field
  USE iofiles, ONLY: ofile
  USE labels,  ONLY: cg_cat_a,cg_thr_l
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iout ! - output unit number -
  INTEGER, INTENT(IN) :: nfs  ! - number of fields -
  INTEGER, INTENT(IN) :: nls  ! - number of lagged fields -
  INTEGER, INTENT(IN) :: it0  ! - time 0 -
  INTEGER, INTENT(IN) :: nt   ! - number of cases -
  INTEGER, INTENT(IN) :: nv   ! - number of series -
  INTEGER, INTENT(IN) :: n3   ! - third dimension -
!
  CHARACTER(LEN=1), INTENT(IN) :: ctype ! - output type -
!
  TYPE(ofile), INTENT(IN) :: afile ! - output file -
!
! - optional input scalars -
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: cl ! - confidence level -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: iuse(:) ! - used variables indices -
!
  REAL(KIND=rp), INTENT(IN) :: v(:,:,:) ! - data -
!
  CHARACTER(LEN=lstn), INTENT(IN) :: cstn(:,:) ! - station names -
!
  LOGICAL, INTENT(IN) :: kuse(:) ! - used cases flags -
!
  TYPE(field), INTENT(IN) :: afield(:) ! - field -
!
! Locals
!
! Local scalars
  INTEGER :: i   ! - series index -
  INTEGER :: k   ! - time index -
  INTEGER :: l   ! - lagged field index -
  INTEGER :: i3  ! - third dimension index -
  INTEGER :: ifd ! - field index -
  INTEGER :: ilf ! - lagged-field index -
  INTEGER :: ii  ! - available series index -
  INTEGER :: kk  ! - available time index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ACHAR
  INTRINSIC ADJUSTL
  INTRINSIC TRIM
!
! Executable Statements
!
! Print tag line
  SELECT CASE (TRIM(ffmts(afile%ffmt%ifmt)))
   CASE ('formatted')
     DO ifd=1,nfs
        l=(ifd-1)*nfs+1
        DO i3=1,n3
           SELECT CASE (ctype)
            CASE ('E') ! - ensemble -
              CALL write_tag (iout,ifail,                                                        &
                              cpt_field=TRIM(afield(l)%var),cpt_m=i3,                            &
                              cpt_nrow=nt*nls,cpt_ncol=afield(l)%nv,cpt_row='T',cpt_col='index', &
                              cpt_units=TRIM(afield(l)%unit),cpt_missing=afield(l)%rmiss)
            CASE ('L') ! - prediction limits -
              CALL write_tag (iout,ifail,                                                        &
                              cpt_field=TRIM(afield(l)%var),                                     &
                              cpt_clev=cl,cpt_limit=cg_thr_l(i3),                                &
                              cpt_nrow=nt*nls,cpt_ncol=afield(l)%nv,cpt_row='T',cpt_col='index', &
                              cpt_units=TRIM(afield(l)%unit),cpt_missing=afield(l)%rmiss)
            CASE ('P') ! - probabilities -
              CALL write_tag (iout,ifail,                                                        &
                              cpt_field=TRIM(afield(l)%var)//'_prob_'//cg_cat_a(i3),             &
                              cpt_nrow=nt*nls,cpt_ncol=afield(l)%nv,cpt_row='T',cpt_col='index', &
                              cpt_units='%',cpt_missing=afield(l)%rmiss)
            CASE ('O') ! - odds -
              CALL write_tag (iout,ifail,                                                        &
                              cpt_field=TRIM(afield(l)%var)//'_odds_'//cg_cat_a(i3),             &
                              cpt_nrow=nt*nls,cpt_ncol=afield(l)%nv,cpt_row='T',cpt_col='index', &
                              cpt_missing=afield(l)%rmiss)
            CASE ('R') ! - relative odds -
              CALL write_tag (iout,ifail,                                                        &
                              cpt_field=TRIM(afield(l)%var)//'_relodds_'//cg_cat_a(i3),          &
                              cpt_nrow=nt*nls,cpt_ncol=afield(l)%nv,cpt_row='T',cpt_col='index', &
                              cpt_missing=afield(l)%rmiss)
           END SELECT
           IF (ifail/=0) GOTO 1
!
! Print index names
           DO i=1,afield(l)%nv
              WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cstn(i,l)))
           END DO
           WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
!
! Print unreferenced data
           kk=0
           DO k=1,nt
              IF (kuse(k)) kk=kk+1
              ii=1
              DO ilf=1,nls
                 l=(ifd-1)*nfs+ilf
                 cout=get_cdate(afield(l)%tdate+(it0+k-1),1)
                 WRITE (UNIT=iout,ADVANCE='no',FMT='(A)',ERR=1) TRIM(ADJUSTL(cout))
                 DO i=1,afield(l)%nv
                    IF (kuse(k)) THEN
                       IF (iuse(ii)==i) THEN
                          WRITE (cout,FMT=*) v(ii,kk,i3)
                          ii=ii+1
                       ELSE
                          WRITE (cout,FMT=*) afield(l)%rmiss
                       END IF
                    ELSE
                       WRITE (cout,FMT=*) afield(l)%rmiss
                    END IF
                    WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
                 END DO
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
              END DO
           END DO
        END DO
     END DO
   CASE ('unformatted')
     DO i3=1,n3
        CALL write_unform (iout,afile,nt,nv,v(:,:,i3),iuse,kuse,afield(1)%rmiss,ifail)
        IF (ifail/=0) RETURN
     END DO
  END SELECT
  ifail=0
!
  RETURN
!
! Error
1 ifail=3
!
  RETURN
  END SUBROUTINE write_unrfs
!
!
!
  SUBROUTINE write_eigs (iout,afile,ne,sv,nt)
!
! Modules
  USE IO_constants, ONLY: faccs,cprcs
  USE iofiles,      ONLY: ofile
  USE numbers,      ONLY: sp,oneh
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iout ! - output unit number -
  INTEGER, INTENT(IN) :: ne   ! - total number of eigenvalues -
  INTEGER, INTENT(IN) :: nt   ! - number of cases -
!
  TYPE(ofile), INTENT(IN) :: afile ! - output file -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: sv(:) ! - singular values -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - EOF index -
!
  REAL(KIND=rp) :: df     ! - number of cases -
  REAL(KIND=rp) :: evalue ! - eigenvalue -
  REAL(KIND=rp) :: tvar   ! - total variance -
  REAL(KIND=rp) :: pvar   ! - percentage variance -
  REAL(KIND=rp) :: svar   ! - cumulative variance -
  REAL(KIND=rp) :: spvar  ! - cumulative percentage variance -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ACHAR
  INTRINSIC ADJUSTL
  INTRINSIC REAL
  INTRINSIC SUM
  INTRINSIC TRIM
!
! Executable Statements
!
! Calculate total variance
  df=REAL(nt-1,KIND=rp)
  tvar=SUM(sv(1:ne)**2/df)
!
! Print headers
  CALL write_tag (iout,ifail, &
                  cpt_field='eigenvalues',cpt_nrow=ne,cpt_ncol=3,cpt_row='mode',cpt_col='index')
  IF (ifail/=0) GOTO 1
!
! Calculate and print eigenvalues
  svar=zero
  SELECT CASE (TRIM(ffmts(afile%ffmt%ifmt)))
   CASE ('formatted')
     WRITE (UNIT=iout,FMT='(7A)',ERR=1) 'Mode',ACHAR(9),'Eigenvalue',ACHAR(9),'% variance',ACHAR(9),'Cum. % variance'
     DO i=1,ne
        evalue=sv(i)**2/df
        svar=svar+evalue
        pvar=oneh*evalue/tvar
        spvar=oneh*svar/tvar
        WRITE (cout,FMT=*) i
        WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) TRIM(ADJUSTL(cout)),ACHAR(9)
        WRITE (cout,FMT=*) evalue
        WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) TRIM(ADJUSTL(cout)),ACHAR(9)
        WRITE (cout,FMT=*) pvar
        WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) TRIM(ADJUSTL(cout)),ACHAR(9)
        WRITE (cout,FMT=*) spvar
        WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1) TRIM(ADJUSTL(cout))
     END DO
   CASE ('unformatted')
     SELECT CASE (TRIM(faccs(afile%ffmt%iacc)))
      CASE ('sequential')
        SELECT CASE (TRIM(cprcs(afile%ffmt%iprc)))
         CASE ('double')
           DO i=1,ne
              evalue=sv(i)**2/df
              svar=svar+evalue
              pvar=oneh*evalue/tvar
              spvar=oneh*svar/tvar
              WRITE (UNIT=iout,ERR=1) evalue,pvar,spvar
           END DO
         CASE ('single')
           DO i=1,ne
              evalue=sv(i)**2/df
              svar=svar+evalue
              pvar=oneh*evalue/tvar
              spvar=oneh*svar/tvar
              WRITE (UNIT=iout,ERR=1) REAL(evalue,KIND=sp),REAL(pvar,KIND=sp),REAL(spvar,KIND=sp)
           END DO
        END SELECT
      CASE ('direct')
        SELECT CASE (TRIM(cprcs(afile%ffmt%iprc)))
         CASE ('double')
           DO i=1,ne
              evalue=sv(i)**2/df
              svar=svar+evalue
              pvar=oneh*evalue/tvar
              spvar=oneh*svar/tvar
              WRITE (UNIT=iout,ERR=1,REC=i) evalue,pvar,spvar
           END DO
         CASE ('single')
           DO i=1,ne
              evalue=sv(i)**2/df
              svar=svar+evalue
              pvar=oneh*evalue/tvar
              spvar=oneh*svar/tvar
              WRITE (UNIT=iout,ERR=1,REC=i) REAL(evalue,KIND=sp),REAL(pvar,KIND=sp),REAL(spvar,KIND=sp)
           END DO
        END SELECT
     END SELECT
  END SELECT
  ifail=0
!
  RETURN
!
! Error
1 ifail=3
!
  RETURN
  END SUBROUTINE write_eigs
!
!
!
  SUBROUTINE write_load (iout,afile,afield,igrid,nfs,nls,ne,nv,iuse,eof,cfld,ifail,rlat,rlng,cstn)
!
! Prints spatial loadings
!
! Modules
  USE fields,         ONLY: field
  USE iofiles,        ONLY: ofile
  USE maths,          ONLY: magnitude
  USE time_constants, ONLY: nmn
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iout  ! - output unit number -
  INTEGER, INTENT(IN) :: igrid ! - data structure -
  INTEGER, INTENT(IN) :: nfs   ! - number of fields -
  INTEGER, INTENT(IN) :: nls   ! - number of lagged fields -
  INTEGER, INTENT(IN) :: ne    ! - number of EOF modes -
  INTEGER, INTENT(IN) :: nv    ! - number of variables -
!
  CHARACTER(LEN=*), INTENT(IN) :: cfld ! - field description -
!
  TYPE(ofile), INTENT(IN) :: afile ! - output file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: iuse(:) ! - used variables indices -
!
  REAL(KIND=rp), INTENT(IN) :: eof(:,:) ! - spatial loadings -
!
  TYPE(field), INTENT(IN) :: afield(:) ! - field -
!
! - optional input arrays -
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: rlat(:,:) ! - latitudes -
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: rlng(:,:) ! - longitudes -
!
  CHARACTER(LEN=lstn), INTENT(IN), OPTIONAL :: cstn(:,:) ! - station names -
!
! Locals
!
! Local scalars
  INTEGER :: i   ! - latitude/EOF index -
  INTEGER :: j   ! - longitude/station/series index -
  INTEGER :: ie  ! - EOF index -
  INTEGER :: l   ! - field index -
  INTEGER :: ii  ! - grid index -
  INTEGER :: ij  ! - available grid index -
  INTEGER :: ilf ! - lagged field index -
  INTEGER :: ifd ! - field index -
!
  CHARACTER(LEN=     8) :: cfmt  ! - format string -
  CHARACTER(LEN=nmn+10) :: cmode ! - mode string -
!
! Intrinsic functions
  INTRINSIC ACHAR
  INTRINSIC ADJUSTL
  INTRINSIC TRIM
!
! Executable Statements
!
! Print formatted data
  SELECT CASE (TRIM(ffmts(afile%ffmt%ifmt)))
   CASE ('formatted')
     IF (nls>1) WRITE (UNIT=cfmt,FMT='(A,2(I1,A))') '(I',magnitude(ne),'.',magnitude(ne),',A)'
     SELECT CASE (igrid)
! - gridded data -
      CASE(1)
        DO ie=1,ne
           ii=0
           ij=1
           DO l=1,nfs*nls
              IF (TRIM(afield(l)%var)/='undefined') THEN
                 IF (nls>1) THEN
                    WRITE (UNIT=cmode,FMT=cfmt) ie,'_('//TRIM(afield(l)%cssn)//')'
                    CALL write_tag (iout,ifail, &
                         cpt_field=TRIM(afield(l)%var)//'_'//TRIM(cfld),cpt_cmode=cmode,cpt_nrow=afield(l)%region%nlts, &
                         cpt_ncol=afield(l)%region%nlgs,cpt_row='Y',cpt_col='X')
                 ELSE
                    CALL write_tag (iout,ifail, &
                         cpt_field=TRIM(afield(l)%var)//'_'//TRIM(cfld),cpt_mode=ie,cpt_nrow=afield(l)%region%nlts, &
                         cpt_ncol=afield(l)%region%nlgs,cpt_row='Y',cpt_col='X')
                 END IF
              ELSE
                 IF (nls>1) THEN
                    WRITE (UNIT=cmode,FMT=cfmt) ie,'_('//TRIM(afield(l)%cssn)//')'
                    CALL write_tag (iout,ifail, &
                         cpt_field=TRIM(cfld),cpt_cmode=cmode,cpt_nrow=afield(l)%region%nlts,cpt_ncol=afield(l)%region%nlgs, &
                         cpt_row='Y',cpt_col='X')
                 ELSE
                    CALL write_tag (iout,ifail, &
                         cpt_field=TRIM(cfld),cpt_mode=ie,cpt_nrow=afield(l)%region%nlts,cpt_ncol=afield(l)%region%nlgs, &
                         cpt_row='Y',cpt_col='X')
                 END IF
              END IF
              IF (ifail/=0) GOTO 1
              DO j=1,afield(l)%region%nlgs
                 WRITE (cout,FMT=*) rlng(j,l)
                 WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
              END DO
              WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
              DO i=1,afield(l)%region%nlts
                 WRITE (cout,FMT=*) rlat(i,l)
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) TRIM(ADJUSTL(cout))
                 DO j=1,afield(l)%region%nlgs
                    ii=ii+1
                    IF (iuse(ij)==ii) THEN
                       WRITE (cout,FMT=*) eof(ij,ie)
                       ij=ij+1
                    ELSE
                       WRITE (cout,FMT=*) afield(l)%rmiss
                    END IF
                    WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
                 END DO
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
              END DO
           END DO
        END DO
! - station data -
      CASE (2)
        ii=0
        ij=1
        DO ifd=1,nfs
           l=(ifd-1)*nls+1
           IF (TRIM(afield(l)%var)/='undefined') THEN
              CALL write_tag (iout,ifail, &
                   cpt_field=TRIM(afield(l)%var)//'_'//TRIM(cfld),cpt_nrow=afield(l)%nv*nls,cpt_ncol=ne,cpt_row='station', &
                   cpt_col='mode')
           ELSE
              CALL write_tag (iout,ifail, &
                   cpt_field=TRIM(cfld),cpt_nrow=afield(l)%nv*nls,cpt_ncol=ne,cpt_row='station',cpt_col='mode')
           END IF
           DO ilf=1,nls
              l=(ifd-1)*nls+ilf
              WRITE (UNIT=iout,FMT='(5A)',ADVANCE='no',ERR=1) 'Station',ACHAR(9),'Latitude',ACHAR(9),'Longitude'
              IF (nls>1) WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),'Season'
              DO ie=1,ne
                 WRITE (cout,FMT=*) ie
                 WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
              END DO
              WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
              DO j=1,afield(l)%nv
                 ii=ii+1
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) TRIM(ADJUSTL(cstn(j,l)))
                 WRITE (cout,FMT=*) rlat(j,l)
                 WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
                 WRITE (cout,FMT=*) rlng(j,l)
                 WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
                 IF (nls>1) WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(afield(l)%cssn)
                 IF (iuse(ij)==ii) THEN
                    DO ie=1,ne
                       WRITE (cout,FMT=*) eof(ij,ie)
                       WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
                    END DO
                    WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
                   ij=ij+1
                 ELSE
                    WRITE (cout,FMT=*) afield(l)%rmiss
                    DO ie=1,ne
                       WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
                    END DO
                    WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
                 END IF
              END DO
           END DO
        END DO
! - unreferenced data -
      CASE (3)
        IF (TRIM(afield(1)%var)/='undefined') THEN
           CALL write_tag (iout,ifail, &
                cpt_field=TRIM(afield(1)%var)//'_'//TRIM(cfld),cpt_nrow=afield(1)%nv,cpt_ncol=ne,cpt_row='station', &
                cpt_col='mode')
        ELSE
           CALL write_tag (iout,ifail, &
                cpt_field=TRIM(cfld),cpt_nrow=nv,cpt_ncol=ne,cpt_row='station',cpt_col='mode')
        END IF
        ii=0
        ij=1
        DO l=1,nls
           WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) 'Station'
           IF (nls>1) WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),'Season'
           DO ie=1,ne
              WRITE (cout,FMT=*) ie
              WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
           END DO
           WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
           DO j=1,afield(l)%nv
              ii=ii+1
              WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) TRIM(ADJUSTL(cstn(j,l)))
              IF (nls>1) WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(afield(l)%cssn)
              IF (iuse(ij)==ii) THEN
                 DO ie=1,ne
                    WRITE (cout,FMT=*) eof(ij,ie)
                    WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
                 END DO
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
                 ij=ij+1
              ELSE
                 WRITE (cout,FMT=*) afield(l)%rmiss
                 DO ie=1,ne
                    WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
                 END DO
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
              END IF
           END DO
        END DO
     END SELECT
   CASE ('unformatted')
     CALL write_unform (iout,afile,ne,nv,eof,iuse,kuse,afield(1)%rmiss,ifail)
     IF (ifail/=0) RETURN
  END SELECT
  ifail=0
!
  RETURN
!
! Error
1 ifail=3
!
  RETURN
  END SUBROUTINE write_load
!
!
!
  SUBROUTINE write_scor (iout,cscore,afile,nt,it0,period1,ne,kuse,miss,ts,ifail)
!
! Modules
  USE arrays,       ONLY: dwk,swk
  USE IO_constants, ONLY: faccs,cprcs
  USE iofiles,      ONLY: ofile
  USE numbers,      ONLY: sp
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iout ! - output unit number -
  INTEGER, INTENT(IN) :: nt   ! - number of cases -
  INTEGER, INTENT(IN) :: ne   ! - number of EOF modes -
  INTEGER, INTENT(IN) :: it0  ! - index of 0th case -
!
  REAL(KIND=rp), INTENT(IN) :: miss ! - missing value flag -
!
  CHARACTER(LEN=*), INTENT(IN) :: cscore ! - score description -
!
  TYPE(ofile), INTENT(IN) :: afile    ! - output file -
  TYPE(period), INTENT(IN) :: period1 ! - first period -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: ts(:,:) ! - time scores -
!
  LOGICAL, INTENT(IN) :: kuse(:) ! - used cases flags -
!
! Locals
!
! Local scalars
  INTEGER :: i  ! - EOF index -
  INTEGER :: k  ! - case index -
  INTEGER :: kk ! - available case index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ACHAR
  INTRINSIC ADJUSTL
  INTRINSIC REAL
  INTRINSIC TRIM
!
! Executable Statements
!
! Print headers
  SELECT CASE (TRIM(ffmts(afile%ffmt%ifmt)))
   CASE ('formatted')
     CALL write_tag (iout,ifail, &
                     cpt_field=cscore,cpt_nrow=nt,cpt_ncol=ne,cpt_row='T',cpt_col='index')
     IF (ifail/=0) GOTO 1
!
! Print temporal scores
     DO i=1,ne
        WRITE (cout,FMT=*) i
        WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
     END DO
     WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
     kk=0
     DO k=1,nt
        IF (kuse(k)) kk=kk+1
        cout=get_cdate(period1+(it0+k-1),1)
        WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) TRIM(ADJUSTL(cout))
        IF (kuse(k)) THEN
           DO i=1,ne
              WRITE (cout,FMT=*) ts(i,k)
              WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
           END DO
           WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
        ELSE
           DO i=1,ne
              WRITE (cout,FMT=*) miss
              WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
           END DO
           WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
        END IF
     END DO
   CASE ('unformatted')
     SELECT CASE (TRIM(faccs(afile%ffmt%iacc)))
      CASE ('sequential')
        SELECT CASE (TRIM(cprcs(afile%ffmt%iprc)))
         CASE ('double')
           DO i=1,ne
              kk=0
              DO k=1,nt
                 IF (kuse(k)) THEN
                    kk=kk+1
                    dwk(k)=ts(i,kk)
                 ELSE
                    dwk(k)=miss
                 END IF
              END DO
              WRITE (UNIT=iout,ERR=1) (dwk(k),k=1,nt)
           END DO
         CASE ('single')
           DO i=1,ne
              kk=0
              DO k=1,nt
                 IF (kuse(k)) THEN
                    kk=kk+1
                    swk(k)=REAL(ts(i,kk),KIND=sp)
                 ELSE
                    swk(k)=REAL(miss,KIND=sp)
                 END IF
              END DO
              WRITE (UNIT=iout,ERR=1) (swk(k),k=1,nt)
           END DO
        END SELECT
      CASE ('direct')
        SELECT CASE (TRIM(ffmts(afile%ffmt%ifmt)))
         CASE ('double')
           DO i=1,ne
              kk=0
              DO k=1,nt
                 IF (kuse(k)) THEN
                    kk=kk+1
                    dwk(k)=ts(i,kk)
                 ELSE
                    dwk(k)=miss
                 END IF
              END DO
              WRITE (UNIT=iout,REC=i,ERR=1) (dwk(k),k=1,nt)
           END DO
         CASE ('single')
           DO i=1,ne
              kk=0
              DO k=1,nt
                 IF (kuse(k)) THEN
                    kk=kk+1
                    swk(k)=REAL(ts(i,kk),KIND=sp)
                 ELSE
                    swk(k)=REAL(miss,KIND=sp)
                 END IF
              END DO
              WRITE (UNIT=iout,REC=i,ERR=1) (swk(k),k=1,nt)
           END DO
        END SELECT
     END SELECT
  END SELECT
  ifail=0
!
  RETURN
!
! Error
1 ifail=3
!
  RETURN
  END SUBROUTINE write_scor
!
!
!
  SUBROUTINE write_canc (iout,afile,nc,mu,ifail)
!
! Modules
  USE IO_constants, ONLY: faccs,cprcs
  USE iofiles,      ONLY: ofile
  USE numbers,      ONLY: sp
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iout ! - output unit number -
  INTEGER, INTENT(IN) :: nc   ! - number of correlations -
!
  TYPE(ofile), INTENT(IN) :: afile ! - output file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: mu(:) ! - singular values -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - canonical mode index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ACHAR
  INTRINSIC ADJUSTL
  INTRINSIC REAL
  INTRINSIC TRIM
!
! Executable Statements
!
! Print headers
  SELECT CASE (TRIM(ffmts(afile%ffmt%ifmt)))
   CASE ('formatted')
     CALL write_tag (iout,ifail, &
                     cpt_field='correlations',cpt_nrow=nc,cpt_ncol=1,cpt_row='mode',cpt_col='correlation')
     IF (ifail/=0) GOTO 1
!
! Print canonical correlations
     DO i=1,nc
        WRITE (cout,FMT=*) i
        WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) TRIM(ADJUSTL(cout)),ACHAR(9)
        WRITE (cout,FMT=*) mu(i)
        WRITE (UNIT=iout,FMT='(2A)',ADVANCE='yes',ERR=1) TRIM(ADJUSTL(cout))
     END DO
   CASE ('unformatted')
     SELECT CASE (TRIM(faccs(afile%ffmt%iacc)))
      CASE ('sequential')
        SELECT CASE (TRIM(cprcs(afile%ffmt%iprc)))
         CASE ('double')
           WRITE (UNIT=iout,ERR=1) (mu(i),i=1,nc)
         CASE ('single')
           WRITE (UNIT=iout,ERR=1) (REAL(mu(i),KIND=sp),i=1,nc)
        END SELECT
      CASE ('direct')
        SELECT CASE (TRIM(ffmts(afile%ffmt%ifmt)))
         CASE ('double')
           WRITE (UNIT=iout,REC=1,ERR=1) (mu(i),i=1,nc)
         CASE ('single')
           WRITE (UNIT=iout,REC=1,ERR=1) (REAL(mu(i),KIND=sp),i=1,nc)
        END SELECT
     END SELECT
  END SELECT
  ifail=0
!
  RETURN
!
! Error
1 ifail=3
!
  RETURN
  END SUBROUTINE write_canc
!
!
!
  SUBROUTINE write_regr (iout,afile,xfile,xfield,nx,iusex,yfile,yfield,ny,iusey,b,ifail, &
             rlatx,rlngx,cstnx,rlaty,rlngy,cstny,b0)
!
! Modules
  USE arrays,       ONLY: dwk,swk
  USE fields,       ONLY: field,tfield
  USE IO_constants, ONLY: faccs,cprcs
  USE iofiles,      ONLY: ofile,ifile
  USE numbers,      ONLY: sp
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iout   ! - output unit number -
  INTEGER, INTENT(IN) :: nx     ! - number of X variables -
  INTEGER, INTENT(IN) :: ny     ! - number of Y variables -
!
  TYPE(ofile), INTENT(IN) :: afile ! - output file -
!
  TYPE(ifile), INTENT(IN) :: xfile ! - X input file -
  TYPE(ifile), INTENT(IN) :: yfile ! - Y input file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: iusex(:) ! - X variable flags -
  INTEGER, INTENT(IN) :: iusey(:) ! - Y variable flags -
!
  REAL(KIND=rp), INTENT(IN) :: b(:,:) ! - regression coefficients -
!
  TYPE(field), INTENT(IN) :: xfield(:) ! - X field -
  TYPE(field), INTENT(IN) :: yfield(:) ! - Y field -
!
! - optional input arrays -
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: rlatx(:,:) ! - X latitudes -
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: rlngx(:,:) ! - X longitudes -
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: rlaty(:,:) ! - Y latitudes -
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: rlngy(:,:) ! - Y longitudes -
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: b0(:)    ! - regression constants -
!
  CHARACTER(LEN=lstn), INTENT(IN), OPTIONAL :: cstnx(:,:) ! - X station names -
  CHARACTER(LEN=lstn), INTENT(IN), OPTIONAL :: cstny(:,:) ! - Y station names -
!
! External Function
  CHARACTER(LEN=15), EXTERNAL :: make_coors
!
! Locals
!
! Local scalars
  INTEGER :: i,j,ij ! - indices -
  INTEGER :: ib0    ! - b0 present indicator -
  INTEGER :: ifdx   ! - X field index -
  INTEGER :: ifdy   ! - Y field index -
  INTEGER :: ilfx   ! - X lagged field index -
  INTEGER :: ilfy   ! - Y lagged field index -
  INTEGER :: lx     ! - X field / lagged field index -
  INTEGER :: ly     ! - Y field / lagged field index -
  INTEGER :: i1,j1  ! - indices -
  INTEGER :: i2,j2  ! - indices -
  INTEGER :: ix,jx  ! - indices -
  INTEGER :: iy,jy  ! - indices -
!
  REAL :: miss ! - missing value flag -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ACHAR
  INTRINSIC ADJUSTL
  INTRINSIC PRESENT
  INTRINSIC REAL
  INTRINSIC TRIM
!
! Executable Statements
!
! Print regression coefficients
  SELECT CASE (TRIM(ffmts(afile%ffmt%ifmt)))
   CASE ('formatted')
     ALLOCATE (tfield(xfile%nfs*xfile%nls))
     tfield(:)=xfield(:)
     tfield(:)%unit='none'
     SELECT CASE (xfile%igrid)
      CASE (1) ! - gridded X data -
        i2=1
        i1=0
        SELECT CASE (yfile%igrid)
         CASE (1) ! - gridded Y data -
           i1=0
           DO ifdy=1,yfile%nfs
              DO ilfy=1,yfile%nls
                 ly=(ifdy-1)*yfile%nls+ilfy
                 DO iy=1,yfield(ly)%region%nlts
                    DO jy=1,yfield(ly)%region%nlgs
                       i1=i1+1
                       IF (yfile%nfs>1) THEN
                          IF (yfile%nls>1) THEN
                             cout=TRIM(make_coors(rlaty(iy,ly),rlngy(jy,ly)))//'; '//TRIM(yfield(ly)%var)//&
                                                                              &'; '//TRIM(yfield(ly)%cssn)
                          ELSE
                             cout=TRIM(make_coors(rlaty(iy,ly),rlngy(jy,ly)))//'; '//TRIM(yfield(ly)%var)
                          END IF
                       ELSE
                          IF (yfile%nls>1) THEN
                             cout=TRIM(make_coors(rlaty(iy,ly),rlngy(jy,ly)))//'; '//TRIM(yfield(ly)%cssn)
                          ELSE
                             cout=TRIM(make_coors(rlaty(iy,ly),rlngy(jy,ly)))
                          END IF
                       END IF
                       CALL write_tag (iout,ifail,cpt_name=cout) 
                       IF (i1==iusey(i2)) THEN
                          CALL write_grid (iout,afile,tfield,xfile%nfs,xfile%nls,1,nx,b(:,i2:i2),iusex,(/.true./), &
                               rlatx,rlngx,ifail)
                          i2=i2+1
                       ELSE
                          CALL write_grid (iout,afile,tfield,xfile%nfs,xfile%nls,1,nx,b(:,1:1),iusex,(/.false./), &
                               rlatx,rlngx,ifail)
                       END IF
                       IF (ifail/=0) GOTO 1
                    END DO
                 END DO
              END DO
           END DO
         CASE (2) ! - station Y data -
           DO ifdy=1,yfile%nfs
              DO ilfy=1,yfile%nls
                 ly=(ifdy-1)*yfile%nls+ilfy
                 DO i=1,yfield(ly)%nv
                    IF (yfile%nfs>1) THEN
                       IF (yfile%nls>1) THEN
                          cout=TRIM(cstny(i,ly))//'; '//TRIM(yfield(ly)%var)//'; '//TRIM(yfield(ly)%cssn)
                       ELSE
                          cout=TRIM(cstny(i,ly))//'; '//TRIM(yfield(ly)%var)
                       END IF
                    ELSE
                       IF (yfile%nls>1) THEN
                          cout=TRIM(cstny(i,ly))//'; '//TRIM(yfield(ly)%cssn)
                       ELSE
                          cout=TRIM(cstny(i,ly))
                       END IF
                    END IF
                    CALL write_tag (iout,ifail,cpt_name=cout) 
                    IF (i==iusey(i2)) THEN
                       CALL write_grid (iout,afile,tfield,xfile%nfs,xfile%nls,1,nx,b(:,i2:i2),iusex,(/.true./), &
                            rlatx,rlngx,ifail)
                       i2=i2+1
                    ELSE
                       CALL write_grid (iout,afile,tfield,xfile%nfs,xfile%nls,1,nx,b(:,1:1),iusex,(/.false./), &
                            rlatx,rlngx,ifail)
                    END IF
                    IF (ifail/=0) GOTO 1
                 END DO
              END DO
           END DO
         CASE (3) ! - unreferenced Y data -
           DO ly=1,yfile%nls
              DO i=1,yfield(ly)%nv
                 IF (yfile%nls>1) THEN
                    cout=TRIM(cstny(i,ly))//'; '//TRIM(yfield(ly)%cssn)
                 ELSE
                    cout=TRIM(cstny(i,ly))
                 END IF
                 CALL write_tag (iout,ifail,cpt_name=cout) 
                 IF (iusey(i2)==i1) THEN
                    CALL write_grid (iout,afile,tfield,xfile%nfs,xfile%nls,1,nx,b(:,i2:i2),iusex,(/.true./), &
                         rlatx,rlngx,ifail)
                    i2=i2+1
                 ELSE
                    CALL write_grid (iout,afile,tfield,xfile%nfs,xfile%nls,1,nx,b(:,1:1),iusex,(/.false./), &
                         rlatx,rlngx,ifail)
                 END IF
                 IF (ifail/=0) GOTO 1
              END DO
           END DO
        END SELECT
      CASE (2) ! - station X data -
        IF (PRESENT(b0)) THEN
           ib0=1
        ELSE
           ib0=0
        END IF
        SELECT CASE (yfile%igrid)
         CASE (1) ! - gridded Y data -
           DO ifdx=1,xfile%nfs
              DO ilfx=1,xfile%nls
                 lx=(ifdx-1)*xfile%nls+ilfx
                 CALL write_tag (iout,ifail,                                                 &
                      cpt_field=TRIM(xfield(lx)%var),cpt_nrow=ny,cpt_ncol=xfield(lx)%nv+ib0, &
                      cpt_row='Y grid',cpt_col='X station',cpt_units='none',cpt_missing=xfield(lx)%rmiss)
                 IF (ifail/=0) GOTO 1
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) ACHAR(9),'Latitude'
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) ACHAR(9),'Longitude'
                 IF (yfile%nfs>1) WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),'Field'
                 IF (yfile%nls>1) WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),'Season'
                 IF (PRESENT(b0)) WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),'Constant'
                 DO i=1,xfield(lx)%nv
                    WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cstnx(i,lx)))
                 END DO
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) 'cpt:Y'
                 DO i=1,xfield(lx)%nv
                    WRITE (cout,FMT=*) rlatx(i,lx)
                    WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
                 END DO
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) 'cpt:X'
                 DO i=1,xfield(lx)%nv
                    WRITE (cout,FMT=*) rlngx(i,lx)
                    WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
                 END DO
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
                 iy=1
                 jy=0
                 DO ifdy=1,yfile%nfs
                    DO ilfy=1,yfile%nls
                       ly=(ifdy-1)*yfile%nls+ilfy
                       DO i=1,yfield(ly)%region%nlts
                          DO j=1,yfield(ly)%region%nlgs
                             jy=jy+1
                             WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) rlaty(j,ly)
                             WRITE (UNIT=cout,FMT=*) rlngy(j,ly)
                             WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                             IF (yfile%nfs>1) WRITE (UNIT=iout,ADVANCE='no',FMT='(2A)',ERR=1) ACHAR(9),TRIM(yfield(ly)%var)
                             IF (yfile%nls>1) WRITE (UNIT=iout,ADVANCE='no',FMT='(2A)',ERR=1) ACHAR(9),TRIM(yfield(ly)%cssn)
                             IF (iusey(iy)==jy) THEN
                                IF (PRESENT(b0)) THEN
                                   WRITE (UNIT=cout,FMT=*) b0(iy)
                                   WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                                END IF
                                ix=1
                                jx=0
                                IF (lx>1) THEN
                                   ix=ix+SUM(xfield(1:lx-1)%nva)
                                   jx=jx+SUM(xfield(1:lx-1)%nv)
                                END IF
                                DO ij=1,xfield(lx)%nv
                                   jx=jx+1
                                   IF (iusex(ix)==jx) THEN
                                      WRITE (UNIT=cout,FMT=*) b(ix,iy)
                                      ix=ix+1
                                   ELSE
                                      WRITE (UNIT=cout,FMT=*) xfield(lx)%rmiss
                                   END IF
                                   WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                                END DO
                                iy=iy+1
                             ELSE
                                WRITE (UNIT=cout,FMT=*) xfield(lx)%rmiss
                                DO ix=1,xfield(lx)%nv+ib0
                                   WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                                END DO
                             END IF
                             WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
                          END DO
                       END DO
                    END DO
                 END DO
              END DO
           END DO
         CASE (2) ! - station Y data -
           DO ifdx=1,xfile%nfs
              DO ilfx=1,xfile%nls
                 lx=(ifdx-1)*xfile%nls+ilfx
                 CALL write_tag (iout,ifail,                                                 &
                      cpt_field=TRIM(xfield(lx)%var),cpt_nrow=ny,cpt_ncol=xfield(lx)%nv+ib0, &
                      cpt_row='Y station',cpt_col='X station',cpt_units='none',cpt_missing=xfield(lx)%rmiss)
                 IF (ifail/=0) GOTO 1
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) ACHAR(9),'Station'
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) ACHAR(9),'Latitude'
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) ACHAR(9),'Longitude'
                 IF (yfile%nfs>1) WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),'Field'
                 IF (yfile%nls>1) WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),'Season'
                 IF (PRESENT(b0)) WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),'Constant'
                 DO i=1,xfield(lx)%nv
                    WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cstnx(i,lx)))
                 END DO
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) 'cpt:Y'
                 DO i=1,xfield(lx)%nv
                    WRITE (cout,FMT=*) rlatx(i,lx)
                    WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
                 END DO
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) 'cpt:X'
                 DO i=1,xfield(lx)%nv
                    WRITE (cout,FMT=*) rlngx(i,lx)
                    WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
                 END DO
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
                 iy=1
                 jy=0
                 DO ifdy=1,yfile%nfs
                    DO ilfy=1,yfile%nls
                       ly=(ifdy-1)*yfile%nls+ilfy
                       DO j=1,yfield(ly)%region%nlts
                          jy=jy+1
                          WRITE (UNIT=iout,ADVANCE='no',FMT='(A)',ERR=1) TRIM(cstny(j,ly))
                          WRITE (UNIT=cout,FMT=*) rlaty(j,ly)
                          WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                          WRITE (UNIT=cout,FMT=*) rlngy(j,ly)
                          WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                          IF (yfile%nfs>1) WRITE (UNIT=iout,ADVANCE='no',FMT='(2A)',ERR=1) ACHAR(9),TRIM(yfield(ly)%var)
                          IF (yfile%nls>1) WRITE (UNIT=iout,ADVANCE='no',FMT='(2A)',ERR=1) ACHAR(9),TRIM(yfield(ly)%cssn)
                          IF (iusey(iy)==jy) THEN
                             IF (PRESENT(b0)) THEN
                                WRITE (UNIT=cout,FMT=*) b0(iy)
                                WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                             END IF
                             ix=1
                             jx=0
                             IF (lx>1) THEN
                                ix=ix+SUM(xfield(1:lx-1)%nva)
                                jx=jx+SUM(xfield(1:lx-1)%nv)
                             END IF
                             DO i=1,xfield(lx)%nv
                                jx=jx+1
                                IF (iusex(ix)==jx) THEN
                                   WRITE (UNIT=cout,FMT=*) b(ix,iy)
                                   ix=ix+1
                                ELSE
                                   WRITE (UNIT=cout,FMT=*) xfield(lx)%rmiss
                                END IF
                                WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                             END DO
                             iy=iy+1
                          ELSE
                             WRITE (UNIT=cout,FMT=*) xfield(lx)%rmiss
                             DO ix=1,xfield(lx)%nv+ib0
                                WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                             END DO
                          END IF
                          WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
                       END DO
                    END DO
                 END DO
              END DO
           END DO
         CASE (3) ! - unreferenced Y data -
           DO ifdx=1,xfile%nfs
              DO ilfx=1,xfile%nls
                 lx=(ifdx-1)*xfile%nls+ilfx
                 CALL write_tag (iout,ifail,                                                 &
                      cpt_field=TRIM(xfield(lx)%var),cpt_nrow=ny,cpt_ncol=xfield(lx)%nv+ib0, &
                      cpt_row='Y index',cpt_col='X station',cpt_units='none',cpt_missing=xfield(lx)%rmiss)
                 IF (ifail/=0) GOTO 1
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) ACHAR(9),'Index'
                 IF (yfile%nls>1) WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),'Season'
                 IF (PRESENT(b0)) WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),'Constant'
                 DO i=1,xfield(lx)%nv
                    WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cstnx(i,lx)))
                 END DO
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) 'cpt:Y'
                 DO i=1,xfield(lx)%nv
                    WRITE (cout,FMT=*) rlatx(i,lx)
                    WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
                 END DO
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) 'cpt:X'
                 DO i=1,xfield(lx)%nv
                    WRITE (cout,FMT=*) rlngx(i,lx)
                    WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cout))
                 END DO
                 WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
                 iy=1
                 jy=0
                 DO ly=1,yfile%nls
                    DO j=1,yfield(ly)%nv
                       jy=jy+1
                       WRITE (UNIT=iout,ADVANCE='no',FMT='(A)',ERR=1) TRIM(cstny(j,ly))
                       IF (yfile%nls>1) WRITE (UNIT=iout,ADVANCE='no',FMT='(2A)',ERR=1) ACHAR(9),TRIM(yfield(ly)%cssn)
                       IF (iusey(iy)==jy) THEN
                          IF (PRESENT(b0)) THEN
                             WRITE (UNIT=cout,FMT=*) b0(iy)
                             WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                          END IF
                          ix=1
                          jx=0
                          IF (lx>1) THEN
                             ix=ix+SUM(xfield(1:lx-1)%nva)
                             jx=jx+SUM(xfield(1:lx-1)%nv)
                          END IF
                          DO i=1,xfield(lx)%nv
                             jx=jx+1
                             IF (iusex(ix)==jx) THEN
                                WRITE (UNIT=cout,FMT=*) b(ix,iy)
                                ix=ix+1
                             ELSE
                                WRITE (UNIT=cout,FMT=*) xfield(lx)%rmiss
                             END IF
                             WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                          END DO
                          iy=iy+1
                       ELSE
                          WRITE (UNIT=cout,FMT=*) xfield(lx)%rmiss
                          DO ix=1,xfield(lx)%nv+ib0
                             WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                          END DO
                       END IF
                       WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
                    END DO
                 END DO
              END DO
           END DO
        END SELECT
      CASE (3) ! - unreferenced X data -
        IF (PRESENT(b0)) THEN
           ib0=1
        ELSE
           ib0=0
        END IF
        SELECT CASE (yfile%igrid)
         CASE (1) ! - gridded Y data -
           DO lx=1,xfile%nls
              CALL write_tag (iout,ifail,                                                 &
                   cpt_field=TRIM(xfield(lx)%var),cpt_nrow=ny,cpt_ncol=xfield(lx)%nv+ib0, &
                   cpt_row='Y grid',cpt_col='X index',cpt_units='none',cpt_missing=xfield(lx)%rmiss)
              IF (ifail/=0) GOTO 1
              WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) 'Latitude'
              WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),'Longitude'
              IF (yfile%nfs>1) WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),'Field'
              IF (yfile%nls>1) WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),'Season'
              IF (PRESENT(b0)) WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),'Constant'
              DO i=1,xfield(lx)%nv
                 WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(ADJUSTL(cstnx(i,lx)))
              END DO
              WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
              iy=1
              jy=0
              DO ifdy=1,yfile%nfs
                 DO ilfy=1,yfile%nls
                    ly=(ifdy-1)*yfile%nls+ilfy
                    DO i=1,yfield(ly)%region%nlts
                       DO j=1,yfield(ly)%region%nlgs
                          jy=jy+1
                          WRITE (UNIT=cout,FMT=*) rlaty(i,ly)
                          WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) ADJUSTL(TRIM(cout))
                          WRITE (UNIT=cout,FMT=*) rlngy(j,ly)
                          WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                          IF (yfile%nfs>1) WRITE (UNIT=iout,ADVANCE='no',FMT='(2A)',ERR=1) ACHAR(9),TRIM(yfield(ly)%var)
                          IF (yfile%nls>1) WRITE (UNIT=iout,ADVANCE='no',FMT='(2A)',ERR=1) ACHAR(9),TRIM(yfield(ly)%cssn)
                          IF (iusey(iy)==jy) THEN
                             IF (PRESENT(b0)) THEN
                                WRITE (UNIT=cout,FMT=*) b0(iy)
                                WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                             END IF
                             ix=1
                             jx=0
                             IF (lx>1) THEN
                                ix=ix+SUM(xfield(1:lx-1)%nva)
                                jx=jx+SUM(xfield(1:lx-1)%nv)
                             END IF
                             DO ij=1,xfield(lx)%nv
                                jx=jx+1
                                IF (iusex(ix)==jx) THEN
                                   WRITE (UNIT=cout,FMT=*) b(ix,iy)
                                   ix=ix+1
                                ELSE
                                   WRITE (UNIT=cout,FMT=*) xfield(lx)%rmiss
                                END IF
                                WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                             END DO
                             iy=iy+1
                          ELSE
                             WRITE (UNIT=cout,FMT=*) xfield(lx)%rmiss
                             DO ix=1,xfield(lx)%nv+ib0
                                WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                             END DO
                          END IF
                          WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
                       END DO
                    END DO
                 END DO
              END DO
           END DO
         CASE (2) ! - station Y data -
           DO lx=1,xfile%nls
              CALL write_tag (iout,ifail,                                                 &
                   cpt_field=TRIM(xfield(lx)%var),cpt_nrow=ny,cpt_ncol=xfield(lx)%nv+ib0, &
                   cpt_row='Y station',cpt_col='X index',cpt_units='none',cpt_missing=xfield(lx)%rmiss)
              IF (ifail/=0) GOTO 1
              WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) 'Station'
              WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),'Latitude'
              WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),'Longitude'
              IF (yfile%nfs>1) WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),'Field'
              IF (yfile%nls>1) WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),'Season'
              IF (PRESENT(b0)) WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),'Constant'
              DO i=1,xfield(lx)%nv
                 WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(cstnx(i,lx))
              END DO
              WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
              iy=1
              jy=0
              DO ifdy=1,yfile%nfs
                 DO ilfy=1,yfile%nls
                    ly=(ifdy-1)*yfile%nls+ilfy
                    DO j=1,yfield(ly)%nv
                       jy=jy+1
                       WRITE (UNIT=iout,ADVANCE='no',FMT='(A)',ERR=1) TRIM(cstny(j,ly))
                       WRITE (UNIT=cout,FMT=*) rlaty(j,ly)
                       WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                       WRITE (UNIT=cout,FMT=*) rlngy(j,ly)
                       WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                       IF (yfile%nfs>1) WRITE (UNIT=iout,ADVANCE='no',FMT='(2A)',ERR=1) ACHAR(9),TRIM(yfield(ly)%var)
                       IF (yfile%nls>1) WRITE (UNIT=iout,ADVANCE='no',FMT='(2A)',ERR=1) ACHAR(9),TRIM(yfield(ly)%cssn)
                       IF (iusey(iy)==jy) THEN
                          IF (PRESENT(b0)) THEN
                             WRITE (UNIT=cout,FMT=*) b0(iy)
                             WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                          END IF
                          ix=1
                          jx=0
                          IF (lx>1) THEN
                             ix=ix+SUM(xfield(1:lx-1)%nva)
                             jx=jx+SUM(xfield(1:lx-1)%nv)
                          END IF
                          DO i=1,xfield(lx)%nv
                             jx=jx+1
                             IF (iusex(ix)==jx) THEN
                                WRITE (UNIT=cout,FMT=*) b(ix,iy)
                                ix=ix+1
                             ELSE
                                WRITE (UNIT=cout,FMT=*) xfield(lx)%rmiss
                             END IF
                             WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                          END DO
                          iy=iy+1
                       ELSE
                          WRITE (UNIT=cout,FMT=*) xfield(lx)%rmiss
                          DO ix=1,xfield(lx)%nv+ib0
                             WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                          END DO
                       END IF
                       WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
                    END DO
                 END DO
              END DO
           END DO
         CASE (3) ! - unreferenced Y data -
           DO lx=1,xfile%nls
              CALL write_tag (iout,ifail,                                                 &
                   cpt_field=TRIM(xfield(lx)%var),cpt_nrow=ny,cpt_ncol=xfield(lx)%nv+ib0, &
                   cpt_row='Y index',cpt_col='X index',cpt_units='none',cpt_missing=xfield(lx)%rmiss)
              IF (ifail/=0) GOTO 1
              WRITE (UNIT=iout,FMT='(A)',ADVANCE='no',ERR=1) ACHAR(9),'Index'
              IF (yfile%nls>1) WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),'Season'
              IF (PRESENT(b0)) WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),'Constant'
              DO i=1,xfield(lx)%nv
                 WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),TRIM(cstnx(i,lx))
              END DO
              WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
              iy=1
              jy=0
              DO ly=1,yfile%nls
                 DO j=1,yfield(ly)%nv
                    jy=jy+1
                    WRITE (UNIT=iout,ADVANCE='no',FMT='(A)',ERR=1) TRIM(cstny(j,ly))
                    IF (yfile%nls>1) WRITE (UNIT=iout,ADVANCE='no',FMT='(2A)',ERR=1) ACHAR(9),TRIM(yfield(ly)%cssn)
                    IF (iusey(iy)==jy) THEN
                       IF (PRESENT(b0)) THEN
                          WRITE (UNIT=cout,FMT=*) b0(iy)
                          WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                       END IF
                       ix=1
                       jx=0
                       IF (lx>1) THEN
                          ix=ix+SUM(xfield(1:lx-1)%nva)
                          jx=jx+SUM(xfield(1:lx-1)%nv)
                       END IF
                       DO i=1,xfield(lx)%nv
                          jx=jx+1
                          IF (iusex(ix)==jx) THEN
                             WRITE (UNIT=cout,FMT=*) b(ix,iy)
                             ix=ix+1
                          ELSE
                             WRITE (UNIT=cout,FMT=*) xfield(lx)%rmiss
                          END IF
                          WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                       END DO
                       iy=iy+1
                    ELSE
                       WRITE (UNIT=cout,FMT=*) xfield(lx)%rmiss
                       DO ix=1,xfield(lx)%nv+ib0
                          WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) ACHAR(9),ADJUSTL(TRIM(cout))
                       END DO
                    END IF
                    WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1)
                 END DO
              END DO
           END DO
        END SELECT
     END SELECT
     DEALLOCATE (tfield)
   CASE ('unformatted')
     SELECT CASE (TRIM(faccs(afile%ffmt%iacc)))
      CASE ('sequential')
        SELECT CASE (TRIM(cprcs(afile%ffmt%iprc)))
         CASE ('double')
           i2=1
           DO i1=1,ny
              IF (iusey(i2)==i1) THEN
                 j2=1
                 DO j1=1,nx
                    IF (iusex(j2)==j1) THEN
                       dwk(j1)=b(j2,i2)
                       j2=j2+1
                    ELSE
                       dwk(j1)=xfield(1)%rmiss
                    END IF
                 END DO
                 WRITE (UNIT=iout,ERR=1) (dwk(j1),j1=1,nx)
                 i2=i2+1
              ELSE
                 WRITE (UNIT=iout,ERR=1) (xfield(1)%rmiss,j1=1,nx)
              END IF
           END DO
         CASE ('single')
           miss=REAL(xfield(1)%rmiss,KIND=sp)
           i2=1
           DO i1=1,ny
              IF (iusey(i2)==i1) THEN
                 j2=1
                 DO j1=1,nx
                    IF (iusex(j2)==j1) THEN
                       swk(j1)=REAL(b(j2,i2),KIND=sp)
                       j2=j2+1
                    ELSE
                       swk(j1)=miss
                    END IF
                 END DO
                 WRITE (UNIT=iout,ERR=1) (swk(j1),j1=1,nx)
                 i2=i2+1
              ELSE
                 WRITE (UNIT=iout,ERR=1) (miss,j1=1,nx)
              END IF
           END DO
        END SELECT
      CASE ('direct')
        SELECT CASE (TRIM(cprcs(afile%ffmt%iprc)))
         CASE ('double')
           i2=1
           DO i1=1,ny
              IF (iusey(i2)==i1) THEN
                 j2=1
                 DO j1=1,nx
                    IF (iusex(j2)==j1) THEN
                       dwk(j1)=b(j2,i2)
                       j2=j2+1
                    ELSE
                       dwk(j1)=xfield(1)%rmiss
                    END IF
                 END DO
                 WRITE (UNIT=iout,REC=i1,ERR=1) (dwk(j1),j1=1,nx)
                 i2=i2+1
              ELSE
                 WRITE (UNIT=iout,REC=i1,ERR=1) (xfield(1)%rmiss,j1=1,nx)
              END IF
           END DO
         CASE ('single')
           miss=REAL(xfield(1)%rmiss,KIND=sp)
           i2=1
           DO i1=1,ny
              IF (iusey(i2)==i1) THEN
                 j2=1
                 DO j1=1,nx
                    IF (iusex(j2)==j1) THEN
                       swk(j1)=REAL(b(j2,i2),KIND=sp)
                       j2=j2+1
                    ELSE
                       swk(j1)=miss
                    END IF
                 END DO
                 WRITE (UNIT=iout,REC=i1,ERR=1) (swk(j1),j1=1,nx)
                 i2=i2+1
              ELSE
                 WRITE (UNIT=iout,REC=i1,ERR=1) (miss,j1=1,nx)
              END IF
           END DO
        END SELECT
     END SELECT
  END SELECT
  ifail=0
!
  RETURN
!
! Error
1 ifail=3
!
  RETURN
  END SUBROUTINE write_regr
!
!
!
  SUBROUTINE write_roc (iout,afile,nb,ng,hit,far,roca,ifail)
!
! Modules
  USE IO_constants, ONLY: faccs,cprcs
  USE iofiles,      ONLY: ofile
  USE numbers,      ONLY: sp
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iout ! - output unit number -
  INTEGER, INTENT(IN) :: nb   ! - number of bins -
  INTEGER, INTENT(IN) :: ng   ! - number of categories -
!
  TYPE(ofile), INTENT(IN) :: afile ! - output file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: roca(:)  ! - ROC areas -
  REAL(KIND=rp), INTENT(IN) :: hit(:,:) ! - hit rates -
  REAL(KIND=rp), INTENT(IN) :: far(:,:) ! - false-alarm rates -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - bin index -
  INTEGER :: j ! - category index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ACHAR
  INTRINSIC ADJUSTL
  INTRINSIC TRIM
!
! Executable Statements
!
! Print hit and flase-alarm rates
  SELECT CASE (TRIM(ffmts(afile%ffmt%ifmt)))
   CASE ('formatted')
     DO j=1,ng
        WRITE (UNIT=iout,FMT=*,ERR=1) j,roca(j)
        WRITE (UNIT=iout,FMT='(3A)',ERR=1) 'Hit rates',ACHAR(9),'False-alarm rates'
        DO i=1,nb
           WRITE (cout,FMT=*) hit(i,j)
           WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) TRIM(ADJUSTL(cout)),ACHAR(9)
           WRITE (cout,FMT=*) far(i,j)
           WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1) TRIM(ADJUSTL(cout))
        END DO
     END DO
   CASE ('unformatted')
     SELECT CASE (TRIM(faccs(afile%ffmt%iacc)))
      CASE ('sequential')
        SELECT CASE (TRIM(cprcs(afile%ffmt%iprc)))
         CASE ('double')
           WRITE (UNIT=iout,ERR=1) roca(1:ng)
           WRITE (UNIT=iout,ERR=1) hit(1:nb,1:ng)
           WRITE (UNIT=iout,ERR=1) far(1:nb,1:ng)
         CASE ('single')
           WRITE (UNIT=iout,ERR=1) REAL(roca(1:ng),KIND=sp)
           WRITE (UNIT=iout,ERR=1) REAL(hit(1:nb,1:ng),KIND=sp)
           WRITE (UNIT=iout,ERR=1) REAL(far(1:nb,1:ng),KIND=sp)
        END SELECT
      CASE ('direct')
        SELECT CASE (TRIM(cprcs(afile%ffmt%iprc)))
         CASE ('double')
           WRITE (UNIT=iout,ERR=1,REC=1) roca(1:ng)
           WRITE (UNIT=iout,ERR=1,REC=2) hit(1:nb,1:ng)
           WRITE (UNIT=iout,ERR=1,REC=3) far(1:nb,1:ng)
         CASE ('single')
           WRITE (UNIT=iout,ERR=1,REC=1) REAL(roca(1:ng),KIND=sp)
           WRITE (UNIT=iout,ERR=1,REC=2) REAL(hit(1:nb,1:ng),KIND=sp)
           WRITE (UNIT=iout,ERR=1,REC=3) REAL(far(1:nb,1:ng),KIND=sp)
        END SELECT
     END SELECT
  END SELECT
  ifail=0
!
  RETURN
!
! Error
1 ifail=3
!
  RETURN
  END SUBROUTINE write_roc
!
!
!
  SUBROUTINE write_rel (iout,afile,ng,nb,afp,orf,ifq,ifail)
!
! Modules
  USE IO_constants, ONLY: faccs,cprcs
  USE iofiles,      ONLY: ofile
  USE labels,       ONLY: cg_cat_l
  USE numbers,      ONLY: sp
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iout ! - output unit number -
  INTEGER, INTENT(IN) :: ng   ! - number of categories -
  INTEGER, INTENT(IN) :: nb   ! - number of probability bins -
!
  TYPE(ofile), INTENT(IN) :: afile ! - output file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: ifq(:,0:) ! - frequency -
!
  REAL(KIND=rp), INTENT(IN) :: afp(:,0:) ! - bin-averaged forecast probability -
  REAL(KIND=rp), INTENT(IN) :: orf(:,0:) ! - observed relative frequency -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - category index -
  INTEGER :: k ! - probability bin index -
!
  REAL(KIND=rp) :: tfq ! - total frequency -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ACHAR
  INTRINSIC ADJUSTL
  INTRINSIC TRIM
!
! Executable Statements
!
! Print hit and flase-alarm rates
  SELECT CASE (TRIM(ffmts(afile%ffmt%ifmt)))
   CASE ('formatted')
     DO i=0,ng
        WRITE (UNIT=iout,FMT='(A)',ERR=1) cg_cat_l(i)
        WRITE (UNIT=iout,FMT='(7A)',ERR=1) 'Forecast probability (x)',ACHAR(9),'Observed relative frequency (y)',ACHAR(9), &
                                           'Frequency',ACHAR(9),'Relative frequency (%)'
        tfq=REAL(SUM(ifq(:,i)),KIND=rp)
        DO k=1,nb
           WRITE (cout,FMT=*) afp(k,i)
           WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) TRIM(ADJUSTL(cout)),ACHAR(9)
           WRITE (cout,FMT=*) orf(k,i)
           WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) TRIM(ADJUSTL(cout)),ACHAR(9)
           WRITE (cout,FMT=*) ifq(k,i)
           WRITE (UNIT=iout,FMT='(2A)',ADVANCE='no',ERR=1) TRIM(ADJUSTL(cout)),ACHAR(9)
           WRITE (cout,FMT=*) REAL(100*ifq(k,i),KIND=rp)/tfq
           WRITE (UNIT=iout,FMT='(A)',ADVANCE='yes',ERR=1) TRIM(ADJUSTL(cout))
        END DO
     END DO
   CASE ('unformatted')
     SELECT CASE (TRIM(faccs(afile%ffmt%iacc)))
      CASE ('sequential')
        SELECT CASE (TRIM(cprcs(afile%ffmt%iprc)))
         CASE ('double')
           WRITE (UNIT=iout,ERR=1) afp(:,:)
           WRITE (UNIT=iout,ERR=1) orf(:,:)
           WRITE (UNIT=iout,ERR=1) ifq(:,:)
         CASE ('single')
           WRITE (UNIT=iout,ERR=1) REAL(afp(:,:),KIND=sp)
           WRITE (UNIT=iout,ERR=1) REAL(orf(:,:),KIND=sp)
           WRITE (UNIT=iout,ERR=1) REAL(ifq(:,:),KIND=sp)
        END SELECT
      CASE ('direct')
        SELECT CASE (TRIM(cprcs(afile%ffmt%iprc)))
         CASE ('double')
           WRITE (UNIT=iout,ERR=1,REC=1) afp(:,:)
           WRITE (UNIT=iout,ERR=1,REC=2) orf(:,:)
           WRITE (UNIT=iout,ERR=1,REC=3) ifq(:,:)
         CASE ('single')
           WRITE (UNIT=iout,ERR=1,REC=1) REAL(afp(:,:),KIND=sp)
           WRITE (UNIT=iout,ERR=1,REC=2) REAL(orf(:,:),KIND=sp)
           WRITE (UNIT=iout,ERR=1,REC=3) REAL(ifq(:,:),KIND=sp)
        END SELECT
     END SELECT
  END SELECT
  ifail=0
!
  RETURN
!
! Error
1 ifail=3
!
  RETURN
  END SUBROUTINE write_rel
!
!
!
  SUBROUTINE write_unform (iout,afile,nt,nv,v,iuse,kuse,miss,ifail)
!
! Outputs unformatted data
!
! Modules
  USE arrays,       ONLY: dwk
  USE IO_constants, ONLY: faccs,cprcs
  USE iofiles,      ONLY: ofile
  USE numbers,      ONLY: sp
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iout ! - output unit number -
  INTEGER, INTENT(IN) :: nt   ! - number of cases -
  INTEGER, INTENT(IN) :: nv   ! - number of series -
!
  REAL(KIND=rp), INTENT(IN) :: miss ! - missing value flag -
!
  TYPE(ofile), INTENT(IN) :: afile ! - output file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: iuse(:) ! - used variables indices -
!
  REAL(KIND=rp), INTENT(IN) :: v(:,:) ! - data -
!
  LOGICAL, INTENT(IN) :: kuse(:) ! - used cases flags -
!
! Locals
!
! Local scalars
  INTEGER :: i  ! - series index -
  INTEGER :: j  ! - available series index -
  INTEGER :: k  ! - time index -
  INTEGER :: kk ! - available time index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC TRIM
!
! Executable Statements
!
! Print unformatted data
  kk=0
  DO k=1,nt
     IF (kuse(k)) kk=kk+1
     j=1
     DO i=1,nv
        IF (kuse(k)) THEN
           IF (iuse(j)==i) THEN
              dwk(i)=v(j,kk)
              j=j+1
           ELSE
              dwk(i)=miss
           END IF
        ELSE
           dwk(i)=miss
        END IF
     END DO
     SELECT CASE (TRIM(faccs(afile%ffmt%iacc)))
      CASE ('sequential')
        SELECT CASE (TRIM(cprcs(afile%ffmt%iprc)))
         CASE ('double')
           WRITE (UNIT=iout,ERR=1) (dwk(i),i=1,nv)
         CASE ('single')
           WRITE (UNIT=iout,ERR=1) (REAL(dwk(i),KIND=sp),i=1,nv)
        END SELECT
      CASE ('direct')
        SELECT CASE (TRIM(cprcs(afile%ffmt%iprc)))
         CASE ('double')
           WRITE (UNIT=iout,REC=k,ERR=1) (dwk(i),i=1,nv)
         CASE ('single')
           WRITE (UNIT=iout,REC=k,ERR=1) (REAL(dwk(i),KIND=sp),i=1,nv)
        END SELECT
     END SELECT
  END DO
  ifail=0
!
  RETURN
!
! Error
1 ifail=3
!
  RETURN
  END SUBROUTINE write_unform
 END SUBROUTINE write_results
!
!
!
 SUBROUTINE open_output (iout,afile,nfs,ifail)
!
! Opens CPT output file and prints XMLNS header
!
! Modules
  USE IO_constants, ONLY: faccs,ffmts,cxmlns_cpt,lfli
  USE iofiles,      ONLY: ofile
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iout ! - output unit number -
  INTEGER, INTENT(IN) :: nfs  ! - number of fields -
!
  TYPE(ofile), INTENT(IN) :: afile ! - output file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC TRIM
!
! Open output file
  SELECT CASE (TRIM(faccs(afile%ffmt%iacc)))
   CASE ('sequential')
#ifdef NAG
     OPEN (UNIT=iout,FILE=afile%ffile,ACCESS='sequential',ACTION='write',FORM=ffmts(afile%ffmt%ifmt), &
           IOSTAT=ifail,RECL=lfli,STATUS='unknown')
#else
     OPEN (UNIT=iout,FILE=afile%ffile,ACCESS='sequential',ACTION='write',FORM=ffmts(afile%ffmt%ifmt), &
           IOSTAT=ifail,STATUS='unknown')
#endif
   CASE ('direct')
     OPEN (UNIT=iout,FILE=afile%ffile,ACCESS='direct',ACTION='write',FORM=ffmts(afile%ffmt%ifmt), &
           IOSTAT=ifail,RECL=afile%ffmt%lrec,STATUS='unknown')
  END SELECT
!
! Error
  IF (ifail/=0) THEN
     ifail=1
     RETURN
  END IF
!
! Print XMLNS header
  SELECT CASE (TRIM(ffmts(afile%ffmt%ifmt)))
   CASE ('formatted')
     WRITE (UNIT=iout,FMT='(A)',IOSTAT=ifail) 'xmlns:cpt='//cxmlns_cpt
     IF (ifail/=0) GOTO 1
!
! Print number of fields
     CALL write_tag (iout,ifail, &
                     cpt_nfields=nfs)
  END SELECT
!
! Error
1 IF (ifail/=0) ifail=3
!
  RETURN
 END SUBROUTINE open_output
!
!
!
 SUBROUTINE write_tag (iout,ifail,                                                                &
                       cpt_nfields,cpt_name,cpt_field,cpt_cmode,cpt_mode,cpt_t,cpt_s,cpt_z,cpt_m, &
                       cpt_clev,cpt_limit,cpt_nrow,cpt_ncol,cpt_row,cpt_col,cpt_units,cpt_missing)
!
! Modules
  USE fields,       ONLY: level
  USE IO_constants, ONLY: lvar
  USE maths,        ONLY: iprec,magnitude
  USE numbers,      ONLY: one
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iout ! - output unit number -
!
! - optional input scalars -
  INTEGER, INTENT(IN), OPTIONAL :: cpt_nfields ! - number of fields -
  INTEGER, INTENT(IN), OPTIONAL :: cpt_mode    ! - current mode -
  INTEGER, INTENT(IN), OPTIONAL :: cpt_m       ! - ensemble member -
  INTEGER, INTENT(IN), OPTIONAL :: cpt_nrow    ! - number of rows -
  INTEGER, INTENT(IN), OPTIONAL :: cpt_ncol    ! - number of columns -
!
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: cpt_clev    ! - confidence level -
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: cpt_missing ! - missing values flag -
!
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: cpt_field ! - field -
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: cpt_name  ! - name -
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: cpt_cmode ! - current mode -
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: cpt_row   ! - rows -
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: cpt_limit ! - confidence limit -
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: cpt_col   ! - columns -
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: cpt_units ! - units -
!
  TYPE(level), INTENT(IN), OPTIONAL :: cpt_z ! - level -
!
  TYPE(period), INTENT(IN), OPTIONAL :: cpt_t ! - date -
!
  TYPE(date), INTENT(IN), OPTIONAL :: cpt_s ! - start date -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
  CHARACTER(LEN=    15) :: cfmt ! - format statement -
  CHARACTER(LEN=lvar+1) :: cout ! - output field -
!
  LOGICAL :: lfirst ! - first output field -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ADJUSTL
  INTRINSIC NINT
  INTRINSIC PRESENT
  INTRINSIC TRIM
!
! Executable Statements
!
! Print nfields
  IF (PRESENT(cpt_nfields)) THEN
     WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(cpt_nfields),')'
     WRITE (UNIT=iout,FMT=cfmt,ERR=1) 'cpt:nfields=',cpt_nfields
     RETURN
  END IF
!
! Print name
  IF (PRESENT(cpt_name)) THEN
     WRITE (UNIT=iout,FMT='(A)',ERR=1) 'cpt:Name='//TRIM(cpt_name)
     RETURN
  END IF
!
! Print tags line
  lfirst=.true.
  IF (PRESENT(cpt_field)) THEN
     WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') 'cpt:field='//TRIM(cpt_field)
     lfirst=.false.
  END IF
  IF (PRESENT(cpt_cmode)) THEN
     IF (.NOT.lfirst) WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') ', '
     WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') 'cpt:mode='//TRIM(cpt_cmode)
     lfirst=.false.
  END IF
  IF (PRESENT(cpt_mode)) THEN
     IF (.NOT.lfirst) WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') ', '
     WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(cpt_mode),')'
     WRITE (UNIT=iout,FMT=cfmt,ERR=1,ADVANCE='no') 'cpt:mode=',cpt_mode
     lfirst=.false.
  END IF
  IF (PRESENT(cpt_m)) THEN
     IF (cpt_m>0) THEN
        IF (.NOT.lfirst) WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') ', '
        WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(cpt_m),')'
        WRITE (UNIT=iout,FMT=cfmt,ERR=1,ADVANCE='no') 'cpt:M=',cpt_m
        lfirst=.false.
     END IF
  END IF
  IF ((PRESENT(cpt_clev)).AND.(PRESENT(cpt_limit))) THEN
     IF (.NOT.lfirst) WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') ', '
     WRITE (UNIT=iout,FMT='(3A)',ERR=1,ADVANCE='no') 'cpt:climit=',TRIM(cpt_limit),' ('
     WRITE (UNIT=cfmt,FMT='(A,2(I1,A))') '(F',iprec(cpt_clev,3)+3,'.',iprec(cpt_clev,3),')'
     WRITE (UNIT=iout,FMT=cfmt,ERR=1,ADVANCE='no') cpt_clev
     WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') '%)'
     lfirst=.false.
  END IF
  IF (PRESENT(cpt_z)) THEN
     IF (TRIM(cpt_z%unit)/='none') THEN
        IF (.NOT.lfirst) WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') ', '
        IF (cpt_z%hght>one) THEN
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(NINT(cpt_z%hght)),')'
           WRITE (UNIT=iout,FMT=cfmt,ERR=1,ADVANCE='no') 'cpt:Z=',cpt_z
        ELSE
           WRITE (UNIT=cout,FMT=*) cpt_z%hght
           WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') 'cpt:Z='//ADJUSTL(cout)//' '//TRIM(cpt_z%unit)
        END IF
        lfirst=.false.
     END IF
  END IF
  IF (PRESENT(cpt_t)) THEN
     IF (.NOT.lfirst) WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') ', '
     cout=get_cdate(cpt_t,1)
     WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') 'cpt:T='//TRIM(cout)
     lfirst=.false.
  END IF
  IF (PRESENT(cpt_s)) THEN
     IF (.NOT.(cpt_s==0)) THEN
        IF (.NOT.lfirst) WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') ', '
        cout=get_cdate(cpt_s,1)
        WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') 'cpt:S='//TRIM(cout)
        lfirst=.false.
     END IF
  END IF
  IF (PRESENT(cpt_nrow)) THEN
     IF (.NOT.lfirst) WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') ', '
     WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(cpt_nrow),')'
     WRITE (UNIT=iout,FMT=cfmt,ERR=1,ADVANCE='no') 'cpt:nrow=',cpt_nrow
     lfirst=.false.
  END IF
  IF (PRESENT(cpt_ncol)) THEN
     IF (.NOT.lfirst) WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') ', '
     WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(cpt_ncol),')'
     WRITE (UNIT=iout,FMT=cfmt,ERR=1,ADVANCE='no') 'cpt:ncol=',cpt_ncol
     lfirst=.false.
  END IF
  IF (PRESENT(cpt_row)) THEN
     IF (.NOT.lfirst) WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') ', '
     WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') 'cpt:row='//cpt_row
     lfirst=.false.
  END IF
  IF (PRESENT(cpt_col)) THEN
     IF (.NOT.lfirst) WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') ', '
     WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') 'cpt:col='//cpt_col
     lfirst=.false.
  END IF
  IF (PRESENT(cpt_units)) THEN
     IF (.NOT.lfirst) WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') ', '
     WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') 'cpt:units='//cpt_units
     lfirst=.false.
  END IF
  IF (PRESENT(cpt_missing)) THEN
     IF (.NOT.lfirst) WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') ', '
     WRITE (UNIT=cout,FMT=*) cpt_missing
     WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='no') 'cpt:missing='//ADJUSTL(cout)
     lfirst=.false.
  END IF
  WRITE (UNIT=iout,FMT='(A)',ERR=1,ADVANCE='yes') ' '
!
  ifail=0
  RETURN
!
1 ifail=1
  RETURN
 END SUBROUTINE write_tag
END MODULE data_output
