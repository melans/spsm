! $Id: verif.f90 1224 2011-03-03 16:28:24Z simon $
MODULE verif
!
! Modules
  USE numbers, ONLY: rp
!
! Implicit declarations
  IMPLICIT NONE
!
CONTAINS
!
!
 FUNCTION attrib_diagram()
!
! Modules
  USE arrays,        ONLY: irobs,rfps,ifq,afp,orf
  USE data_output,   ONLY: save_rel
  USE CPT_constants, ONLY: nb,ng
  USE errors,        ONLY: error
  USE gui,           ONLY: yesno
  USE labels,        ONLY: cg_cat_l
  USE settings,      ONLY: nur,mya
  USE statistics,    ONLY: calc_rel
!
! Function type
  INTEGER :: attrib_diagram
!
! Locals
!
! Local scalars
  INTEGER :: i,j   ! - category indices -
  INTEGER :: ifail ! - error indicator -
!
  REAL(KIND=rp) :: tfq ! - total frequency -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SUM
!
! Executable Statements
!
! Initialise
  ifail=init_attrib()
  IF (ifail/=0) GOTO 1
!
! Calculate attributes statistics
  CALL calc_rel (nur,mya,ng,nb,irobs,rfps,ifq,afp,orf)
!
! Print results
  DO i=0,ng
     WRITE (UNIT=*,FMT='(2A)') 'Category: ',cg_cat_l(i)
     WRITE (UNIT=*,FMT='(4A18)') 'Forecast prob. (%)','Obs. rel. freq.','Frequency','Rel. freq. (%)'
     tfq=REAL(SUM(ifq(:,i)),KIND=rp)
     DO j=1,nb
        WRITE (UNIT=*,FMT='(2F18.3,I18,F18.3)') afp(j,i),orf(j,i),ifq(j,i),REAL(100*ifq(j,i),KIND=rp)/tfq
     END DO
  END DO
!
! Prompt to save results?
  WRITE (UNIT=*,FMT=*) 'Do you want to save the results (Y/N)?'
  IF (yesno()==1) ifail=save_rel()
!
! Errors
1 IF (ifail/=0) CALL error ('attrib_diagram',ifail)
!
! Close
  attrib_diagram=close_attrib()
!
  RETURN
!
 CONTAINS
!
!
  FUNCTION init_attrib()
!
! Modules
  USE arrays,        ONLY: ifq,afp,orf
  USE CPT_constants, ONLY: nb,ng
!
! Function type
  INTEGER :: init_attrib
!
! Locals
!
! Local scalars
  INTEGER :: istat ! - memory allocation status -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ALLOCATED
!
! Executable Statements
!
! Initialise data space
  init_attrib=1
! - frequencies -
  IF (.NOT.ALLOCATED(ifq)) THEN
     ALLOCATE (ifq(nb,0:ng),STAT=istat)
     IF (istat/=0) RETURN
  END IF
! - average binned forecast probabilities -
  IF (.NOT.ALLOCATED(afp)) THEN
     ALLOCATE (afp(nb,0:ng),STAT=istat)
     IF (istat/=0) RETURN
  END IF
! - observed relative frequencies -
  IF (.NOT.ALLOCATED(orf)) THEN
     ALLOCATE (orf(nb,0:ng),STAT=istat)
     IF (istat/=0) RETURN
  END IF
!
  init_attrib=0
!
  RETURN
  END FUNCTION init_attrib
!
!
!
  FUNCTION close_attrib()
!
! Modules
  USE arrays, ONLY: ifq,afp,orf
!
! Function type
  INTEGER :: close_attrib
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ALLOCATED
!
! Executable Statements
!
! Free memory
  IF (ALLOCATED(orf)) DEALLOCATE (orf)
  IF (ALLOCATED(afp)) DEALLOCATE (afp)
  IF (ALLOCATED(ifq)) DEALLOCATE (ifq)
  close_attrib=0
!
  RETURN
  END FUNCTION close_attrib
 END FUNCTION attrib_diagram
!
!
!
 FUNCTION prob_scores()
!
! Modules
  USE arrays,   ONLY: irobs,pobs,rfps,pscores
  USE errors,   ONLY: error
  USE settings, ONLY: nur,mya
!
! Function type
  INTEGER :: prob_scores
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
!
! Executable Statements
!
! Calculate probabilistic scores
  ifail=init_pscores ()
  IF (ifail/=0) GOTO 1
  CALL calc_pscores (nur,mya,irobs,pobs,rfps,pscores)
!
! Print tables
! - print frequency table -
  CALL write_pscores ()
  ifail=0
  prob_scores=0
  RETURN 
!
! Errors
1 SELECT CASE (ifail)
   CASE (0)
     prob_scores=1
   CASE (1)
     prob_scores=close_pscores()
     CALL error ('prob_scores',ifail)
  END SELECT
!
  RETURN
!
 CONTAINS
!
!
  FUNCTION init_pscores()
!
! Modules
  USE arrays,        ONLY: pscores
  USE CPT_constants, ONLY: nps
!
! Function type
  INTEGER :: init_pscores
!
! Locals
!
! Local scalars
  INTEGER :: istat ! - memory allocation status -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ALLOCATED
!
! Executable Statements
!
! Initialise data space
  init_pscores=1
! - frequencies -
  IF (.NOT.ALLOCATED(pscores)) THEN
     ALLOCATE (pscores(nps),STAT=istat)
     IF (istat/=0) RETURN
  END IF
!
  init_pscores=0
!
  RETURN
  END FUNCTION init_pscores
 END FUNCTION prob_scores
!
!
!
 SUBROUTINE calc_pscores (n,m,iobs,cps,fps,pscores)
!
! Modules
  USE analysis,   ONLY: prog,dprog
  USE numbers,    ONLY: zero,one
  USE statistics, ONLY: ng,&
                        effective_interest,hbrier,linear_prob,&
                        ranked_prob_score,two_afc_mp,two_afc_2p
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
  INTEGER, INTENT(IN) :: m ! - number of variables -
!
! Input arrays
  INTEGER, INTENT(IN) :: iobs(:,:) ! - observed categories -
!
  REAL(KIND=rp), INTENT(IN) :: fps(:,:,:) ! - forecast probabilities -
  REAL(KIND=rp), INTENT(IN) :: cps(:,:)   ! - climatological probabilities -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: pscores(:) ! - scores -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - score index -
!
! Executable Statements
!
! Calculate progress increment
  prog=zero
  dprog=one/REAL(6,KIND=rp)
!
! Calculate scores
! - Brier scores -
  CALL hbrier (n,m,ng,iobs(:,:),fps(:,:,:),cps(:,:),pscores(1:3*ng-2:3),pscores(2:3*ng-1:3))
  prog=prog+dprog
! - ROC areas -
  CALL two_afc_2p (n,m,ng,iobs(:,:),fps(:,:,:),pscores(3:3*ng:3))
  prog=prog+dprog
! - RPS -
  i=3*ng
  CALL ranked_prob_score (n,m,ng,iobs(:,:),fps(:,:,:),cps(:,:),pscores(i+1),pscores(i+2))
  prog=prog+dprog
! - effective interest rate -
  i=i+3
  pscores(i)=effective_interest(n,m,iobs(:,:),fps(:,:,:),cps(:,:))
  prog=prog+dprog
! - 2AFC -
  i=i+1
  pscores(i)=two_afc_mp(n,m,ng,iobs(:,:),fps(:,:,:))
  prog=prog+dprog
! - linear probability score -
  i=i+1
  pscores(i)=linear_prob(n,m,iobs(:,:),fps(:,:,:))
  prog=one
!
  RETURN
 END SUBROUTINE calc_pscores
!
!
!
 SUBROUTINE write_pscores ()
!
! Modules
  USE arrays,        ONLY: pscores
  USE CPT_constants, ONLY: nps
  USE labels,        ONLY: cg_pvscores_t
!
! Locals
!
! Local scalars
  INTEGER :: i ! - score index -
!
! Executable Statements
!
! Print scores
  DO i=1,nps
     WRITE (UNIT=*,FMT='(A,F7.3)') cg_pvscores_t(i),pscores(i)
  END DO
!
  RETURN
 END SUBROUTINE write_pscores
!
!
!
 FUNCTION close_pscores()
!
! Modules
  USE arrays, ONLY: pscores
!
! Function type
  INTEGER :: close_pscores
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ALLOCATED
!
! Executable Statements
!
! Free memory
  IF (ALLOCATED(pscores)) DEALLOCATE (pscores)
  close_pscores=0
!
  RETURN
 END FUNCTION close_pscores
!
!
!
 FUNCTION roc_diagram()
!
! Modules
  USE arrays,        ONLY: irobs,rfps,hits,fars
  USE data_output,   ONLY: save_roc
  USE CPT_constants, ONLY: nb,ng
  USE errors,        ONLY: error
  USE gui,           ONLY: yesno
  USE labels,        ONLY: cg_cat_a
  USE settings,      ONLY: nur,mya
  USE statistics,    ONLY: rocas, &
                           rocp
!
! Function type
  INTEGER :: roc_diagram
!
! Locals
!
! Local scalars
  INTEGER :: i     ! - bin index -
  INTEGER :: j     ! - category index -
  INTEGER :: n     ! - total number of forecasts -
  INTEGER :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC RESHAPE
!
! Executable Statements
!
! Initialise
  ifail=init_roc()
  IF (ifail/=0) GOTO 1
!
! Calculate ROC statistics
  n=nur*mya
  CALL rocp (n,ng,nb,RESHAPE(irobs,(/n/)),RESHAPE(rfps,(/n,ng/)),rocas,hits,fars)
! 
! Print ROC
  DO j=1,ng
     WRITE (UNIT=*,FMT='(3A,F10.3)') ' ROC area (',cg_cat_a(j),'):',rocas(j)
     WRITE (UNIT=*,FMT=*)
     WRITE (UNIT=*,FMT='(6A)') '           Hit rates','   False-alarm rates'
     DO i=1,nb
        WRITE (UNIT=*,FMT='(2F20.3)') hits(i,j),fars(i,j)
     END DO
     WRITE (UNIT=*,FMT=*)
  END DO
!
! Prompt to save results
  WRITE (UNIT=*,FMT=*) 'Do you want to save the results (Y/N)?'
  IF (yesno()==1) ifail=save_roc()
!
! Errors
1 SELECT CASE (ifail)
   CASE (0)
     roc_diagram=1
   CASE (1)
     roc_diagram=close_roc()
     CALL error ('roc_diagram',ifail)
  END SELECT
!
  RETURN
!
 CONTAINS
!
!
  FUNCTION init_roc()
!
! Modules
  USE arrays,        ONLY: hits,fars
  USE CPT_constants, ONLY: nb,ng
!
! Function type
  INTEGER :: init_roc
!
! Locals
!
! Local scalars
  INTEGER :: istat ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ALLOCATED
!
! Executable Statements
!
! Allocate workspace
  init_roc=1
! - hit rates -
  IF (.NOT.ALLOCATED(hits)) THEN
     ALLOCATE (hits(nb,ng),STAT=istat)
     IF (istat/=0) RETURN
  END IF
! - false alarm rates -
  IF (.NOT.ALLOCATED(fars)) THEN
     ALLOCATE (fars(nb,ng),STAT=istat)
     IF (istat/=0) RETURN
  END IF
!
  init_roc=0
!
  RETURN
  END FUNCTION init_roc
 END FUNCTION roc_diagram
!
!
!
 FUNCTION close_roc()
!
! Modules
  USE arrays, ONLY: hits,fars
!
! Function type
  INTEGER :: close_roc
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ALLOCATED
!
! Executable Statements
!
! Free memory
  IF (ALLOCATED(fars)) DEALLOCATE (fars)
  IF (ALLOCATED(hits)) DEALLOCATE (hits)
  close_roc=0
!
  RETURN
 END FUNCTION close_roc
!
!
!
 FUNCTION wrlt_diagram()
!
! Modules
  USE arrays,        ONLY: irobs,rfps,pobs,cump,eir
  USE data_output,   ONLY: save_wrlt
  USE errors,        ONLY: error
  USE gui,           ONLY: yesno
  USE IO_constants,  ONLY: lprd
  USE iofiles,       ONLY: yfile
  USE labels,        ONLY: cg_wrlts_t
  USE settings,      ONLY: nur,mya
  USE statistics,    ONLY: profits
  USE time
!
! Function type
  INTEGER :: wrlt_diagram
!
! Locals
!
! Local scalars
  INTEGER :: k     ! - case index -
  INTEGER :: ifail ! - error indicator -
!
  CHARACTER(LEN=lprd) :: cout ! - output field -
!
! Executable Statements
!
! Initialise
  ifail=init_wrlt()
  IF (ifail/=0) GOTO 1
!
! Calculate cumulative profits
  CALL profits (nur,mya,irobs,rfps,pobs,cump(0:,1),eir(0:,1))
! 
! Print results
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') 'Weather Roulette'
  WRITE (UNIT=*,FMT='(3A)') 'Date     ',cg_wrlts_t(1),cg_wrlts_t(2)
  DO k=0,nur
     cout=get_cdate(yfile%period1+(yfile%it1+k-2),2)
     WRITE (UNIT=*,FMT='(A,2F20.2)') TRIM(cout),cump(k,1),eir(k,1)
  END DO
  WRITE (UNIT=*,FMT=*)
!
! Prompt to save results
  WRITE (UNIT=*,FMT=*) 'Do you want to save the results (Y/N)?'
  IF (yesno()==1) ifail=save_wrlt()
!
! Errors
1 SELECT CASE (ifail)
   CASE (0)
     wrlt_diagram=1
   CASE (1)
     wrlt_diagram=close_wrlt()
     CALL error ('wrlt_diagram',ifail)
  END SELECT
!
  RETURN
!
 CONTAINS
!
!
  FUNCTION init_wrlt()
!
! Modules
  USE arrays,        ONLY: cump,eir
  USE CPT_constants, ONLY: nwr
  USE settings,      ONLY: nur
!
! Function type
  INTEGER :: init_wrlt
!
! Locals
!
! Local scalars
  INTEGER :: istat ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ALLOCATED
!
! Executable Statements
!
! Allocate workspace
  init_wrlt=1
! - cumulative profits -
  IF (.NOT.ALLOCATED(cump)) THEN
     ALLOCATE (cump(0:nur,nwr),STAT=istat)
     IF (istat/=0) RETURN
  END IF
! - effective interest rates -
  IF (.NOT.ALLOCATED(eir)) THEN
     ALLOCATE (eir(0:nur,nwr),STAT=istat)
     IF (istat/=0) RETURN
  END IF
  init_wrlt=0
!
  RETURN
  END FUNCTION init_wrlt
 END FUNCTION wrlt_diagram
!
!
!
  FUNCTION close_wrlt()
!
! Modules
  USE arrays, ONLY: cump,eir
!
! Function type
  INTEGER :: close_wrlt
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ALLOCATED
!
! Executable Statements
!
! Free memory
  IF (ALLOCATED(eir))  DEALLOCATE (eir)
  IF (ALLOCATED(cump)) DEALLOCATE (cump)
  close_wrlt=0
!
  RETURN
  END FUNCTION close_wrlt
END MODULE verif
