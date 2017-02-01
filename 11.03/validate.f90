! $Id: validate.f90 1225 2011-03-03 16:29:15Z simon $
MODULE validate
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
 FUNCTION validate_cv()
!
! Function type
  INTEGER :: validate_cv
!
! Executable Statements
!
! Validate cross-validated forecasts
  validate_cv=validates('Cross-validated scores',validation_cv)
!
  RETURN
 END FUNCTION validate_cv
!
!
!
 FUNCTION validate_ra()
!
! Function type
  INTEGER :: validate_ra
!
! Executable Statements
!
! Validate retroactive forecasts
  validate_ra=validates('Retroactive scores',validation_ra)
!
  RETURN
 END FUNCTION validate_ra
!
!
!
 FUNCTION validates(title,validations)
!
! Modules
  USE CPT_constants, ONLY: nscore
  USE fields,        ONLY: yfield,iffy, &
                           check_ivf,update_grid
  USE iofiles,       ONLY: yfile
  USE labels,        ONLY: cg_scores
  USE settings,      ONLY: ivf,ivfa
  USE statistics,    ONLY: scores, &
                           init_scores
!
! Function type
  INTEGER :: validates
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: title ! - window title -
!
! Procedure arguments
  INTEGER, EXTERNAL :: validations ! - validation function -
!
! Locals
!
! Local functions
  INTEGER :: j ! - score index -
!
! Executable Statements
!
! Initialise memory
  IF (init_scores()/=0) RETURN
!
! Construct and add coordinate label for current point
  DO iffy=1,yfile%nfs*yfile%nls
     DO ivfa=1,yfield(iffy)%nva
        WRITE (UNIT=*,FMT=*)
        WRITE (UNIT=*,FMT='(A)') title
        j=check_ivf(iffy,ivfa)
        CALL update_grid (ivf,iffy,yfile%igrid)
!
! Calculate validation statistics
        validates=validations()
!
! Print validation statistics
        DO j=1,nscore
           IF (j==1) THEN
              WRITE (UNIT=*,FMT=*)
              WRITE (UNIT=*,FMT='(A)') 'Continuous measures:'
           ELSE IF (j==nscore/2+1) THEN
              WRITE (UNIT=*,FMT=*)
              WRITE (UNIT=*,FMT='(A)') 'Categorical measures:'
           END IF
           SELECT CASE (j)
            CASE (1,2,8,15,16)
              WRITE (UNIT=*,FMT='(A,T40,F10.4)') cg_scores(j),scores(j)
            CASE (3,9:14)
              WRITE (UNIT=*,FMT='(A,T40,F10.2,A)') cg_scores(j),scores(j),'%'
            CASE (4:7)
              WRITE (UNIT=*,FMT='(A,T40,F10.2)') cg_scores(j),scores(j)
           END SELECT
        END DO
     END DO
  END DO
!
  RETURN
 END FUNCTION validates
!
!
!
 FUNCTION validation_cv()
!
! Modules
  USE arrays,   ONLY: y,yhat,iobs,ifor,pobs
  USE settings, ONLY: iva,nu
!
! Function type
  INTEGER :: validation_cv
!
! Executable Statements
!
! Calculate validation statistics
  validation_cv=validation(nu,y(iva,:),yhat(iva,:),iobs(iva,:),ifor(iva,:),pobs(iva,:))
!
  RETURN
 END FUNCTION validation_cv
!
!
!
 FUNCTION validation_ra()
!
! Modules
  USE arrays,   ONLY: y,yret,irobs,irfor,pobs
  USE settings, ONLY: iva,nu1,nur,nu
!
! Function type
  INTEGER :: validation_ra
!
! Executable Statements
!
! Calculate validation statistics
  validation_ra=validation(nur,y(iva,nu1+1:nu),yret(iva,:),irobs(iva,:),irfor(iva,:),pobs(iva,:))
!
  RETURN
 END FUNCTION validation_ra
!
!
!
 FUNCTION validation(n,y,yhat,iobs,ifor,p)
!
! Modules
  USE statistics, ONLY: scores, &
                        calc_scores
!
! Function type
  INTEGER :: validation
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
! Input arrays
  INTEGER, INTENT(IN) :: iobs(:) ! - observed categories -
  INTEGER, INTENT(IN) :: ifor(:) ! - forecast categories -
!
  REAL(KIND=rp), INTENT(IN) :: y(:)    ! - observed values -
  REAL(KIND=rp), INTENT(IN) :: yhat(:) ! - forecast values -
  REAL(KIND=rp), INTENT(IN) :: p(:)    ! - percentiles -
!
! Executable Statements
!
! Caclulate scores
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') 'Calculating validation statistics ...'
  CALL calc_scores (n,yhat,y,ifor,iobs,p,scores)
!
  WRITE (UNIT=*,FMT='(A)') 'Done!'
  validation=0
!
  RETURN
 END FUNCTION validation
! 
!
!
 FUNCTION roc_cv()
!
! Function type
  INTEGER :: roc_cv
!
! Executable Statements
!
! Cross-validated ROC
  roc_cv=print_roc('Cross-validated ROC',get_roc_cv)
!
  RETURN
 END FUNCTION roc_cv
!
!
!
 FUNCTION roc_ra()
!
! Function type
  INTEGER :: roc_ra
!
! Executable Statements
!
! Retroactive ROC
  roc_ra=print_roc('Retroactive ROC',get_roc_ra)
!
  RETURN
 END FUNCTION roc_ra
!
!
!
 FUNCTION print_roc(title,get_roc)
!
! Modules
  USE fields,   ONLY: yfield,iffy, &
                      check_ivf,update_grid
  USE iofiles,  ONLY: yfile
  USE settings, ONLY: ivf,ivfa
!
! Function type
  INTEGER :: print_roc
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: title ! - window title -
!
! Procedure arguments
  INTEGER, EXTERNAL :: get_roc ! - ROC function -
!
! Executable Statements
!
! Construct and add coordinate label for current point
  DO iffy=1,yfile%nfs*yfile%nls
     DO ivfa=1,yfield(iffy)%nva
        WRITE (UNIT=*,FMT=*)
        WRITE (UNIT=*,FMT='(A)') title
        print_roc=check_ivf(iffy,ivfa)
        CALL update_grid (ivf,iffy,yfile%igrid)
!
! Calculate ROC
        print_roc=get_roc()
     END DO
  END DO
!
  RETURN
 END FUNCTION print_roc
!
!
!
 FUNCTION get_roc_cv()
!
! Modules
  USE arrays,   ONLY: yhat,iobs
  USE settings, ONLY: iva,nu
!
! Function type
  INTEGER :: get_roc_cv
!
! Executable Statements
!
! Calculate ROC
  CALL get_roc(nu,iobs(iva,:),yhat(iva,:))
  get_roc_cv=0
!
  RETURN
 END FUNCTION get_roc_cv
!
!
!
 FUNCTION get_roc_ra()
!
! Modules
  USE arrays,   ONLY: yret,irobs
  USE settings, ONLY: iva,nur
!
! Function type
  INTEGER :: get_roc_ra
!
! Executable Statements
!
! Calculate ROC
  CALL get_roc(nur,irobs(iva,:),yret(iva,:))
  get_roc_ra=0
!
  RETURN
 END FUNCTION get_roc_ra
! 
!
!
 SUBROUTINE get_roc (n,iobs,yhat)
!
! Modules 
  USE arrays,        ONLY: rnkf,hit,far, &
                           rank_data
  USE CPT_constants, ONLY: ng
  USE errors,        ONLY: error
  USE statistics,    ONLY: roca, &
                           roc
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
! Input arrays
  INTEGER, INTENT(IN) :: iobs(:) ! - observed categories -
!
  REAL(KIND=rp), INTENT(IN) :: yhat(:) ! - forecasts -
!
! Locals
!
! Local scalars
  INTEGER :: k     ! - case index -
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
! - forecast ranks -
  IF (.NOT.ALLOCATED(rnkf)) THEN
     ALLOCATE (rnkf(n),STAT=istat)
     IF (istat/=0) GOTO 1
  END IF
! - hit rates -
  IF (.NOT.ALLOCATED(hit)) THEN
     ALLOCATE (hit(n,2),STAT=istat)
     IF (istat/=0) GOTO 1
  END IF
! - false alarm rates -
  IF (.NOT.ALLOCATED(far)) THEN
     ALLOCATE (far(n,2),STAT=istat)
     IF (istat/=0) GOTO 1
  END IF
! 
! Calculate ROC 
  CALL rank_data (yhat(:),n,'a',rnkf)
  CALL roc (n,1,iobs(:),rnkf(:),roca(1),hit(1:n,1),far(1:n,1))
  CALL roc (n,ng,iobs(:),rnkf(:),roca(2),hit(1:n,2),far(1:n,2))
!
! Print ROC
  WRITE (UNIT=*,FMT='(A,F10.3)') ' ROC area (B):',roca(1)
  WRITE (UNIT=*,FMT='(A,F10.3)') ' ROC area (A):',roca(2)
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(4A)') '            Hit rates (B)','    False-alarm rates (B)', &
                            '            Hit rates (A)','    False-alarm rates (A)'
  DO k=1,n
     WRITE (UNIT=*,FMT='(4F25.3)') hit(k,1),far(k,1),hit(k,2),far(k,2)
  END DO
  WRITE (UNIT=*,FMT=*)
!
  RETURN
!
! Errors
1 CALL error ('get_roc',istat)
  IF (ALLOCATED(far))  DEALLOCATE (far)
  IF (ALLOCATED(hit))  DEALLOCATE (hit)
  IF (ALLOCATED(rnkf)) DEALLOCATE (rnkf)
!
  RETURN
 END SUBROUTINE get_roc
END MODULE validate
