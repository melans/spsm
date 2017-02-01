! $Id: maps.f90 1224 2011-03-03 16:28:24Z simon $
MODULE maps
!
! Implicit declarations
  IMPLICIT NONE
!
CONTAINS
!
!
 FUNCTION init_skill()
!
! Modules
  USE arrays,     ONLY: skills,pvalues
  USE errors,     ONLY: error
  USE settings,   ONLY: mya,ipval
  USE statistics, ONLY: init_scores
!
! Function type
  INTEGER :: init_skill
!
! Locals
!
! Local scalars
  INTEGER :: istat ! - error flag -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ALLOCATED
!
! Executable Statements
!
! Allocate workspace
  IF (init_scores()/=0) GOTO 1
!
! Allocate additional workspace
! - skill scores -
  IF (.NOT.ALLOCATED(skills)) THEN
     ALLOCATE (skills(mya),STAT=istat)
     IF (istat/=0) GOTO 1
  END IF
  IF ((.NOT.ALLOCATED(pvalues)).AND.(ipval==1)) THEN
     ALLOCATE (pvalues(mya),STAT=istat)
     IF (istat/=0) GOTO 1
  END IF
!
  init_skill=0
  RETURN
!
! Errors
1 init_skill=1
  CALL error ('init_skill',init_skill)
  IF (ALLOCATED(pvalues)) DEALLOCATE (pvalues)
  IF (ALLOCATED(skills)) DEALLOCATE (skills)
!
  RETURN
 END FUNCTION init_skill
!
!
!
 FUNCTION skill_maps_cv()
!
! Function type
  INTEGER :: skill_maps_cv
!
! Executable Statements
!
! Set cursor
  skill_maps_cv=skill_maps('Cross-validated results',calc_skill_cv)
!
  RETURN
 END FUNCTION skill_maps_cv
!
!
!
 FUNCTION skill_maps_ra()
!
! Function type
  INTEGER :: skill_maps_ra
!
! Executable Statements
!
! Set cursor
  skill_maps_ra=skill_maps('Retroactive results',calc_skill_ra)
!
  RETURN
 END FUNCTION skill_maps_ra
!
!
!
 FUNCTION skill_maps(title,calc_skill)
!
! Modules
  USE data_output, ONLY: save_skill
  USE labels,      ONLY: cg_skill_t
  USE statistics,  ONLY: nskill,iskill,iskills
!
! Function type
  INTEGER :: skill_maps
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: title ! - window title -
!
! Procedure arguments
  INTEGER, EXTERNAL :: calc_skill
!
! Executable Statements
!
! Allocate memory
  IF (init_skill()/=0) RETURN
!
! Prompt for score
  iskills(:)=0
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') title
  WRITE (UNIT=*,FMT=*)
1 WRITE (UNIT=*,FMT='(A)') 'Skill scores:'
  DO iskill=1,nskill
     WRITE (UNIT=*,FMT='(I2,2A)') iskill,'. ',cg_skill_t(iskill)
  END DO
  READ (UNIT=*,FMT=*,ERR=1) iskill
  iskills(iskill)=1
!
! Plot skill map
  skill_maps=calc_skill()
  skill_maps=save_skill()
!
  RETURN
 END FUNCTION skill_maps
!
!
!
 FUNCTION calc_skill_cv()
!
! Modules
  USE arrays,     ONLY: y,yhat,iobs,ifor,pobs,skills,pvalues
  USE bootstrap,  ONLY: get_pvalues
  USE labels,     ONLY: cg_skill_t, &
                        cg_done
  USE settings,   ONLY: nu,mya,ipval
  USE statistics, ONLY: nskill,iskills,iskill, &
                        get_skills
!
! Function type
  INTEGER :: calc_skill_cv
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC TRIM
!
! Executable Statements
!
! Identify metric
  DO iskill=1,nskill
     IF (iskills(iskill)==1) EXIT
  END DO
!
! Calculate new skill scores
  WRITE (UNIT=*,FMT='(3A)') 'Calculating ',TRIM(cg_skill_t(iskill)),' ...'
  CALL get_skills (iskill,nu,mya,yhat,y,ifor,iobs,pobs,skills)
  IF (ipval==1) CALL get_pvalues (iskill,nu,mya,yhat,y,ifor,iobs,pobs,skills,pvalues)
  WRITE (UNIT=*,FMT='(A)') TRIM(cg_done)//'!'  
!
  calc_skill_cv=0
!
  RETURN
 END FUNCTION calc_skill_cv
!
!
!
 FUNCTION calc_skill_ra()
!
! Modules
  USE arrays,     ONLY: y,yret,irobs,irfor,pobs,skills,pvalues
  USE bootstrap,  ONLY: get_pvalues
  USE labels,     ONLY: cg_done,cg_skill_t
  USE settings,   ONLY: nur,nu1,nu,mya,ipval
  USE statistics, ONLY: nskill,iskills,iskill, &
                        get_skills
!
! Function type
  INTEGER :: calc_skill_ra
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC TRIM
!
! Executable Statements
!
! Identify metric
  DO iskill=1,nskill
     IF (iskills(iskill)==1) EXIT
  END DO
!
! Calculate new skill scores
  WRITE (UNIT=*,FMT='(3A)') 'Calculating ',TRIM(cg_skill_t(iskill)),' ...'
  CALL get_skills (iskill,nur,mya,yret,y(1:mya,nu1+1:nu),irfor,irobs,pobs,skills)
  IF (ipval==1) CALL get_pvalues (iskill,nur,mya,yret,y(1:mya,nu1+1:nu),irfor,irobs,pobs,skills,pvalues)
  WRITE (UNIT=*,FMT='(A)') TRIM(cg_done)//'!'
!
  calc_skill_ra=0
!
  RETURN
 END FUNCTION calc_skill_ra
END MODULE maps
