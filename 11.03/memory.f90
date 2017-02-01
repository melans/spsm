! $Id: memory.f90 1215 2011-02-25 21:30:20Z simon $
MODULE memory
!
! Modules
  USE numbers, ONLY: rp
!
! Implicit declarations
  IMPLICIT NONE
!
!
 INTERFACE free_memory
   !free one dimensional REAL array (DOUBLE PRECISION)
   MODULE PROCEDURE free_real_array
   !free two dimensional REAL arrays (DOUBLE PRECISION)
   MODULE PROCEDURE free_real_arrays
   !free three dimensional REAL arrays (DOUBLE PRECISION)
   MODULE PROCEDURE free_real_arrayss
   !free one dimensional REAL array (SINGLE PRECISION)
   MODULE PROCEDURE free_reals_array
   !free one dimensional INTEGER array
   MODULE PROCEDURE free_int_array
   !free two dimensional INTEGER arrays
   MODULE PROCEDURE free_int_arrays
   !free one dimensional LOGICAL array
   MODULE PROCEDURE free_logic_array
 END INTERFACE free_memory
!
CONTAINS
!
!
 SUBROUTINE free_int_array( freeme )
!
! Input Array
  INTEGER, ALLOCATABLE, INTENT(INOUT) :: freeme(:)
!
! Executable Statements
!
! Free memory
  IF (ALLOCATED(freeme))     THEN
      IF(SIZE(freeme)>0) DEALLOCATE (freeme)
  END IF    
  RETURN
 END SUBROUTINE free_int_array
!
!
!
 SUBROUTINE free_int_arrays( freeme )
!
! Input/Output Array
  INTEGER, ALLOCATABLE, INTENT(INOUT) :: freeme(:,:)
!
! Executable Statements
!
! Free memory
  IF (ALLOCATED(freeme))     THEN
      IF(SIZE(freeme)>0) DEALLOCATE (freeme)
  END IF    
  RETURN
 END SUBROUTINE free_int_arrays
!
!
!
 SUBROUTINE free_real_array( freeme )
!
! Input Array
  REAL(KIND=rp), ALLOCATABLE, INTENT(INOUT) :: freeme(:)
!
! Executable Statements
!
! Free memory
  IF (ALLOCATED(freeme))     THEN
      IF(SIZE(freeme)>0) DEALLOCATE (freeme)
  END IF    
  RETURN
 END SUBROUTINE free_real_array
!
!
!
 SUBROUTINE free_reals_array( freeme )
!
! Input Array
  REAL, ALLOCATABLE, INTENT(INOUT) :: freeme(:)
!
! Executable Statements
!
! Free memory
  IF (ALLOCATED(freeme))     THEN
      IF(SIZE(freeme)>0) DEALLOCATE (freeme)
  END IF    
  RETURN
 END SUBROUTINE free_reals_array
!
!
!
 SUBROUTINE free_real_arrays( freeme )
!
! Input Array
  REAL(KIND=rp), ALLOCATABLE, INTENT(INOUT) :: freeme(:,:)
!
! Executable Statements
!
! Free memory
  IF (ALLOCATED(freeme))     THEN
      IF(SIZE(freeme)>0) DEALLOCATE (freeme)
  END IF    
  RETURN
 END SUBROUTINE free_real_arrays
!
!
!
 SUBROUTINE free_real_arrayss( freeme )
!
! Input Array
  REAL(KIND=rp), ALLOCATABLE, INTENT(INOUT) :: freeme(:,:,:)
!
! Executable Statements
!
! Free memory
  IF (ALLOCATED(freeme))     THEN
      IF(SIZE(freeme)>0) DEALLOCATE (freeme)
  END IF    
  RETURN
 END SUBROUTINE free_real_arrayss
!
!
!
 SUBROUTINE free_logic_array( freeme )
!
! Input Array
  LOGICAL, ALLOCATABLE, INTENT(INOUT) :: freeme(:)
!
! Executable Statements
!
! Free memory
  IF (ALLOCATED(freeme))     THEN
      IF(SIZE(freeme)>0) DEALLOCATE (freeme)
  END IF    
  RETURN
 END SUBROUTINE free_logic_array
!
END MODULE memory
