! $Id: gui.f90 1245 2011-03-04 21:14:47Z simon $
MODULE gui
!
! Modules
  USE gui_constants, ONLY: mcol
  USE version,       ONLY: lver
!
! Implicit declarations
  IMPLICIT NONE
!
! Scalars
!
! Integer scalars
  INTEGER, PRIVATE :: ichara ! - 'a' in collating sequence -
  INTEGER, PRIVATE :: icharz ! - 'z' in collating sequence -
  INTEGER, PRIVATE :: ichard ! - difference between 'A' and 'a' in collating sequence -
  INTEGER, PRIVATE :: igcmo  ! - GCM options grey menu flag -
!
  INTEGER, PUBLIC :: iscree  ! - scree plot grey menu flag -
  INTEGER, PUBLIC :: jcca    ! - CCA grey menu flag -
  INTEGER, PUBLIC :: jpcr    ! - PCR grey menu flag -
  INTEGER, PUBLIC :: jmlr    ! - MLR grey menu flag -
  INTEGER, PUBLIC :: jgcm    ! - GCM grey menu flag -
  INTEGER, PUBLIC :: jgauss  ! - transform Y data grey menu flag -
  INTEGER, PUBLIC :: irv     ! - retroactive verification grey menu flag -
! - graphics related scalars -
  INTEGER, PUBLIC :: igsize  ! - graphics area dimension -
  INTEGER, PUBLIC :: igsz    ! - graphics/map dimension -
  INTEGER, PUBLIC :: ncol=18 ! - number of shading colours -
!
! Character scalars
  CHARACTER(LEN= 25), PUBLIC :: cfont   ! - font -
  CHARACTER(LEN=128), PUBLIC :: cwtitle ! - CPT window title -
!
! Arrays
!
! Integer arrays
  INTEGER, PUBLIC :: icol(0:mcol+20) ! - colours -
!
!
CONTAINS
!
!
 SUBROUTINE print_error (msg1,msg2,msg3,nopause)
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: msg1 ! - error message -
! - optional input scalars -
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: msg2 ! - error message -
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: msg3 ! - error message -
!
  LOGICAL, INTENT(IN), OPTIONAL :: nopause ! no pause flag -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC PRESENT
!
! Executable Statements
!
! Print error message
  WRITE (UNIT=*,FMT='(2A)') 'ERROR: ',msg1
  IF (PRESENT(msg2)) WRITE (UNIT=*,FMT='(2A)') '       ',msg2
  IF (PRESENT(msg3)) WRITE (UNIT=*,FMT='(2A)') '       ',msg3
  IF (PRESENT(nopause)) RETURN 
  READ (UNIT=*,FMT=*)
!
  RETURN
 END SUBROUTINE print_error
!
!
!
 SUBROUTINE print_warning (msg1,msg2,nopause)
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: msg1 ! - error message -
! - optional input scalars -
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: msg2 ! - error message -
!
  LOGICAL, INTENT(IN), OPTIONAL :: nopause ! no pause flag -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC PRESENT
!
! Executable Statements
!
! Print warning message
  WRITE (UNIT=*,FMT='(2A)') 'WARNING: ',msg1
  IF (PRESENT(msg2)) WRITE (UNIT=*,FMT='(2A)') '         ',msg2
  IF (PRESENT(nopause)) RETURN 
  READ (UNIT=*,FMT=*)
!
  RETURN
 END SUBROUTINE print_warning
!
!
!
 SUBROUTINE print_advisory (msg1,msg2,nopause)
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: msg1 ! - advisory message -
! - optional input scalars -
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: msg2 ! - error message -
!
  LOGICAL, INTENT(IN), OPTIONAL :: nopause ! no pause flag -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC PRESENT
!
! Executable Statements
!
! Print advisory message
  WRITE (UNIT=*,FMT='(2A)') 'ADVISORY: ',msg1
  IF (PRESENT(msg2)) WRITE (UNIT=*,FMT='(2A)') '         ',msg2
  IF (PRESENT(nopause)) RETURN 
  READ (UNIT=*,FMT=*)
!
  RETURN
 END SUBROUTINE print_advisory
!
!
!
 SUBROUTINE print_write_error (msg,ffile,nopause)
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: msg   ! - error message -
  CHARACTER(LEN=*), INTENT(IN) :: ffile ! - input file -
!
! - optional input scalars -
  LOGICAL, INTENT(IN), OPTIONAL :: nopause ! no pause flag -
!
! Executable Statements
!
! Print write-error message
  WRITE (UNIT=*,FMT='(2A)') 'ERROR: ',msg
  WRITE (UNIT=*,FMT='(A)') ffile
  IF (PRESENT(nopause)) RETURN 
  READ (UNIT=*,FMT=*)
!
  RETURN
 END SUBROUTINE print_write_error
!
!
!
 FUNCTION yesno()
!
! Returns 0=no or 1=yes
!
! Function type
  INTEGER :: yesno
!
! Locals
!
! Local scalars
  CHARACTER(LEN=1) :: yn ! - yes/no response -
!
! Executable Statements
!
! Prompt for yes/no response
1 READ (UNIT=*,FMT='(A)') yn
  SELECT CASE (yn)
   CASE ('N','n')
     yesno=0
   CASE ('Y','y')
     yesno=1
   CASE DEFAULT
     GOTO 1
  END SELECT
!
  RETURN
 END FUNCTION yesno
!
!
!
 SUBROUTINE init_chars ()
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC IACHAR
!
! Executable Statements
!
! Identify character positions
  ichara=IACHAR('a')
  icharz=IACHAR('z')
  ichard=IACHAR('A')-ichara
!
 END SUBROUTINE init_chars
!
!
!
 SUBROUTINE upcase (c)
!
! Arguments
!
! Input/output scalars
  CHARACTER(LEN=*), INTENT(INOUT) :: c ! - character -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ACHAR
  INTRINSIC IACHAR
  INTRINSIC LEN_TRIM
!
! Exectuable Statements
!
! Convert to uppercase
  DO i=1,LEN_TRIM(c)
     IF ((IACHAR(c(i:i))>=ichara).AND.(IACHAR(c(i:i))<=icharz)) c(i:i)=ACHAR(IACHAR(c(i:i))+ichard)
  END DO
!
  RETURN
 END SUBROUTINE upcase
!
!
!
 SUBROUTINE set_greyflags (ianal,ixgrid,iygrid)
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: ianal  ! - analysis flag -
  INTEGER, INTENT(IN) :: ixgrid ! - X file structure -
  INTEGER, INTENT(IN) :: iygrid ! - Y file structure -
!
! Executable Statements
!
! Set grey menu items
  SELECT CASE (ianal)
   CASE (1) ! - CCA -
     iscree=1
     igcmo=0
     jcca=0
     jpcr=1
     jgcm=1
     jgauss=1
   CASE (2) ! - PCR -
     iscree=1
     igcmo=0
     jcca=1
     jpcr=0
     jgcm=1
     jgauss=1
   CASE (3) ! - MLR -
     iscree=0
     igcmo=0
     jcca=1
     jpcr=1
     jgcm=1
     jgauss=1
   CASE (4) ! - GCM -
     iscree=0
     igcmo=1
     jcca=1
     jpcr=1
     jgcm=0
     jgauss=0
  END SELECT
!
! Adjust view flags if input files are open
! - MLR -
  IF (ianal==3) THEN
     jmlr=0
  ELSE
     SELECT CASE (ixgrid)
      CASE (0,3)
        jmlr=1
      CASE DEFAULT
        jmlr=0
     END SELECT
  END IF
! - GCM -
  IF (ianal==4) THEN
     jgcm=0
  ELSE
     IF (iygrid==3) THEN
        jgcm=0
     ELSE
        SELECT CASE (ixgrid)
         CASE (0,1)
           jgcm=1
         CASE DEFAULT
           jgcm=0
        END SELECT
     END IF
  END IF
!
  RETURN
 END SUBROUTINE set_greyflags
END MODULE gui
