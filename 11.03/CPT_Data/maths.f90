MODULE maths
!
! Implicit declarations
  IMPLICIT NONE
!
! Explicit Interfaces
!
! Generic interfaces
  INTERFACE magnitude
   MODULE PROCEDURE magnitude_int
   MODULE PROCEDURE magnitude_sp
   MODULE PROCEDURE magnitude_dp
  END INTERFACE magnitude
!
CONTAINS
!
!
 SUBROUTINE init_numbers ()
!
! Modules
  USE numbers, ONLY: rp,one,two,pi,base,bignum,eps,ihuge,sfmax,sfmin,smlnum,sqrpi,sqrtwo,tol
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC EPSILON
  INTRINSIC HUGE
  INTRINSIC REAL
  INTRINSIC SQRT
  INTRINSIC TINY
!
! Executable Statements
!
! Determine machine constants
  sqrtwo=SQRT(two)              ! - square root of two -
  sqrpi=SQRT(pi)                ! - square root of pi -
  ihuge=HUGE(rp)                ! - huge number -
  base=REAL(RADIX(one),KIND=rp) ! - base -
  eps=EPSILON(one)              ! - machine precision -
  sfmin=TINY(one)               ! - safe minimum -
  sfmax=one/sfmin               ! - safe maximum -
  smlnum=SQRT(sfmin)/eps        ! - small number -
  bignum=one/smlnum             ! - big number -
  tol=SQRT(eps)                 ! - tolerance -
!
  RETURN
 END SUBROUTINE init_numbers
!
!
!
 FUNCTION magnitude_int(ix) RESULT (magnitude)
!
! Calculates order of magnitude of an integer value
!
! Function type
  INTEGER :: magnitude
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: ix
!
! Locals
!
! Local scalars
  INTEGER :: iax ! - absolute value of argument -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ABS
!
! Executable Statements
!
! Identify order of magnitude
  iax=ABS(ix)
  IF (iax>0) THEN
     magnitude=1
     DO
        IF (iax<10**magnitude) EXIT
        magnitude=magnitude+1
     END DO
  ELSE 
     magnitude=0
  END IF
!
  RETURN
 END FUNCTION magnitude_int
!
!
!
 FUNCTION magnitude_sp(x) RESULT (magnitude)
!
! Calculates order of magnitude of a single precision value
!
! Modules
  USE numbers, ONLY: sp,zero=>zero_sp,one=>one_sp,ten=>ten_sp
!
! Function type
  INTEGER :: magnitude
!
! Arguments
!
! Input scalars
  REAL(KIND=sp), INTENT(IN) :: x
!
! Locals
!
! Local scalars
  REAL(KIND=sp) :: ax ! - absolute value of argument -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ABS
!
! Executable Statements
!
! Identify order of magnitude
  ax=ABS(x)
  IF (ax<one) THEN
     IF (ax>zero) THEN
        magnitude=-1
        DO
           IF (ax>ten**magnitude) EXIT
           magnitude=magnitude-1
        END DO
     ELSE
        magnitude=0
     END IF
  ELSE
     magnitude=1
     DO
        IF (ax<ten**magnitude) EXIT
        magnitude=magnitude+1
     END DO
  END IF
!
  RETURN
 END FUNCTION magnitude_sp
!
!
!
 FUNCTION magnitude_dp(x) RESULT (magnitude)
!
! Calculates order of magnitude of a double precision value
!
! Modules
  USE numbers, ONLY: dp,zero=>zero_dp,one=>one_dp,ten=>ten_dp
!
! Function type
  INTEGER :: magnitude
!
! Arguments
!
! Input scalars
  REAL(KIND=dp), INTENT(IN) :: x
!
! Locals
!
! Local scalars
  REAL(KIND=dp) :: ax ! - absolute value of argument -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ABS
!
! Executable Statements
!
! Identify order of magnitude
  ax=ABS(x)
  IF (ax<one) THEN
     IF (ax>zero) THEN
        magnitude=-1
        DO
           IF (ax>ten**magnitude) EXIT
           magnitude=magnitude-1
        END DO
     ELSE
        magnitude=0
     END IF
  ELSE
     magnitude=1
     DO
        IF (ax<ten**magnitude) EXIT
        magnitude=magnitude+1
     END DO
  END IF
!
  RETURN
 END FUNCTION magnitude_dp
!
!
!
 FUNCTION iprec(r,mprec)
!
! Returns number of decimal places
!
! Modules
  USE numbers, ONLY: rp,ten
!
! Function type
  INTEGER :: iprec
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: mprec ! - maximum precision required -
!
  REAL(KIND=rp), INTENT(IN) :: r ! - value -
!
! Locals
!
! Local scalars
  INTEGER :: ip ! - current precision -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MOD
  INTRINSIC NINT
!
! Executable Statements
!
! Identify precision
  iprec=0
  DO ip=mprec,1,-1
     IF (MOD(NINT(r*ten**mprec),10**ip)==0) EXIT
     iprec=iprec+1
  END DO
!
  RETURN
 END FUNCTION iprec
!
!
!
 FUNCTION random_int(lower,upper)
!
! Generates a random number between the specified limits
!
! Modules
  USE numbers, ONLY: rp
!
! Function type
  INTEGER :: random_int
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: lower ! - lower integer limit -
  INTEGER, INTENT(IN) :: upper ! - upper integer limit -
!
! Locals
!
! Local scalars
  REAL(KIND=rp) :: r ! - random number -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC NINT
  INTRINSIC RANDOM_NUMBER
  INTRINSIC REAL
!
! Executable Statements
!
! Generate a random number
  CALL RANDOM_NUMBER (r)
!
! Convert to range
  random_int=lower+NINT((REAL(upper,KIND=rp)-REAL(lower,KIND=rp))*r)
!
  RETURN
 END FUNCTION random_int
!
!
!
 FUNCTION norm_rand(xbar,xvar)
!
! Returns a normally-distributed random number
!
! Modules
  USE numbers, ONLY: rp,two,pi
!
! Function type
  REAL(KIND=rp) :: norm_rand
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: xbar ! - mean -
  REAL(KIND=rp), INTENT(IN) :: xvar ! - variance -
!
! Locals
!
! Local arrays
  REAL(KIND=rp) :: r(2) ! - uniform random numbers -
!
! Functions and subroutines
!
! Intrinsic functions
  INTRINSIC LOG
  INTRINSIC RANDOM_NUMBER
  INTRINSIC SIN
  INTRINSIC SQRT
!
! Executable Statements
!
! Calculate mean absolute-error
  CALL RANDOM_NUMBER (r)
  norm_rand=SQRT(-two*LOG(r(1)))*SIN(two*pi*r(2))
  norm_rand=xbar+norm_rand*SQRT(xvar)
!
  RETURN
 END FUNCTION norm_rand
!
!
!
 FUNCTION get_cordn (i)
!
! Returns the appropriate suffix for an ordinal number
!
! Modules
  USE numbers, ONLY: cordns
!
! Function type
  CHARACTER(LEN=2) :: get_cordn
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: i ! - number -
!
! Locals
  INTEGER :: i100 ! - MOD(i,100) -
  INTEGER :: i10  ! - MOD(i,10) -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MOD
!
! Executable Statements
!
! Determine appropriate suffix
  i100=MOD(i,100)
  SELECT CASE (i100)
   CASE (0:3)
     get_cordn=cordns(i100)
   CASE (4:20)
     get_cordn=cordns(0)
   CASE DEFAULT
     i10=MOD(i100,10)
     SELECT CASE (i10)
      CASE (0:3)
        get_cordn=cordns(i100)
      CASE (4:9)
        get_cordn=cordns(0)
     END SELECT
  END SELECT
!
  RETURN
 END FUNCTION get_cordn
!
!
!
 FUNCTION force_odd(inew,iold)
!
! Forces value to be odd
!
! Function type
  INTEGER :: force_odd
!
! Arguments
!
! Input/output scalars
  INTEGER, INTENT(INOUT) :: inew ! - new value -
  INTEGER, INTENT(INOUT) :: iold ! - old value -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MOD
!
! Executable Statements
!
! Force lcw to be odd
  IF ((MOD(inew,2)==0).AND.(inew>0)) THEN
     IF (inew>iold) THEN
        inew=inew+1
     ELSE
        inew=inew-1
     END IF
  END IF
  iold=inew
  force_odd=2
!
  RETURN
 END FUNCTION force_odd
END MODULE maths
