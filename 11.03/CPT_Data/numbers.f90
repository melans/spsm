MODULE numbers
!
! Implicit declarations
  IMPLICIT NONE
!
! Parameters
!
! Data kind numbers
  INTEGER, PARAMETER, PUBLIC :: dp=KIND(1.0d0) ! - double precision -
  INTEGER, PARAMETER, PUBLIC :: sp=KIND(1.0e0) ! - single precision -
  INTEGER, PARAMETER, PUBLIC :: rp=dp          ! - default precision -
!
! Whole numbers
  REAL(KIND=rp), PARAMETER, PUBLIC :: zero=      0.0_rp ! - zero -
  REAL(KIND=rp), PARAMETER, PUBLIC :: one=       1.0_rp ! - one -
  REAL(KIND=rp), PARAMETER, PUBLIC :: two=       2.0_rp ! - two -
  REAL(KIND=rp), PARAMETER, PUBLIC :: three=     3.0_rp ! - three -
  REAL(KIND=rp), PARAMETER, PUBLIC :: four=      4.0_rp ! - four -
  REAL(KIND=rp), PARAMETER, PUBLIC :: five=      5.0_rp ! - five -
  REAL(KIND=rp), PARAMETER, PUBLIC :: six=       6.0_rp ! - six -
  REAL(KIND=rp), PARAMETER, PUBLIC :: seven=     7.0_rp ! - seven -
  REAL(KIND=rp), PARAMETER, PUBLIC :: eight=     8.0_rp ! - eight -
  REAL(KIND=rp), PARAMETER, PUBLIC :: nine=      9.0_rp ! - nine -
  REAL(KIND=rp), PARAMETER, PUBLIC :: ten=      10.0_rp ! - ten -
  REAL(KIND=rp), PARAMETER, PUBLIC :: eleven=   11.0_rp ! - eleven -
  REAL(KIND=rp), PARAMETER, PUBLIC :: twelve=   12.0_rp ! - twelve -
  REAL(KIND=rp), PARAMETER, PUBLIC :: sixten=   16.0_rp ! - sixteen -
  REAL(KIND=rp), PARAMETER, PUBLIC :: thirty=   30.0_rp ! - thirty -
  REAL(KIND=rp), PARAMETER, PUBLIC :: fifty=    50.0_rp ! - fifty -
  REAL(KIND=rp), PARAMETER, PUBLIC :: oneh=    100.0_rp ! - one hundred -
  REAL(KIND=rp), PARAMETER, PUBLIC :: onet=   1000.0_rp ! - one thousand -
  REAL(KIND=rp), PARAMETER, PUBLIC :: onem=1000000.0_rp ! - one million -
!
! - single precision - 
  REAL(KIND=sp), PARAMETER, PUBLIC :: zero_sp=0.0_sp ! - zero -
  REAL(KIND=sp), PARAMETER, PUBLIC :: one_sp= 1.0_sp ! - one -
  REAL(KIND=sp), PARAMETER, PUBLIC :: ten_sp=10.0_sp ! - ten -
! - double precision - 
  REAL(KIND=dp), PARAMETER, PUBLIC :: zero_dp=0.0_dp ! - zero -
  REAL(KIND=dp), PARAMETER, PUBLIC :: one_dp= 1.0_dp ! - one -
  REAL(KIND=dp), PARAMETER, PUBLIC :: ten_dp=10.0_dp ! - ten -
!
! Fractions
  REAL(KIND=rp), PARAMETER, PUBLIC :: onetth=0.001_rp ! - one thousandth -
  REAL(KIND=rp), PARAMETER, PUBLIC :: onehth= 0.01_rp ! - one hundredth -
  REAL(KIND=rp), PARAMETER, PUBLIC :: tenth=   0.1_rp ! - one tenth -
  REAL(KIND=rp), PARAMETER, PUBLIC :: third=one/three ! - one third -
  REAL(KIND=rp), PARAMETER, PUBLIC :: half=    0.5_rp ! - half -
!
! Angles
  REAL(KIND=rp), PARAMETER, PUBLIC :: r90=  90.0_rp ! - 90 degrees -
  REAL(KIND=rp), PARAMETER, PUBLIC :: r180=180.0_rp ! - 180 degrees -
  REAL(KIND=rp), PARAMETER, PUBLIC :: r270=270.0_rp ! - 270 degrees -
  REAL(KIND=rp), PARAMETER, PUBLIC :: r360=360.0_rp ! - 360 degrees -
!
! Irrational numbers
  REAL(KIND=rp), PARAMETER, PUBLIC :: pi=3.141592653589793_rp ! - pi -
  REAL(KIND=rp), PARAMETER, PUBLIC :: hpi=pi/two              ! - half pi -
!
! Ordinal numbers
  CHARACTER(LEN=2), PARAMETER, PUBLIC :: cordns(0:3)=& ! - ordinal number suffixes -
     (/'th','st','nd','rd'/)
!
! Geographical parameters
  REAL(KIND=rp), PARAMETER, PUBLIC :: re=6372795.0_rp ! - radius of Earth -
!
! Temperature parameters
  REAL(KIND=rp), PARAMETER, PUBLIC :: zero_K=273.15_rp ! - zero degrees in Kelvin -
  REAL(KIND=rp), PARAMETER, PUBLIC :: zero_F= 32.00_rp ! - zero degrees in Farenheit -
!
! Time parameters
  REAL(KIND=rp), PARAMETER, PUBLIC :: secpmin=60.0_rp ! - number of seconds per minute -
  REAL(KIND=rp), PARAMETER, PUBLIC :: minphr= 60.0_rp ! - number of minutes per hour -
  REAL(KIND=rp), PARAMETER, PUBLIC :: hrpdy=  24.0_rp ! - number of hours per day -
!
! Digits
  CHARACTER(LEN=10), PARAMETER, PUBLIC :: digits='0123456789' ! - digits -
!
! Scalars
!
! Integer scalars
  INTEGER, PUBLIC :: ihuge  ! - huge number -
!
! Real scalars
  REAL(KIND=rp), PUBLIC :: sqrtwo ! - square root of two -
  REAL(KIND=rp), PUBLIC :: sqrpi  ! - square root of pi -
  REAL(KIND=rp), PUBLIC :: base   ! - base -
  REAL(KIND=rp), PUBLIC :: eps    ! - machine precision -
  REAL(KIND=rp), PUBLIC :: sfmin  ! - safe minimum -
  REAL(KIND=rp), PUBLIC :: sfmax  ! - safe maximum -
  REAL(KIND=rp), PUBLIC :: smlnum ! - small number -
  REAL(KIND=rp), PUBLIC :: bignum ! - big number -
  REAL(KIND=rp), PUBLIC :: tol    ! - tolerance for identifying probable floating errors -
!
END MODULE numbers
