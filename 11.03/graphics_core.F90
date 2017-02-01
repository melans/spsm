! $Id: graphics_core.F90 1034 2010-09-07 20:42:41Z lsong $
MODULE screen
!
! Modules
  USE gui_constants
  USE numbers,       ONLY: rp=>dp
!
! Implicit declarations
  IMPLICIT NONE
!
! Scalars
!
! Integer scalars
  INTEGER, PUBLIC :: ix_font ! - horizontal font size in pixels -
  INTEGER, PUBLIC :: iy_font ! - vertical font size in pixels -
!
! Derived types
!
! Derived type definitions
! - font -
  TYPE font
     REAL(KIND=rp) :: width  ! - font width -
     REAL(KIND=rp) :: height ! - font height -
  END TYPE font
!
! - frame -
  TYPE frame
     INTEGER :: left   ! - left -
     INTEGER :: right  ! - right -
     INTEGER :: length ! - length -
     INTEGER :: top    ! - top -
     INTEGER :: bottom ! - bottom -
     INTEGER :: depth  ! - depth -
  END TYPE frame
!
! - view -
  TYPE viewport
     REAL(KIND=rp) :: xmin   ! - minimum horizontal value -
     REAL(KIND=rp) :: xmax   ! - maximum horizontal value -
     REAL(KIND=rp) :: length ! - viewport length -
     REAL(KIND=rp) :: ymin   ! - minimum vertical value -
     REAL(KIND=rp) :: ymax   ! - maximum vertical value -
     REAL(KIND=rp) :: height ! - viewport height -
  END TYPE viewport
!
! Derived type scalars
  TYPE(font), PUBLIC :: font1 ! - defualt font -
  TYPE(font), PUBLIC :: font2 ! - title font -
  TYPE(font), PUBLIC :: font3 ! - numeric labels font -
!
  TYPE(frame), PUBLIC :: margin ! - margins -
!
  TYPE(viewport), PUBLIC :: view ! - viewport -
!
CONTAINS
!
!
 SUBROUTINE margins (x1,x2,y1,y2)
!
! Sets margin for graphics area
!
! Modules
  USE gui, ONLY: igsz
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: x1 ! - left margin -
  REAL(KIND=rp), INTENT(IN) :: x2 ! - right margin -
  REAL(KIND=rp), INTENT(IN) :: y1 ! - top margin -
  REAL(KIND=rp), INTENT(IN) :: y2 ! - bottom margin -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC NINT
  INTRINSIC REAL
!
! Executable Statements
!
! Set plotting region
  margin%left=NINT(REAL(igsz,KIND=rp)*x1)
  margin%right=igsz-NINT(REAL(igsz,KIND=rp)*x2)
  margin%top=NINT(REAL(igsz,KIND=rp)*y1)
  margin%bottom=igsz-NINT(REAL(igsz,KIND=rp)*y2)
!
! Set viewport border widths
  margin%length=margin%right+1-margin%left
  margin%depth=margin%bottom+1-margin%top
!
  RETURN
 END SUBROUTINE margins
!
!
!
 SUBROUTINE init_view (view,x1,x2,y1,y2)
!
! Sets viewport border widths
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: x1 ! - x minimum -
  REAL(KIND=rp), INTENT(IN) :: x2 ! - x maximum -
  REAL(KIND=rp), INTENT(IN) :: y1 ! - y minimum -
  REAL(KIND=rp), INTENT(IN) :: y2 ! - y maximum -
!
! Output scalars
  TYPE(viewport), INTENT(OUT) :: view ! - viewport -
!
! Executable Statements
  view%xmin=x1
  view%xmax=x2
  view%length=x2-x1
  view%ymin=y1
  view%ymax=y2
  view%height=y2-y1
!
  RETURN
 END SUBROUTINE init_view
!
!
!
 FUNCTION ix_pixel (x)
!
! Converts x-value to a pixel
!
! Function type
  INTEGER :: ix_pixel
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: x ! - x value -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC NINT
  INTRINSIC REAL
!
! Executable Statements
!
! Calculate position in pixels
  ix_pixel=margin%left+NINT(REAL(margin%length,KIND=rp)*(x-view%xmin)/view%length)
!
  RETURN
 END FUNCTION ix_pixel
!
!
!
 FUNCTION iy_pixel (y)
!
! Converts y-value to a pixel
!
! Function type
  INTEGER :: iy_pixel
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: y ! - y value -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC NINT
  INTRINSIC REAL
!
! Executable Statements
!
! Calculate position in pixels
  iy_pixel=margin%bottom-NINT(REAL(margin%depth,KIND=rp)*(y-view%ymin)/view%height)
!
  RETURN
 END FUNCTION iy_pixel
!
!
!
 SUBROUTINE init_fonts ()
!
! Initialises font sizes
!
! Modules
!  USE clrwin, ONLY: select_font@
  USE gui,    ONLY: cfont
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC TRIM
!
! Executable Statements
!
! Initialise font sizes
! - default font -
  font1%width=REAL(0.014,KIND=rp)
  font1%height=REAL(0.035,KIND=rp)
! - title font -
  font2%width=REAL(0.016,KIND=rp)
  font2%height=REAL(0.040,KIND=rp)
! - numeric labels font -
  font3%width=REAL(0.012,KIND=rp)
  font3%height=REAL(0.030,KIND=rp)
!
! Initialise font
  CALL font_size (font1)
!  CALL select_font@ (TRIM(cfont))
!
  RETURN
 END SUBROUTINE init_fonts
!
!
!
 SUBROUTINE font_size (f)
!
! Sets font size
!
! Modules
!  USE clrwin, ONLY: size_in_pixels@
  USE gui,    ONLY: igsz
!
! Arguments
!
! Input scalars
  TYPE(font), INTENT(IN) :: f ! - font -
!
! Executable Statements
!
! Calculate position in pixels
  ix_font=NINT(REAL(igsz,KIND=rp)*f%width)
  iy_font=NINT(REAL(igsz,KIND=rp)*f%height)
!
! Set size
!  CALL size_in_pixels@ (iy_font,ix_font)
!
  RETURN
 END SUBROUTINE font_size
END MODULE screen
!
!
!
MODULE axes
!
! Modules
  USE numbers, ONLY: rp=>dp
  USE screen
!
! Derived types
!
! Derived type definitions
! - axis -
  TYPE axis
     INTEGER :: i_min ! - minimum plotting position -
     INTEGER :: i_max ! - maximum plotting position -
!
     REAL(KIND=rp) :: amin   ! - axis minimum -
     REAL(KIND=rp) :: amax   ! - axis maximum -
     REAL(KIND=rp) :: length ! - axis length -
     REAL(KIND=rp) :: aint   ! - intercept -
     REAL(KIND=rp) :: tick   ! - tick-mark interval -
     REAL(KIND=rp) :: scale  ! - scaling -
!
     CHARACTER(LEN=64) :: label ! - label -
!
     LOGICAL :: laxis ! - logaritihmic axis flag -
  END TYPE axis
!
! Derived type scalars
  TYPE(axis), PUBLIC :: x_axis ! - x-axis -
  TYPE(axis), PUBLIC :: y_axis ! - y-axis -
!
CONTAINS
!
!
 SUBROUTINE set_axis (aaxis,clab,laxis,i_pixel)
!
! Initialises axis settings
!
! Modules
  USE maths,   ONLY: magnitude
  USE numbers, ONLY: zero,one
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: clab ! - axis label -
!
  LOGICAL, INTENT(IN) :: laxis ! - log axis flag -
!
! Input/output scalars
  TYPE(axis), INTENT(INOUT) :: aaxis ! - axis -
!
! Procedure arguments
  INTEGER, EXTERNAL :: i_pixel ! - axis function -
!
! Locals
!
! Local scalars
  INTEGER :: iom  ! - order of magnitude -
!
  CHARACTER(LEN=11) :: fmt    ! - format statement -
  CHARACTER(LEN=15) :: cscale ! - scaling label -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC NINT
  INTRINSIC TRIM
!
! Executable Statements
!
! Set log axis flag
  aaxis%laxis=laxis
!
! Set intercept
! - extremes both negative -
  IF ((aaxis%amin<zero).AND.(aaxis%amax<zero)) THEN
     aaxis%aint=aaxis%amax
! - extremes are of opposite signs -
  ELSE IF ((aaxis%amin<zero).AND.(.NOT.aaxis%laxis)) THEN
     aaxis%aint=zero
! - extremes both positive -
  ELSE
     aaxis%aint=aaxis%amin
  END IF
!
! Define limits
  aaxis%i_min=i_pixel(aaxis%amin)
  aaxis%i_max=i_pixel(aaxis%amax)
!
! Construct scaling label
  IF (aaxis%scale/=one) THEN
     iom=magnitude(aaxis%scale)
     IF (iom>0) THEN
        IF (iom<=3) THEN
           WRITE(fmt,FMT='(A,I1,A)') '(A,I',iom,',A)'
           WRITE(cscale,FMT=fmt) '(x ',NINT(aaxis%scale),')'
        ELSE
           WRITE(fmt,FMT='(A,I4,A)') '(A,I',magnitude(iom-1),',A)'
           WRITE(cscale,FMT=fmt) '(x 10^',iom-1,')'
        END IF
        aaxis%label=clab//' '//TRIM(cscale)
     ELSE IF (iom<0) THEN
        IF (iom>=-3) THEN
           WRITE(fmt,FMT='(A,I1,A)') '(A,I',1-iom,',A)'
           WRITE(cscale,FMT=fmt) '(/ ',NINT(one/aaxis%scale),')'
        ELSE
           WRITE(fmt,FMT='(A,I4,A)') '(A,I',magnitude(-iom-1),',A)'
           WRITE(cscale,FMT=fmt) '(/ 10^',-iom-1,')'
        END IF
        aaxis%label=clab//' '//TRIM(cscale)
     ELSE
        aaxis%label=clab
     END IF
  ELSE
     aaxis%label=clab
  END IF
!
  RETURN
 END SUBROUTINE set_axis
!
!
!
 SUBROUTINE calc_xaxis_limits (aaxis,itf,n)
!
! Calculates appropriate axis limits when data are years
!
! Modules
  USE numbers, ONLY: one,five,ten
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: itf ! - first year -
  INTEGER, INTENT(IN) :: n   ! - number of years -
!
! Input/output scalars
  TYPE(axis), INTENT(INOUT) :: aaxis ! - axis -
!
! Locals
!
! Local scalars
  INTEGER :: iygf  ! - first year on graph -
  INTEGER :: iygl  ! - last year on graph -
  INTEGER :: iya   ! - year rounding -
  INTEGER :: iymax ! - maximum number of years -
  INTEGER :: iom   ! - order of magnitude -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MOD
  INTRINSIC REAL
!
! Executable Statements
!
! Round years to nearest 5
  iygf=itf-MOD(itf,5)
  iygl=itf+n
  iya=MOD(iygl,5)
  IF (iya>0) iygl=iygl+5-iya
!
! Round if more than 50 years
  iom=0
  DO
     iya=5*10**iom
     iymax=5*10**(iom+1)
     IF (iygl-iygf>=iymax) THEN
        IF (MOD(iygf,10)==iya) iygf=iygf-iya
        IF (MOD(iygl,10)==iya) iygl=iygl+iya
     ELSE
        aaxis%tick=ten**iom
        EXIT
     END IF
     iom=iom+1
  END DO
  IF (iom==0) aaxis%tick=five
  aaxis%amin=REAL(iygf,KIND=rp)
  aaxis%amax=REAL(iygl,KIND=rp)
  aaxis%scale=one
!
  RETURN
 END SUBROUTINE calc_xaxis_limits
!
!
!
 SUBROUTINE calc_axis_limits (aaxis,alow,ahgh)
!
! Calculates appropriate axis limits given data limits
!
! Modules
  USE numbers, ONLY: zero,half,one
!
! Arguments
!
! Input scalars
! - optional input scalars -
  REAL(KIND=rp), INTENT(IN) :: alow ! - lowest axis value -
  REAL(KIND=rp), INTENT(IN) :: ahgh ! - highest axis value -
!
! Input/output scalars
  TYPE(axis), INTENT(INOUT) :: aaxis ! - axis -
!
! Locals
!
! Local scalars
  REAL(KIND=rp) :: arange ! - axis range -
  REAL(KIND=rp) :: r      ! - ratio -
  REAL(KIND=rp) :: r_old  ! - old ratio -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ABS
  INTRINSIC MOD
  INTRINSIC NINT
  INTRINSIC PRESENT
  INTRINSIC REAL
!
! Executable Statements
!
! Get preliminary estimates for axis limits
  IF (ABS(alow)<ABS(ahgh)) THEN
     aaxis%amin=rescaled(alow,'l',aaxis%scale)
     aaxis%amax=rescaled(ahgh,'u',aaxis%scale)
  ELSE
     aaxis%amax=rescaled(ahgh,'u',aaxis%scale)
     aaxis%amin=rescaled(alow,'l',aaxis%scale)
  END IF
  IF (.NOT.(aaxis%amin<aaxis%amax.OR.aaxis%amin>aaxis%amax)) THEN
     aaxis%amin=aaxis%amin-one
     aaxis%amax=aaxis%amax+one
  END IF
  aaxis%amin=aaxis%amin*aaxis%scale
  aaxis%amax=aaxis%amax*aaxis%scale
!
! Reset if x=0 is nearby
  r=-one
! - extremes both negative -
1 IF ((alow<zero).AND.(ahgh<zero)) THEN
     arange=ahgh-alow
     IF ((arange>alow).OR.(-ahgh/arange<0.2_rp)) aaxis%amax=zero
! - extremes both positive -
  ELSE IF (alow>zero) THEN
     arange=ahgh-alow
     IF ((arange>ahgh).OR.(alow/arange<0.2_rp)) aaxis%amin=zero
  END IF
!
! Define appropriate tick-mark interval
  CALL get_tick (aaxis)
!
! Compare axis range to data range, and reduce axis range if data range is too small
  r_old=r
  r=ABS(((ahgh-alow)*aaxis%scale)/(aaxis%amax-aaxis%amin))
  IF ((r<half).AND.(r>zero).AND.(r/=r_old)) THEN
     IF (ahgh>alow) THEN
        DO
           IF (aaxis%amax-aaxis%tick>ahgh*aaxis%scale) THEN
              aaxis%amax=aaxis%amax-aaxis%tick
           ELSE
              EXIT
           END IF
        END DO
        DO
           IF (aaxis%amin+aaxis%tick<alow*aaxis%scale) THEN
              aaxis%amin=aaxis%amin+aaxis%tick
           ELSE
              EXIT
           END IF
        END DO
     ELSE
        DO
           IF (aaxis%amax+aaxis%tick<ahgh*aaxis%scale) THEN
              aaxis%amax=aaxis%amax+aaxis%tick
           ELSE
              EXIT
           END IF
        END DO
        DO
           IF (aaxis%amin-aaxis%tick>alow*aaxis%scale) THEN
              aaxis%amin=aaxis%amin-aaxis%tick
           ELSE
              EXIT
           END IF
        END DO
     END IF
     GOTO 1
  END IF
!
  RETURN
 END SUBROUTINE calc_axis_limits
!
!
!
 SUBROUTINE get_tick (aaxis)
!
! Calculates an appropriate tick-mark interval
!
! Modules
  USE numbers, ONLY: zero,one,two,four,five,ten,oneh
!
! Arguments
!
! Input/output scalars
  TYPE(axis), INTENT(INOUT) :: aaxis ! - axis -
!
! Locals
!
! Local scalars
  REAL(KIND=rp) :: a ! - absolute value -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ABS
!
! Executable Statements
!
! Define appropriate tick-mark interval
  a=ABS(aaxis%amax-aaxis%amin)
  aaxis%tick=one
  IF (a>zero) THEN
     rescale1: DO
        IF (a<=one) THEN
           aaxis%tick=aaxis%tick/ten
           a=a*ten
        ELSE IF (a>=oneh) THEN
           aaxis%tick=aaxis%tick*oneh
           a=a/oneh
        END IF
        IF (a<=four) THEN
           aaxis%tick=aaxis%tick/two
           a=a*two
        ELSE IF (a>=ten) THEN
           aaxis%tick=aaxis%tick*five
           a=a/five
        ELSE
           EXIT rescale1
        END IF
     END DO rescale1
  END IF
!
  RETURN
 END SUBROUTINE get_tick
!
!
!
 FUNCTION rescaled (z,ext,zscale)
!
! Modules
  USE numbers, ONLY: zero,onetth,one,oneh,onet
!
! Function type
  REAL(KIND=rp) :: rescaled
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: z ! - value -
!
  CHARACTER(LEN=1), INTENT(IN) :: ext ! - tail -
!
! Output scalars
  REAL(KIND=rp), INTENT(OUT) :: zscale ! - scaling factor -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - rounded absolute value -
!
  REAL(KIND=rp) :: a ! - absolute value -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MOD
  INTRINSIC REAL
  INTRINSIC SIGN
!
! Executable Statements
!
! Rescale
  a=ABS(z)
  zscale=one
  IF (a>zero) THEN
     rescale2: DO
        IF (a<one) THEN
           zscale=zscale*oneh
           a=a*oneh
        ELSE IF (a>onet) THEN
           zscale=zscale*onetth
           a=a*onetth
        ELSE
           EXIT rescale2
        END IF
     END DO rescale2
  END IF
!
! Define appropriate limit
  i=INT(a)
  IF (((z<zero).AND.(ext=='l')).OR.((z>zero).AND.(ext=='u'))) THEN
     i=i+1
  ELSE IF (((z>zero).AND.(ext=='l')).OR.((z<zero).AND.(ext=='u'))) THEN
     i=i-1
  END IF
  SELECT CASE (i)
   CASE (0:15)
     CONTINUE
   CASE (16:40)
     IF (MOD(i,5)/=0) THEN
        IF (((z<zero).AND.(ext=='l')).OR.((z>zero).AND.(ext=='u'))) THEN
           i=i+5-MOD(i,5)
        ELSE
           i=i-MOD(i,5)
        END IF
     END IF
   CASE DEFAULT
     IF (MOD(i,10)/=0) THEN
        IF (((z<zero).AND.(ext=='l')).OR.((z>zero).AND.(ext=='u'))) THEN
           i=i+10-MOD(i,10)
        ELSE
           i=i-MOD(i,10)
        END IF
     END IF
  END SELECT
  rescaled=REAL(i,KIND=rp)/zscale
  rescaled=SIGN(rescaled,z)
!
  RETURN
 END FUNCTION rescaled
!
END MODULE axes
!
!
!
MODULE cross
!
! Modules
  USE axes
!
! Derived typese
!
! Derived type definitions
! - cross -
  TYPE marker
     INTEGER :: ixd ! - X cross dimension -
     INTEGER :: iyd ! - Y cross dimension -
  END TYPE marker
!
! Derived type scalars
  TYPE(marker), PUBLIC :: gcross  ! - cross marker -
!
CONTAINS
!
!
 SUBROUTINE init_cross (x_axis,y_axis,ix_pixel,iy_pixel)
!
! Modules
  USE numbers, ONLY: onehth
!
! Arguments
!
! Input scalars
  TYPE(axis), INTENT(IN) :: x_axis ! - x-axis -
  TYPE(axis), INTENT(IN) :: y_axis ! - x-axis -
!
! Procedure arguments
  INTEGER, EXTERNAL :: ix_pixel ! - x-axis function -
  INTEGER, EXTERNAL :: iy_pixel ! - y-axis function -
!
! Executable Statements
!
! Draw cross
  gcross%ixd=ix_pixel(x_axis%amin+(x_axis%amax-x_axis%amin)*onehth)-x_axis%i_min
  gcross%iyd=iy_pixel(y_axis%amax-(y_axis%amax-y_axis%amin)*onehth)-y_axis%i_max
!
  RETURN
 END SUBROUTINE init_cross
END MODULE cross
!
!
!
MODULE graphics_core
!
! Modules
  USE axes
  USE cross
  USE gui_constants, ONLY: mtitle,mcol
  USE IO_constants,  ONLY: iin
  USE numbers,       ONLY: rp=>dp
  USE screen
!
! Implicit declarations
  IMPLICIT NONE
!
! Scalars
!
! Integer scalars
  INTEGER, PUBLIC :: icross=1 ! - crosses on graph flag -
  INTEGER, PUBLIC :: iland=0  ! - mask land flag -
  INTEGER, PUBLIC :: ilake=0  ! - mask lakes flag -
  INTEGER, PUBLIC :: ivl=1    ! - vertical lines on graph flag -
  INTEGER, PUBLIC :: ipi=0    ! - prediction intervals on graph flag -
!
! Character scalars
  CHARACTER(LEN=mtitle), PRIVATE :: title_old ! - backup image title -
!
! Logical scalars
  LOGICAL, PUBLIC :: ltitle ! - title set flag -
!
! Derived types
!
! Derived type definitions
! - image -
  TYPE image
     INTEGER :: isize   ! - image size -
     INTEGER :: ncs     ! - number of contours -
!
     REAL(KIND=rp) :: ymin  ! - y-axis minimum -
     REAL(KIND=rp) :: ymax  ! - y-axis maximum -
     REAL(KIND=rp) :: scale ! - scaling -
!
     CHARACTER(LEN=mtitle) :: title ! - image title -
!
     LOGICAL :: lcontour ! - contours set flag -
!
     REAL(KIND=rp) :: contour(mcol) ! - contour levels -
  END TYPE image
!
  TYPE(image) :: img_fcst ! - forecasts graph -
  TYPE(image) :: img_fens ! - forecast ensemble graph -
  TYPE(image) :: img_pexc ! - probability of exceedance graph -
  TYPE(image) :: img_fval ! - forecasts map -
  TYPE(image) :: img_flow ! - lower prediction limits map -
  TYPE(image) :: img_fupp ! - upper prediction limits map -
  TYPE(image) :: img_fpsb ! - below-normal forecast probabilities m
  TYPE(image) :: img_fpsn ! - near-normal forecast probabilities ma
  TYPE(image) :: img_fpsa ! - above-normal forecast probabilities m

CONTAINS
!
!
 SUBROUTINE init_exceed (img,n,v)
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
! Input/output scalars
  TYPE(image), INTENT(INOUT) :: img ! - graph -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: v(:) ! - series (sorted in ascending order) -
!
! Executable Statements
!
! Obtain graph limits
  IF (.NOT.img%lcontour) THEN
     CALL calc_axis_limits (x_axis,alow=v(1),ahgh=v(n))
     img%ymin=x_axis%amin
     img%ymax=x_axis%amax
     img%scale=x_axis%scale
  END IF
!
  RETURN
 END SUBROUTINE init_exceed
!
!
!
  FUNCTION gridsp (rltn,rlts,rlgw,rlge)
!
! Modules
  USE numbers, ONLY: r360
!
! Defines an appropriate grid spacing
!
! Function type
  REAL(KIND=rp) :: gridsp
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: rltn,rlts ! - north and south map limits -
  REAL(KIND=rp), INTENT(IN) :: rlgw,rlge ! - west and east map limits -
!
! Locals
!
! Local parameters
  INTEGER, PARAMETER :: ns=6 ! - number of grid-spacing options -
!
! Local arrays
  REAL(KIND=rp), PARAMETER :: gss(ns)= & ! - grid-spacing options -
     (/ 2.0_rp, 5.0_rp,10.0_rp,15.0_rp,30.0_rp, 45.0_rp/)
  REAL(KIND=rp), PARAMETER :: gsl(ns)= & ! - grid-spacing limits -
     (/ 10.0_rp,20.0_rp,40.0_rp,70.0_rp,90.0_rp,120.0_rp/)
!
! Local scalars
  INTEGER :: i ! - indices -
!
  REAL(KIND=rp) :: rmap ! - largest map extent -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MAX
!
! Executable Statements
!
! Identify largest map extent
  IF (rlgw<rlge) THEN
     rmap=MAX(rltn-rlts,rlge-rlgw)
  ELSE
     rmap=MAX(rltn-rlts,r360-(rlgw-rlge))
  END IF
!
! Identify appropriate spacing
  gridsp=gss(1)
  DO i=1,ns
     IF (rmap>=gsl(i)) THEN
        gridsp=gss(i)
     ELSE
        EXIT
     END IF
  END DO
!
  RETURN
  END FUNCTION gridsp
!
!
!
  FUNCTION make_coor(latlng,r)
!
! Modules
  USE numbers, ONLY: zero,ten,oneh,r180,r360
!
! Function type
  CHARACTER(LEN=4) :: make_coor
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: r ! - latitude / longitude -
!
  CHARACTER(LEN=3), INTENT(IN) :: latlng ! - latitude / longitude flag -
!
! Locals
!
! Local scalars
  REAL(KIND=rp) :: rw ! - longitude in western hemisphere -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC NINT
!
! Executable Statements
!
! Construct coordinate label
! - latitudes -
  SELECT CASE (latlng)
   CASE ('lat')
     IF (r>zero) THEN
        IF (r<ten) THEN
           WRITE (make_coor,FMT='(I1,A)') NINT(r),'N'
        ELSE
           WRITE (make_coor,FMT='(I2,A)') NINT(r),'N'
        END IF
     ELSE IF (r<zero) THEN
        IF (r>-ten) THEN
           WRITE (make_coor,FMT='(I1,A)') -NINT(r),'S'
        ELSE
           WRITE (make_coor,FMT='(I2,A)') -NINT(r),'S'
        END IF
     ELSE
        make_coor='0   '
     END IF
! - longitudes -
   CASE ('lon')
     IF (r>zero) THEN
        IF (r<ten) THEN
           WRITE (make_coor,FMT='(I1,A)') NINT(r),'E'
        ELSE IF (r<oneh) THEN
           WRITE (make_coor,FMT='(I2,A)') NINT(r),'E'
        ELSE IF (r<r180) THEN
           WRITE (make_coor,FMT='(I3,A)') NINT(r),'E'
        ELSE IF (r>r180) THEN
           rw=r-r360
           IF (rw>-ten) THEN
              WRITE (make_coor,FMT='(I1,A)') -NINT(rw),'W'
           ELSE IF (rw>-oneh) THEN
              WRITE (make_coor,FMT='(I2,A)') -NINT(rw),'W'
           ELSE
              WRITE (make_coor,FMT='(I3,A)') -NINT(rw),'W'
           END IF
        ELSE
           make_coor='180 '
        END IF
     ELSE IF (r<zero) THEN
        IF (r>-ten) THEN
           WRITE (make_coor,FMT='(I1,A)') -NINT(r),'W'
        ELSE IF (r>-oneh) THEN
           WRITE (make_coor,FMT='(I2,A)') -NINT(r),'W'
        ELSE
           WRITE (make_coor,FMT='(I3,A)') -NINT(r),'W'
        END IF
     ELSE
        make_coor='0   '
     END IF
  END SELECT
!
  RETURN
  END FUNCTION make_coor
!
!
!
  FUNCTION get_lng(rlg,rrlge,x_axis_imin,x_axis_imax,rrlgw)
!
! Modules
  USE numbers,       ONLY: rp=>dp
!
! Input Scalars
  REAL(KIND=rp),INTENT(IN) :: rlg ! longitude
  REAL(KIND=rp),INTENT(IN) :: rrlge ! - rescaled eastern domain limit -
  REAL(KIND=rp),INTENT(IN) :: rrlgw ! - rescaled western domain limit -
  REAL(KIND=rp),INTENT(IN) :: x_axis_imin ! - minimum plotting position -
  REAL(KIND=rp),INTENT(IN) :: x_axis_imax ! - maximum plotting position -
!
! Function type
  INTEGER :: get_lng
!
! Executable Statements
!
! Determine longitude position
  IF (rlg>rrlge) THEN
     get_lng=x_axis_imax
  ELSE IF (rlg<rrlgw) THEN
     get_lng=x_axis_imin
  ELSE
     get_lng=ix_pixel(rlg)
  END IF
!
  RETURN
  END FUNCTION get_lng
!
!
!
  FUNCTION get_lat(rlt,rltn,rlts,y_axis_imin,y_axis_imax)
!
! Modules
  USE numbers,       ONLY: rp=>dp
!
! Input Scalars
  REAL(KIND=rp),INTENT(IN) :: rlt ! latitude
  REAL(KIND=rp),INTENT(IN) :: rltn ! - northern area limit -
  REAL(KIND=rp),INTENT(IN) :: rlts ! - southern area limit -
  REAL(KIND=rp),INTENT(IN) :: y_axis_imin ! - minimum plotting position -
  REAL(KIND=rp),INTENT(IN) :: y_axis_imax ! - maximum plotting position -
!
! Function type
  INTEGER :: get_lat
!
! Executable Statements
!
! Determine latitude position
  IF (rlt>rltn) THEN
     get_lat=y_axis_imax
  ELSE IF (rlt<rlts) THEN
     get_lat=y_axis_imin
  ELSE
     get_lat=iy_pixel(rlt)
  END IF
!
  RETURN
  END FUNCTION get_lat
!
!
 SUBROUTINE contours (zlow,zhgh,contour,ncs,zscale)
!
! Modules
  USE gui,     ONLY: ncol
  USE numbers, ONLY: zero,one,two,seven,ten
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: zlow ! - lowest value -
  REAL(KIND=rp), INTENT(IN) :: zhgh ! - highest value -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ncs ! - number of contours -
!
  REAL(KIND=rp), INTENT(OUT) :: zscale ! - scaling factor -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: contour(:) ! - contours -
!
! Locals
!
! Local scalars
  REAL(KIND=rp) :: cmax  ! - maximum number of contours -
  REAL(KIND=rp) :: z     ! - range -
  REAL(KIND=rp) :: zint  ! - contour interval -
  REAL(KIND=rp) :: znext ! - next contour interval -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
!
! Executable Statements
!
! Define first contour level
  ncs=1
  contour(ncs)=rescaled(zlow,'l',zscale)
  cmax=REAL(ncol,KIND=rp)
!
! Define appropriate contour interval
  z=zhgh-contour(ncs)
  IF (z>zero) THEN
     zint=one
     rescale1: DO
        IF (z<one) THEN
           zint=zint/ten
           z=z*ten
        ELSE IF (z>cmax) THEN
           zint=zint*ten
           z=z/ten
        END IF
        IF (z<seven) THEN
           zint=zint/two
           z=z*two
        ELSE IF (z>cmax) THEN
           zint=zint*two
           z=z/two
        ELSE
           EXIT rescale1
        END IF
     END DO rescale1
!
! Define additional contour levels
     set_contours: DO
        znext=contour(ncs)+zint
        IF (znext<zhgh) THEN
           ncs=ncs+1
           contour(ncs)=znext
        ELSE
           EXIT set_contours
        END IF
        IF (ncs==10) EXIT set_contours
     END DO set_contours
  END IF
!
  RETURN
 END SUBROUTINE contours
!
!
!
 SUBROUTINE init_graphic (x1,x2,y1,y2)
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: x1 ! - left margin -
  REAL(KIND=rp), INTENT(IN) :: x2 ! - right margin -
  REAL(KIND=rp), INTENT(IN) :: y1 ! - top margin -
  REAL(KIND=rp), INTENT(IN) :: y2 ! - bottom margin -
!
! Executable Statements
!
! Initialise viewport
  CALL margins (x1,x2,y1,y2)
!
  RETURN
 END SUBROUTINE init_graphic
!
!
!
 SUBROUTINE plot_fser_prepare ( width,height)
!
! Modules
  USE gui,           ONLY: igsize
  USE arrays,        ONLY: fcast,y,yhat
  USE CPT_constants, ONLY: csequ
  USE iofiles,       ONLY: yfile,zfile
  USE settings,      ONLY: iva,lag,nf,istd,nu
!
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: width ! - plot width -
  INTEGER, INTENT(IN) :: height ! - plot height -

  CALL init_forecast (width,height)

  SELECT CASE (istd)
   CASE (0)
     img_fcst%title='Forecasts and Cross-Validated Hindcasts'
   CASE (1)
     img_fcst%title='Forecasts and Cross-Validated Hindcast Anomalies'
   CASE (2)
     img_fcst%title='Standardized Forecasts and Cross-Validated Hindcasts'
   CASE (3)
     img_fcst%title='Forecasts and Cross-Validated Hindcast %s of Average'
  END SELECT

  CALL fgraph_prepare (img_fcst,csequ(zfile%iseq),nu,nf,1,&
                       y(iva,:),yhat(iva,:),fcast(iva,:,0:0),&
                       yfile%fdate%iyr,zfile%fdate%iyr+lag)

  RETURN
 END SUBROUTINE plot_fser_prepare

 SUBROUTINE fgraph_prepare (img,cxlab,n,nf,nens,yin,yhatin,yfcastin,itf_y,itf_f)
!
! Modules
  USE gui,      ONLY: icol,igsize,igsz
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n     ! - number of cases -
  INTEGER, INTENT(IN) :: nf    ! - number of forecasts -
  INTEGER, INTENT(IN) :: nens  ! - number of ensemble members -
  INTEGER, INTENT(IN) :: itf_y ! - first year of data -
  INTEGER, INTENT(IN) :: itf_f ! - first year of forecasts -
!
  CHARACTER(LEN=*), INTENT(IN) :: cxlab ! - x-axis label -
!
! Input/Ouput scalars 
  Type(image), INTENT(INOUT) :: img   ! - graph -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: yin(:)        ! - observations -
  REAL(KIND=rp), INTENT(IN) :: yhatin(:)     ! - cross-validated forecasts -
  REAL(KIND=rp), INTENT(IN) :: yfcastin(:,:) ! - forecasts -
!
! Locals
!
! Local scalars
  INTEGER :: i   ! - ensemble member index -
  INTEGER :: k   ! - forecast index -
  INTEGER :: ix1 ! - X plotting position -
!
  REAL(KIND=rp) :: ylow ! - lowest y-value -
  REAL(KIND=rp) :: yhgh ! - highest y-value -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MAX
  INTRINSIC MAXVAL
  INTRINSIC MIN
  INTRINSIC MINVAL
  INTRINSIC PRESENT
  INTRINSIC REAL
  INTRINSIC TRIM
!
! Executable Statements
!
! Initialise
  igsz=igsize
  CALL init_graphic (dleft,dright,dbottom,dtop)
!
! Obtain graph limits
! - x-axis -
  ix1=MIN(itf_y,itf_f)
  CALL calc_xaxis_limits (x_axis,ix1,MAX(itf_y+n-ix1,itf_f+nf-ix1))
! - y-axis -
  IF (img%lcontour) THEN ! - preset limits -
     y_axis%amin=img%ymin*img%scale
     y_axis%amax=img%ymax*img%scale
     y_axis%scale=img%scale
     CALL get_tick (y_axis)
  ELSE ! - calculate limits -
     ylow=MIN(MINVAL(yin(1:n)),MINVAL(yhatin(1:n)),MINVAL(yfcastin(1:nf,1:nens)))
     yhgh=MAX(MAXVAL(yin(1:n)),MAXVAL(yhatin(1:n)),MAXVAL(yfcastin(1:nf,1:nens)))
     CALL calc_axis_limits (y_axis,alow=ylow,ahgh=yhgh)
     img%ymin=y_axis%amin
     img%ymax=y_axis%amax
     img%scale=y_axis%scale
  END IF
!
! Initialise axes
  CALL init_view (view,x_axis%amin,x_axis%amax,y_axis%amin,y_axis%amax)
  CALL set_axis (x_axis,TRIM(cxlab),.false.,ix_pixel)
  CALL set_axis (y_axis,'Observations (red) / Forecasts (green)',.false.,iy_pixel)
!
! Draw graph base
!  CALL draw_graph_base (img%title,x_axis,y_axis,ivl,nty=nts,ty=tobs)
!
! Define cross dimensions
  CALL init_cross (x_axis,y_axis,ix_pixel,iy_pixel)
!
! Plot data
! - observations -
!  CALL add_line (.false.,n,y(1:n),itf_y-1,1,2, &
!       kuse=kuse)
! - hindcasts -
!  CALL add_line (.false.,n,yhat(1:n),itf_y-1,2,2, &
!      kuse=kuse)
!
! Add forecasts
!  DO k=1,nf
!     DO i=1,nens
!        ix1=ix_pixel(REAL(itf_f+k-1,KIND=rp))
!        iy1=iy_pixel(yfcast(k,i)*y_axis%scale)
!        CALL draw_cross (ix1,iy1,2,icol(3))
!     END DO
!  END DO
! - add prediction intervals -
!  IF (PRESENT(yfpls)) THEN
!     IF (ipi==1) CALL add_errors (nf,yfpls(:,2)*y_axis%scale,yfpls(:,1)*y_axis%scale,itf_f-1,gcross%ixd,icol(3),1)
!  END IF
!
  RETURN
 END SUBROUTINE fgraph_prepare

!
!
 SUBROUTINE get_plotdesc(xmin,xmax,xinterval,yintx,ymin,ymax,&
            yinterval,xinty,x_axis_name,xnl,y_axis_name,ynl)
!
! Modules
  USE Screen
  USE numbers, ONLY: ten,one,zero
!
! Output Scalars
	 REAL(KIND=rp), INTENT(OUT) ::xmin      !/* minimum of x axis */
  REAL(KIND=rp), INTENT(OUT) ::xmax      !/* maximum of x axis */
  REAL(KIND=rp), INTENT(OUT) ::xinterval !/* interval for x axis */
  REAL(KIND=rp), INTENT(OUT) ::xinty     !/* point where X axis intercepts on Y axis. Default to (0,0) the origin */
  REAL(KIND=rp), INTENT(OUT) ::yintx     !/* point where Y axis intercepts on X axis. Default to (0,0) the origin */
  REAL(KIND=rp), INTENT(OUT) ::ymin      !/* minimum of y axis */
  REAL(KIND=rp), INTENT(OUT) ::ymax      !/* maximum of y axis */
  REAL(KIND=rp), INTENT(OUT) ::yinterval !/* interval for y axis */
!
  INTEGER, INTENT(OUT) :: xnl            !/* total number of characters of x axis name */
  INTEGER, INTENT(OUT) :: ynl            !/* total number of characters of y axis name */
! 
  CHARACTER(LEN=80), INTENT(OUT) :: x_axis_name
  CHARACTER(LEN=80), INTENT(OUT) :: y_axis_name
!
! Local scalars
  INTEGER :: i  ! - index -
!
  REAL(KIND=rp) :: x ! - x-value -
  REAL(KIND=rp) :: y ! - y-value -
  REAL(KIND=rp) :: tick ! - tick -
  REAL(KIND=rp) :: atick ! - absolute tick -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC NINT
  INTRINSIC REAL
  INTRINSIC ABS
  INTRINSIC MIN
  INTRINSIC MAX 
  INTRINSIC MOD
  INTRINSIC TRIM 
  INTRINSIC LEN_TRIM 
!
! Executable Statements
!
 i=0;
!--------------- Set up X axis information 
! initialize to value
 xmin = x_axis%i_min
 xmax = x_axis%i_max;
 ymin = y_axis%i_min;
 ymax = y_axis%i_max;
 xinty = iy_pixel(y_axis%aint);
!xinty = y_axis%aint;
!
! Add tick marks
  xinterval= x_axis%tick*REAL(1,KIND=rp)
  x_axis_name = TRIM(x_axis%label)
  xnl=LEN_TRIM(x_axis%label)
  atick=ABS(xinterval)
  IF ( atick>zero ) THEN
     xmin=MIN(x_axis%amin,x_axis%amax)
     xmax=MAX(x_axis%amin,x_axis%amax)
  END IF
!--------------- Set up Y axis information 
 yintx = ix_pixel(x_axis%aint)
 !yintx = x_axis%aint
 ymin=y_axis%i_min
 ymax=y_axis%i_max
!
! Add tick marks
  IF (.NOT.y_axis%laxis) THEN
     tick=y_axis%tick*REAL(1,KIND=rp)
     atick=ABS(tick)
  ELSE
     tick=one
     atick=one
  END IF
  yinterval = tick
  y_axis_name = TRIM(y_axis%label)
  ynl = LEN_TRIM(y_axis%label)
  IF (atick>zero) THEN
     IF (.NOT.y_axis%laxis) THEN ! - not logaritihmic axis - 
        ymin=MIN(y_axis%amin,y_axis%amax)
        ymax=MAX(y_axis%amin,y_axis%amax)
     ELSE ! - not logaritihmic axis - 
        ymin=y_axis%amin
        ymax=y_axis%amax
     END IF
 END IF

  RETURN 
 END SUBROUTINE get_plotdesc
!
!
SUBROUTINE get_plotdesctickmarks(xticks_names,totalxticks,yticks_names,totalyticks)
!
! Modules
  USE numbers, ONLY: ten,one,zero
  USE Screen
!
! Output Scalars
!
  INTEGER, INTENT(OUT) :: totalxticks    !/* total number of xticks_names */
  INTEGER, INTENT(OUT) :: totalyticks    !/* total number of yticks_names */
!
! Output arrays 
  REAL(KIND=rp), INTENT(OUT) ::xticks_names(1); !/* name of x axis tick marks */
  REAL(KIND=rp), INTENT(OUT) ::yticks_names(1); !/* name of y axis tick marks */
!
! Local scalars
  !INTEGER :: ix ! - line coordinates -
  INTEGER :: i  ! - index -
  INTEGER :: ix1,ix2,iy1,iy2  ! - index -

!
  REAL(KIND=rp) :: x ! - x-value -
  REAL(KIND=rp) :: xmin  ! - x minimum value -
  REAL(KIND=rp) :: y ! - y-value -
  REAL(KIND=rp) :: ymin ! - y maximum value -
  REAL(KIND=rp) :: ymax ! - y maximum value -
  REAL(KIND=rp) :: tick ! - tick-mark interval -
  REAL(KIND=rp) :: atick ! - absolute tick-mark interval -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC NINT
  INTRINSIC REAL
  INTRINSIC ABS
  INTRINSIC MIN
  INTRINSIC MAX 
  INTRINSIC MOD
  INTRINSIC TRIM 
  INTRINSIC LEN_TRIM 
!
! Executable Statements
!
 i=1;
!--------------- Set up X axis tick marks information 
! initialize to value
 xmin = x_axis%i_min
!
! Add tick marks
  tick=x_axis%tick*REAL(1,KIND=rp)
  atick=ABS(tick)
  IF ( atick>zero ) THEN
     xmin=MIN(x_axis%amin,x_axis%amax)
     x=xmin-MOD(xmin,tick)
     IF (x>xmin) x=x-atick
     xticks_names(i)=x; !/* first x axis tick marks */
     DO
        x=x+atick
        IF (x>=MAX(x_axis%amin,x_axis%amax)) THEN 
            i=i+1
            xticks_names(i)=x; !/* last x axis tick marks */
            EXIT
        END IF
        i=i+1
        xticks_names(i)=x; !/* name of x axis tick marks */
     END DO
  END IF
  totalxticks=i
!--------------- Set up Y axis tick marks information 
 ymin=y_axis%i_min
 ymax=y_axis%i_max
!
! Add tick marks
  IF (.NOT.y_axis%laxis) THEN
     tick=y_axis%tick*REAL(1,KIND=rp)
     atick=ABS(tick)
  ELSE
     tick=one
     atick=one
  END IF
  IF (atick>zero) THEN
     IF (.NOT.y_axis%laxis) THEN ! - not logaritihmic axis - 
        ymin=MIN(y_axis%amin,y_axis%amax)
        y=ymin-MOD(ymin,tick)
        IF (y>ymin) y=y-atick
        ymax=MAX(y_axis%amin,y_axis%amax)
     ELSE ! - not logaritihmic axis - 
        ymin=y_axis%amin
        ymax=y_axis%amax
        y=ymin-tick
     END IF
     i=1
     DO
        IF (y>=ymax) THEN
           ! add last tick mark
           IF (.NOT.y_axis%laxis) THEN
              yticks_names(i)=y
           ELSE
              IF (y<zero) THEN
                 yticks_names(i)=NINT(ten**(-y))
              ELSE
                 yticks_names(i)=NINT(ten**y)
              END IF
           END IF
           EXIT
        END IF 
!
! Add numberic labels
! - linear axes -
        IF (.NOT.y_axis%laxis) THEN
              yticks_names(i)=y
! - log axes -
        ELSE
              IF (y<zero) THEN
                 yticks_names(i)=NINT(ten**(-y))
              ELSE
                 yticks_names(i)=NINT(ten**y)
              END IF
        END IF
        i=i+1
        y=y+atick
     END DO
  END IF
  totalyticks=i

  iy1=y_axis%i_min
  iy2=y_axis%i_max
  ! - below -
  ix1=x_axis%i_min
  ix2=MIN(x_axis%i_min,x_axis%i_max)
  !ix2=MIN(MAX(ix_pixel(tx(1)*x_axis%scale),x_axis%i_min),x_axis%i_max)


  RETURN 
 END SUBROUTINE get_plotdesctickmarks
!
!
 SUBROUTINE init_forecast (igsize,ihsize)
!
! Initialises scatter plots
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: igsize ! - graphics area dimension -
  INTEGER, INTENT(IN) :: ihsize ! - reduced graphics area dimension -
!
! Executable Statements
!
! Initialise graph size
  img_fcst%isize=igsize
  img_fens%isize=igsize
  img_pexc%isize=(igsize*3)/2
  img_fval%isize=ihsize
  img_flow%isize=ihsize
  img_fupp%isize=ihsize
  img_fpsb%isize=ihsize
  img_fpsn%isize=ihsize
  img_fpsa%isize=ihsize
!
! Initialise default y-axis
  img_fcst%lcontour=.false.
  img_fens%lcontour=.false.
!
  RETURN
 END SUBROUTINE init_forecast

END MODULE graphics_core
