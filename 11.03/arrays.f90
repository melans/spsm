! $Id: arrays.f90 1215 2011-02-25 21:30:20Z simon $
MODULE arrays
!
! Modules
  USE numbers, ONLY: rp
!
! User-accessed Arrays
!
! Integer arrays
  INTEGER, ALLOCATABLE, PUBLIC :: iobs(:,:)  ! - observed categories -
  INTEGER, ALLOCATABLE, PUBLIC :: ifor(:,:)  ! - forecast categories -
  INTEGER, ALLOCATABLE, PUBLIC :: irobs(:,:) ! - retroactive observed categories -
  INTEGER, ALLOCATABLE, PUBLIC :: irfor(:,:) ! - retroactive forecast categories -
  INTEGER, ALLOCATABLE, PUBLIC :: ifq(:,:)   ! - frequencies -
!
! Real arrays
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: ave(:)      ! - climatological averages -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: sdev(:)     ! - climatological standard deviations -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: svx(:)      ! - singular values of x -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: svy(:)      ! - singular values of y -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: mu(:)       ! - canonical correlations -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: skills(:)   ! - skill scores -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: pscores(:)  ! - pobabilistic verification scores -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: pvalues(:)  ! - p-values -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: f(:)        ! - probabilities of exceedance given forecast -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: e(:)        ! - empirical probabilities of exceedance -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: c(:)        ! - climatological probabilities of exceedance -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: b0(:)       ! - regression constants (for MLR) -
!
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: x(:,:)      ! - explanatory variables -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: y(:,:)      ! - response variables -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: z(:,:)      ! - updated explanatory variables -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: clim(:,:)   ! - climatological (sorted) response data -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: eofx(:,:)   ! - x EOF patterns -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: eofy(:,:)   ! - y EOF patterns -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: tsx(:,:)    ! - time-series of x EOFs (transposed) -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: tsy(:,:)    ! - time-series of y EOFs (transposed) -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: r(:,:)      ! - canonical Y EOF weights -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: s(:,:)      ! - canonical X EOF weights (transposed) -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: yhat(:,:)   ! - cross-validated hindcasts -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: yret(:,:)   ! - retroactive predictions -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: yfit(:,:)   ! - fitted values of response variables -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: pev(:,:)    ! - prediction error variance -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: hx_map(:,:) ! - X homogeneous covariance maps -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: hx_ser(:,:) ! - X homogeneous covariance map time series -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: hy_map(:,:) ! - Y homogeneous covariance maps -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: hy_ser(:,:) ! - Y homogeneous covariance map time series -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: bz(:,:)     ! - principal component regression coefficients -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: b(:,:)      ! - regression coefficients -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: hit(:,:)    ! - hit rates -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: far(:,:)    ! - false alarm rates -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: hits(:,:)   ! - hit rates -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: fars(:,:)   ! - false alarm rates -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: rclim(:,:)  ! - retroactive climatologies -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: afp(:,:)    ! - binned average forecast probabilities -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: orf(:,:)    ! - observed relative frequencies -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: bss(:,:)    ! - Brier scores -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: cump(:,:)   ! - cumulative profits -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: eir(:,:)    ! - effective interest rates -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: tobs(:,:)   ! - climatological absolute thresholds -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: pobs(:,:)   ! - climatological percentile thresholds -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: xhat(:,:)   ! - predictor time scores -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: xiny(:,:)   ! - predictors interpolated to predictand locations -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: ziny(:,:)   ! - updated predictors interpolated to predictand locations -
!
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: yrpls(:,:,:) ! - retroactive forecast prediction limits -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: fcast(:,:,:) ! - forecast values -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: fpls(:,:,:)  ! - prediction limits -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: fps(:,:,:)   ! - forecast probabilities -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: odds(:,:,:)  ! - odds -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: oddr(:,:,:)  ! - odds relative to climatology -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: rfps(:,:,:)  ! - retroactive forecast probabilities -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: rodds(:,:,:) ! - retroactive odds -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: roddr(:,:,:) ! - retroactive odds relative to climatology -
!
! Internal arrays
!
! Integer arrays
  INTEGER, ALLOCATABLE, PUBLIC :: iusex(:) ! - indices of used X variables -
  INTEGER, ALLOCATABLE, PUBLIC :: iusey(:) ! - indices of used Y variables -
!
! Real arrays
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: xm(:)       ! - means of explanatory variables -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: ym(:)       ! - means of response variables -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: xsd(:)      ! - standard deviations of explanatory variables -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: ysd(:)      ! - standard deviations of response variables -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: prjc(:)     ! - projections onto CCA modes -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: rnko(:)     ! - observation ranks -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: rnkf(:)     ! - forecast ranks -
!
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: yt(:,:)     ! - transformed response data -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: yhatt(:,:)  ! - transformed cross-validated hindcasts -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: yrett(:,:)  ! - transformed retroactive predictions -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: fcastt(:,:) ! - transformed forecast values -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: tobst(:,:)  ! - transformed climatological absolute thresholds -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: xvp(:,:)    ! - predictors during forecast period -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: ce(:,:)     ! - EOF cross-correlations -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: xc(:,:)     ! - cross-validated training period explanatory data -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: yc(:,:)     ! - cross-validated training period response data -
!
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: yopt(:,:,:) ! - cross-validated hindcasts for optimization -
!
! Logical arrays
  LOGICAL, ALLOCATABLE, PUBLIC :: kuse(:)  ! - used cases flags -
  LOGICAL, ALLOCATABLE, PUBLIC :: kfuse(:) ! - used forecasts flags -
!
  LOGICAL, POINTER, PUBLIC :: kax(:,:)=>NULL() ! - X available cases flags -
  LOGICAL, POINTER, PUBLIC :: kay(:,:)=>NULL() ! - Y available cases flags -
  LOGICAL, POINTER, PUBLIC :: kaz(:,:)=>NULL() ! - Z available cases flags -
!
! Workspace
!
! Integer workspace
  INTEGER, ALLOCATABLE, PUBLIC :: iwk(:) ! - integer workspace -
!
! Real workspace
! - single precision -
  REAL, ALLOCATABLE, PUBLIC :: swk(:) ! - single precision workspace -
! - double precision -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: dwk(:) ! - double precision workspace -
!
CONTAINS
!
!
 SUBROUTINE init_arrays ()
!
! Executable Statements
!
! Initialise pointers
  NULLIFY (kax)
  NULLIFY (kay)
  NULLIFY (kaz)
!
  RETURN
 END SUBROUTINE init_arrays
!
!
!
 SUBROUTINE stdize (m,n,x,ave,sdev,istd)
!
! Standardizes data
!
! Modules
  USE numbers, ONLY: zero,eps,oneh
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: m    ! - number of variables -
  INTEGER, INTENT(IN) :: n    ! - number of cases -
  INTEGER, INTENT(IN) :: istd ! - standardization option -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: ave(:)  ! - mean -
  REAL(KIND=rp), INTENT(IN) :: sdev(:) ! - standard deviation -
!
! Input/output arrays
  REAL(KIND=rp), INTENT(INOUT) :: x(:,:) ! - data to be standardized -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - variable index -
  INTEGER :: k ! - case index -
!
! Executable Statements
!
! Standardize data
  SELECT CASE (istd)
   CASE (0) ! - no standardization -
     CONTINUE
   CASE (1) ! - anomalies
     DO k=1,n
        x(1:m,k)=x(1:m,k)-ave(1:m)
     END DO
   CASE (2) ! - standardized anomalies -
     DO i=1,m
        IF (sdev(i)>eps) THEN
           DO k=1,n
              x(i,k)=(x(i,k)-ave(i))/sdev(i)
           END DO
        ELSE
           x(i,1:n)=zero
        END IF
     END DO
   CASE (3) ! - % of average -
     DO i=1,m
        IF (ave(i)>eps) THEN
           DO k=1,n
              x(i,k)=oneh*x(i,k)/ave(i)
           END DO
        ELSE
           x(i,1:n)=zero
        END IF
     END DO
  END SELECT
!
  RETURN
 END SUBROUTINE stdize
!
!
!
 SUBROUTINE ustdize (m,n,x,ave,sdev,istd)
!
! Unstandardizes data
!
! Modules
  USE numbers, ONLY: zero,eps,oneh
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: m    ! - number of variables -
  INTEGER, INTENT(IN) :: n    ! - number of cases -
  INTEGER, INTENT(IN) :: istd ! - standardization option -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: ave(:)  ! - mean -
  REAL(KIND=rp), INTENT(IN) :: sdev(:) ! - standard deviation -
!
! Input/output arrays
  REAL(KIND=rp), INTENT(INOUT) :: x(:,:) ! - data to be unstandardized -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - variable index -
  INTEGER :: k ! - case index -
!
! Executable Statements
!
! Unstandardize data
  SELECT CASE (istd)
   CASE (0) ! - no standardization -
     CONTINUE
   CASE (1) ! - anomalies
     DO k=1,n
        x(1:m,k)=x(1:m,k)+ave(1:m)
     END DO
   CASE (2) ! - standardized anomalies -
     DO i=1,m
        IF (sdev(i)>eps) THEN
           DO k=1,n
              x(i,k)=x(i,k)*sdev(i)+ave(i)
           END DO
        ELSE
           x(i,1:n)=ave(i)
        END IF
     END DO
   CASE (3) ! - % of average -
     DO i=1,m
        IF (ave(i)>eps) THEN
           DO k=1,n
              x(i,k)=x(i,k)*ave(i)/oneh
           END DO
        ELSE
           x(i,1:n)=zero
        END IF
     END DO
  END SELECT
!
  RETURN
 END SUBROUTINE ustdize
!
!
!
 SUBROUTINE restdize (m,n,x,ave,sdev,istd_old,istd_new)
!
! Restandardizes data
!
! Modules
  USE numbers, ONLY: zero,eps,oneh
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: m        ! - number of variables -
  INTEGER, INTENT(IN) :: n        ! - number of cases -
  INTEGER, INTENT(IN) :: istd_old ! - old standardization option -
  INTEGER, INTENT(IN) :: istd_new ! - new standardization option -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: ave(:)  ! - mean -
  REAL(KIND=rp), INTENT(IN) :: sdev(:) ! - standard deviation -
!
! Input/output arrays
  REAL(KIND=rp), INTENT(INOUT) :: x(:,:) ! - data to be unstandardized -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - variable index -
  INTEGER :: k ! - case index -
!
! Executable Statements
!
! Restandardize data from no standardization
  SELECT CASE (istd_old)
   CASE (0)
     CALL stdize (m,n,x,ave,sdev,istd_new)
!
! Restandardize data from anomalies
   CASE (1)
     SELECT CASE (istd_new)
      CASE (0) ! - to no standardization -
        DO k=1,n
           x(1:m,k)=x(1:m,k)+ave(1:m)
        END DO
      CASE (1) ! - to anomalies -
        CONTINUE
      CASE (2) ! - to standardized anomalies -
        DO i=1,m
           IF (sdev(i)>eps) THEN
              DO k=1,n
                 x(i,k)=x(i,k)/sdev(i)
              END DO
           END IF
        END DO
      CASE (3) ! - to % of average -
        DO i=1,m
           IF (ave(i)>eps) THEN
              DO k=1,n
                 x(i,k)=oneh*(x(i,k)+ave(i))/ave(i)
              END DO
           ELSE
              x(i,1:n)=zero
           END IF
        END DO
     END SELECT
!
! Restandardize data from standardized anomalies
   CASE (2)
     SELECT CASE (istd_new)
      CASE (0) ! - to no standardization -
        DO i=1,m
           IF (sdev(i)>eps) THEN
              DO k=1,n
                 x(i,k)=x(i,k)*sdev(i)+ave(i)
              END DO
           ELSE
              x(i,1:n)=ave(i)
           END IF
        END DO
      CASE (1) ! - to anomalies -
        DO i=1,m
           IF (sdev(i)>eps) THEN
              DO k=1,n
                 x(i,k)=x(i,k)*sdev(i)
              END DO
           ELSE
              x(i,1:n)=zero
           END IF
        END DO
      CASE (2) ! - to standardized anomalies -
        CONTINUE
      CASE (3) ! - to % of average -
        DO i=1,m
           IF (ave(i)>eps) THEN
              DO k=1,n
                 x(i,k)=oneh*(x(i,k)*sdev(i)+ave(i))/ave(i)
              END DO
           ELSE
              x(i,1:n)=zero
           END IF
        END DO
     END SELECT
   CASE (3) ! - from % of average -
     SELECT CASE (istd_new)
      CASE (0) ! - to no standardization -
        DO k=1,n
           x(1:m,k)=x(1:m,k)*ave(1:m)/oneh
        END DO
      CASE (1) ! - to anomalies -
        DO k=1,n
           x(1:m,k)=x(1:m,k)*ave(1:m)/oneh-ave(1:m)
        END DO
      CASE (2) ! - to standardized anomalies -
        CALL ustdize (m,n,x,ave,sdev,istd_old)
        CALL ustdize (m,n,x,ave,sdev,istd_new)
      CASE (3) ! - to % of average -
        CONTINUE
     END SELECT
  END SELECT
!
  RETURN
 END SUBROUTINE restdize
!
!
!
 SUBROUTINE insertion_sort (x,n,order)
!
! Sorts data in ascending or descending order using insertion sort algorithm
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
  CHARACTER(LEN=1), INTENT(IN) :: order ! - order -
!
! Input/output arrays
  REAL(KIND=rp), INTENT(INOUT) :: x(:) ! - data to be sorted -
!
! Locals
!
! Local scalars
  INTEGER :: i,j ! - indices -
!
  REAL(KIND=rp) :: swp ! - swapped value -
!
! Executable Statements
!
! Sort data
! - ascending -
  SELECT CASE (order)
   CASE ('A','a')
     DO i=2,n
        swp=x(i)
        DO j=1,i-1
           IF (x(i)<x(j)) EXIT
        END DO
        IF (j<i) THEN
           x(j+1:i)=x(j:i-1)
           x(j)=swp
        END IF
     END DO
! - descending -
   CASE ('D','d')
     DO i=2,n
        swp=x(i)
        DO j=1,i-1
           IF (x(i)>x(j)) EXIT
        END DO
        IF (j<i) THEN
           x(j+1:i)=x(j:i-1)
           x(j)=swp
        END IF
     END DO
  END SELECT
!
  RETURN
 END SUBROUTINE insertion_sort
!
!
!
 SUBROUTINE rank_data (x,n,order,r)
!
! Ranks data in ascending or descending order
!
! Modules
  USE numbers, ONLY: half,one
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
  CHARACTER(LEN=1), INTENT(IN) :: order ! - order -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:) ! - data to be ranked -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: r(:) ! - ranks -
!
! Locals
!
! Local scalars
  INTEGER :: i,j ! - indices -
!
! Executable Statements
!
! Initialise ranks
  r(1:n)=one
!
! Rank data
! - ascending -
  SELECT CASE (order)
   CASE ('A','a')
     DO i=1,n-1
        DO j=i+1,n
           IF (x(j)>x(i)) THEN
              r(j)=r(j)+one
           ELSE IF (x(j)<x(i)) THEN
              r(i)=r(i)+one
           ELSE
              r(i)=r(i)+half
              r(j)=r(j)+half
           END IF
        END DO
     END DO
! - descending -
   CASE ('D','d')
     DO i=1,n-1
        DO j=i+1,n
           IF (x(j)<x(i)) THEN
              r(j)=r(j)+one
           ELSE IF (x(j)>x(i)) THEN
              r(i)=r(i)+one
           ELSE
              r(i)=r(i)+half
              r(j)=r(j)+half
           END IF
        END DO
     END DO
  END SELECT
!
  RETURN
 END SUBROUTINE rank_data
!
!
!
 FUNCTION quantile(x,n,p)
!
! Calculates value for given percentile from an empirical distribution
!
! Modules
  USE numbers, ONLY: one
!
! Function type
  REAL(KIND=rp) :: quantile
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
  REAL(KIND=rp), INTENT(IN) :: p ! - percentile -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:) ! - data sorted in ascending order -
!
! Locals
!
! Local scalars
  INTEGER :: indx ! - category boundary index -
!
  REAL(KIND=rp) :: d    ! - distance to category boundary -
  REAL(KIND=rp) :: df   ! - number of cases -
  REAL(KIND=rp) :: rndx ! - category boundary index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC INT
!
! Executable Statements
!
! Calculate quantile
  df=REAL(n+1,KIND=rp)
  rndx=df*p
  indx=INT(rndx)
  d=rndx-REAL(indx,KIND=rp)
  IF ((indx>0).AND.(indx<n)) THEN
     quantile=x(indx)*(one-d)+x(indx+1)*d
  ELSE IF (indx>0) THEN
     quantile=x(n)+(x(n)-x(n-1))*d
  ELSE
     IF (n>1) THEN
        quantile=x(1)-(x(2)-x(1))*(one-d)
     ELSE
        quantile=x(1)
     END IF
  END IF
!
  RETURN
 END FUNCTION quantile
!
!
!
 FUNCTION percentile(x,n,t)
!
! Calculates percentile for a deviate, t, given an empirical distribution, x
!
! Modules
  USE numbers, ONLY: zero,half,one
!
! Function type
  REAL(KIND=rp) :: percentile
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
  REAL(KIND=rp), INTENT(IN) :: t ! - deviate -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:) ! - data sorted in ascending order -
!
! Locals
!
! Local scalars
  INTEGER :: i    ! - case index -
  INTEGER :: indx ! - category boundary index -
!
  REAL(KIND=rp) :: d ! - distance to category boundary -
  REAL(KIND=rp) :: w ! - distance between sorted values -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC EXP
  INTRINSIC REAL
!
! Executable Statements
!
! Calculate quantile
  indx=0
  DO i=1,n
     IF (x(i)<t) THEN
        indx=indx+1
        CYCLE
     END IF
     EXIT
  END DO
  IF ((indx>0).AND.(indx<n)) THEN
     w=x(indx+1)-x(indx)
     IF (w>zero) THEN
        d=(t-x(indx))/w
        percentile=(REAL(indx,KIND=rp)+d)/REAL(n+1,KIND=rp)
     ELSE
        percentile=REAL(indx,KIND=rp)/REAL(n+1,KIND=rp)
     END IF
  ELSE IF (indx>0) THEN
     w=x(n)-x(n-1)
     IF (w>zero) THEN
        w=one-EXP((x(n)-t)/w)
     ELSE
        w=half
     END IF
     percentile=(n+w)*(one/REAL(n+1,KIND=rp))
  ELSE
     w=x(2)-x(1)
     IF (w>zero) THEN
        w=one-EXP((t-x(1))/w)
     ELSE
        w=half
     END IF
     percentile=(one-w)*(one/REAL(n+1,KIND=rp))
  END IF
  IF (percentile==zero) percentile=one/REAL(n+2,KIND=rp)
  IF (percentile==one) percentile=REAL(n+1,KIND=rp)/REAL(n+2,KIND=rp)
!
  RETURN
 END FUNCTION percentile
!
!
!
 FUNCTION get_flag(iflags)
!
! Function type
  INTEGER :: get_flag
!
! Arguments
!
! Input arrays
  INTEGER, INTENT(IN) :: iflags(:) ! - flags -
!
! Executable Statements
!
! Identify flagged index
  get_flag=1
  DO
     IF (iflags(get_flag)==1) EXIT
     get_flag=get_flag+1
  END DO
!
  RETURN
 END FUNCTION get_flag
!
!
!
 SUBROUTINE convert_units (aunits,bunits,v,ifail)
!
! Converts units of measurement
!
! Modules
  USE numbers, ONLY: zero_F,zero_K,secpmin,minphr,hrpdy,five,nine,ten,oneh,onet
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: bunits ! - new units -
!
! Input/output scalars
  CHARACTER(LEN=*), INTENT(INOUT) :: aunits ! - original units -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input/output arrays
  REAL(KIND=rp), INTENT(INOUT) :: v(:,:) ! - data to be converted -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC TRIM
!
! Executable Statements
!
! Temperature conversions
  SELECT CASE (TRIM(aunits))
! - from Kelvin -
   CASE ('Kelvin_scale','K')
     SELECT CASE (TRIM(bunits))
      CASE ('C') ! - to Celsius -
        v(:,:)=v(:,:)-zero_K
      CASE ('F') ! - to Farenheit -
        v(:,:)=zero_F+(v(:,:)-zero_K)*nine/five
      CASE DEFAULT
        GOTO 1
     END SELECT
! - from Celsius -
   CASE ('C')
     SELECT CASE (TRIM(bunits))
      CASE ('Kelvin_scale','K') ! - to Kelvin -
        v(:,:)=zero_K+v(:,:)
      CASE ('F') ! - to Farenheit -
        v(:,:)=zero_F+v(:,:)*nine/five
      CASE DEFAULT
        GOTO 1
     END SELECT
! - from Farenheit -
   CASE ('F')
     SELECT CASE (TRIM(bunits))
      CASE ('Kelvin_scale','K') ! - to Kelvin -
        v(:,:)=zero_K+(v(:,:)-zero_F)*five/nine
      CASE ('C') ! - to Celsius -
        v(:,:)=(v(:,:)-zero_F)*nine/five
      CASE DEFAULT
        GOTO 1
     END SELECT
!
! Rainfall conversions
! - from metres per second -
   CASE ('m/s')
     SELECT CASE (TRIM(bunits))
      CASE ('mm/day') ! - to mm per day -
        v(:,:)=v(:,:)*onet*secpmin*minphr*hrpdy
      CASE ('cm/day') ! - to cm per day -
        v(:,:)=v(:,:)*oneh*secpmin*minphr*hrpdy
      CASE DEFAULT
        GOTO 1
     END SELECT
! - from mm per day -
   CASE ('mm/day')
     SELECT CASE (TRIM(bunits))
      CASE ('m/s') ! - to metres per second -
        v(:,:)=v(:,:)/(onet*hrpdy*minphr*secpmin)
      CASE ('cm/day') ! - to cm per day -
        v(:,:)=v(:,:)/ten
      CASE DEFAULT
        GOTO 1
     END SELECT
! - from cm per day -
   CASE ('cm/day')
     SELECT CASE (TRIM(bunits))
      CASE ('m/s') ! - to metres per second -
        v(:,:)=v(:,:)/(oneh*hrpdy*minphr*secpmin)
      CASE ('cm/day') ! - to mm per day -
        v(:,:)=v(:,:)*ten
      CASE DEFAULT
        GOTO 1
     END SELECT
   CASE DEFAULT
     GOTO 1
  END SELECT
!
! Set new units
  aunits=bunits
  ifail=0
  RETURN
!
! Unable to convert
1 ifail=1
!
  RETURN
 END SUBROUTINE convert_units
END MODULE arrays
