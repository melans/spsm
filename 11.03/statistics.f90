! $Id: statistics.f90 1238 2011-03-04 16:25:04Z simon $
MODULE statistics
!
! Modules
  USE CPT_constants, ONLY: ng,nscore,nskill,nts
  USE numbers,       ONLY: rp,zero,one,oneh
!
! Implicit declarations
  IMPLICIT NONE
!
! Arrays
!
! Integer arrays
  INTEGER, PUBLIC :: iskills(nskill) ! - skill score selections -
!
! Real arrays
  REAL(KIND=rp), PUBLIC :: scores(nscore) ! - scores -
  REAL(KIND=rp), PUBLIC :: roca(nts)      ! - ROC areas (individual points) -
  REAL(KIND=rp), PUBLIC :: rocas(ng)      ! - ROC areas (all points) -
  REAL(KIND=rp), PUBLIC :: cs(ng,ng)      ! - LEPS coefficients -
  REAL(KIND=rp), PUBLIC :: gs(ng,ng)      ! - Gerrity coefficients -
!
! Scalars
!
! Integer scalars
  INTEGER, PUBLIC :: iskill ! - skill score selection -
!
! Explicit Interfaces
!
! Generic interfaces
  INTERFACE two_afc_mp
   MODULE PROCEDURE two_afc_mp_1
   MODULE PROCEDURE two_afc_mp_2
  END INTERFACE
!
CONTAINS
!
!
 FUNCTION init_scores()
!
! Modules
  USE arrays,   ONLY: hit,far,rnko,rnkf
  USE errors,   ONLY: error
  USE settings, ONLY: nu
!
! Function type
  INTEGER :: init_scores
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
! - observation ranks -
  IF (.NOT.ALLOCATED(rnko)) THEN
     ALLOCATE (rnko(nu),STAT=istat)
     IF (istat/=0) GOTO 1
  END IF
! - forecast ranks -
  IF (.NOT.ALLOCATED(rnkf)) THEN
     ALLOCATE (rnkf(nu),STAT=istat)
     IF (istat/=0) GOTO 1
  END IF
! - hit rates -
  IF (.NOT.ALLOCATED(hit)) THEN
     ALLOCATE (hit(nu,2),STAT=istat)
     IF (istat/=0) GOTO 1
  END IF
! - false alarm rates -
  IF (.NOT.ALLOCATED(far)) THEN
     ALLOCATE (far(nu,2),STAT=istat)
     IF (istat/=0) GOTO 1
  END IF
!
  init_scores=0
  RETURN
!
! Errors
1 init_scores=1
  CALL error ('init_scores',init_scores)
  IF (ALLOCATED(far))  DEALLOCATE (far)
  IF (ALLOCATED(hit))  DEALLOCATE (hit)
  IF (ALLOCATED(rnkf)) DEALLOCATE (rnkf)
  IF (ALLOCATED(rnko)) DEALLOCATE (rnko)
!
  RETURN
 END FUNCTION init_scores
!
!
!
 SUBROUTINE moments (m,n,x,xm,xsd)
!
! Calculates arithmetic column means and standard deviations of a 2-D array
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: m ! - number of variables -
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:,:) ! - variables -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: xm(:)  ! - means -
  REAL(KIND=rp), INTENT(OUT) :: xsd(:) ! - standard deviations -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - indices -
!
  REAL(KIND=rp) :: df  ! - number of cases -
  REAL(KIND=rp) :: dof ! - degrees of freedom -
!
! Functions and subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SQRT
  INTRINSIC SUM
!
! Executable Statements
!
! Calculate degrees of freedom
  df=REAL(n,KIND=rp)
  dof=REAL(n-1,KIND=rp)
!
! Calculate means
  DO i=1,m
     xm(i)=SUM(x(i,1:n))/df
     xsd(i)=SQRT(SUM((x(i,1:n)-xm(i))**2)/dof)
  END DO
!
  RETURN
 END SUBROUTINE moments
!
!
!
 FUNCTION p_corr(n,x,y)
!
! Calculates Pearson's product moment correlation
!
! Modules
  USE maths,   ONLY: magnitude
  USE numbers, ONLY: eps,ten
!
! Function type
  REAL(KIND=rp) :: p_corr
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:) ! - independent variables -
  REAL(KIND=rp), INTENT(IN) :: y(:) ! - dependent variables -
!
! Locals
!
! Local scalars
  INTEGER :: imx ! - order of magnitude of X variance -
  INTEGER :: imy ! - order of magnitude of Y variance -
!
  REAL(KIND=rp) :: df        ! - number of cases -
  REAL(KIND=rp) :: xbar,ybar ! - means -
  REAL(KIND=rp) :: sx2,sy2   ! - sums of squares -
  REAL(KIND=rp) :: s2        ! - product of sums of squares -
  REAL(KIND=rp) :: sxy       ! - sum of cross products -
  REAL(KIND=rp) :: sclx      ! - X scaling -
  REAL(KIND=rp) :: scly      ! - Y scaling -
!
! Functions and subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SQRT
  INTRINSIC SUM
!
! Executable Statements
!
! Calculate number of cases
  df=REAL(n,KIND=rp)
!
! Calculate correlation
  xbar=SUM(x(1:n))/df
  ybar=SUM(y(1:n))/df
  sx2=SUM((x(1:n)-xbar)**2)
  sy2=SUM((y(1:n)-ybar)**2)
  IF ((sx2>eps).AND.(sy2>eps)) THEN
     sxy=SUM((x(1:n)-xbar)/SQRT(sx2)*(y(1:n)-ybar)/SQRT(sy2))
     p_corr=sxy
  ELSE ! - rescale if variances are small -
     imx=magnitude(sx2)
     imy=magnitude(sy2)
     IF (imx>1) THEN
        sclx=ten**(1-imx)
     ELSE IF (imx<0) THEN
        sclx=ten**(-imx)
     ELSE
        sclx=one
     END IF
     IF (imy>1) THEN
        scly=ten**(1-imy)
     ELSE IF (imy<0) THEN
        scly=ten**(-imy)
     ELSE
        scly=one
     END IF
     sx2=sx2*sclx
     sy2=sy2*scly
     sxy=SUM((x(1:n)-xbar)*SQRT(sclx)*(y(1:n)-ybar)*SQRT(scly))
     s2=sx2*sy2
     IF (s2>eps) THEN
        p_corr=sxy/SQRT(s2)
     ELSE
        p_corr=zero
     END IF
  END IF
!
  RETURN
 END FUNCTION p_corr
!
!
!
 FUNCTION s_corr(n,x,y)
!
! Calculates Spearman's rank-order correlation
!
! Modules
  USE numbers, ONLY: six
!
! Function type
  REAL(KIND=rp) :: s_corr
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:) ! - ranks of independent variables -
  REAL(KIND=rp), INTENT(IN) :: y(:) ! - ranks of dependent variables -
!
! Locals
!
! Local scalars
  REAL(KIND=rp) :: df  ! - number of cases -
  REAL(KIND=rp) :: sd2 ! - sum of squared rank differences -
!
! Functions and subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SUM
!
! Executable Statements
!
! Calculate number of cases
  df=REAL(n,KIND=rp)
!
! Calculate correlation
  sd2=SUM((x(1:n)-y(1:n))**2)
  s_corr=one-six*sd2/(df*(df**2-one))
!
  RETURN
 END FUNCTION s_corr
!
!
!
 FUNCTION mserror(n,x,y)
!
! Calculates mean squared-error
!
! Function type
  REAL(KIND=rp) :: mserror
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:) ! - independent variables -
  REAL(KIND=rp), INTENT(IN) :: y(:) ! - dependent variables -
!
! Locals
!
! Local scalars
  REAL(KIND=rp) :: df ! - number of cases -
!
! Functions and subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SUM
!
! Executable Statements
!
! Calculate mean squared-error
  df=REAL(n,KIND=rp)
  mserror=SUM((x(1:n)-y(1:n))**2)/df
!
  RETURN
 END FUNCTION mserror
!
!
!
 FUNCTION maerror(n,x,y)
!
! Calculates mean absolute-error
!
! Function type
  REAL(KIND=rp) :: maerror
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:) ! - independent variables -
  REAL(KIND=rp), INTENT(IN) :: y(:) ! - dependent variables -
!
! Locals
!
! Local scalars
  REAL(KIND=rp) :: df ! - number of cases -
!
! Functions and subroutines
!
! Intrinsic functions
  INTRINSIC ABS
  INTRINSIC REAL
  INTRINSIC SUM
!
! Executable Statements
!
! Calculate mean absolute-error
  df=REAL(n,KIND=rp)
  maerror=SUM(ABS(x(1:n)-y(1:n)))/df
!
  RETURN
 END FUNCTION maerror
!
!
!
 FUNCTION var_ratio(n,x,y)
!
! Calculates ratio of forecasts to observations
!
! Modules
  USE numbers, ONLY: eps
!
! Function type
  REAL(KIND=rp) :: var_ratio
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:) ! - forecasts -
  REAL(KIND=rp), INTENT(IN) :: y(:) ! - observations -
!
! Locals
!
! Local scalars
  REAL(KIND=rp) :: df   ! - number of cases -
  REAL(KIND=rp) :: xbar ! - mean -
  REAL(KIND=rp) :: ybar ! - mean -
  REAL(KIND=rp) :: xss  ! - sum of squares -
  REAL(KIND=rp) :: yss  ! - sum of squares -
!
! Functions and subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SUM
!
! Executable Statements
!
! Calculate degrees of freedom
  df=REAL(n,KIND=rp)
!
! Calculate variance ratio
  xbar=SUM(x(1:n))/df
  ybar=SUM(y(1:n))/df
  xss=SUM((x(1:n)-xbar)**2)
  yss=SUM((y(1:n)-ybar)**2)
  IF (yss>eps) THEN
     var_ratio=SUM((x(1:n)-xbar)**2)/SUM((y(1:n)-ybar)**2)
  ELSE IF (xss<eps) THEN
     var_ratio=one
  ELSE
     var_ratio=zero
  END IF
!
  RETURN
 END FUNCTION var_ratio
!
!
!
 FUNCTION mbias(n,x,y)
!
! Calculates mean bias
!
! Function type
  REAL(KIND=rp) :: mbias
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:) ! - independent variables -
  REAL(KIND=rp), INTENT(IN) :: y(:) ! - dependent variables -
!
! Locals
!
! Local scalars
  REAL(KIND=rp) :: df ! - number of cases -
!
! Functions and subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SUM
!
! Executable Statements
!
! Calculate mean bias
  df=REAL(n,KIND=rp)
  mbias=(SUM(y(1:n))-SUM(x(1:n)))/df
!
  RETURN
 END FUNCTION mbias
!
!
!
 SUBROUTINE heidke (n,ng,iobs,ifor,cps,hs,hss)
!
! Calculates Heidke skill scores
!
! Modules
  USE numbers, ONLY: eps
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n  ! - number of cases -
  INTEGER, INTENT(IN) :: ng ! - number of categories -
!
! Output scalars
  REAL(KIND=rp), INTENT(OUT) :: hs  ! - hit score -
  REAL(KIND=rp), INTENT(OUT) :: hss ! - hit skill score -
!
! Input arrays
  INTEGER, INTENT(IN) :: iobs(:) ! - verification data -
  INTEGER, INTENT(IN) :: ifor(:) ! - forecast data -
!
  REAL(KIND=rp), INTENT(IN) :: cps(:) ! - climatological probabilities -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - indices -
!
  REAL(KIND=rp) :: ehit ! - expected number of hits -
  REAL(KIND=rp) :: ahit ! - actual number of hits -
  REAL(KIND=rp) :: df   ! - number of cases -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC COUNT
  INTRINSIC REAL
!
! Executable Statements
!
! Calculate expected number of hits from random guessing
  df=REAL(n,KIND=rp)
  ehit=cps(1)**2
  DO i=2,ng
     ehit=ehit+cps(i)**2
  END DO
  ehit=df*ehit
!
! Calculate actual number of hits
  ahit=REAL(COUNT(iobs(1:n)==ifor(1:n)),KIND=rp)
!
! Calculate hit scores
  hs=oneh*ahit/df
!
! Calculate Heidke skill scores
  IF (ahit>=ehit) THEN
     IF (ehit<df) THEN
        hss=oneh*(ahit-ehit)/(df-ehit)
     ELSE
        hss=oneh*(ahit-df)/df
     END IF
  ELSE
     IF (ehit>eps) THEN
        hss=oneh*(ahit-ehit)/ehit
     ELSE
        hss=oneh*ahit/df
     END IF
  END IF
!
  RETURN
 END SUBROUTINE heidke
!
!
!
 SUBROUTINE leps_coeffs (ng,cps,cs)
!
! Calculates coefficients for LEPS tables
!
! Modules
  USE numbers, ONLY: eps
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: ng ! - number of groups -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: cps(:) ! - climatological probabilities (dimension (ng)) -
!
! Input arrays
  REAL(KIND=rp), INTENT(OUT) :: cs(:,:)  ! - LEPS coefficients (dimension (ng,ng)) -
!
! Locals
!
! Local arrays
  REAL(KIND=rp) :: cbnd(ng+1) ! - category boundaries -
!
! Local scalars
  INTEGER :: i,j ! - indices -
!
  REAL(KIND=rp) :: sp ! - sum of perfect scores -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
!
! Executable Statements
!
! Set up bounds
  cbnd(1)=zero
  DO i=1,ng-1
     cbnd(i+1)=cbnd(i)+cps(i)
  END DO
  cbnd(ng+1)=one
!
! Calculate LEPS2 coefficients
  DO i=1,ng
     DO j=1,ng
        cs(j,i)=cint(cbnd(i),cbnd(i+1),cbnd(j),cbnd(j+1))
     END DO
  END DO
!
! Calculate re-scaled scores
! - re-scale perfect scores to sum to nc-1 -
  sp=zero
  DO i=1,ng
     sp=sp+cs(i,i)
  END DO
  IF (sp>eps) cs(:,:)=cs(:,:)*REAL(ng-1)/sp
!
  RETURN
!
 CONTAINS
!
!
  FUNCTION cint (x1,x2,y1,y2)
!
! Modules
  USE numbers, ONLY: two,three
!
! Function type
  REAL(KIND=rp) :: cint
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: x1,x2,y1,y2 ! - limits -
!
! Executable Statements
!
! Calculate score
  cint=(x2-x1)*(y2-y1)*two/three   &
      +(y2-y1)*(x2**3-x1**3)/three &
      +(x2-x1)*(y2**3-y1**3)/three
!
! Check for overlap
  IF (y1>=x2) THEN
     cint=cint-(x2-x1)*(y2**2-y1**2)
  ELSE IF (y2<=x1) THEN
     cint=cint-(y2-y1)*(x2**2-x1**2)
  ELSE
     cint=cint+two*(x1*x2**2-two*x2**3/three-x1**3/three)
  END IF
!
  RETURN
  END FUNCTION cint
 END SUBROUTINE leps_coeffs
!
!
!
 FUNCTION lepscat(n,iobs,ifor,cs)
!
! Calculates LEPS score for categorical forecasts
!
! Modules
  USE numbers, ONLY: eps
!
! Function type
  REAL(KIND=rp) :: lepscat
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
! Input arrays
  INTEGER, INTENT(IN) :: iobs(:) ! - verification categories -
  INTEGER, INTENT(IN) :: ifor(:) ! - forecast categories -
!
  REAL(KIND=rp), INTENT(IN) :: cs(:,:) ! - LEPS coefficients -
!
! Locals
!
! Local scalars
  INTEGER :: k ! - case index -
!
  REAL(KIND=rp) :: sf ! - score -
  REAL(KIND=rp) :: sp ! - perfect score -
!
! Executable Statements
!
! Calculate LEPS score
  sf=zero
  sp=zero
  DO k=1,n
     sf=sf+cs(iobs(k),ifor(k))
     sp=sp+cs(iobs(k),iobs(k))
  END DO
!
  IF (sp>eps) THEN
     lepscat=sf*oneh/sp
  ELSE
     lepscat=zero
  END IF
!
  RETURN
 END FUNCTION lepscat
!
!
!
 SUBROUTINE gerrity_coeffs (ng,n,iobs,cs)
!
! Calculates coefficients for Gerrity tables
!
! Modules
  USE numbers, ONLY: eps
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: ng ! - number of groups -
  INTEGER, INTENT(IN) :: n  ! - number of cases -
!
! Input arrays
  INTEGER, INTENT(IN) :: iobs(:) ! - observed categories -
!
! Input arrays
  REAL(KIND=rp), INTENT(OUT) :: cs(:,:)  ! - Gerrity coefficients (dimension (ng,ng)) -
!
! Locals
!
! Local arrays
  REAL(KIND=rp) :: ps(ng) ! - sample probablities -
  REAL(KIND=rp) :: a(ng)  ! - weights -
!
! Local scalars
  INTEGER :: i,j ! - indices -
!
  REAL(KIND=rp) :: df    ! - number of tresholds -
  REAL(KIND=rp) :: asum  ! - sum of weights -
  REAL(KIND=rp) :: aisum ! - sum of inverse of weights -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SUM
!
! Executable Statements
!
! Calculate sample probablities
  ps(:)=zero
  DO i=1,n
     ps(iobs(i))=ps(iobs(i))+one
  END DO
  DO j=2,ng-1
     ps(j)=ps(j)+ps(j-1)
  END DO
  ps(:)=ps(:)/REAL(n,KIND=rp)
!
! Calculate weights
  df=REAL(ng-1,KIND=rp)
  asum=zero
  DO i=1,ng-1
     IF (ps(i)>eps) THEN
        a(i)=(one-ps(i))/ps(i)
     ELSE
        a(i)=zero
     END IF
     asum=asum+a(i)
  END DO
  a(ng)=zero
!
! Calculate Gerrity coefficients
! - diagonals -
  aisum=zero
  DO i=1,ng-1
     cs(i,i)=(aisum+asum)/df
     asum=asum-a(i)
     IF (a(i)>eps) aisum=aisum+one/a(i)
  END DO
  cs(ng,ng)=(aisum+asum)/df
! - off-diagonals -
  aisum=zero
  DO i=1,ng-1
     asum=SUM(a(i+1:ng))
     DO j=i+1,ng
        cs(j,i)=(aisum+asum-REAL(j-i,KIND=rp))/df
        cs(i,j)=cs(j,i)
        asum=asum-a(j)
     END DO
     IF (a(i)>eps) aisum=aisum+one/a(i)
  END DO
!
  RETURN
 END SUBROUTINE gerrity_coeffs
!
!
!
 FUNCTION gerrity(n,iobs,ifor,gs)
!
! Calculates Gerrity score for categorical forecasts
!
! Function type
  REAL(KIND=rp) :: gerrity
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
! Input arrays
  INTEGER, INTENT(IN) :: iobs(:) ! - verification categories -
  INTEGER, INTENT(IN) :: ifor(:) ! - forecast categories -
!
  REAL(KIND=rp), INTENT(IN) :: gs(:,:) ! - Gerrity coefficients -
!
! Locals
!
! Local scalars
  INTEGER :: k ! - case index -
!
! Executable Statements
!
! Calculate Gerrity score
  gerrity=zero
  DO k=1,n
     gerrity=gerrity+gs(iobs(k),ifor(k))
  END DO
  gerrity=gerrity*oneh/REAL(n,KIND=rp)
!
  RETURN
 END FUNCTION gerrity
!
!
!
 SUBROUTINE roc (n,ig,iobs,rnkf,roca,hit,far)
!
! Calculates relative operating characteristics (ROC) from ordinal forecasts
!
! Modules
  USE numbers, ONLY: half
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n  ! - number of forecasts -
  INTEGER, INTENT(IN) :: ig ! - category of interest -
!
! Output scalars
  REAL(KIND=rp), INTENT(OUT) :: roca ! - ROC area -
!
! Input arrays
  INTEGER, INTENT(IN) :: iobs(:) ! - verification categories -
!
  REAL(KIND=rp), INTENT(IN) :: rnkf(:) ! - ranked forecasts -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: hit(:) ! - hit rate -
  REAL(KIND=rp), INTENT(OUT) :: far(:) ! - false alarm rate -
!
! Locals
!
! Local scalars
  INTEGER :: k    ! - indices -
  INTEGER :: nes  ! - number of events -
  INTEGER :: indx ! - rank -
!
  REAL(KIND=rp) :: dhit ! - incremental hit rate -
  REAL(KIND=rp) :: dfar ! - incremental false alarm rate -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC COUNT
  INTRINSIC NINT
  INTRINSIC REAL
!
! Executable Statements
!
! Calculate number of events
  nes=COUNT(iobs(1:n)==ig)
!
! Initialise
  hit(1:n)=zero
  far(1:n)=zero
  IF (nes>0) THEN
     dhit=one/REAL(nes,KIND=rp)
  ELSE
     dhit=zero
  END IF
  IF (nes<n) THEN
     dfar=one/REAL(n-nes,KIND=rp)
  ELSE
     dfar=zero
  END IF
  roca=zero
!
! Calculate hit and false alarm rates
  SELECT CASE (ig)
   CASE (1)
     DO k=1,n
        indx=NINT(rnkf(k))
        IF (iobs(k)==ig) THEN ! hit
           hit(indx:n)=hit(indx:n)+dhit
        ELSE                  ! false alarm
           far(indx:n)=far(indx:n)+dfar
        END IF
     END DO
   CASE (ng)
     DO k=1,n
        indx=n+1-NINT(rnkf(k))
        IF (iobs(k)==ig) THEN ! hit
           hit(indx:n)=hit(indx:n)+dhit
        ELSE                  ! false alarm
           far(indx:n)=far(indx:n)+dfar
        END IF
     END DO
  END SELECT
!
! Calculate skill area
  roca=half*hit(1)*far(1)
  DO k=2,n
     roca=roca+half*(hit(k-1)+hit(k))*(far(k)-far(k-1))
  END DO
!
  RETURN
 END SUBROUTINE roc
!
!
!
 SUBROUTINE rocp (nf,ng,nb,iobs,fps,roca,hit,far)
!
! Calculates ROC given probabilistic forecasts.
!
! Modules
  USE numbers, ONLY: half
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nf ! - number of forecasts -
  INTEGER, INTENT(IN) :: ng ! - number of categories -
  INTEGER, INTENT(IN) :: nb ! - number of probability bins -
!
! Input arrays
  INTEGER, INTENT(IN) :: iobs(:) ! - observed categories -
!
  REAL(KIND=rp), INTENT(IN) :: fps(:,:) ! - forecast probabilities -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: roca(:)  ! - ROC area -
  REAL(KIND=rp), INTENT(OUT) :: hit(:,:) ! - hit rates -
  REAL(KIND=rp), INTENT(OUT) :: far(:,:) ! - false-alarm rates -
!
! Locals
!
! Local scalars
  INTEGER :: i   ! - category index -
  INTEGER :: j   ! - probability threshold index -
  INTEGER :: k   ! - forecast index -
  INTEGER :: nev ! - number of events 
!
  REAL(KIND=rp) :: pbt ! - number of probability bin thresholds -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC NINT
  INTRINSIC REAL
!
! Executable Statements
!
! Calculate ROC
  pbt=REAL(nb-1,KIND=rp)/SUM(fps(1,:))    ! - calculate scaling factor -
  DO i=1,ng
     nev=COUNT(iobs(1:nf)==i)             ! - calculate number of events -
     IF ((nev>0).AND.(nev<nf)) THEN
        far(1:nb,i)=zero
        hit(1:nb,i)=zero
        DO k=1,nf                         ! - repeat for each forecast -
           j=nb-NINT(fps(k,i)*pbt)        ! - identify probability bin -
           IF (iobs(k)==i) THEN
              hit(j:nb,i)=hit(j:nb,i)+one ! - accumulate hit rates -
           ELSE
              far(j:nb,i)=far(j:nb,i)+one ! - accumulate false-alarm rates -
           END IF
        END DO
        hit(1:nb,i)=hit(1:nb,i)/REAL(nev,KIND=rp)
        far(1:nb,i)=far(1:nb,i)/REAL(nf-nev,KIND=rp)
!
! Calculate skill area
        roca(i)=half*hit(1,i)*far(1,i)
        DO j=2,nb
           roca(i)=roca(i)+half*(hit(j-1,i)+hit(j,i))*(far(j,i)-far(j-1,i))
        END DO
!
! No events / non-events
     ELSE
        DO j=1,nb
           hit(j,i)=REAL(j-1,KIND=rp)/REAL(nb-1,KIND=rp)
           far(j,i)=hit(j,i)
        END DO
        roca(i)=half
     END IF
  END DO
!
  RETURN
 END SUBROUTINE rocp
!
!
!
 SUBROUTINE calc_scores (n,x,y,ifor,iobs,cps,scores,ornk)
!
! Modules
  USE analysis, ONLY: prog,dprog
  USE arrays,   ONLY: hit,far,rnkf,rnko=>dwk, &
                      rank_data
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
! Input arrays
  INTEGER, INTENT(IN) :: ifor(:) ! - forecast categories -
  INTEGER, INTENT(IN) :: iobs(:) ! - observed categories -
!
  REAL(KIND=rp), INTENT(IN) :: x(:)   ! - forecasts -
  REAL(KIND=rp), INTENT(IN) :: y(:)   ! - observations -
  REAL(KIND=rp), INTENT(IN) :: cps(:) ! - climatological probabilities -
!
! - optional input arrays -
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: ornk(:) ! - observed ranks -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: scores(:) ! - scores -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - score index -
!
  REAL(KIND=rp) :: ddprog ! - progress meter increment -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC PRESENT
!
! Executable Statements
!
! Calculate progress increment
  IF (PRESENT(ornk)) THEN
     ddprog=dprog/REAL(nscore,KIND=rp)
  ELSE
     ddprog=dprog/REAL(nscore+1,KIND=rp)
  END IF
!
! Calculate ranks
! - observations -
  IF (PRESENT(ornk)) THEN
     rnko(1:n)=ornk(1:n)
  ELSE
     CALL rank_data (y,n,'a',rnko)
     prog=prog+ddprog
  END IF
! - forecasts -
  CALL rank_data (x,n,'a',rnkf)
  prog=prog+ddprog
!
! Calculate scores
! - Pearson's correlation -
  i=1
  scores(i)=p_corr(n,x,y)
  prog=prog+ddprog
! - Spearman's correlation -
  i=i+1
  scores(i)=s_corr(n,rnkf,rnko)
  prog=prog+ddprog
! - 2AFC -
  i=i+1
  scores(i)=two_afc_cc(n,y,x)
  prog=prog+ddprog
! - % variance -
  i=i+1
  scores(i)=oneh*scores(1)**2
  prog=prog+ddprog
! - variance ratio -
  i=i+1
  scores(i)=var_ratio(n,x,y)
  prog=prog+ddprog
! - mean bias -
  i=i+1
  scores(i)=mbias(n,y,x)
  prog=prog+ddprog
! - root mean squared-error -
  i=i+1
  scores(i)=SQRT(mserror(n,y,x))
  prog=prog+ddprog
! - mean absolute error -
  i=i+1
  scores(i)=maerror(n,y,x)
  prog=prog+ddprog
! - hit and hit skill scores -
  i=i+1
  CALL heidke (n,ng,iobs,ifor,cps,scores(i),scores(i+1))
  prog=prog+ddprog
! - LEPS scores -
  i=i+2
  CALL leps_coeffs (ng,cps,cs)
  scores(i)=lepscat(n,iobs,ifor,cs)
  prog=prog+ddprog
! - Gerrity score -
  i=i+1
  CALL gerrity_coeffs (ng,n,iobs,gs)
  scores(i)=gerrity(n,iobs,ifor,gs)
  prog=prog+ddprog
! - 2AFC (categorical forecasts) -
  i=i+1
  scores(i)=two_afc_mm(n,ng,iobs,ifor)
  prog=prog+ddprog
! - 2AFC (continuous forecasts) -
  i=i+1
  scores(i)=two_afc_mc(n,ng,iobs,x)
  prog=prog+ddprog
! - ROC areas -
  i=i+1
  CALL roc (n,1,iobs,rnkf,scores(i),hit(1:n,1),far(1:n,1))
  roca(1)=scores(i)
  prog=prog+ddprog
  i=i+1
  CALL roc (n,ng,iobs,rnkf,scores(i),hit(1:n,2),far(1:n,2))
  roca(2)=scores(i)
  prog=prog+ddprog
!
  RETURN
 END SUBROUTINE calc_scores
!
!
!
 SUBROUTINE calc_rel (n,m,ng,nb,iobs,fps,ifq,afp,orf)
!
! Calculates results for a reliability diagram
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n  ! - number of forecasts -
  INTEGER, INTENT(IN) :: m  ! - number of gridpoints/stations -
  INTEGER, INTENT(IN) :: ng ! - number of categories -
  INTEGER, INTENT(IN) :: nb ! - number of probability bins -
!
! Input arrays
  INTEGER, INTENT(IN) :: iobs(:,:) ! - observed categories -
!
  REAL(KIND=rp), INTENT(IN) :: fps(:,:,:) ! - forecast probabilities -
!
! Output arrays
  INTEGER, INTENT(OUT) :: ifq(:,0:) ! - probability bin frequencies -
!
  REAL(KIND=rp), INTENT(OUT) :: afp(:,0:) ! - average bin probability -
  REAL(KIND=rp), INTENT(OUT) :: orf(:,0:) ! - observed relative frequencies -
!
! Locals
!
! Local scalars
  INTEGER :: ic ! - category index -
  INTEGER :: ib ! - probability bin index -
  INTEGER :: j  ! - gridpoint/station index -
  INTEGER :: k  ! - forecast index -
!
  REAL(KIND=rp) :: pbt ! - number of probability bin thresholds -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC NINT
  INTRINSIC REAL
!
! Executable Statements
!
! Calculate scaling factor for identifying probability bin
  pbt=REAL(nb-1,KIND=rp)/oneh
!
! Calculate sample conditional relative frequencies
  DO ic=1,ng                                                   ! - repeat for each category -
     afp(1:nb,ic)=zero
     orf(1:nb,ic)=zero
     ifq(1:nb,ic)=0
     DO k=1,n                                                  ! - repeat for each forecast -
        DO j=1,m                                               ! - repeat for each gridpoint/station -
           ib=1+NINT(fps(j,k,ic)*pbt)                          ! - identify probability bin -
           afp(ib,ic)=afp(ib,ic)+fps(j,k,ic)                   ! - calculate average forecast probability in each bin -
           ifq(ib,ic)=ifq(ib,ic)+1                             ! - count frequency for each bin -
           IF (iobs(j,k)==ic) orf(ib,ic)=orf(ib,ic)+one        ! - count observed frequencies -
        END DO
     END DO
     DO ib=1,nb                                                ! - repeat for each probability bin -
        IF (ifq(ib,ic)>0) THEN
           orf(ib,ic)=oneh*orf(ib,ic)/REAL(ifq(ib,ic),KIND=rp) ! - calculate observed relative frequencies -
           afp(ib,ic)=afp(ib,ic)/REAL(ifq(ib,ic),KIND=rp)      ! - calculate average forecast probability in each bin -
        ELSE
           orf(ib,ic)=-one
           afp(ib,ic)=-one
        END IF
     END DO
  END DO
!
! Calculate average
  ifq(1:nb,0)=SUM(ifq(1:nb,1:ng),DIM=2)
  DO ib=1,nb
     IF (ifq(ib,0)>0) THEN
        afp(ib,0)=SUM(afp(ib,1:ng)*ifq(ib,1:ng))/REAL(ifq(ib,0),KIND=rp)
        orf(ib,0)=SUM(orf(ib,1:ng)*ifq(ib,1:ng))/REAL(ifq(ib,0),KIND=rp)
     ELSE
        afp(ib,0)=-one
        orf(ib,0)=-one
     END IF
  END DO
!
  RETURN
 END SUBROUTINE calc_rel
!
!
!
 SUBROUTINE hbrier (n,m,ng,iobs,fps,cps,bs,bss)
!
! Calculates half-Brier score and skill score against a fixed probability
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n  ! - number of forecasts -
  INTEGER, INTENT(IN) :: m  ! - number of gridpoints/stations -
  INTEGER, INTENT(IN) :: ng ! - number of categories -
!
! Input arrays
  INTEGER, INTENT(IN) :: iobs(:,:) ! - observed data -
!
  REAL(KIND=rp), INTENT(IN) :: fps(:,:,:) ! - forecast probabilities -
  REAL(KIND=rp), INTENT(IN) :: cps(:,:)   ! - constant probabilities for skill score -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: bs(:)  ! - Brier score -
  REAL(KIND=rp), INTENT(OUT) :: bss(:) ! - Brier skill score -
!
! Locals
!
! Local scalars
  INTEGER :: ic ! - current category -
  INTEGER :: j  ! - gridpoint/station index -
  INTEGER :: k  ! - case index -
!
  REAL(KIND=rp) :: fpss ! - forecast probability scaling -
  REAL(KIND=rp) :: cpss ! - climatological probability scaling -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SUM
!
! Executable Statements
!
! Confirm probability scaling
  fpss=SUM(fps(1,1,:))
  cpss=SUM(cps(1,:))
!
! Calculate squared probability error
  DO ic=1,ng
     bs(ic)=zero
     bss(ic)=zero
     DO k=1,n
        DO j=1,m
           IF (iobs(j,k)==ic) THEN
              bs(ic)=bs(ic)+(fps(j,k,ic)/fpss-one)**2
              bss(ic)=bss(ic)+(cps(j,ic)/cpss-one)**2
           ELSE
              bs(ic)=bs(ic)+(fps(j,k,ic)/fpss)**2
              bss(ic)=bss(ic)+(cps(j,ic)/cpss)**2
           END IF
        END DO
     END DO
     bs(ic)=bs(ic)/REAL(n*m,KIND=rp)
     bss(ic)=bss(ic)/REAL(n*m,KIND=rp)
!
! Calculate skill
     IF (bss(ic)>zero) THEN
        IF (bss(ic)>bs(ic))  THEN
           bss(ic)=one-bs(ic)/bss(ic)
        ELSE
           bss(ic)=(bss(ic)-bs(ic))/(one-bss(ic))
        END IF
     ELSE
        bss(ic)=(bss(ic)-bs(ic))/(one-bss(ic))
     END IF
  END DO
  bs(:)=bs(:)*oneh
  bss(:)=bss(:)*oneh
!
  RETURN
 END SUBROUTINE hbrier
!
!
!
 SUBROUTINE ranked_prob_score (n,m,ng,iobs,fps,cps,rps,rpss)
!
! Calculates ranked probability score and skill score
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n  ! - number of forecasts -
  INTEGER, INTENT(IN) :: m  ! - number of gridpoints/stations -
  INTEGER, INTENT(IN) :: ng ! - number of categories -
!
! Output scalars
  REAL(KIND=rp), INTENT(OUT) :: rps  ! - ranked probability score -
  REAL(KIND=rp), INTENT(OUT) :: rpss ! - ranked probability skill score -
!
! Input arrays
  INTEGER, INTENT(IN) :: iobs(:,:) ! - observed data -
!
  REAL(KIND=rp), INTENT(IN) :: fps(:,:,:) ! - forecast probabilities -
  REAL(KIND=rp), INTENT(IN) :: cps(:,:)   ! - constant probabilities for skill score -
!
! Locals
!
! Local scalars
  INTEGER :: ic ! - current category -
  INTEGER :: j  ! - gridpoint/station index -
  INTEGER :: k  ! - case index -
  INTEGER :: nt ! - number of thresholds -
!
  REAL(KIND=rp) :: fpss ! - forecast probability scaling -
  REAL(KIND=rp) :: cpss ! - climatological probability scaling -
  REAL(KIND=rp) :: cpo  ! - cumulative probability of observations -
  REAL(KIND=rp) :: cpf  ! - cumulative probability of forecasts -
  REAL(KIND=rp) :: cpc  ! - cumulative probability of constant forecasts -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SUM
!
! Executable Statements
!
! Confirm probability scaling
  fpss=SUM(fps(1,1,:))
  cpss=SUM(cps(1,:))
!
! Calculate cumulative probabilities
  nt=ng-1
  rps=zero
  rpss=zero
  DO k=1,n
     DO j=1,m
        cpo=zero
        cpf=zero
        cpc=zero
        DO ic=1,nt
           IF (iobs(j,k)==ic) cpo=one
           cpf=cpf+fps(j,k,ic)/fpss
           cpc=cpc+cps(j,ic)/cpss
!
! Calculate squared probability errors
           rps=rps+(cpf-cpo)**2
           rpss=rpss+(cpc-cpo)**2
        END DO
     END DO
  END DO
  rps=rps/REAL(n*ng*m,KIND=rp)
  rpss=rpss/REAL(n*ng*m,KIND=rp)
!
! Calculate skill
  IF (rpss>zero) THEN
     IF (rpss>rps)  THEN
        rpss=one-rps/rpss
     ELSE
        rpss=(rpss-rps)/(one-rpss)
     END IF
  ELSE
     rpss=(rpss-rps)/(one-rpss)
  END IF
  rps=rps*oneh
  rpss=rpss*oneh
!
  RETURN
 END SUBROUTINE ranked_prob_score
!
!
!
 SUBROUTINE profits (n,m,iobs,fps,cps,cump,eir)
!
! Calculates cumulative profits
!
! Modules
  USE arrays, ONLY: cp=>dwk
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of forecasts -
  INTEGER, INTENT(IN) :: m ! - number of gridpoints/stations -
!
! Input arrays
  INTEGER, INTENT(IN) :: iobs(:,:) ! - observed categories -
!
  REAL(KIND=rp), INTENT(IN) :: fps(:,:,:) ! - forecast probabilities -
  REAL(KIND=rp), INTENT(IN) :: cps(:,:)   ! - climatological probabilities -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: cump(0:) ! - cumulative profits -
  REAL(KIND=rp), INTENT(OUT) :: eir(0:)  ! - effective interest rates -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - location index -
  INTEGER :: k ! - forecast index -
!
  REAL(KIND=rp) :: df   ! - number of locations -
  REAL(KIND=rp) :: fpss ! - forecast probability scaling -
  REAL(KIND=rp) :: cpss ! - climatological probability scaling -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SUM
!
! Executable Statements
!
! Confirm probability scaling
  fpss=SUM(fps(1,1,:))
  cpss=SUM(cps(1,:))
!
! Calculate cumulative profits
  cump(0)=zero
  eir(0)=zero
  cp(1:m)=one
  df=REAL(m,KIND=rp)
  DO k=1,n
     DO i=1,m
        cp(i)=cp(i)*(fps(i,k,iobs(i,k))/fpss)/(cps(i,iobs(i,k))/cpss)
     END DO
     cump(k)=SUM(cp(1:m))/df-one
!
! Calculate effective interest rates
     eir(k)=(((cump(k)+one)**(one/REAL(k,KIND=rp)))-one)*oneh
  END DO
!
  RETURN
 END SUBROUTINE profits
!
!
!
 FUNCTION effective_interest(n,m,iobs,fps,cps)
!
! Calculates effective interest rate
!
! Function type
  REAL(KIND=rp) :: effective_interest
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of forecasts -
  INTEGER, INTENT(IN) :: m ! - number of gridpoints/stations -
!
! Input arrays
  INTEGER, INTENT(IN) :: iobs(:,:) ! - observed categories -
!
  REAL(KIND=rp), INTENT(IN) :: fps(:,:,:) ! - forecast probabilities -
  REAL(KIND=rp), INTENT(IN) :: cps(:,:)   ! - climatological probabilities -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - location index -
  INTEGER :: k ! - forecast index -
!
  REAL(KIND=rp) :: cump ! - cumulative profits -
  REAL(KIND=rp) :: cp   ! - cumulative profit -
  REAL(KIND=rp) :: fpss ! - forecast probability scaling -
  REAL(KIND=rp) :: cpss ! - climatological probability scaling -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SUM
!
! Executable Statements
!
! Confirm probability scaling
  fpss=SUM(fps(1,1,:))
  cpss=SUM(cps(1,:))
!
! Calculate cumulative profits
  cump=zero
  DO i=1,m
     cp=one
     DO k=1,n
        cp=cp*(fps(i,k,iobs(i,k))/fpss)/(cps(i,iobs(i,k))/cpss)
     END DO
     cump=cump+cp
  END DO
  cump=cump/REAL(m,KIND=rp)-one
!
! Calculate effective interest rates
  effective_interest=(((cump+one)**(one/REAL(n,KIND=rp)))-one)*oneh
!
  RETURN
 END FUNCTION effective_interest
!
!
!
 FUNCTION linear_prob(n,m,iobs,fps)
!
! Calculates linear probability score
!
! Function type
  REAL(KIND=rp) :: linear_prob
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of forecasts -
  INTEGER, INTENT(IN) :: m ! - number of gridpoints/stations -
!
! Input arrays
  INTEGER, INTENT(IN) :: iobs(:,:) ! - observed categories -
!
  REAL(KIND=rp), INTENT(IN) :: fps(:,:,:) ! - forecast probabilities -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - location index -
  INTEGER :: k ! - forecast index -
!
  REAL(KIND=rp) :: fpss ! - forecast probability scaling -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SUM
!
! Executable Statements
!
! Confirm probability scaling
  fpss=SUM(fps(1,1,:))
!
! Calculate cumulative profits
  linear_prob=zero
  DO k=1,n
     DO i=1,m
        linear_prob=linear_prob+fps(i,k,iobs(i,k))/fpss
     END DO
  END DO
  linear_prob=oneh*linear_prob/REAL(n*m,KIND=rp)
!
  RETURN
 END FUNCTION linear_prob
!
!
!
 SUBROUTINE two_afc_2p (n,m,ng,iobs,fps,afc)
!
! Calculates 2AFC test score for dichotomous observations, discrete probabilistic forecasts
!
! Modules
  USE numbers, ONLY: half,fifty
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n  ! - number of cases -
  INTEGER, INTENT(IN) :: m  ! - number of locations -
  INTEGER, INTENT(IN) :: ng ! - number of forecast categories -
!
! Input arrays
  INTEGER, INTENT(IN) :: iobs(:,:) ! - observations -
!
  REAL(KIND=rp), INTENT(IN) :: fps(:,:,:) ! - forecast probabilities -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: afc(:) ! - 2AFC scores -
!
! Locals
!
! Local arrays
  INTEGER :: nn(ng) ! - total number of pairings -
!
! Local scalars
  INTEGER :: i1  ! - first location -
  INTEGER :: i2  ! - second location -
  INTEGER :: io1 ! - category of first case -
  INTEGER :: io2 ! - category of second case -
  INTEGER :: j   ! - category index -
  INTEGER :: k1  ! - first case index -
  INTEGER :: k2  ! - second case index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
!
! Executable Statements
!
! Calculate 2afc test score
  afc(:)=zero
  nn(:)=0
  DO i1=1,m
     DO k1=1,n
        io1=iobs(i1,k1)
        DO i2=i1,m
           DO k2=1,n
              IF ((i2==i1).AND.(k2<=k1)) CYCLE
              io2=iobs(i2,k2)
              IF (io2==io1) CYCLE
              nn(io1)=nn(io1)+1
              IF (fps(i1,k1,io1)>fps(i2,k2,io1)) THEN
                 afc(io1)=afc(io1)+one
              ELSE IF (.NOT.fps(i1,k1,io1)<fps(i2,k2,io1)) THEN
                 afc(io1)=afc(io1)+half
              END IF
              nn(io2)=nn(io2)+1
              IF (fps(i1,k1,io2)<fps(i2,k2,io2)) THEN
                 afc(io2)=afc(io2)+one
              ELSE IF (.NOT.fps(i1,k1,io2)>fps(i2,k2,io2)) THEN
                 afc(io2)=afc(io2)+half
              END IF
           END DO
        END DO
     END DO
  END DO
  DO j=1,ng
     IF (nn(j)>0) THEN
        afc(j)=oneh*afc(j)/REAL(nn(j),KIND=rp)
     ELSE
        afc=fifty
     END IF
  END DO
!
  RETURN
 END SUBROUTINE two_afc_2p
!
!
!
 FUNCTION two_afc_mm(n,ng,iobs,ifor) RESULT(afc)
!
! Calculates 2AFC test score for polychotomous observations, polychotomous forecasts
!
! Modules
  USE numbers, ONLY: half,fifty
!
! Function type
  REAL(KIND=rp) :: afc
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n  ! - number of cases -
  INTEGER, INTENT(IN) :: ng ! - number of categories -
!
! Input arrays
  INTEGER, INTENT(IN) :: iobs(:) ! - observations. dimension (n) -
  INTEGER, INTENT(IN) :: ifor(:) ! - forecasts. dimension (n) -
!
! Locals
!
! Local arrays
  INTEGER :: nn(ng,0:ng) ! - numbers of forecasts per category -
!
! Local scalars
  INTEGER :: i  ! - category index -
  INTEGER :: j  ! - category index -
  INTEGER :: k  ! - case index -
  INTEGER :: l  ! - case index -
  INTEGER :: ne ! - number of pairings -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
!
! Executable Statements
!
! Construct contingency table
  nn(:,:)=0
  DO k=1,n
     nn(iobs(k),ifor(k))=nn(iobs(k),ifor(k))+1
  END DO
  nn(:,0)=SUM(nn(:,1:),DIM=2)
!
! Calculate 2afc test score
  IF (COUNT(nn(:,0)>0)>1) THEN
     ne=0
     afc=zero
     DO k=1,ng-1
        DO l=k+1,ng
           ne=ne+nn(k,0)*nn(l,0)
           DO i=1,ng-1
              DO j=i+1,ng
                 afc=afc+REAL(nn(k,i)*nn(l,j),KIND=rp)
              END DO
              afc=afc+half*REAL(nn(k,i)*nn(l,i),KIND=rp)
           END DO
           afc=afc+half*REAL(nn(k,ng)*nn(l,ng),KIND=rp)
        END DO
     END DO
     afc=oneh*afc/REAL(ne,KIND=rp)
  ELSE
     afc=fifty
  END IF
!
  RETURN
 END FUNCTION two_afc_mm
!
!
!
 FUNCTION two_afc_mc(n,ng,iobs,for) RESULT(afc)
!
! Calculates 2AFC test score for polychotomous observations, continuous forecasts
!
! Modules
  USE arrays,  ONLY: rank_data
  USE numbers, ONLY: fifty
!
! Function type
  REAL(KIND=rp) :: afc
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n  ! - number of cases -
  INTEGER, INTENT(IN) :: ng ! - number of categories -
!
! Input arrays
  INTEGER, INTENT(IN) :: iobs(:) ! - observations. dimension (n) -
!
  REAL(KIND=rp), INTENT(IN) :: for(:) ! - forecasts. dimension (n) -
!
! Locals
!
! Local arrays
  REAL(KIND=rp) :: f(n) ! - partial copy of forecasts -
  REAL(KIND=rp) :: r(n) ! - ranks -
!
! Local scalars
  INTEGER :: i  ! - case index -
  INTEGER :: k  ! - category index -
  INTEGER :: l  ! - category index -
  INTEGER :: ne ! - number of events -
  INTEGER :: nn ! - number of non-events -
  INTEGER :: nt ! - partial number of cases -
  INTEGER :: np ! - total number of pairings -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
!
! Executable Statements
!
! Extract current categories
  np=0
  afc=zero
  DO l=2,ng
     ne=0
     DO i=1,n
        IF (iobs(i)==l) THEN ! - event -
           ne=ne+1
           f(ne)=for(i)
        END IF
     END DO
     DO k=1,l-1
        nn=0
        nt=ne
        DO i=1,n
           IF (iobs(i)==k) THEN ! - non-event -
              nn=nn+1
              nt=nt+1
              f(nt)=for(i)
           END IF
        END DO
!
! Rank forecasts
        CALL rank_data (f,nt,'a',r)
!
! Calculate 2afc test score
        IF ((ne>0).AND.(nn>0)) THEN
           afc=afc+SUM(r(1:ne))-REAL(ne*(ne+1)/2,KIND=rp)
           np=np+ne*nn
        END IF
     END DO
  END DO
  IF (np>0) THEN
     afc=oneh*afc/REAL(np,KIND=rp)
  ELSE
     afc=fifty
  END IF
!
  RETURN
 END FUNCTION two_afc_mc
!
!
!
 FUNCTION two_afc_mp_1(n,ng,iobs,fps) RESULT(afc)
!
! Calculates 2AFC test score for polychotomous observations, probabilistic forecasts
!
! Modules
  USE numbers, ONLY: half,fifty
!
! Function type
  REAL(KIND=rp) :: afc
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n  ! - number of cases -
  INTEGER, INTENT(IN) :: ng ! - number of categories -
!
! Input arrays
  INTEGER, INTENT(IN) :: iobs(:) ! - observations. dimension (n) -
!
  REAL(KIND=rp), INTENT(IN) :: fps(:,:) ! - forecast probabilities. dimensions (n,ng) -
!
! Locals
!
! Local scalars
  INTEGER :: io1 ! - category of first case -
  INTEGER :: io2 ! - category of second case -
  INTEGER :: k1  ! - first case index -
  INTEGER :: k2  ! - second case index -
  INTEGER :: l1  ! - first category index -
  INTEGER :: l2  ! - second category index -
  INTEGER :: nn  ! - total number of pairings -
!
  REAL(KIND=rp) :: p    ! - probability -
  REAL(KIND=rp) :: fpss ! - forecast probability scaling -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SUM
!
! Executable Statements
!
! Confirm probability scaling
  fpss=SUM(fps(1,:))
!
! Calculate 2afc test score
  afc=zero
  nn=0
  DO k1=1,n-1
     io1=iobs(k1)
     DO k2=k1+1,n
        io2=iobs(k2)
        IF (io2==io1) CYCLE
        nn=nn+1
        p=zero
        DO l1=1,ng-1
           DO l2=l1+1,ng
              p=p+fps(k1,l1)*fps(k2,l2)/fpss**2
           END DO
        END DO
        IF (p>half) THEN
           IF (io2>io1) afc=afc+one
        ELSE IF (p<half) THEN
           IF (io2<io1) afc=afc+one
        ELSE
           afc=afc+half
        END IF
     END DO
  END DO
  IF (nn>0) THEN
     afc=oneh*afc/REAL(nn,KIND=rp)
  ELSE
     afc=fifty
  END IF
!
  RETURN
 END FUNCTION two_afc_mp_1
!
!
!
 FUNCTION two_afc_mp_2(n,m,ng,iobs,fps) RESULT(afc)
!
! Calculates 2AFC test score for polychotomous observations, probabilistic forecasts
!
! Modules
  USE numbers, ONLY: half,fifty
!
! Function type
  REAL(KIND=rp) :: afc
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n  ! - number of cases -
  INTEGER, INTENT(IN) :: m  ! - number of locations -
  INTEGER, INTENT(IN) :: ng ! - number of categories -
!
! Input arrays
  INTEGER, INTENT(IN) :: iobs(:,:) ! - observations. dimension (n) -
!
  REAL(KIND=rp), INTENT(IN) :: fps(:,:,:) ! - forecast probabilities. dimensions (n,ng) -
!
! Locals
!
! Local scalars
  INTEGER :: i1  ! - first location -
  INTEGER :: i2  ! - second location -
  INTEGER :: io1 ! - category of first case -
  INTEGER :: io2 ! - category of second case -
  INTEGER :: k1  ! - first case index -
  INTEGER :: k2  ! - second case index -
  INTEGER :: l1  ! - first category index -
  INTEGER :: l2  ! - second category index -
  INTEGER :: nn  ! - total number of pairings -
!
  REAL(KIND=rp) :: p    ! - probability -
  REAL(KIND=rp) :: fpss ! - forecast probability scaling -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SUM
!
! Executable Statements
!
! Confirm probability scaling
  fpss=SUM(fps(1,1,:))
!
! Calculate 2afc test score
  afc=zero
  nn=0
  DO i1=1,m
     DO k1=1,n
        io1=iobs(i1,k1)
        DO i2=i1,m
           DO k2=1,n
              IF ((i2==i1).AND.(k2<=k1)) CYCLE
              io2=iobs(i2,k2)
              IF (io2==io1) CYCLE
              nn=nn+1
              p=zero
              DO l1=1,ng-1
                 DO l2=l1+1,ng
                    p=p+fps(i1,k1,l1)*fps(i2,k2,l2)/fpss**2
                 END DO
              END DO
              IF (p>half) THEN
                 IF (io2>io1) afc=afc+one
              ELSE IF (p<half) THEN
                 IF (io2<io1) afc=afc+one
              ELSE
                 afc=afc+half
              END IF
           END DO
        END DO
     END DO
  END DO
  IF (nn>0) THEN
     afc=oneh*afc/REAL(nn,KIND=rp)
  ELSE
     afc=fifty
  END IF
!
  RETURN
 END FUNCTION two_afc_mp_2
!
!
!
 FUNCTION two_afc_cc(n,obs,for) RESULT(afc)
!
! Calculates 2AFC test score for continuous observations, continuous forecasts
!
! Modules
  USE numbers, ONLY: half,two
!
! Function type
  REAL(KIND=rp) :: afc
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: obs(:) ! - observations. dimension (n) -
  REAL(KIND=rp), INTENT(IN) :: for(:) ! - forecasts. dimension (n) -
!
! Locals
!
! Local scalars
  INTEGER :: k ! - case index -
  INTEGER :: l ! - case index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SIGN
!
! Executable Statements
!
! Calculate 2afc test score
  afc=zero
  DO k=1,n-1
     DO l=k+1,n
        IF ((obs(k)/=obs(l)).OR.(for(k)/=for(l))) THEN
           afc=afc+ABS(SIGN(one,obs(k)-obs(l))+SIGN(one,for(k)-for(l)))/two
        ELSE
           afc=afc+half
        END IF
     END DO
  END DO
  afc=oneh*afc/REAL(n*(n-1)/2,KIND=rp)
!
  RETURN
 END FUNCTION two_afc_cc
!
!
!
 FUNCTION goodness_opts()
!
! Prompts for correlation coefficient to calculate goodness index
!
! Modules
  USE analysis, ONLY: reset
  USE settings, ONLY: igood
!
! Function type
  INTEGER :: goodness_opts
!
! Executable Statements
!
  goodness_opts=0
!
! Check for completed calculations
  IF (reset('Changing the goodness index options')==1) RETURN
!
! Prompt for GCM options
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)',ADVANCE='yes') 'Goodness index options'
1 WRITE (UNIT=*,FMT='(A)') 'Select correlation coefficient for calculating &
                            &goodness index:'
  WRITE (UNIT=*,FMT='(A)',ADVANCE='yes') "1. Pearson's "
  WRITE (UNIT=*,FMT='(A)',ADVANCE='yes') "2. Spearman's "
  WRITE (UNIT=*,FMT='(A)',ADVANCE='yes') "3. Kendall's"
  READ (UNIT=*,FMT=*,ERR=1) igood
  IF ((igood<1).OR.(igood>3)) GOTO 1
!
  RETURN
 END FUNCTION goodness_opts
!
!
!
 FUNCTION goodness(igood,n,m,x,y)
!
! Calculates goodness of fit index, defined depending on the value of igood:
! igood = 1: the average of Pearson's correlations transformed to normal deviates, and then transformed back to a correlation
! igood = 2: the average of Spearman's correlations transformed to normal deviates, and then transformed back to a correlation
! igood = 3: the average of Kendall's correlations
!
! Modules
  USE numbers, ONLY: half,two
!
! Function type
  REAL(KIND=rp) :: goodness
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: igood ! - goodness score flag -
  INTEGER, INTENT(IN) :: n     ! - number of cases -
  INTEGER, INTENT(IN) :: m     ! - number of pairs of variables -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:,:) ! - independent variables -
  REAL(KIND=rp), INTENT(IN) :: y(:,:) ! - dependent variables -
!
! Locals
  INTEGER :: i ! - variable index -
!
  REAL(KIND=rp) :: r ! - correlation -
  REAL(KIND=rp) :: s ! - simple average of correlations -
  REAL(KIND=rp) :: z ! - average of correlations transformed to normal deviates -
!
  LOGICAL :: lperfect ! - perfect correlations flag -
!
! Functions
!
! Intrinsic functions
  INTRINSIC ABS
  INTRINSIC EXP
  INTRINSIC LOG
  INTRINSIC REAL
!
! Executable Statements
!
! Calculate average of scores
  z=zero
  s=zero
  lperfect=.false.
  SELECT CASE (igood)
   CASE (1) ! - Pearson's correlation -
     DO i=1,m
        r=p_corr(n,x(i,1:n),y(i,1:n))
        s=s+r
        IF (ABS(r)<one) THEN
           z=z+half*LOG((one+r)/(one-r))
         ELSE
           lperfect=.true.
        END IF
     END DO
   CASE (2) ! - Spearman's correlation -
     DO i=1,m
        r=s_corr(n,x(i,1:n),y(i,1:n))
        s=s+r
        IF (ABS(r)<one) THEN
           z=z+half*LOG((one+r)/(one-r))
         ELSE
           lperfect=.true.
        END IF
     END DO
   CASE (3) ! - Kendall's correlation -
     DO i=1,m
        r=two_afc_cc(n,x(i,1:n),y(i,1:n))*two/oneh-one
        s=s+r
     END DO
  END SELECT
!
! Revert to correlation
  IF ((.NOT.lperfect).AND.(igood/=3)) THEN
     z=z/REAL(m,KIND=rp)
     z=EXP(two*z)
     goodness=(z-1)/(z+1)
!
! Calculate simple average if any of the correlations are perfect
  ELSE
     s=s/REAL(m,KIND=rp)
     goodness=s
  END IF
!
  RETURN
 END FUNCTION goodness
!
!
!
 SUBROUTINE get_corrs (n,m,x,y,skill)
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - total number of cases -
  INTEGER, INTENT(IN) :: m ! - total number of gridpoints -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:,:) ! - independent data -
  REAL(KIND=rp), INTENT(IN) :: y(:)   ! - independent data -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: skill(:) ! - skill values -
!
! Locals
!
! Local scalars
  INTEGER :: i   ! - grid index -
!
! Executable Statements
!
! Calculate skill
  DO i=1,m
     skill(i)=p_corr(n,x(i,1:n),y(1:n))
  END DO
!
  RETURN
 END SUBROUTINE get_corrs
!
!
!
 SUBROUTINE get_skills (iskill,n,m,x,y,ifor,iobs,cps,skill)
!
! Modules
  USE arrays, ONLY: hit,far,rnko,rnkf, &
                    rank_data
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iskill ! - skill metric identifier -
  INTEGER, INTENT(IN) :: n      ! - total number of cases -
  INTEGER, INTENT(IN) :: m      ! - total number of gridpoints -
!
! Input arrays
  INTEGER, INTENT(IN) :: iobs(:,:) ! - observed categories -
  INTEGER, INTENT(IN) :: ifor(:,:) ! - forecast categories -
!
  REAL(KIND=rp), INTENT(IN) :: x(:,:)   ! - forecasts -
  REAL(KIND=rp), INTENT(IN) :: y(:,:)   ! - observations -
  REAL(KIND=rp), INTENT(IN) :: cps(:,:) ! - climatological proabilities -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: skill(:) ! - skill values -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - index -
!
  REAL(KIND=rp) :: dum ! - dummy argument -
!
! Executable Statements
!
! Calculate skill
! - Pearson's product moment correlation -
  SELECT CASE (iskill)
   CASE (1)
     DO i=1,m
        skill(i)=p_corr(n,x(i,1:n),y(i,1:n))
     END DO
! - Spearman's rank-order correlation -
   CASE (2)
     DO i=1,m
        CALL rank_data (x(i,1:n),n,'a',rnkf)
        CALL rank_data (y(i,1:n),n,'a',rnko)
        skill(i)=s_corr(n,rnkf,rnko)
     END DO
! - 2AFC -
   CASE (3)
     DO i=1,m
        skill(i)=two_afc_cc(n,y(i,1:n),x(i,1:n))
     END DO
! - hit score -
   CASE (4)
     DO i=1,m
        CALL heidke (n,ng,iobs(i,1:n),ifor(i,1:n),cps(i,:),skill(i),dum)
     END DO
! - hit skill score -
   CASE (5)
     DO i=1,m
        CALL heidke (n,ng,iobs(i,1:n),ifor(i,1:n),cps(i,:),dum,skill(i))
     END DO
! - LEPS score -
   CASE (6)
     DO i=1,m
        CALL leps_coeffs (ng,cps(i,:),cs)
        skill(i)=lepscat(n,iobs(i,1:n),ifor(i,1:n),cs)
     END DO
! - Gerrity score -
   CASE (7)
     DO i=1,m
        CALL gerrity_coeffs (ng,n,iobs(i,1:n),gs)
        skill(i)=gerrity(n,iobs(i,1:n),ifor(i,1:n),gs)
     END DO
! - 2AFC (categorical forecasts) -
   CASE (8)
     DO i=1,m
        skill(i)=two_afc_mm(n,ng,iobs(i,1:n),ifor(i,1:n))
     END DO
! - 2AFC (continuous forecasts) -
   CASE (9)
     DO i=1,m
        skill(i)=two_afc_mc(n,ng,iobs(i,1:n),x(i,1:n))
     END DO
! - ROC (below) -
   CASE (10)
     DO i=1,m
        CALL rank_data (x(i,1:n),n,'a',rnkf)
        CALL roc (n,1,iobs(i,1:n),rnkf,skill(i),hit(1:n,1),far(1:n,1))
     END DO
! - ROC (above) -
   CASE (11)
     DO i=1,m
        CALL rank_data (x(i,1:n),n,'a',rnkf)
        CALL roc (n,ng,iobs(i,1:n),rnkf,skill(i),hit(1:n,2),far(1:n,2))
     END DO
  END SELECT
!
  RETURN
 END SUBROUTINE get_skills
!
!
!
 FUNCTION get_mean(n,v,vmiss)
!
! Calculates arithmetic mean
!
! Function type
  REAL(KIND=rp) :: get_mean
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
  REAL(KIND=rp), INTENT(IN) :: vmiss ! - missing value -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: v(:) ! - data -
!
! Locals
!
! Local scalars
  INTEGER :: k   ! - case index -
  INTEGER :: nnm ! - number of non-missing values -
!
! Functions and subroutines
!
! Intrinsic functions
  INTRINSIC REAL
!
! Executable Statements
!
! Calculate mean
  get_mean=zero
  nnm=0
  DO k=1,n
     IF (v(k)/=vmiss) THEN
        nnm=nnm+1
        get_mean=get_mean+v(k)
     END IF
  END DO
  get_mean=get_mean/REAL(nnm,KIND=rp)
!
  RETURN
 END FUNCTION get_mean
!
!
!
 FUNCTION get_median(n,v,vmiss)
!
! Calculates median
!
! Modules
  USE arrays,  ONLY: dwk, &
                     insertion_sort,quantile
  USE numbers, ONLY: half
!
! Function type
  REAL(KIND=rp) :: get_median
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
  REAL(KIND=rp), INTENT(IN) :: vmiss ! - missing value -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: v(:) ! - data -
!
! Locals
!
! Local scalars
  INTEGER :: k   ! - case index -
  INTEGER :: nnm ! - number of non-missing values -
!
! Executable Statements
!
! Calculate mean
  nnm=0
  DO k=1,n
     IF (v(k)/=vmiss) THEN
        nnm=nnm+1
        dwk(nnm)=v(k)
     END IF
  END DO
  CALL insertion_sort (dwk,nnm,'a')
  get_median=quantile(dwk,nnm,half)
!
  RETURN
 END FUNCTION get_median
!
!
!
 FUNCTION get_var(n,v,vbar,vmiss)
!
! Calculates variance
!
! Function type
  REAL(KIND=rp) :: get_var
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
  REAL(KIND=rp), INTENT(IN) :: vbar  ! - mean -
  REAL(KIND=rp), INTENT(IN) :: vmiss ! - missing value -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: v(:) ! - data -
!
! Locals
!
! Local scalars
  INTEGER :: k   ! - case index -
  INTEGER :: nnm ! - number of non-missing values -
!
! Functions and subroutines
!
! Intrinsic functions
  INTRINSIC REAL
!
! Executable Statements
!
! Calculate variance
  get_var=zero
  DO k=1,n
     IF (v(k)/=vmiss) THEN
        nnm=nnm+1
        get_var=get_var+(v(k)-vbar)**2
     END IF
  END DO
  get_var=get_var/REAL(nnm-1,KIND=rp)
!
  RETURN
 END FUNCTION get_var
!
!
!
 SUBROUTINE get_regr (n,x,y,b0,b1)
!
! Estimates regression parameters
!
! Modules
  USE numbers, ONLY: eps
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n   ! - number of cases -
!
! Output scalars
  REAL(KIND=rp), INTENT(OUT) :: b0 ! - regression constant -
  REAL(KIND=rp), INTENT(OUT) :: b1 ! - regression coefficient -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: x(:) ! - independent data -
  REAL(KIND=rp), INTENT(IN) :: y(:) ! - dependent data -
!
! Locals
!
! Local scalars
  REAL(KIND=rp) :: xbar  ! - mean -
  REAL(KIND=rp) :: ybar  ! - mean -
  REAL(KIND=rp) :: df    ! - degrees of freedom -
  REAL(KIND=rp) :: sxx   ! - sum of squares of independent variable -
  REAL(KIND=rp) :: sxy   ! - sum of cross products -
!
! Functions and subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SUM
!
! Executable Statements
!
! Fit regression line
  df=REAL(n)
  xbar=SUM(x(1:n))/df
  ybar=SUM(y(1:n))/df
  sxx=SUM((x(1:n)-xbar)**2)
  sxy=SUM((x(1:n)-xbar)*(y(1:n)-ybar))
  IF (sxx>eps) THEN
     b1=sxy/sxx
     b0=ybar-b1*xbar
  ELSE
     b1=zero
     b0=ybar
  END IF
!
  RETURN
 END SUBROUTINE get_regr
END MODULE statistics
