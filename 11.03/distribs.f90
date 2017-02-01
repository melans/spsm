! $Id: distribs.f90 1215 2011-02-25 21:30:20Z simon $
MODULE distribs
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
 FUNCTION studnt(t,dof)
!
! Algorithm AS 27  Appl. Statist. 19 (1)
!
! Calculate the upper-tail area under Student's t-distribution
!
! Modules
  USE numbers, ONLY: zero,half,one
!
! Function type
  REAL(KIND=rp) :: studnt
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: t   ! - Student's t-deviate -
  REAL(KIND=rp), INTENT(IN) :: dof ! - degrees of freedom -
!
! Locals
!
! Local parameters
  REAL(KIND=rp), PARAMETER :: a1=0.09979441_rp,a2=-0.581821_rp,a3=1.390993_rp,a4=-1.222452_rp,a5=2.151185_rp
  REAL(KIND=rp), PARAMETER :: b1=5.537409_rp,b2=11.42343_rp
  REAL(KIND=rp), PARAMETER :: c1=0.04431742_rp,c2=-0.2206018_rp,c3=-0.03317253_rp,c4=5.679969_rp,c5=-12.96519_rp
  REAL(KIND=rp), PARAMETER :: d1=5.166733_rp,d2=13.49862_rp
  REAL(KIND=rp), PARAMETER :: e1=0.009694901_rp,e2=-0.1408854_rp,e3=1.88993_rp,e4=-12.75532_rp,e5=25.77532_rp
  REAL(KIND=rp), PARAMETER :: f1=4.233736_rp,f2=14.3963_rp
  REAL(KIND=rp), PARAMETER :: g1=-9.187228d-5,g2=0.03789901_rp,g3=-1.280346_rp,g4=9.249528_rp,g5=-19.08115_rp
  REAL(KIND=rp), PARAMETER :: h1=2.777816_rp,h2=16.46132_rp
  REAL(KIND=rp), PARAMETER :: i1=5.79602d-4,i2=-0.02763334_rp,i3=0.4517029_rp,i4=-2.657697_rp,i5=5.127212_rp
  REAL(KIND=rp), PARAMETER :: j1=0.5657187_rp,j2=21.83269_rp
!
! Local scalars
  REAL(KIND=rp) :: abst ! - absolute Student's t-deviate -
  REAL(KIND=rp) :: v    ! - inverse degrees of freedom -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ABS
!
! Executable Statments
!
! Evaluate series
  v=one/dof
  abst=ABS(t)
  studnt=half*(one+abst*(((a1+v*(a2+v*(a3+v*(a4+v*a5))))/(one-v*(b1-v*b2)))+ &
                   abst*(((c1+v*(c2+v*(c3+v*(c4+v*c5))))/(one-v*(d1-v*d2)))+ &
                   abst*(((e1+v*(e2+v*(e3+v*(e4+v*e5))))/(one-v*(f1-v*f2)))+ &
                   abst*(((g1+v*(g2+v*(g3+v*(g4+v*g5))))/(one-v*(h1-v*h2)))+ &
                   abst* ((i1+v*(i2+v*(i3+v*(i4+v*i5))))/(one-v*(j1-v*j2))))))))**(-8)
  IF (t<zero) studnt=one-studnt
!
  RETURN
 END FUNCTION studnt
!
!
!
 FUNCTION studnt2(p,dof)
!
! Algorithm ACM 396  Comm. ACM 13 (10)
!
! Calculates positive quantiles of the Student's t-distribution at the two-tailed probability p
!
! Modules
  USE numbers, ONLY: half,one,two,three,four,five,six,seven,twelve,hpi
!
! Function type
  REAL(KIND=rp) :: studnt2
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: p   ! - two tailed area -
  REAL(KIND=rp), INTENT(IN) :: dof ! - degrees of freedom -
!
! Locals
!
! Local scalars
  REAL(KIND=rp) :: a,b,c,d,x,y ! - constants -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC COS
  INTRINSIC EXP
  INTRINSIC SIN
  INTRINSIC SQRT
!
! Executable Statments
!
! Evaluate quantile
  IF (dof==one) THEN
     a=p*hpi
     studnt2=COS(a)/SIN(a)
  ELSE IF (dof==two) THEN
     studnt2=SQRT(two/(p*(two-p))-two)
  ELSE
     a=one/(dof-half)
     b=48.0_rp/a**2
     c=((20700.0_rp*a/b-98.0_rp)*a-16.0_rp)*a+96.36_rp
     d=((94.5_rp/(b+c)-three)/b+one)*SQRT(a*hpi)*dof
     x=d*p
     y=x**(two/dof)
     IF (y>0.05_rp+a) THEN
        x=normq(p*half)
        y=x**2
        IF (dof<five) c=c+0.3_rp*(dof-4.5_rp)*(x+0.6_rp)
        c=(((0.05_rp*d*x-five)*x-seven)*x-two)*x+b+c
        y=(((((0.4_rp*y+6.3_rp)*y+36.0_rp)*y+94.5_rp)/c-y-three)/b+one)*x
        y=a*y**2
        IF (y>0.002_rp) THEN
           y=EXP(y)-one
        ELSE
           y=((y+four)*y+twelve)*y*y/24.0_rp+y
        END IF
     ELSE
        y=((one/(((dof+six)/(dof*y)-0.089_rp*d-0.822_rp)* &
          (dof+two)*three)+half/(dof+four))*y-one)*(dof+one)/(dof+two)+one/y
     END IF
     studnt2=SQRT(dof*y)
  END IF
!
  RETURN
 END FUNCTION studnt2
!
!
!
 FUNCTION normq(p)
!
! Algorithm AS 241  Appl. Statist. 37 (3)
!
! Produces the normal deviate Z corresponding to a given lower
! tail area of P; Z is accurate to about 1 part in 10**16.
!
! Modules
  USE numbers, ONLY: zero,half,one,five
!
! Function type
  REAL(KIND=rp) :: normq
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: p ! - quantile -
!
! Locals
!
! Local parameters
  REAL(KIND=rp), PARAMETER :: split1=0.425000_rp
  REAL(KIND=rp), PARAMETER :: split2=five
  REAL(KIND=rp), PARAMETER :: const1=0.180625_rp
  REAL(KIND=rp), PARAMETER :: const2=1.600000_rp
!
  REAL(KIND=rp), PARAMETER :: a0=3.3871328727963666080d0  ! - coefficients for P close to 0.5 -
  REAL(KIND=rp), PARAMETER :: a1=1.3314166789178437745d+2 ! - coefficients for P close to 0.5 -
  REAL(KIND=rp), PARAMETER :: a2=1.9715909503065514427d+3 ! - coefficients for P close to 0.5 -
  REAL(KIND=rp), PARAMETER :: a3=1.3731693765509461125d+4 ! - coefficients for P close to 0.5 -
  REAL(KIND=rp), PARAMETER :: a4=4.5921953931549871457d+4 ! - coefficients for P close to 0.5 -
  REAL(KIND=rp), PARAMETER :: a5=6.7265770927008700853d+4 ! - coefficients for P close to 0.5 -
  REAL(KIND=rp), PARAMETER :: a6=3.3430575583588128105d+4 ! - coefficients for P close to 0.5 -
  REAL(KIND=rp), PARAMETER :: a7=2.5090809287301226727d+3 ! - coefficients for P close to 0.5 -
  REAL(KIND=rp), PARAMETER :: b1=4.2313330701600911252d+1 ! - coefficients for P close to 0.5 -
  REAL(KIND=rp), PARAMETER :: b2=6.8718700749205790830d+2 ! - coefficients for P close to 0.5 -
  REAL(KIND=rp), PARAMETER :: b3=5.3941960214247511077d+3 ! - coefficients for P close to 0.5 -
  REAL(KIND=rp), PARAMETER :: b4=2.1213794301586595867d+4 ! - coefficients for P close to 0.5 -
  REAL(KIND=rp), PARAMETER :: b5=3.9307895800092710610d+4 ! - coefficients for P close to 0.5 -
  REAL(KIND=rp), PARAMETER :: b6=2.8729085735721942674d+4 ! - coefficients for P close to 0.5 -
  REAL(KIND=rp), PARAMETER :: b7=5.2264952788528545610d+3 ! - coefficients for P close to 0.5 -
!
  REAL(KIND=rp), PARAMETER :: c0=1.42343711074968357734d0  ! - coefficients for P not close to 0, 0.5 or 1 -
  REAL(KIND=rp), PARAMETER :: c1=4.63033784615654529590d0  ! - coefficients for P not close to 0, 0.5 or 1 -
  REAL(KIND=rp), PARAMETER :: c2=5.76949722146069140550d0  ! - coefficients for P not close to 0, 0.5 or 1 -
  REAL(KIND=rp), PARAMETER :: c3=3.64784832476320460504d0  ! - coefficients for P not close to 0, 0.5 or 1 -
  REAL(KIND=rp), PARAMETER :: c4=1.27045825245236838258d0  ! - coefficients for P not close to 0, 0.5 or 1 -
  REAL(KIND=rp), PARAMETER :: c5=2.41780725177450611770d-1 ! - coefficients for P not close to 0, 0.5 or 1 -
  REAL(KIND=rp), PARAMETER :: c6=2.27238449892691845833d-2 ! - coefficients for P not close to 0, 0.5 or 1 -
  REAL(KIND=rp), PARAMETER :: c7=7.74545014278341407640d-4 ! - coefficients for P not close to 0, 0.5 or 1 -
  REAL(KIND=rp), PARAMETER :: d1=2.05319162663775882187d0  ! - coefficients for P not close to 0, 0.5 or 1 -
  REAL(KIND=rp), PARAMETER :: d2=1.67638483018380384940d0  ! - coefficients for P not close to 0, 0.5 or 1 -
  REAL(KIND=rp), PARAMETER :: d3=6.89767334985100004550d-1 ! - coefficients for P not close to 0, 0.5 or 1 -
  REAL(KIND=rp), PARAMETER :: d4=1.48103976427480074590d-1 ! - coefficients for P not close to 0, 0.5 or 1 -
  REAL(KIND=rp), PARAMETER :: d5=1.51986665636164571966d-2 ! - coefficients for P not close to 0, 0.5 or 1 -
  REAL(KIND=rp), PARAMETER :: d6=5.47593808499534494600d-4 ! - coefficients for P not close to 0, 0.5 or 1 -
  REAL(KIND=rp), PARAMETER :: d7=1.05075007164441684324d-9 ! - coefficients for P not close to 0, 0.5 or 1 -
!
  REAL(KIND=rp), PARAMETER :: e0=6.65790464350110377720d0   ! - coefficients for P near 0 or 1 -
  REAL(KIND=rp), PARAMETER :: e1=5.46378491116411436990d0   ! - coefficients for P near 0 or 1 -
  REAL(KIND=rp), PARAMETER :: e2=1.78482653991729133580d0   ! - coefficients for P near 0 or 1 -
  REAL(KIND=rp), PARAMETER :: e3=2.96560571828504891230d-1  ! - coefficients for P near 0 or 1 -
  REAL(KIND=rp), PARAMETER :: e4=2.65321895265761230930d-2  ! - coefficients for P near 0 or 1 -
  REAL(KIND=rp), PARAMETER :: e5=1.24266094738807843860d-3  ! - coefficients for P near 0 or 1 -
  REAL(KIND=rp), PARAMETER :: e6=2.71155556874348757815d-5  ! - coefficients for P near 0 or 1 -
  REAL(KIND=rp), PARAMETER :: e7=2.01033439929228813265d-7  ! - coefficients for P near 0 or 1 -
  REAL(KIND=rp), PARAMETER :: f1=5.99832206555887937690d-1  ! - coefficients for P near 0 or 1 -
  REAL(KIND=rp), PARAMETER :: f2=1.36929880922735805310d-1  ! - coefficients for P near 0 or 1 -
  REAL(KIND=rp), PARAMETER :: f3=1.48753612908506148525d-2  ! - coefficients for P near 0 or 1 -
  REAL(KIND=rp), PARAMETER :: f4=7.86869131145613259100d-4  ! - coefficients for P near 0 or 1 -
  REAL(KIND=rp), PARAMETER :: f5=1.84631831751005468180d-5  ! - coefficients for P near 0 or 1 -
  REAL(KIND=rp), PARAMETER :: f6=1.42151175831644588870d-7  ! - coefficients for P near 0 or 1 -
  REAL(KIND=rp), PARAMETER :: f7=2.04426310338993978564d-15 ! - coefficients for P near 0 or 1 -
!
! Local scalars
  REAL(KIND=rp) :: q,r ! - constants -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ABS
  INTRINSIC LOG
  INTRINSIC SQRT
!
! Executable Statments
!
! Evaluate quantile
  q=p-half
  IF (ABS(q)<=split1) THEN
      r=const1-q*q
      normq=q*(((((((a7*r+a6)*r+a5)*r+a4)*r+a3)*r+a2)*r+a1)*r+a0)/ &
              (((((((b7*r+b6)*r+b5)*r+b4)*r+b3)*r+b2)*r+b1)*r+one)
     RETURN
  ELSE
     IF (q<zero) THEN
        r=p
     ELSE
        r=one-p
     END IF
     r=SQRT(-LOG(r))
     IF (r<=split2) THEN
        r=r-const2
        normq=(((((((c7*r+c6)*r+c5)*r+c4)*r+c3)*r+c2)*r+c1)*r+c0)/ &
              (((((((d7*r+d6)*r+d5)*r+d4)*r+d3)*r+d2)*r+d1)*r+one)
     ELSE
        r=r-split2
        normq=(((((((e7*r+e6)*r+e5)*r+e4)*r+e3)*r+e2)*r+e1)*r+e0)/ &
              (((((((f7*r+f6)*r+f5)*r+f4)*r+F3)*r+f2)*r+f1)*r+one)
     END IF
     IF (q<zero) normq=-normq
     RETURN
  END IF
!
 END FUNCTION normq
!
!
!
 FUNCTION normq_inv (z)
!
! Produces the lower tail area corresponding to a normal deviate.
!
! Modules
  USE numbers, ONLY: half,sqrtwo
!
! Function type
  REAL(KIND=rp) :: normq_inv
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: z ! - normal deviate -
!
! Executable Statements
!
! Calculate tail area
  normq_inv=half*error_f(-z/sqrtwo)
!
  RETURN
 END FUNCTION normq_inv
!
!
!
 FUNCTION error_f(x)
!
! Computes approximate values for error_f(x).
!
! The main computation evaluates near-minimax approximations
! from "Rational Chebyshev approximations for the error function"
! by W. J. Cody, Math. Comp., 1969, PP. 631-638.  This
! transportable program uses rational functions that theoretically
! approximate  error_f(x)  to at least 18 significant
! decimal digits.  The accuracy achieved depends on the arithmetic
! system, the compiler, the intrinsic functions, and proper
! selection of the machine-dependent constants.
!
! Written by: W. J. Cody
!             Mathematics and Computer Science Division
!             Argonne National Laboratory
!             Argonne, IL 60439
!
!  Written: January 8, 1985
!  Latest modification: March 19, 1990
!
! Modules
  USE numbers, ONLY: zero,one,two,four,sixten,sqrpi,eps
!
! Function type
  REAL(KIND=rp) :: error_f
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: x ! - error function argument -
!
! Locals
!
! Local parameters
  REAL(KIND=rp), PARAMETER :: thresh=0.46875_rp
  REAL(KIND=rp), PARAMETER :: xbig=  26.543_rp ! - largest argument acceptable to ERFC -
  REAL(KIND=rp), PARAMETER :: a(5)= & ! - coefficients for approximation to error_f in first interval -
     (/3.16112374387056560d00,1.13864154151050156d02,3.77485237685302021d02,3.20937758913846947d03, &
       1.85777706184603153d-1/)
  REAL(KIND=rp), PARAMETER :: b(4)= & ! - coefficients for approximation to error_f in first interval -
     (/2.36012909523441209d01,2.44024637934444173d02,1.28261652607737228d03,2.84423683343917062d03/)
  REAL(KIND=rp), PARAMETER :: c(9)= & ! - coefficients for approximation to error_f in second interval -
     (/5.64188496988670089d-1,8.88314979438837594d00,6.61191906371416295d01,2.98635138197400131d02, &
       8.81952221241769090d02,1.71204761263407058d03,2.05107837782607147d03,1.23033935479799725d03, &
       2.15311535474403846d-8/)
  REAL(KIND=rp), PARAMETER :: d(8)= & ! - coefficients for approximation to error_f in second interval -
     (/1.57449261107098347d01,1.17693950891312499d02,5.37181101862009858d02,1.62138957456669019d03, &
       3.29079923573345963d03,4.36261909014324716d03,3.43936767414372164d03,1.23033935480374942d03/)
  REAL(KIND=rp), PARAMETER :: p(6)= & ! - coefficients for approximation to error_f in third interval -
     (/3.05326634961232344d-1,3.60344899949804439d-1,1.25781726111229246d-1,1.60837851487422766d-2, &
       6.58749161529837803d-4,1.63153871373020978d-2/)
  REAL(KIND=rp), PARAMETER :: q(5)= & ! - coefficients for approximation to error_f in third interval -
     (/2.56852019228982242d00,1.87295284992346047d00,5.27905102951428412d-1,6.05183413124413191d-2, &
       2.33520497626869185d-3/)
!
! Local scalars
  INTEGER :: i ! - polynomial index -
!
  REAL(KIND=rp) :: y ! - absolute value of argument -
  REAL(KIND=rp) :: del
  REAL(KIND=rp) :: xnum
  REAL(KIND=rp) :: xden
  REAL(KIND=rp) :: ysq
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ABS
  INTRINSIC AINT
  INTRINSIC EXP
!
! Executable Statements
!
! Evaluate error_f for |X| <= 0.46875
  y=ABS(x)
  IF (y<thresh) THEN
     IF (y>eps) THEN
        ysq=y*y
     ELSE
        ysq=zero
     END IF
     xnum=a(5)*ysq
     xden=ysq
     DO i=1,3
        xnum=(xnum+a(i))*ysq
        xden=(xden+b(i))*ysq
     END DO
     error_f=x*(xnum+a(4))/(xden+b(4))
     error_f=one-error_f
     RETURN
!
! Evaluate error_f for 0.46875 <= |X| <= 4.0
  ELSE IF (y<four) THEN
     xnum=c(9)*y
     xden=y
     DO i=1,7
        xnum=(xnum+c(i))*y
        xden=(xden+d(i))*y
     END DO
     error_f=(xnum+c(8))/(xden+d(8))
     ysq=AINT(y*sixten)/sixten
     del=(y-ysq)*(y+ysq)
     error_f=EXP(-ysq*ysq)*EXP(-del)*error_f
!
! Evaluate error_f for |X| > 4.0
  ELSE
     error_f=zero
     IF (y<xbig) THEN
        ysq=one/(y*y)
        xnum=p(6)*ysq
        xden=ysq
        DO i=1,4
           xnum=(xnum+p(i))*ysq
           xden=(xden+q(i))*ysq
        END DO
        error_f=ysq*(xnum+p(5))/(xden+q(5))
        error_f=(SQRPI-error_f)/y
        ysq=AINT(y*sixten)/sixten
        del=(y-ysq)*(y+ysq)
        error_f=EXP(-ysq*ysq)*EXP(-del)*error_f
     END IF
  END IF
!
! Fix up for negative argument
  IF (x<zero) error_f=two-error_f
!
  RETURN
 END FUNCTION error_f
!
!
!
 SUBROUTINE gaussian (n,y,g)
!
! Converts data to a gaussian distribution via a uniform distribution
!
! Modules
  USE numbers, ONLY: half,one
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: y(:) ! - data to be transformed -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: g(:) ! - transformed data -
!
! Locals
!
! Local scalars
  INTEGER :: k1 ! - case index -
  INTEGER :: k2 ! - case index -
!
  REAL(KIND=rp) :: df ! - number of cases -
!
! Executable Statements
!
! Convert to uniform distribution
  df=REAL(n+1,KIND=rp)
  g(1:n)=one
  DO k1=1,n-1
     DO k2=k1+1,n
        IF (y(k1)>y(k2)) THEN
           g(k1)=g(k1)+one
        ELSE IF (y(k1)<y(k2)) THEN
           g(k2)=g(k2)+one
        ELSE
           g(k1)=g(k1)+half
           g(k2)=g(k2)+half
        END IF
     END DO
  END DO
  g(1:n)=g(1:n)/df
!
! Convert to normal distribution
  DO k1=1,n
     g(k1)=normq(g(k1))
  END DO
!
  RETURN
 END SUBROUTINE gaussian
!
!
!
 SUBROUTINE gaussian_inv (n,y,nr,ref)
!
! Converts data from a gaussian distribution to an empirical distribution via a uniform distribution
!
! Modules
  USE arrays, ONLY: insertion_sort,quantile
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n  ! - number of cases -
  INTEGER, INTENT(IN) :: nr ! - number of cases in reference data -
!
! Input/output arrays
  REAL(KIND=rp), INTENT(INOUT) :: y(:)   ! - data to be transformed -
  REAL(KIND=rp), INTENT(INOUT) :: ref(:) ! - reference empirical distribution (sorted on exit) -
!
! Locals
!
! Local scalars
  INTEGER :: k ! - case index -
!
! Executable Statements
!
! Sort reference data for empirical distribution
  CALL insertion_sort (ref,nr,'a')
!
! Convert to uniform distribution
  DO k=1,n
     y(k)=normq_inv(y(k))
!
! Convert to empirical distribution
     y(k)=quantile(ref,nr,y(k))
  END DO
!
  RETURN
 END SUBROUTINE gaussian_inv
END MODULE distribs
