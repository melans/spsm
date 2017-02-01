! $Id: missing.f90 1238 2011-03-04 16:25:04Z simon $
MODULE missing
!
! Modules
  USE CPT_constants, ONLY: nmo
  USE numbers,       ONLY: rp
!
! Implicit declarations
  IMPLICIT NONE
!
! Arrays
!
! Integer arrays
  INTEGER, PUBLIC :: irmx(nmo)   ! - replace missing X option flags -
  INTEGER, PUBLIC :: irmy(nmo)   ! - replace missing Y option flags -
!
! Scalars
!
! Integer scalars
  INTEGER, PUBLIC :: immx ! - method for replacing missing X data flag -
  INTEGER, PUBLIC :: immy ! - method for replacing missing Y data flag -
  INTEGER, PUBLIC :: ipmx ! - maximum % of missing X data -
  INTEGER, PUBLIC :: ipmy ! - maximum % of missing X data -
  INTEGER, PUBLIC :: ipvx ! - maximum % of missing X variables -
  INTEGER, PUBLIC :: ipvy ! - maximum % of missing X variables -
!
! Real scalars
  REAL(KIND=rp), PUBLIC :: xmiss ! - X missing values -
  REAL(KIND=rp), PUBLIC :: ymiss ! - Y missing values -
!
CONTAINS
!
!
 FUNCTION missing_opts()
!
! Prompts for missing value settings and options
!
! Modules
  USE CPT_constants, ONLY: ipm
  USE labels,        ONLY: cg_dsds_l
  USE fields,        ONLY: xfield,yfield
  USE iofiles,       ONLY: xfile,yfile
  USE maths,         ONLY: magnitude
!
! Function type
  INTEGER :: missing_opts
!
! Locals
!
! Local scalars
  INTEGER :: imiss        ! - missing value flag -
!
  CHARACTER(LEN=9) :: cfmt ! - format statement -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC NINT
!
! Executable Statements
!
! Prompt for missing X value settings
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') 'Missing values' 
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') 'X variables'
! - missing value flag -
  irmx(:)=0
  imiss=NINT(xfield(1)%rmiss)
  IF (imiss>=0) THEN
     WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(imiss),',A)'
  ELSE
     WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(imiss)+1,',A)'
  END IF
1 WRITE (UNIT=*,FMT=cfmt,ADVANCE='no') 'Missing value flag (must be integer; current value is ',imiss,'): '
  READ (UNIT=*,FMT=*,ERR=1) xfield(1)%rmiss
! - maximum % of missing values -
  WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(ipmx),',A)'
2 WRITE (UNIT=*,FMT=cfmt,ADVANCE='no') 'Maximum % of missing values (current value is ',ipmx,'%): '
  READ (UNIT=*,FMT=*,ERR=2) ipmx
  IF ((ipmx<0).OR.(ipmx>ipm)) GOTO 3
! - maximum % of missing variables -
  WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(3A,I',magnitude(ipvx),',A)'
3 WRITE (UNIT=*,FMT=cfmt,ADVANCE='no') 'Maximum % of missing ',TRIM(cg_dsds_l(xfile%igrid)),' (current value is ',ipvx,'%): '
  READ (UNIT=*,FMT=*,ERR=3) ipvx
  IF ((ipvx<0).OR.(ipvx>ipm)) GOTO 3
! - replacement method -
4 WRITE (UNIT=*,FMT='(A)') 'Missing value replacement method:'
  WRITE (UNIT=*,FMT='(A)') '1.  Long-term means'
  WRITE (UNIT=*,FMT='(A)') '2.  Long-term medians'
  WRITE (UNIT=*,FMT='(A)') '3.  Random numbers'
  WRITE (UNIT=*,FMT='(A)') '4.  Best nearest neighbour'
  READ (UNIT=*,FMT=*,ERR=4) immx
  IF ((immx<1).OR.(immx>5)) GOTO 4
  irmx(immx)=1 ! immx method is selected
!
! Prompt for missing Y value settings
  irmy(:)=0
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') 'Y variables'
! - missing value flag -
  imiss=NINT(yfield(1)%rmiss)
  IF (imiss>=0) THEN
     WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(imiss),',A)'
  ELSE
     WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(imiss)+1,',A)'
  END IF
5 WRITE (UNIT=*,FMT=cfmt,ADVANCE='no') 'Missing value flag (must be integer; current value is ',imiss,'): '
  READ (UNIT=*,FMT=*,ERR=5) yfield(1)%rmiss
! - maximum % of missing values -
  WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(ipmy),',A)'
6 WRITE (UNIT=*,FMT=cfmt,ADVANCE='no') 'Maximum % of missing values (current value is ',ipmy,'): '
  READ (UNIT=*,FMT=*,ERR=6) ipmy
  IF ((ipmy<0).OR.(ipmy>ipm)) GOTO 6
! - maximum % of missing variables -
  WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(3A,I',magnitude(ipvy),',A)'
7 WRITE (UNIT=*,FMT=cfmt,ADVANCE='no') 'Maximum % of missing ',TRIM(cg_dsds_l(yfile%igrid)),' (current value is ',ipvy,'): '
  READ (UNIT=*,FMT=*,ERR=7) ipvy
  IF ((ipvy<0).OR.(ipvy>ipm)) GOTO 7
! - replacement method -
8 WRITE (UNIT=*,FMT='(A)') 'Missing value replacement method:'
  WRITE (UNIT=*,FMT='(A)') '1.  Long-term means'
  WRITE (UNIT=*,FMT='(A)') '2.  Long-term medians'
  WRITE (UNIT=*,FMT='(A)') '3.  Random numbers'
  WRITE (UNIT=*,FMT='(A)') '4.  Best nearest neighbour'
  READ (UNIT=*,FMT=*,ERR=8) immy
  IF ((immy<1).OR.(immy>4)) GOTO 8
  irmy(immy)=1
!
!
! Duplicate missing value flag for additional fields
  IF (xfile%nfl>1) xfield(2:)%rmiss=xfield(1)%rmiss
  IF (yfile%nfl>1) yfield(2:)%rmiss=yfield(1)%rmiss

  missing_opts=0
!
  RETURN
 END FUNCTION missing_opts
!
!
!
 SUBROUTINE non_missing (nfs,nv,n,v,izero,imm,ipm,ipv,vmiss,nva,mva,iuse,kuse,ifail)
!
! Replaces missing values and removes missing variables
!
! On exit:
!    ifail = 0 No errors
!    ifail = 1 mva=0
!    ifail = 2 at least one of the fields have no missing variables
!    ifail = 3 insufficient variables for nearest neighbour
!    ifail = 4 insufficient memory for nearest neighbour
!
! Modules
  USE analysis, ONLY: prog,dprog
  USE arrays,   ONLY: vcopy=>xc,dwk, &
                      insertion_sort,quantile
  USE maths,    ONLY: norm_rand
  USE numbers,  ONLY: zero,half,three,oneh,onem
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nfs   ! - number of fields / lagged fields -
  INTEGER, INTENT(IN) :: n     ! - number of cases -
  INTEGER, INTENT(IN) :: izero ! - zero-bound flag -
  INTEGER, INTENT(IN) :: ipm   ! - maximum % of missing values -
  INTEGER, INTENT(IN) :: ipv   ! - maximum % of missing variables -
!
! Input/output scalars
  INTEGER, INTENT(INOUT) :: imm ! - missing value replacement method -
!
! Output scalars
  INTEGER, INTENT(OUT) :: mva   ! - total number of non-missing variables -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: nv(:)  ! - number of variables by field -
!
  REAL(KIND=rp), INTENT(IN) :: vmiss(:) ! - missing value indicators -
!
! Input/output arrays
  REAL(KIND=rp), INTENT(INOUT) :: v(:,:) ! - variables (missing variables deleted on output) -
!
  LOGICAL, INTENT(INOUT) :: kuse(:) ! - used cases flags -
!
! Output arrays
  INTEGER, INTENT(OUT) :: nva(:)  ! - number of non-missing variables by field -
  INTEGER, INTENT(OUT) :: iuse(:) ! - indices of used variables -
!
! Locals
!
! Local scalars
  INTEGER :: i     ! - variable index -
  INTEGER :: j     ! - variable index by field -
  INTEGER :: k     ! - time index -
  INTEGER :: l     ! - field index -
  INTEGER :: mv    ! - total number of variables -
  INTEGER :: i1,i2 ! - indices -
  INTEGER :: imiss ! - maximum number of missing cases permitted -
  INTEGER :: kmiss ! - maximum number of missing variables permitted -
  INTEGER :: nmiss ! - actual number of missing values -
  INTEGER :: nnm   ! - number of non-missing values -
  INTEGER :: istat ! - memory allocation status -
!
  REAL(KIND=rp) :: vbar ! - mean -
  REAL(KIND=rp) :: vvar ! - variance -
  REAL(KIND=rp) :: vmed ! - median -
  REAL(KIND=rp) :: vm   ! - current missing value -
  REAL(KIND=rp) :: tol  ! - missing value rounding tolerance -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ALL
  INTRINSIC ANY
  INTRINSIC COUNT
  INTRINSIC REAL
  INTRINSIC SUM
!
! Executable Statements
!
! Determine maximum number of missing values per series
  dprog=dprog/three
  mv=SUM(nv(1:nfs))
  imiss=n*REAL(ipm,KIND=rp)/oneh
!
! Identify which variables to use
  mva=0
  nva(1:nfs)=0
  iuse(:)=0
  i=0
  dprog=dprog*REAL(n,KIND=rp)/REAL(mv,KIND=rp)
  DO l=1,nfs
     tol=vmiss(l)/onem
     DO j=1,nv(l)
        i=i+1
        IF (ANY(v(i,2:n)/=v(i,1))) THEN
           nva(l)=nva(l)+1
           mva=mva+1
           iuse(mva)=i
           WHERE ((v(i,1:n)>vmiss(l)-tol).AND.(v(i,1:n)<vmiss(l)+tol)) v(i,1:n)=vmiss(l)
           nmiss=COUNT(v(i,:)==vmiss(l))
           IF (nmiss>imiss) THEN
              v(i,:)=vmiss(l)
              iuse(mva)=0
              nva(l)=nva(l)-1
              mva=mva-1
           END IF
        END IF
        prog=prog+dprog
     END DO
  END DO
  dprog=dprog*REAL(mv,KIND=rp)/REAL(n,KIND=rp)
!
! Check number of available variables
  IF (ALL(nva(1:nfs)>0)) THEN
     ifail=0
  ELSE
     IF (ALL(nva(1:nfs)==0)) THEN
        ifail=1
     ELSE
        ifail=2
     END IF
     RETURN
  END IF
!
! Compress data array
  DO i=1,mva
     IF (iuse(i)>i) v(i,1:n)=v(iuse(i),1:n)
  END DO
!
! Determine maximum number of missing series per case
  kmiss=mva*REAL(ipv,KIND=rp)/oneh
!
! Identify which cases to use
  DO k=1,n
     IF (kuse(k))  THEN
        nmiss=0
        i1=1
        i2=0
        DO l=1,nfs
           i2=i2+nva(l)
           nmiss=nmiss+COUNT(v(i1:i2,k)==vmiss(l))
           i1=i1+nva(l)
        END DO
        IF (nmiss>kmiss) kuse(k)=.false.
     END IF
     prog=prog+dprog
  END DO
!
! Replace missing values
  IF (imiss>0) THEN
     dprog=dprog*REAL(n,KIND=rp)/REAL(mva,KIND=rp)
     IF (imm==4) THEN
        IF (mva>1) THEN
           ALLOCATE (vcopy(mva,n),STAT=istat)
           IF (istat/=0) THEN
              imm=1
              ifail=4
           END IF
        ELSE
           imm=1
           ifail=3
        END IF
     END IF
     SELECT CASE (imm)
! - long-term mean -
      CASE (1)
        i=0
        DO l=1,nfs
           DO j=1,nva(l)
              i=i+1
              nmiss=COUNT(v(i,:)==vmiss(l))
              IF (nmiss>0) THEN
                 vbar=SUM(v(i,1:n),MASK=(v(i,1:n)/=vmiss(l)))/REAL(n-nmiss,KIND=rp)
                 WHERE (v(i,1:n)==vmiss(l)) v(i,1:n)=vbar
              END IF
              prog=prog+dprog
           END DO
        END DO
! - long-term median -
      CASE (3)
        i=0
        DO l=1,nfs
           DO j=1,nva(l)
              i=i+1
              nmiss=COUNT(v(i,:)==vmiss(l))
              IF (nmiss>0) THEN
                 nnm=0
                 DO k=1,n
                    IF (v(i,k)/=vmiss(l)) THEN
                       nnm=nnm+1
                       dwk(nnm)=v(i,k)
                    END IF
                 END DO
                 CALL insertion_sort (dwk,nnm,'a')
                 vmed=quantile(dwk,nnm,half)
                 WHERE (v(i,1:n)==vmiss(l)) v(i,1:n)=vmed
              END IF
              prog=prog+dprog
           END DO
        END DO
! - random numbers -
      CASE (2)
        i=0
        DO l=1,nfs
           DO j=1,nva(l)
              i=i+1
              nmiss=COUNT(v(i,:)==vmiss(l))
              IF (nmiss>0) THEN
                 vbar=SUM(v(i,1:n),MASK=(v(i,1:n)/=vmiss(l)))/REAL(n-nmiss,KIND=rp)
                 vvar=SUM((v(i,1:n)-vbar)**2,MASK=(v(i,1:n)/=vmiss(l)))/REAL(n-nmiss-1,KIND=rp)
                 DO k=1,n
                    IF (v(i,k)/=vmiss(l)) THEN
                       v(i,k)=norm_rand(vbar,vvar)
                       IF (izero==1) THEN
                          IF (v(i,k)<zero) v(i,k)=zero
                       END IF
                    END IF
                 END DO
              END IF
              prog=prog+dprog
           END DO
        END DO
! - nearest neighbour -
      CASE (4)
        i=0
        DO l=1,nfs
           DO j=1,nva(l)
              i=i+1
              DO k=1,n
                 IF (v(i,k)==vmiss(l)) THEN
                    vcopy(i,k)=neighbour(nfs,nva,n,v,i,k,l,vmiss(:))
                    IF (izero==1) THEN
                       IF (vcopy(i,k)<zero) vcopy(i,k)=zero
                    END IF
                 END IF
              END DO
              prog=prog+dprog
           END DO
        END DO
        i1=1
        i2=0
        DO l=1,nfs
           i2=i2+nva(l)
           vm=vmiss(l)
           WHERE (v(i1:i2,1:n)==vm) v(i1:i2,1:n)=vcopy(i1:i2,1:n)
           i1=i1+nva(l)
        END DO
        DEALLOCATE (vcopy)
     END SELECT
     dprog=dprog*REAL(mva,KIND=rp)/REAL(n,KIND=rp)
  ELSE
     prog=prog+dprog*REAL(n,KIND=rp)
  END IF
  dprog=dprog*three
!
  RETURN
 END SUBROUTINE non_missing
!
!
!
 SUBROUTINE non_missingz (nfs,nva,nf,z,imm,zmiss,iuse)
!
! Replaces missing values and removes missing variables in forecast data
!
! Modules
  USE analysis,   ONLY: prog,dprog
  USE arrays,     ONLY: x
  USE maths,      ONLY: norm_rand
  USE numbers,    ONLY: tol
  USE settings,   ONLY: nu
  USE statistics, ONLY: get_mean,get_median,get_var
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nfs ! - number of fields / lagged fields -
  INTEGER, INTENT(IN) :: nf  ! - number of forecasts -
  INTEGER, INTENT(IN) :: imm ! - missing value replacement method -
!
! Input arrays
  INTEGER, INTENT(IN) :: nva(:)  ! - number of used variables per field -
  INTEGER, INTENT(IN) :: iuse(:) ! - used variables indices -
!
  REAL(KIND=rp), INTENT(IN) :: zmiss(:) ! - missing value indicator -
!
! Input/output arrays
  REAL(KIND=rp), INTENT(INOUT) :: z(:,:) ! - variables (missing variables deleted on output) -
!
! Locals
!
! Local scalars
  INTEGER :: i     ! - variable index -
  INTEGER :: j     ! - variable index by field -
  INTEGER :: k     ! - time index -
  INTEGER :: l     ! - field index -
  INTEGER :: nmiss ! - actual number of missing values -
!
  REAL(KIND=rp) :: vbar ! - mean -
  REAL(KIND=rp) :: vvar ! - variance -
  REAL(KIND=rp) :: vmed ! - median -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC COUNT
!
! Executable Statements
!
! Remove unused variables and replace missing values
  i=0
  DO l=1,nfs
     DO j=1,nva(l)
        i=i+1
        IF (i<iuse(i)) z(i,1:nf)=z(iuse(i),1:nf)
        WHERE ((z(i,1:nf)>zmiss(l)-tol).AND.(z(i,1:nf)<zmiss(l)+tol)) z(i,1:nf)=zmiss(l)
        nmiss=COUNT(z(i,1:nf)==zmiss(l))
        IF (nmiss/=0) THEN
           SELECT CASE (imm)
            CASE (1) ! - long-term mean -
              vbar=get_mean(nu,x(i,1:nu),zmiss(l))
              WHERE (z(i,1:nf)==zmiss(l)) z(i,1:nf)=vbar
            CASE (3) ! - long-term median -
              vmed=get_median(nu,x(i,1:nu),zmiss(l))
              WHERE (z(i,1:nf)==zmiss(l)) z(i,1:nf)=vmed
            CASE (2) ! - random numbers -
              vbar=get_mean(nu,x(i,1:nu),zmiss(l))
              vvar=get_var(nu,x(i,1:nu),vbar,zmiss(l))
              WHERE (z(i,1:nf)==zmiss(l)) z(i,1:nf)=norm_rand(vbar,vvar)
            CASE (4) ! - nearest neighbour -
              DO k=1,nf
                 IF (z(i,k)==zmiss(l)) z(i,k)=get_neighbour(nfs,nva,z,i,k,l,zmiss(:))
              END DO
           END SELECT
        END IF
        prog=prog+dprog
     END DO
  END DO
!
  RETURN
 END SUBROUTINE non_missingz
!
!
!
 FUNCTION neighbour(nfs,nva,n,v,i,k,l,vmiss)
!
! Estimates a missing value from nearest neighbour
!
! Modules
  USE numbers, ONLY: zero,eps
!
! Function type
  REAL(KIND=rp) :: neighbour
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nfs ! - number of fields nd lagged fields -
  INTEGER, INTENT(IN) :: n   ! - number of cases -
  INTEGER, INTENT(IN) :: i   ! - current series -
  INTEGER, INTENT(IN) :: k   ! - current case -
  INTEGER, INTENT(IN) :: l   ! - current field / lagged-field -
!
! Input arrays
  INTEGER, INTENT(IN) :: nva(:) ! - number of series per field -
!
  REAL(KIND=rp), INTENT(IN) :: v(:,:) ! - data -
  REAL(KIND=rp), INTENT(IN) :: vmiss(:) ! - missing value flags -
!
! Locals
!
! Local scalars
  INTEGER :: j     ! - variable by field index -
  INTEGER :: ij    ! - variable index -
  INTEGER :: ik    ! - case index -
  INTEGER :: il    ! - field / lagged-field index -
  INTEGER :: nnm   ! - number of non-missing cases -
  INTEGER :: ibest ! - nearest neighbour -
!
  REAL(KIND=rp) :: xbar  ! - mean -
  REAL(KIND=rp) :: ybar  ! - mean -
  REAL(KIND=rp) :: df    ! - degrees of freedom -
  REAL(KIND=rp) :: sxx   ! - sum of squares of independent variable -
  REAL(KIND=rp) :: syy   ! - sum of squares of dependent variable -
  REAL(KIND=rp) :: sxy   ! - sum of cross products -
  REAL(KIND=rp) :: r     ! - correlation -
  REAL(KIND=rp) :: rbest ! - best correlation -
  REAL(KIND=rp) :: b0    ! - regression constant -
  REAL(KIND=rp) :: b1    ! - regression coefficient -
!
! Functions and subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SQRT
!
! Executable Statements
!
! Fit regression line
  ibest=0
  rbest=zero
  ij=0
  DO il=1,nfs
     DO j=1,nva(il)
        ij=ij+1
        IF (ij==i) CYCLE
        IF (v(ij,k)==vmiss(il)) CYCLE
! - calculate means -
        nnm=0
        xbar=zero
        ybar=zero
        DO ik=1,n
           IF ((v(i,ik)/=vmiss(l)).AND.(v(ij,ik)/=vmiss(il))) THEN
              nnm=nnm+1
              xbar=xbar+v(ij,ik)
              ybar=ybar+v(i,ik)
           END IF
        END DO
        IF (nnm<=2) CYCLE
        df=REAL(nnm,KIND=rp)
        xbar=xbar/df
        ybar=ybar/df
! - calculate sum of squares and cross-products -
        sxx=zero
        syy=zero
        sxy=zero
        DO ik=1,n
           IF ((v(i,ik)/=vmiss(l)).AND.(v(ij,ik)/=vmiss(il))) THEN
              sxx=sxx+(v(ij,ik)-xbar)**2
              syy=syy+(v(i,ik)-ybar)**2
              sxy=sxy+(v(ij,ik)-xbar)*(v(i,ik)-ybar)
           END IF
        END DO
! - calculate correlation -
        IF ((sxx>eps).AND.(syy>eps)) THEN
           r=sxy/SQRT(sxx*syy)
           IF (r>rbest) THEN
! - calculate coefficients -
              ibest=j
              rbest=r
              b1=sxy/sxx
              b0=ybar-b1*xbar
           END IF
        END IF
     END DO
  END DO
!
! Calculate mean if no close neighbours
  IF (ibest/=0) THEN
     neighbour=b0+b1*v(ibest,k)
  ELSE
     nnm=0
     ybar=zero
     DO ik=1,n
        IF (v(i,ik)/=vmiss(l)) THEN
           nnm=nnm+1
           ybar=ybar+v(i,ik)
        END IF
     END DO
     df=REAL(nnm,KIND=rp)
     ybar=ybar/df
     neighbour=ybar
  END IF
!
  RETURN
 END FUNCTION neighbour
!
!
!
 FUNCTION get_neighbour(nfs,nva,z,i,k,l,xmiss)
!
! Returns best estimate of a missing value using nearest neighbour
!
! Modules
  USE arrays,   ONLY: x
  USE numbers,  ONLY: zero,eps
  USE settings, ONLY: nu
!
! Function type
  REAL(KIND=rp) :: get_neighbour
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nfs ! - number of fields and lagged fields -
  INTEGER, INTENT(IN) :: i   ! - current series -
  INTEGER, INTENT(IN) :: k   ! - current forecast -
  INTEGER, INTENT(IN) :: l   ! - current field / lagged-field -
!
! Input arrays
  INTEGER, INTENT(IN) :: nva(:) ! - number of series per field -
!
  REAL(KIND=rp), INTENT(IN) :: z(:,:)   ! - data -
  REAL(KIND=rp), INTENT(IN) :: xmiss(:) ! - missing value flags -
!
! Locals
!
! Local scalars
  INTEGER :: j     ! - variable by field index -
  INTEGER :: ij    ! - variable index -
  INTEGER :: ik    ! - case index -
  INTEGER :: il    ! - field / lagged-field index -
  INTEGER :: nnm   ! - number of non-missing cases -
  INTEGER :: ibest ! - nearest neighbour -
!
  REAL(KIND=rp) :: xbar  ! - mean -
  REAL(KIND=rp) :: ybar  ! - mean -
  REAL(KIND=rp) :: df    ! - degrees of freedom -
  REAL(KIND=rp) :: sxx   ! - sum of squares of independent variable -
  REAL(KIND=rp) :: syy   ! - sum of squares of dependent variable -
  REAL(KIND=rp) :: sxy   ! - sum of cross products -
  REAL(KIND=rp) :: r     ! - correlation -
  REAL(KIND=rp) :: rbest ! - best correlation -
  REAL(KIND=rp) :: b0    ! - regression constant -
  REAL(KIND=rp) :: b1    ! - regression coefficient -
!
! Functions and subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SQRT
!
! Executable Statements
!
! Fit regression line
  ibest=0
  rbest=zero
  ij=0
  DO il=1,nfs
     DO j=1,nva(il)
        ij=ij+1
        IF (ij==i) CYCLE
        IF (x(ij,k)==xmiss(il)) CYCLE
! - calculate means -
        nnm=0
        xbar=zero
        ybar=zero
        DO ik=1,nu
           IF ((x(i,ik)/=xmiss(l)).AND.(x(ij,ik)/=xmiss(il))) THEN
              nnm=nnm+1
              xbar=xbar+x(ij,ik)
              ybar=ybar+x(i,ik)
           END IF
        END DO
        IF (nnm<=2) CYCLE
        df=REAL(nnm,KIND=rp)
        xbar=xbar/df
        ybar=ybar/df
! - calculate sum of squares and cross-products -
        sxx=zero
        syy=zero
        sxy=zero
        DO ik=1,nu
           IF ((x(i,ik)/=xmiss(l)).AND.(x(ij,ik)/=xmiss(il))) THEN
              sxx=sxx+(x(ij,ik)-xbar)**2
              syy=syy+(x(i,ik)-ybar)**2
              sxy=sxy+(x(ij,ik)-xbar)*(x(i,ik)-ybar)
           END IF
        END DO
! - calculate correlation -
        IF ((sxx>eps).AND.(syy>eps)) THEN
           r=sxy/SQRT(sxx*syy)
           IF (r>rbest) THEN
! - calculate coefficients -
              ibest=j
              rbest=r
              b1=sxy/sxx
              b0=ybar-b1*xbar
           END IF
        END IF
     END DO
  END DO
!
! Calculate mean if no close neighbours
  IF (ibest/=0) THEN
     get_neighbour=b0+b1*z(ibest,k)
  ELSE
     nnm=0
     ybar=zero
     DO ik=1,nu
        IF (x(i,ik)/=xmiss(il)) THEN
           nnm=nnm+1
           ybar=ybar+x(i,ik)
        END IF
     END DO
     df=REAL(nnm,KIND=rp)
     ybar=ybar/df
     get_neighbour=ybar
  END IF
!
  RETURN
 END FUNCTION get_neighbour
END MODULE missing
