! $Id: bootstrap.f90 1224 2011-03-03 16:28:24Z simon $
MODULE bootstrap
!
! Modules
  USE CPT_constants, ONLY: nts
  USE numbers,       ONLY: rp
  USE statistics,    ONLY: nscore
!
! Implicit declarations
  IMPLICIT NONE
!
! Derived Type Definitions
!
! - confidence limits -
  TYPE cflims
     REAL(KIND=rp) :: lower ! - lower limit -
     REAL(KIND=rp) :: upper ! - upper limit -
  END TYPE cflims
!
! Arrays
!
! Integer arrays
  INTEGER, ALLOCATABLE, PUBLIC :: indices(:) ! - permutation indices -
  INTEGER, ALLOCATABLE, PUBLIC :: ixboot(:)  ! - bootstrap sample -
  INTEGER, ALLOCATABLE, PUBLIC :: iyboot(:)  ! - bootstrap sample -
  INTEGER, ALLOCATABLE, PUBLIC :: iperm(:,:) ! - p-value permutation sample -
!
! Real arrays
  REAL(KIND=rp), PUBLIC :: pval(nscore) ! - p-values -
!
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: xboot(:)   ! - bootstrap sample -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: yboot(:)   ! - bootstrap sample -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: boot(:,:)  ! - bootstrapped scores -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: yperm(:,:) ! - p-value permutation sample -
  REAL(KIND=rp), ALLOCATABLE, PUBLIC :: perm(:)    ! - permuted scores -
!
! Derived type arrays
  TYPE(cflims), PUBLIC :: boot_cls(nscore) ! - bootstrap confidence limits -
!
! Scalars
!
! Integer scalars
  INTEGER, PUBLIC :: nboot ! - size of bootstrap sample -
  INTEGER, PUBLIC :: nperm ! - size of permutation sample -
!
! Real scalars
  REAL(KIND=rp), PUBLIC :: clb ! - bootstrap confidence level -
!
! Derived type scalars
  TYPE(cflims), PUBLIC :: pcls ! - bootstrap confidence limits -
!
CONTAINS
!
!
 FUNCTION init_boot(n,nboot)
!
! Initialises CPT for bootstrapping
!
! Modules
  USE errors,     ONLY: error
  USE numbers,    ONLY: zero,one,two,oneh
  USE statistics, ONLY: init_scores
!
! Function type
  INTEGER :: init_boot
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n     ! - number of cases -
  INTEGER, INTENT(IN) :: nboot ! - size of bootstrap sample -
!
! Locals
!
! Local scalars
  INTEGER :: istat ! - memory allocation status -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ALLOCATED
!
! Executable Statements
!
! Free memory
  IF (ALLOCATED(boot))    DEALLOCATE (boot)
  IF (ALLOCATED(xboot))   DEALLOCATE (xboot)
  IF (ALLOCATED(yboot))   DEALLOCATE (yboot)
  IF (ALLOCATED(ixboot))  DEALLOCATE (ixboot)
  IF (ALLOCATED(iyboot))  DEALLOCATE (iyboot)
  IF (ALLOCATED(indices)) DEALLOCATE (indices)
!
! Initialise memory for validation
  IF(init_scores()/=0) GOTO 1
!
! Initialise data space
! - permutation indices -
  ALLOCATE (indices(n),STAT=istat)
  IF (istat/=0) GOTO 1
! - bootstrap sample -
  ALLOCATE (ixboot(n),STAT=istat)
  IF (istat/=0) GOTO 1
  ALLOCATE (iyboot(n),STAT=istat)
  IF (istat/=0) GOTO 1
  ALLOCATE (xboot(n),STAT=istat)
  IF (istat/=0) GOTO 1
  ALLOCATE (yboot(n),STAT=istat)
  IF (istat/=0) GOTO 1
! - bootstrapped scores -
  ALLOCATE (boot(nboot,nscore),STAT=istat)
  IF (istat/=0) GOTO 1
!
! Calculate confidence limit percentiles
  pcls%lower=(one-clb/oneh)/two
  pcls%upper=one-pcls%lower
!
  boot_cls%lower=zero
  boot_cls%upper=zero
  pval(:)=zero
  init_boot=0
  RETURN
!
! Errors
1 init_boot=close_boot()
  init_boot=1
  CALL error ('init_boot',init_boot)
!
  RETURN
 END FUNCTION init_boot
!
!
!
 FUNCTION get_boot_opts()
!
! Modules
  USE labels,  ONLY: cg_resamples_t
  USE numbers, ONLY: zero,oneh
!
! Function type
  INTEGER :: get_boot_opts
!
! Executable Statements
!
! Prompt for resampling settings
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') TRIM(cg_resamples_t) !'Resampling Options'
! - bootstrap settings -
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') 'Bootstrapping:'
! - number of bootstrap samples -
1 WRITE (UNIT=*,FMT='(A)',ADVANCE='no') 'Number of bootstrap samples: '
  READ (UNIT=*,FMT=*,ERR=1) nboot
  IF (nboot<100) GOTO 1
! - bootstrap confidence level -
2 WRITE (UNIT=*,FMT='(A)',ADVANCE='no') 'Confidence level (%): '
  READ (UNIT=*,FMT=*,ERR=2) clb
  IF (.NOT.((clb>zero).AND.(clb<oneh))) GOTO 2
! - permutation settings -
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') 'Permutations:'
! - number of permutations -
3 WRITE (UNIT=*,FMT='(A)',ADVANCE='no') 'Number of permutations: '
  READ (UNIT=*,FMT=*,ERR=3) nperm
  IF (nperm<100) GOTO 3
  get_boot_opts=0
!
  RETURN
 END FUNCTION get_boot_opts
!
!
!
 FUNCTION bootstrap_cv()
!
! Modules
  USE settings, ONLY: nu
!
! Function type
  INTEGER :: bootstrap_cv
!
! Executable Statements
!
! Bootstrap cross-validated scores
  bootstrap_cv=bootstraps('Cross-validated scores',nu,boots_cv)
!
  RETURN
 END FUNCTION bootstrap_cv
!
!
!
 FUNCTION bootstrap_ra()
!
! Modules
  USE settings, ONLY: nur
!
! Function type
  INTEGER :: bootstrap_ra
!
! Executable Statements
!
! Bootstrap retroactive scores
  bootstrap_ra=bootstraps('Retroactive scores',nur,boots_ra)
!
  RETURN
 END FUNCTION bootstrap_ra
!
!
!
 FUNCTION bootstraps(title,n,boots)
!
! Modules
  USE fields,   ONLY: yfield,iffy, &
                      check_ivf,update_grid
  USE iofiles,  ONLY: yfile
  USE settings, ONLY: ivf,ivfa
!
! Function type
  INTEGER :: bootstraps
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
  CHARACTER(LEN=*), INTENT(IN) :: title ! - window title -
!
! Procedure arguments
  INTEGER, EXTERNAL :: boots
!
! Executable Statements
!
! Initialise bootstrapping
  bootstraps=init_boot(n,nboot)
  IF (bootstraps/=0) RETURN
!
! Construct and add coordinate label for current point
  DO iffy=1,yfile%nfs*yfile%nfs
     DO ivfa=1,yfield(iffy)%nva
        WRITE (UNIT=*,FMT=*)
        WRITE (UNIT=*,FMT='(A)') title
        bootstraps=check_ivf(iffy,ivfa)
        CALL update_grid (ivf,iffy,yfile%igrid)
!
! Calculate results
        bootstraps=boots()
     END DO
  END DO
!
! Calculate results
  bootstraps=boots()
!
  RETURN
 END FUNCTION bootstraps
!
!
!
 FUNCTION boots_cv()
!
! Modules
  USE arrays,   ONLY: y,yhat,iobs,ifor,pobs
  USE settings, ONLY: iva,nu
!
! Function type
  INTEGER :: boots_cv
!
! Executable Statements
!
! Calculate validation statistics
  boots_cv=calc_boots(nu,y(iva,:),yhat(iva,:),iobs(iva,:),ifor(iva,:),pobs(iva,:))
!
  RETURN
 END FUNCTION boots_cv
!
!
!
 FUNCTION boots_ra()
!
! Modules
  USE arrays,   ONLY: y,yret,irobs,irfor,pobs
  USE settings, ONLY: iva,nu1,nur,nu
!
! Function type
  INTEGER :: boots_ra
!
! Executable Statements
!
! Calculate validation statistics
  boots_ra=calc_boots(nur,y(iva,nu1+1:nu),yret(iva,:),irobs(iva,:),irfor(iva,:),pobs(iva,:))
!
  RETURN
 END FUNCTION boots_ra
!
!
!
 FUNCTION calc_boots(n,y,yhat,iobs,ifor,clim)
!
! Modules
  USE analysis,      ONLY: prog,dprog
  USE arrays,        ONLY: rnko, &
                           insertion_sort,quantile,rank_data
  USE CPT_constants, ONLY: lscore
  USE labels,        ONLY: cg_done,cg_scores
  USE numbers,       ONLY: zero,one
  USE statistics,    ONLY: scores, &
                           calc_scores
!
! Function type
  INTEGER :: calc_boots
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
! Input arrays
  INTEGER, INTENT(IN) :: iobs(:) ! - observational thresholds -
  INTEGER, INTENT(IN) :: ifor(:) ! - forecast thresholds -
!
  REAL(KIND=rp), INTENT(IN) :: y(:)    ! - observed values -
  REAL(KIND=rp), INTENT(IN) :: yhat(:) ! - forecast values -
  REAL(KIND=rp), INTENT(IN) :: clim(:) ! - climatological probabilities -
!
! Locals
!
! Local scalars
  INTEGER :: i !  - score index -
  INTEGER :: j !  - bootstrap index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC TRIM
!
! Executable Statements
!
! Initialise progress meter
  calc_boots=1
  prog=zero
  dprog=one/REAL(1+nboot+nscore+nperm,KIND=rp)
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') 'Calculating bootstrap statistics and p-values ...'
!
! Calculate sample statistics
  CALL calc_scores (n,yhat(:),y(:),ifor(:),iobs(:),clim(:),scores(:))

!
! Print sample scores
  WRITE (UNIT=*,FMT='(A)') ' '
  WRITE (UNIT=*,FMT='(A)') 'Continuous measures:'
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT=*)
  DO i=1,nscore/2
     WRITE (UNIT=*,FMT='(3X,A)') cg_scores(i)
     SELECT CASE (i)
      CASE (1,2,5)
        WRITE (UNIT=*,FMT='(F15.4)') scores(i)
      CASE (3,4)
        WRITE (UNIT=*,FMT='(F15.2,A)') scores(i),'%'
      CASE (6:8)
        WRITE (UNIT=*,FMT='(F15.2)') scores(i)
     END SELECT
  END DO
  WRITE (UNIT=*,FMT='(A)') ' '
  WRITE (UNIT=*,FMT='(A)') 'Categorical measures:'
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT=*)
  DO i=nscore/2+1,nscore
     WRITE (UNIT=*,FMT='(3X,A)') cg_scores(i)
     SELECT CASE (i)
      CASE (9:14)
        WRITE (UNIT=*,FMT='(F15.2,A)') scores(i),'%'
      CASE (15:16)
        WRITE (UNIT=*,FMT='(F15.4)') scores(i)
     END SELECT
  END DO
!
! Calculate bootstrap statistics
  DO j=1,nboot
     CALL get_boot (n,yhat(:),y(:),ifor(:),iobs(:),xboot(:),yboot(:),ixboot(:),iyboot(:))
     CALL calc_scores (n,xboot(:),yboot(:),ixboot(:),iyboot(:),clim(:),boot(j,:))
  END DO
!
! Calculate bootstrap confidence limits
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A,F7.3,A)')       'Confidence level:',clb,'%'
  DO i=1,nscore
     CALL insertion_sort (boot(:,i),nboot,'a')
     boot_cls(i)%lower=quantile(boot(:,i),nboot,pcls%lower)
     boot_cls(i)%upper=quantile(boot(:,i),nboot,pcls%upper)
     SELECT CASE (i)
      CASE (1,2,5,15:16)
        WRITE (UNIT=*,FMT='(F15.4,A,F7.4)') boot_cls(i)%lower,' to ',boot_cls(i)%upper
      CASE (3,4,10:14)
        WRITE (UNIT=*,FMT='(F14.2,A,F6.2,A)') boot_cls(i)%lower,'% to ',boot_cls(i)%upper,'%'
      CASE (6:8)
        WRITE (UNIT=*,FMT='(F15.2,A,F15.2)') boot_cls(i)%lower,' to ',boot_cls(i)%upper
      CASE (9)
        WRITE (UNIT=*,FMT=*)
        WRITE (UNIT=*,FMT=*)
        WRITE (UNIT=*,FMT='(F14.2,A,F6.2,A)') boot_cls(i)%lower,'% to ',boot_cls(i)%upper,'%'
     END SELECT
     prog=prog+dprog
  END DO
!
! Calculate permuted statistics
  CALL rank_data (y,n,'a',rnko)
  pval(:)=zero
  DO j=1,nperm
     CALL get_perm1 (n,yhat(:),xboot(:),ifor(:),ixboot(:))
     CALL calc_scores (n,xboot(:),y(:),ixboot(:),iobs(:),clim(:),boot(j,:),rnko(:))
     DO i=1,nscore
        IF (lscore(i)) THEN
           IF (boot(j,i)<scores(i)) pval(i)=pval(i)+one
        ELSE
           IF (boot(j,i)>scores(i)) pval(i)=pval(i)+one
        END IF
     END DO
  END DO
  pval(:)=one-pval(:)/REAL(nperm,KIND=rp)
  prog=one
!
! Print validation statistics
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(T45,A,F7.3,A)') 'Confidence level is ',clb,'%'
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A,T45,A,T70,A)') 'Score:','Confidence limits:','P-value:'
  WRITE (UNIT=*,FMT='(A)') 'Continuous measures:'
  DO i=1,nscore/2
     SELECT CASE (i)
      CASE (1,2,8)
        WRITE (UNIT=*,FMT='(A,T30,F10.4,T45,F10.4,A,F10.4,T70,F10.4)') &
           cg_scores(i),scores(i),boot_cls(i)%lower,' to ',boot_cls(i)%upper,pval(i)
      CASE (3)
        WRITE (UNIT=*,FMT='(A,T30,F9.2,A,T45,F9.2,A,F9.2,A,T70,F10.4)') &
           cg_scores(i),scores(i),'%',boot_cls(i)%lower,'% to ',boot_cls(i)%upper,'%',pval(i)
      CASE (4:7)
        WRITE (UNIT=*,FMT='(A,T30,F10.2,T45,F10.2,A,F10.2,T70,F10.4)') &
           cg_scores(i),scores(i),boot_cls(i)%lower,' to ',boot_cls(i)%upper,pval(i)
      CASE (9)
        WRITE (UNIT=*,FMT='(F7.4)') pval(i)
      CASE DEFAULT
        WRITE (UNIT=*,FMT='(F7.4)') pval(i)
     END SELECT
  END DO
!
! Update progress meter
  prog=one
  WRITE (UNIT=*,FMT='(A)') TRIM(cg_done)//'!'
  RETURN
!
 END FUNCTION calc_boots
!
!
!
 FUNCTION init_pval(n,m)
!
! Initialises CPT for calvulating p-values
!
! Modules
  USE errors, ONLY: error
!
! Function type
  INTEGER :: init_pval
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
  INTEGER, INTENT(IN) :: m ! - number of locations -
!
! Locals
!
! Local scalars
  INTEGER :: istat ! - memory allocation status -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ALLOCATED
!
! Executable Statements
!
! Free memory
  IF (ALLOCATED(perm))    DEALLOCATE (perm)
  IF (ALLOCATED(yperm))   DEALLOCATE (yperm)
  IF (ALLOCATED(iperm))   DEALLOCATE (iperm)
  IF (ALLOCATED(indices)) DEALLOCATE (indices)
!
! Initialise data space
! - permutation indices -
  ALLOCATE (indices(n),STAT=istat)
  IF (istat/=0) GOTO 1
! - permutation sample -
  ALLOCATE (iperm(m,n),STAT=istat)
  IF (istat/=0) GOTO 1
  ALLOCATE (yperm(m,n),STAT=istat)
  IF (istat/=0) GOTO 1
! - permuted scores -
  ALLOCATE (perm(m),STAT=istat)
  IF (istat/=0) GOTO 1
!
  init_pval=0
  RETURN
!
! Errors
1 init_pval=close_pval()
  init_pval=1
  CALL error ('init_pval',init_pval)
!
  RETURN
 END FUNCTION init_pval
!
!
!
 SUBROUTINE get_pvalues (iskill,n,m,x,y,ifor,iobs,clim,skills,pvalues)
!
! Modules
  USE analysis,   ONLY: prog,dprog
  USE numbers,    ONLY: zero,one
  USE statistics, ONLY: get_skills
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
  REAL(KIND=rp), INTENT(IN) :: x(:,:)    ! - forecasts -
  REAL(KIND=rp), INTENT(IN) :: y(:,:)    ! - observations -
  REAL(KIND=rp), INTENT(IN) :: clim(:,:) ! - climatological probabilities -
  REAL(KIND=rp), INTENT(IN) :: skills(:) ! - skill values -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: pvalues(:) ! - p-values -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - location index -
  INTEGER :: j ! - permutation index -
!
! Executable Statements
!
! Initialise progress meter
  IF (init_pval(n,m)/=0) RETURN
!
! Initialise progress meter
  dprog=one/REAL(nperm+1,KIND=rp)
  prog=dprog
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') 'Calculating p-values ...'
!
! Calculate permuted statistics
  pvalues(:)=zero
  DO j=1,nperm
     CALL get_perm2 (n,m,y(:,:),yperm(:,:),ifor(:,:),iperm(:,:))
     CALL get_skills (iskill,n,m,x,yperm,iperm,iobs,clim,perm(:))
     DO i=1,m
        IF (perm(i)<skills(i)) pvalues(i)=pvalues(i)+one
     END DO
     prog=prog+dprog
  END DO
  pvalues(:)=one-pvalues(:)/REAL(nperm,KIND=rp)
!
! Update progress meter
  prog=one
  i=close_pval()
!
  RETURN
 END SUBROUTINE get_pvalues
!
!
!
 SUBROUTINE get_boot (n,x,y,ix,iy,xboot,yboot,ixboot,iyboot)
!
! Sets up a bootstrap sample
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - size of bootstrap sample -
!
! Input arrays
  INTEGER, INTENT(IN) :: ix(:) ! - sample 1 -
  INTEGER, INTENT(IN) :: iy(:) ! - sample 2 -
!
  REAL(KIND=rp), INTENT(IN) :: x(:) ! - sample 1 -
  REAL(KIND=rp), INTENT(IN) :: y(:) ! - sample 2 -
!
! Output arrays
  INTEGER, INTENT(OUT) :: ixboot(:) ! - sample 1 -
  INTEGER, INTENT(OUT) :: iyboot(:) ! - sample 2 -
!
  REAL(KIND=rp), INTENT(OUT) :: xboot(:) ! - bootstrap sample 1 -
  REAL(KIND=rp), INTENT(OUT) :: yboot(:) ! - bootstrap sample 2 -
!
! Locals
!
! Local scalars
  INTEGER :: i    ! - case index -
  INTEGER :: indx ! - bootstrap sample index -
!
  REAL(KIND=rp) :: r  ! - random number -
  REAL(KIND=rp) :: df ! - number of cases -
!
! Functions and subroutines
!
! Intrinsic functions
  INTRINSIC INT
  INTRINSIC RANDOM_NUMBER
  INTRINSIC REAL
!
! Executable Statements
!
! Generate a bootstrap sample
  df=REAL(n,KIND=rp)
  DO i=1,n
     CALL RANDOM_NUMBER (r)
     indx=INT(r*df)+1
     xboot(i)=x(indx)
     yboot(i)=y(indx)
     ixboot(i)=ix(indx)
     iyboot(i)=iy(indx)
  END DO
!
  RETURN
 END SUBROUTINE get_boot
!
!
!
 SUBROUTINE get_perm1 (n,x,xperm,ix,ixperm)
!
! Sets up a permutation sample
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - size of permutation sample -
!
! Input arrays 
  INTEGER, INTENT(IN) :: ix(:) ! - sample -
!
  REAL(KIND=rp), INTENT(IN) :: x(:) ! - sample -
!
! Output arrays
  INTEGER, INTENT(OUT) :: ixperm(:) ! - permutation sample -
!
  REAL(KIND=rp), INTENT(OUT) :: xperm(:) ! - permutation sample -
!
! Locals
!
! Local scalars
  INTEGER :: i     ! - case index -
  INTEGER :: indx  ! - permutation index -
  INTEGER :: nleft ! - number of cases remaining -
!
  REAL(KIND=rp) :: r  ! - random number -
!
! Functions and subroutines
!
! Intrinsic functions
  INTRINSIC INT
  INTRINSIC RANDOM_NUMBER
  INTRINSIC REAL
!
! Executable Statements
!
! Generate a permutation sample
  DO i=1,n
     indices(i)=i
  END DO
  DO i=1,n-1
     CALL RANDOM_NUMBER (r)
     nleft=n+1-i
     indx=INT(r*REAL(nleft,KIND=rp))+1
     xperm(i)=x(indices(indx))
     ixperm(i)=ix(indices(indx))
     IF (indx<nleft) indices(indx:nleft-1)=indices(indx+1:nleft)
  END DO
  xperm(n)=x(indices(1))
  ixperm(n)=ix(indices(1))
!
  RETURN
 END SUBROUTINE get_perm1
!
!
!
 SUBROUTINE get_perm2 (n,m,x,xperm,ix,ixperm)
!
! Sets up a permutation sample
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
  INTEGER, INTENT(IN) :: m ! - number of vraiables -
!
! Input arrays 
  INTEGER, INTENT(IN) :: ix(:,:) ! - sample -
!
  REAL(KIND=rp), INTENT(IN) :: x(:,:) ! - sample -
!
! Output arrays
  INTEGER, INTENT(OUT) :: ixperm(:,:) ! - permutation sample -
!
  REAL(KIND=rp), INTENT(OUT) :: xperm(:,:) ! - permutation sample -
!
! Locals
!
! Local scalars
  INTEGER :: k     ! - case index -
  INTEGER :: indx  ! - permutation index -
  INTEGER :: nleft ! - number of cases remaining -
!
  REAL(KIND=rp) :: r  ! - random number -
!
! Functions and subroutines
!
! Intrinsic functions
  INTRINSIC INT
  INTRINSIC RANDOM_NUMBER
  INTRINSIC REAL
!
! Executable Statements
!
! Generate a permutation sample
  DO k=1,n
     indices(k)=k
  END DO
  DO k=1,n-1
     CALL RANDOM_NUMBER (r)
     nleft=n+1-k
     indx=INT(r*REAL(nleft,KIND=rp))+1
     xperm(1:m,k)=x(1:m,indices(indx))
     ixperm(1:m,k)=ix(1:m,indices(indx))
     IF (indx<nleft) indices(indx:nleft-1)=indices(indx+1:nleft)
  END DO
  xperm(1:m,n)=x(1:m,indices(1))
  ixperm(1:m,n)=ix(1:m,indices(1))
!
  RETURN
 END SUBROUTINE get_perm2
!
!
 FUNCTION close_boot()
!
! Function type
  INTEGER :: close_boot
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ALLOCATED
!
! Executable Statements
!
! Free memory
  IF (ALLOCATED(boot))    DEALLOCATE (boot)
  IF (ALLOCATED(xboot))   DEALLOCATE (xboot)
  IF (ALLOCATED(yboot))   DEALLOCATE (yboot)
  IF (ALLOCATED(ixboot))  DEALLOCATE (ixboot)
  IF (ALLOCATED(iyboot))  DEALLOCATE (iyboot)
  IF (ALLOCATED(indices)) DEALLOCATE (indices)
  close_boot=0
!
  RETURN
 END FUNCTION close_boot
!
!
!
 FUNCTION close_pval()
!
! Modules
!
! Function type
  INTEGER :: close_pval
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ALLOCATED
!
! Executable Statements
!
! Stop any unfinished calculations
  close_pval=0
!
! Free memory
  IF (ALLOCATED(perm))    DEALLOCATE (perm)
  IF (ALLOCATED(yperm))   DEALLOCATE (yperm)
  IF (ALLOCATED(iperm))   DEALLOCATE (iperm)
  IF (ALLOCATED(indices)) DEALLOCATE (indices)
!
  RETURN
 END FUNCTION close_pval
END MODULE bootstrap
