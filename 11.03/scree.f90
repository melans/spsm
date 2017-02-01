! $Id: scree.f90 1215 2011-02-25 21:30:20Z simon $
MODULE scree
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
 FUNCTION scree_plots()
!
! Modules
  USE analysis, ONLY: ianal
  USE arrays,   ONLY: svx,svy
  USE pcs,      ONLY: npx,npy
  USE settings, ONLY: nu
!
! Function type
  INTEGER :: scree_plots
!
! Executable Statements
!
! Open results window
  scree_plots=0
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') 'Scree plots'
!
! Print X scree
  IF (npx>1) THEN
     WRITE (UNIT=*,FMT=*)
     WRITE (UNIT=*,FMT='(A)') 'X EOFs'
     CALL scree_plot (npx,nu,svx)
  END IF
!
! Print Y scree
  IF ((ianal==1).AND.(npy>1)) THEN
     WRITE (UNIT=*,FMT=*)
     WRITE (UNIT=*,FMT='(A)') 'Y EOFs'
     CALL scree_plot (npy,nu,svy)
  END IF
!
  RETURN
 END FUNCTION scree_plots
!
!
!
 FUNCTION get_x_scree_plot_data(arraysize,modeindexx,evaluesx,pvarsx,spvarsx,bsticksx)
!
! Modules
  USE arrays,   ONLY: svx
  USE pcs,      ONLY: npx
  USE settings, ONLY: nu
!
! Function type
  INTEGER :: get_x_scree_plot_data
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: arraysize ! - mode index -
!
! Input/output arrays
  INTEGER, INTENT(INOUT) :: modeindexx(arraysize) ! - mode index -
!
  REAL(KIND=rp), INTENT(INOUT) :: evaluesx(arraysize) ! - eigenvalue -
  REAL(KIND=rp), INTENT(INOUT) :: pvarsx(arraysize)   ! - percentage variance -
  REAL(KIND=rp), INTENT(INOUT) :: spvarsx(arraysize)  ! - cumulative percentage variance -
  REAL(KIND=rp), INTENT(INOUT) :: bsticksx(arraysize) ! - broken sticks -
!
! Executable Statements
!
! Open results window
  get_x_scree_plot_data=0
!
! get X scree
  IF (npx>1) THEN
     CALL get_scree_plot_data(npx,nu,svx,arraysize,modeindexx,evaluesx,pvarsx,spvarsx,bsticksx)
  ELSE
     get_x_scree_plot_data=-1
  END IF
!
  RETURN
 END FUNCTION get_x_scree_plot_data
!
!
!
 SUBROUTINE scree_plot(m,n,sv)
!
! Modules
  USE arrays,  ONLY: bstick=>dwk
  USE numbers, ONLY: one,zero,oneh
!
! Aguments
!
! Input scalars
  INTEGER, INTENT(IN) :: m ! - number of modes -
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: sv(:) ! - singular values -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - mode index -
!
  REAL(KIND=rp) :: seg    ! - stick segment -
  REAL(KIND=rp) :: df     ! - number of cases -
  REAL(KIND=rp) :: evalue ! - eigenvalue -
  REAL(KIND=rp) :: tvar   ! - total variance -
  REAL(KIND=rp) :: pvar   ! - percentage variance -
  REAL(KIND=rp) :: svar   ! - cumulative variance -
  REAL(KIND=rp) :: spvar  ! - cumulative percentage variance -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SUM
!
! Executable Statements
!
! Calculate broken stick
  bstick=oneh/REAL(m,KIND=rp)
  seg=zero
  DO i=m,1,-1
     seg=seg+one/REAL(i,KIND=rp)
     bstick(i)=bstick(i)*seg
  END DO
!
! Print results
  WRITE (UNIT=*,FMT='(A10,4A20)') 'Mode','Eigenvalue','% variance','Cum. % variance','Broken-stick segment'
  df=REAL(n-1,KIND=rp)
  tvar=SUM(sv(1:m)**2/df)
  svar=zero
  DO i=1,m
     evalue=sv(i)**2/df
     svar=svar+evalue
     pvar=oneh*evalue/tvar
     spvar=oneh*svar/tvar
     WRITE (UNIT=*,FMT='(I10,4G20.12)') i,evalue,pvar,spvar,bstick(i)
  END DO
!
  RETURN
 END SUBROUTINE scree_plot
!
!
!
 SUBROUTINE get_scree_plot_data (m,n,sv,arraysz,modeindex,evalues,pvars,spvars,bsticks)
!
! Modules
  USE numbers, ONLY: one,zero,oneh
!
! Aguments
!
! Input scalars
  INTEGER, INTENT(IN) :: m       ! - number of modes -
  INTEGER, INTENT(IN) :: n       ! - number of cases -
  INTEGER, INTENT(IN) :: arraysz ! - size of output array -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: sv(:) ! - singular values -
!
! Input/output values
  INTEGER, INTENT(INOUT) :: modeindex(arraysz) ! - mode index -
!
  REAL(KIND=rp), INTENT(INOUT) :: evalues(arraysz) ! - eigenvalue -
  REAL(KIND=rp), INTENT(INOUT) :: pvars(arraysz)   ! - percentage variance -
  REAL(KIND=rp), INTENT(INOUT) :: spvars(arraysz)  ! - cumulative percentage variance -
  REAL(KIND=rp), INTENT(INOUT) :: bsticks(arraysz) ! - broken sticks -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - mode index -
!
  REAL(KIND=rp) :: seg    ! - stick segment -
  REAL(KIND=rp) :: df     ! - number of cases -
  REAL(KIND=rp) :: evalue ! - eigenvalue -
  REAL(KIND=rp) :: tvar   ! - total variance -
  REAL(KIND=rp) :: pvar   ! - percentage variance -
  REAL(KIND=rp) :: svar   ! - cumulative variance -
  REAL(KIND=rp) :: spvar  ! - cumulative percentage variance -
!
! Local arrays
  REAL(KIND=rp) :: tmp(arraysz) ! - broken sticks -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC REAL
  INTRINSIC SUM

! Executable Statements
!
! Calculate broken stick
  tmp=oneh/REAL(m,KIND=rp)
  seg=zero
  DO i=m,1,-1
     seg=seg+one/REAL(i,KIND=rp)
     tmp(i)=tmp(i)*seg
  END DO
  DO i=1,m
    bsticks(i) = tmp(i)
  END DO
!
  df=REAL(n-1,KIND=rp)
  tvar=SUM(sv(1:m)**2/df)
  svar=zero
  DO i=1,m
     evalue=sv(i)**2/df
     svar=svar+evalue
     pvar=oneh*evalue/tvar
     spvar=oneh*svar/tvar

     modeindex(i)=i
     evalues(i)=evalue
     pvars(i)=pvar
     spvars(i)=spvar
  END DO
!
  RETURN
 END SUBROUTINE get_scree_plot_data
END MODULE scree
