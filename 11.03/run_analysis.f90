! $Id: run_analysis.f90 1240 2011-03-04 16:43:14Z simon $
FUNCTION run_analysis()
!
! Modules
  USE analysis,      ONLY: ianal,prog, &
                           analysis_flags,close_analysis
  USE arrays,        ONLY: x,y,iusex,iusey,kuse
  USE categories,    ONLY: climate_per,ithr, &
                           check_climates,check_analogues
  USE cca,           ONLY: perform_cca
  USE CPT_constants, ONLY: mnt
  USE data_input,    ONLY: get_data
  USE errors,        ONLY: error
  USE fields,        ONLY: xfield,yfield
  USE gcm,           ONLY: perform_gcm
  USE gui,           ONLY: irv,iscree
  USE iofiles,       ONLY: xfile,yfile,zfile
  USE labels,        ONLY: cg_atypes_a,cg_climate,cg_done,cg_dsds
  USE missing,       ONLY: ipmx,ipvx,immx,ipmy,ipvy,immy, &
                           non_missing
  USE numbers,       ONLY: zero,one
  USE pcr,           ONLY: perform_pcr
  USE pcs,           ONLY: npx,npy, &
                           check_pcs
  USE settings,      ONLY: iretro,izero,istd,istdo,ifcast,lcw,nu,nt,mxa,mya, &
                           check_n,get_cv,get_retro,set_nused
!
! Implicit declarations
  IMPLICIT NONE
!
! Function type
  INTEGER :: run_analysis
!
! Locals
!
! Local scalars
  INTEGER :: k     ! - case index -
  INTEGER :: ifail ! - error indicator -
  INTEGER :: ierr  ! - error parameter -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ANY
  INTRINSIC TRIM
!
! Executable Statements
!
! Check for opened gridded or station X file prior to viewing MLR
  IF ((ianal==3).AND.(xfile%igrid/=3)) THEN
     ifail=1
     CALL error ('run_analysis',ifail, &
          i_arg1=xfile%igrid,c_arg1=xfile%ffile)
     run_analysis=1
     RETURN
!
! Check for opened gridded or station X file prior to viewing GCM
  ELSE IF (ianal==4) THEN
     IF (xfile%igrid/=1) THEN
        ifail=2
        CALL error ('run_analysis',ifail, &
             i_arg1=xfile%igrid,c_arg1=xfile%ffile)
        run_analysis=1
        RETURN
     ELSE IF (yfile%igrid>2) THEN
        ifail=3
        CALL error ('run_analysis',ifail, &
             i_arg1=xfile%igrid,c_arg1=xfile%ffile)
        run_analysis=1
        RETURN
     END IF
     npx=0
     npy=0
  END IF
!
! Check for invalid length of training period
  ifail=check_n(nt,lcw,iretro,ierr)
  IF (ifail/=0) THEN
     CALL error ('check_n',ifail, &
          i_arg1=ierr)
     run_analysis=1
     RETURN
  END IF
!
! Check for cancelled retroactive calculations
  SELECT CASE (iretro)
   CASE (0) ! - cross-validation -
     run_analysis=get_cv()
   CASE (1) ! - retroactive -
     run_analysis=get_retro()
     IF (iretro==-1) THEN
        iretro=0
        run_analysis=1
        RETURN
     END IF
  END SELECT
  run_analysis=1
!
! - initialise calculation flags -
  CALL analysis_flags ('on')
!
! Read data
  ifail=get_data()
  IF (ifail/=0) GOTO 1
!
! Replace missing values and check for missing variables
  WRITE (UNIT=*,FMT='(A)') 'Checking for missing values ...'
  kuse(:)=.true.
! - X data -
  CALL non_missing (xfile%nfl,xfield(:)%nv,nt,x,0,immx,ipmx,ipvx,xfield(:)%rmiss,xfield(:)%nva,mxa,iusex,kuse,ifail)
  SELECT CASE (ifail)
   CASE (0)
     SELECT CASE (xfile%igeog)
      CASE (1)
        WRITE (UNIT=*,FMT='(A,T55,I10)') 'Total number of non-missing '//TRIM(cg_dsds(xfile%igrid))//' in X domain:' ,mxa
      CASE (0)
        WRITE (UNIT=*,FMT='(A,T55,I10)') 'Total number of non-missing '//TRIM(cg_dsds(xfile%igrid))//' in file:',mxa
     END SELECT
   CASE (1,2)
     CALL error ('non_missing',ifail, &
          c_arg1='X',c_arg2=TRIM(xfile%ffile))
     GOTO 1
   CASE (3,4)
     CALL error ('non_missing',ifail)
  END SELECT
! - Y data -
  CALL non_missing (yfile%nfl,yfield(:)%nv,nt,y,izero,immy,ipmy,ipvy,yfield(:)%rmiss,yfield(:)%nva,mya,iusey,kuse,ifail)
  SELECT CASE (ifail)
   CASE (0)
     SELECT CASE (yfile%igeog)
      CASE (1)
        WRITE (UNIT=*,FMT='(A,T55,I10)') 'Total number of non-missing '//TRIM(cg_dsds(yfile%igrid))//' in Y domain:' ,mya
      CASE (0)
        WRITE (UNIT=*,FMT='(A,T55,I10)') 'Total number of non-missing '//TRIM(cg_dsds(yfile%igrid))//' in file:',mya
     END SELECT
   CASE (1,2)
     CALL error ('non_missing',ifail, &
          c_arg1='Y',c_arg2=TRIM(yfile%ffile))
     GOTO 1
   CASE (3,4)
     CALL error ('non_missing',ifail)
  END SELECT
!
! Identify usable cases, and compress arrays
  nu=0
  DO k=1,nt
     IF (kuse(k)) nu=nu+1
     IF (nu==0) CYCLE
     IF (nu<k) THEN
        x(1:mxa,nu)=x(1:mxa,k)
        y(1:mya,nu)=y(1:mya,k)
     END IF
  END DO
!
! Double check for invalid length of training period given missing values
  IF (nu<nt) THEN
     ifail=check_n(nu,lcw,iretro,ierr)
     IF (ifail/=0) THEN
        CALL error ('check_n',ifail, &
             i_arg1=ierr)
        GOTO 1
     END IF
  END IF
  IF (set_nused()/=0) THEN
     ifail=1
     CALL error ('set_nused',ifail, &
          i_arg1=mnt-1)
  END IF
!
! Check for zero-bound
  IF (izero==1) THEN
     IF (ANY(y(1:mya,1:nu)<zero)) THEN
        ifail=4
        CALL error ('run_analysis',ifail, &
             c_arg1=TRIM(yfile%ffile))
        GOTO 1
     END IF
  END IF
!
! End progress meter
  prog=one
  WRITE (UNIT=*,FMT='(A)') 'Data read successfully'
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') 'Beginning analysis ...'
!
! Set climatologies
  CALL check_climates ()
  WRITE (UNIT=*,FMT='(54A)') TRIM(cg_climate),': ',TRIM(climate_per%clim1),' to ',TRIM(climate_per%clim2)
!
! Check analogue years
  IF (ithr==3) THEN
     ifail=check_analogues()
  END IF
!
! Check validity of EOF and CCA settings
  ifail=check_pcs()
  SELECT CASE (ifail)
   CASE (0)
     CONTINUE
   CASE (-1)
     GOTO 1
   CASE DEFAULT
     CALL error ('check_pcs',ifail)
     IF (ifail>0) GOTO 1
  END SELECT
!
! Perform analysis
  istdo=istd
  SELECT CASE (ianal)
   CASE (1) ! - CCA -
     ifail=perform_cca()
   CASE (2) ! - PCR -
     ifail=perform_pcr()
   CASE (3) ! - MLR -
     ifail=perform_pcr()
   CASE (4) ! - GCM -
     ifail=perform_gcm()
  END SELECT
  SELECT CASE (ifail)
   CASE (0)
     CONTINUE
   CASE (-1)
     GOTO 1
   CASE DEFAULT
     CALL error ('perform_'//cg_atypes_a(ianal),ifail, &
          c_arg1=cg_atypes_a(ianal))
     GOTO 1
  END SELECT
!
! Permit forecasts if Z data file is set
  IF (zfile%lset) THEN
     ifcast=1
  ELSE
     ifcast=0
  END IF
!
! Permit scree plots if number of modes is more than one
  IF ((npx>1).OR.(npy>1)) THEN
     iscree=1
  ELSE
     iscree=0
  END IF
!
! End analysis
  irv=iretro
! - switch off calculation flags -
  CALL analysis_flags ('end')
! - end progress meter -
  prog=one
  WRITE (UNIT=*,FMT='(A)') TRIM(cg_done)//'!'
  RETURN
!
! Terminate analysis
1 CALL analysis_flags ('off')
  ifail=close_analysis() ! - clear memory -
  prog=zero
!
  RETURN
END FUNCTION run_analysis
