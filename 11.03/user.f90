! $Id: user.f90 1224 2011-03-03 16:28:24Z simon $
MODULE user
!
! Modules
  USE numbers, ONLY: rp
!
! Implicit declarations
  IMPLICIT NONE
!
! Scalars
!
! Character scalars
  CHARACTER(LEN=256+7), PUBLIC :: CPT_ini     ! - CPT initialization file -
!
CONTAINS
!
!
 SUBROUTINE get_user ()
!
! Identifies current user
!
! Executable Statements
!
! Identify current user and directory
  CPT_ini='CPT.ini'
!
  RETURN
 END SUBROUTINE get_user
!
!
!
 RECURSIVE SUBROUTINE read_ini (CPT_ini)
!
! Modules
  USE bootstrap,    ONLY: nboot,clb,nperm
  USE categories,   ONLY: ithr,thr,pthr
  USE CPT_text,     ONLY: ilang
  USE errors,       ONLY: error
  USE fields,       ONLY: xarea,yarea
  USE IO_constants, ONLY: iin
  USE iofiles,      ONLY: odir
  USE missing,      ONLY: immx,immy,ipmx,ipmy,ipvx,ipvy,xmiss,ymiss
  USE numbers,      ONLY: sp,one
  USE pcs,          ONLY: nxe,nye,ncc,mxe,mye,mcc
  USE settings,     ONLY: istd,igcms,iev,iprec,intp,lcw,lcw_old,clf,nens,igood
  USE version,      ONLY: ver
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: CPT_ini ! - CPT initialization file -
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
!
  REAL(KIND=sp) :: ver_this ! - version number of .ini file -
!
! Executable Statements
!
! Read customised defaults
  OPEN (UNIT=iin,FILE=CPT_ini,ACCESS='sequential',ACTION='read',FORM='formatted',&
        ERR=1,STATUS='old')
  READ (UNIT=iin,FMT='(40X,F8.2)',ERR=2,END=2) ver_this
  IF (ver_this<ver) GOTO 3
  READ (UNIT=iin,FMT='(40X,A)',ERR=2,END=2) odir
  READ (UNIT=iin,FMT='(40X,I8)',ERR=2,END=2) lcw
  READ (UNIT=iin,FMT='(40X,I8)',ERR=2,END=2) mxe
  READ (UNIT=iin,FMT='(40X,I8)',ERR=2,END=2) nxe
  READ (UNIT=iin,FMT='(40X,I8)',ERR=2,END=2) mye
  READ (UNIT=iin,FMT='(40X,I8)',ERR=2,END=2) nye
  READ (UNIT=iin,FMT='(40X,I8)',ERR=2,END=2) mcc
  READ (UNIT=iin,FMT='(40X,I8)',ERR=2,END=2) ncc
  READ (UNIT=iin,FMT='(40X,I8)',ERR=2,END=2) immx
  READ (UNIT=iin,FMT='(40X,I8)',ERR=2,END=2) ipmx
  READ (UNIT=iin,FMT='(40X,I8)',ERR=2,END=2) ipvx
  READ (UNIT=iin,FMT='(40X,G24.12)',ERR=2,END=2) xmiss
  READ (UNIT=iin,FMT='(40X,I8)',ERR=2,END=2) immy
  READ (UNIT=iin,FMT='(40X,I8)',ERR=2,END=2) ipmy
  READ (UNIT=iin,FMT='(40X,I8)',ERR=2,END=2) ipvy
  READ (UNIT=iin,FMT='(40X,G24.12)',ERR=2,END=2) ymiss
  READ (UNIT=iin,FMT='(40X,G24.12)',ERR=2,END=2) xarea%rltn  
  READ (UNIT=iin,FMT='(40X,G24.12)',ERR=2,END=2) xarea%rlts  
  READ (UNIT=iin,FMT='(40X,G24.12)',ERR=2,END=2) xarea%rlgw  
  READ (UNIT=iin,FMT='(40X,G24.12)',ERR=2,END=2) xarea%rlge  
  READ (UNIT=iin,FMT='(40X,G24.12)',ERR=2,END=2) yarea%rltn  
  READ (UNIT=iin,FMT='(40X,G24.12)',ERR=2,END=2) yarea%rlts  
  READ (UNIT=iin,FMT='(40X,G24.12)',ERR=2,END=2) yarea%rlgw  
  READ (UNIT=iin,FMT='(40X,G24.12)',ERR=2,END=2) yarea%rlge  
  READ (UNIT=iin,FMT='(40X,I8)',ERR=2,END=2) nboot
  READ (UNIT=iin,FMT='(40X,G24.12)',ERR=2,END=2) clb
  READ (UNIT=iin,FMT='(40X,I8)',ERR=2,END=2) nperm
  READ (UNIT=iin,FMT='(40X,G24.12)',ERR=2,END=2) clf
  READ (UNIT=iin,FMT='(40X,I8)',ERR=2,END=2) ithr
  READ (UNIT=iin,FMT='(40X,G24.12)',ERR=2,END=2) pthr(1)
  READ (UNIT=iin,FMT='(40X,G24.12)',ERR=2,END=2) pthr(2)
  READ (UNIT=iin,FMT='(40X,G24.12)',ERR=2,END=2) thr(1)
  READ (UNIT=iin,FMT='(40X,G24.12)',ERR=2,END=2) thr(2)
  READ (UNIT=iin,FMT='(40X,I8)',ERR=2,END=2) iev
  READ (UNIT=iin,FMT='(40X,I8)',ERR=2,END=2) istd
  READ (UNIT=iin,FMT='(40X,I8)',ERR=2,END=2) iprec
  READ (UNIT=iin,FMT='(40X,I8)',ERR=2,END=2) nens
  READ (UNIT=iin,FMT='(40X,I8)',IOSTAT=ifail) ilang
  SELECT CASE (ifail)
   CASE (0)
     READ (UNIT=iin,FMT='(40X,I8)',IOSTAT=ifail) intp
     SELECT CASE (ifail)
      CASE (0)
        READ (UNIT=iin,FMT='(40X,I8)',IOSTAT=ifail) igood
        SELECT CASE (ifail)
         CASE (0)
           CONTINUE
         CASE (-1)
           igood=3
         CASE DEFAULT
           GOTO 2
        END SELECT
      CASE (-1)
        intp=0
        igood=3
      CASE DEFAULT
        GOTO 2
     END SELECT
   CASE (-1)
     ilang=1
     intp=0
     igood=3
   CASE DEFAULT
     GOTO 2
  END SELECT
  CLOSE (UNIT=iin)
!
! Set GCM standardization option
  igcms=istd
!
! Set backups
  lcw_old=lcw
!
! Apply threshold settings
  pthr(2)=one-pthr(2)
  RETURN
!
! Errors
1 ifail=1
  CALL error ('read_ini',ifail, &
       c_arg1=TRIM(CPT_ini))
  STOP
!
2 IF (TRIM(CPT_ini)=='CPT.ini') THEN
     ifail=2
  ELSE
     ifail=3
  END IF
  CALL error ('read_ini',ifail, &
       c_arg1=TRIM(CPT_ini))
  STOP
!
3 ifail=4
  CALL error ('read_ini',ifail, &
       c_arg1='CPT.ini')
  STOP
!
 END SUBROUTINE read_ini
END MODULE user
