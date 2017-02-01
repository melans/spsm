MODULE get_input_file
!
! Modules
  USE fields,  ONLY: field,domain,area
  USE iofiles, ONLY: ifile
  USE numbers, ONLY: rp
  USE time
!
! Implicit declarations
  IMPLICIT NONE
!
CONTAINS
!
!
 FUNCTION get_xfile()
!
! Accesses an X file, and determines its structure
!
! Error indicators:
!    ifail =  0 Successful
!    ifail =  1 Problem allocating memory
!
! Modules
  USE analysis, ONLY: ianal,&
                      reset
  USE arrays,   ONLY: kax,kaz
  USE fields,   ONLY: xfield,rlatx,rlngx,cstnx,rlatdx,rlngdx,cstndx,idomx, &
                      zfield,                                              &
                      init_field
  USE gui,      ONLY: set_greyflags                    
  USE iofiles,  ONLY: xfile,yfile,zfile,bkfile,ddir, &
                      init_ifile
  USE missing,  ONLY: xmiss
  USE pcs,      ONLY: eofx_opts
  USE settings, ONLY: nf,nt,nv,nx,nz,xfd_old
!
! Function type
  INTEGER :: get_xfile
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ASSOCIATED
!
! Executable Statements
!
! Check for completed calculations
  get_xfile=2
  IF (reset('Opening a new X file')==1) RETURN
!
! Get X input file
  nv=>nx
  CALL get_ifile ('X',xfile,xfield,yfile,nv,xmiss,rlatx,rlngx,rlatdx,rlngdx,cstnx,cstndx,idomx,kax,eofx_opts,ifail)
!
! Reset forecast file
  IF (ifail==0) THEN
     IF (xfile%ffile/=bkfile%ffile) THEN ! - if X file is the same as previous, update the settings in case the file was modified -
        xfd_old=xfile%fdate%iyr
        zfile=xfile
        IF ((nt>0).AND.(xfile%fdate+(nt-1)<xfile%periodn%sdate)) THEN 
           ! - set default first forecast date to after end of training period - 
           zfile%fdate=xfile%fdate+nt
           nf=date_diff(zfile%fdate,zfile%periodn%sdate,zfile%iseq)+1
        ELSE
           zfile%fdate=zfile%periodn%sdate
           nf=1
        END IF
        CALL init_field (zfield,zfile%nfl,zfield(:)%rmiss,ifail) ! - reset Z fields -
        IF (ifail==0) THEN
           zfield(:)=xfield(:)
           nz=nx
        ELSE
           ifail=1
           CALL init_ifile (zfile,dir=ddir)
           CALL init_field (zfield,1,(/xmiss/),ifail)
           nz=0
        END IF
     END IF
     IF ((zfile%ffile==xfile%ffile).AND.(ifail==0)) THEN ! - update forecast file settings in case of modified xfile contents -
        zfile%cdate1=xfile%cdate1
        zfile%cdaten=xfile%cdaten
        zfile%cssn=xfile%cssn
        zfile%period1=xfile%period1
        zfile%periodn=xfile%periodn
        IF (ASSOCIATED(kaz)) THEN
           DEALLOCATE (kaz)
           NULLIFY (kaz)
        END IF
        ALLOCATE (kaz(xfile%nt,xfile%nfl),STAT=ifail)
        IF (ifail==0) THEN
           kaz(:,:)=kax(:,:)
        ELSE
           ifail=1
           CALL init_ifile (zfile,dir=ddir)
           CALL init_field (zfield,1,(/xmiss/),ifail)
           nz=0
        END IF
     END IF
!
! Disable any invalid analyses
     CALL set_greyflags (ianal,xfile%igrid,yfile%igrid)
  ELSE
     get_xfile=1
  END IF
!
  RETURN
 END FUNCTION get_xfile
!
!
!
 FUNCTION get_yfile()
!
! Accesses a Y file, and determines its structure
!
! Modules
  USE analysis,   ONLY: ianal, &
                        reset
  USE arrays,     ONLY: kay
  USE categories, ONLY: iclim,climate_bak,climate_per, &
                        init_climate
  USE fields,     ONLY: yfield,rlaty,rlngy,cstny,rlatdy,rlngdy,cstndy,idomy,dsdy
  USE gui,        ONLY: set_greyflags
  USE iofiles,    ONLY: xfile,yfile
  USE labels,     ONLY: cg_dsds
  USE missing,    ONLY: ymiss
  USE pcs,        ONLY: eofy_opts
  USE settings,   ONLY: izero,nv,ny,yfd_old
!
! Function type
  INTEGER :: get_yfile
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
!
! Executable Statements
!
! Check for completed calculations
  get_yfile=2
  IF (reset('Opening a new Y file')==1) RETURN
!
! Get Y input file
  nv=>ny
  CALL get_ifile ('Y',yfile,yfield,xfile,nv,ymiss,rlaty,rlngy,rlatdy,rlngdy,cstny,cstndy,idomy,kay,eofy_opts,ifail)
!
! Get data structure description, and set default zero-bound setting 
  IF (ifail==0) THEN
     dsdy=cg_dsds(yfile%igrid)
     yfd_old=yfile%fdate%iyr
     IF (yfile%nfs==1) THEN
        SELECT CASE (yfield(1)%var)
         CASE ('prcp')
           izero=1
         CASE DEFAULT
           izero=0
        END SELECT
     ELSE
        izero=0
     END IF
!
! Reset climatology
     climate_bak=climate_per
     IF ((climate_bak%it1>=yfile%period1%sdate).AND.(climate_bak%it2<=yfile%periodn%sdate)) THEN
        climate_per%it1=climate_bak%it1
        climate_per%it2=climate_bak%it2
     ELSE
        CALL init_climate ()
     END IF
     iclim=1
!
! Disable any invalid analyses
     CALL set_greyflags (ianal,xfile%igrid,yfile%igrid)
  ELSE
     get_yfile=1
  END IF
!
  RETURN
 END FUNCTION get_yfile
!
!
!
 FUNCTION get_zfile()
!
! Modules
  USE iofiles, ONLY: zfile,bkfile, &
                     get_old_file
!
! Function type
  INTEGER :: get_zfile
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
!
! Executable Statements
!
! Select input file
  bkfile=zfile
  CALL get_old_file ('Forecast Data File',zfile%ffile,zfile%fdir,zfile%fname,ifail)
  IF (ifail==0) THEN
     get_zfile=check_zfile()
  ELSE
     zfile%lset=.false.
     get_zfile=3
  END IF
!
  RETURN
!
 CONTAINS
!
!
  FUNCTION check_zfile()
!
! Accesses a forecast file, and determines its structure
!
! Error indicators:
!    ifail =  0 Successful
!    ifail =  1 Problem allocating memory
!    ifail =  2 File version is inconsistent with corresponding X file
!    ifail =  3 File structure is inconsistent with corresponding X file
!    4 <= ifail <= 8 Fields are inconsistent with corresponding X file
!
! Modules
  USE analysis,     ONLY: ifc, &
                          close_fcast
  USE arrays,       ONLY: kaz
  USE errors,       ONLY: cproc, &
                          error
  USE fields,       ONLY: xfield,rlatx,rlngx,                                  &
                          zfield,rlatz,rlngz,cstnz,rlatdz,rlngdz,cstndz,idomz, &
                          reset_grids,init_field
  USE IO_constants, ONLY: iin,lfil
  USE iofiles,      ONLY: xfile, &
                          open_infile,init_ifile,file_version
  USE missing,      ONLY: xmiss
  USE settings,     ONLY: ifcast,nt,nf,nx,nz,mza, &
                          record_change
!
! Function type
  INTEGER :: check_zfile
!
! Locals
!
! Local scalars
  INTEGER :: l     ! - field / lagged field index -
  INTEGER :: iarg1 ! - error optional argument -
  INTEGER :: iarg2 ! - error optional argument -
  INTEGER :: ifd   ! - current field -
  INTEGER :: ilf   ! - current lagged field -
  INTEGER :: ifail ! - error indicator -
!
  CHARACTER(LEN=lfil) :: carg2 ! - error optional argument -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ANY
  INTRINSIC TRIM
!
! Executable Statements
!
! Initialise routine
  cproc='check_zfile'
  carg2=' '
  iarg1=1
  iarg2=1
!
! Open file
  CALL open_infile (iin,zfile%ffile,.true.,.true.,ifail)
  IF (ifail/=0) THEN
     cproc='open_infile'
     GOTO 1
  END IF
!
! Copy field information from X
  IF (zfile%ffile==bkfile%ffile) THEN
     check_zfile=2
     RETURN
  ELSE IF (zfile%ffile==xfile%ffile) THEN
     CALL init_field (zfield,zfile%nfl,zfield(:)%rmiss,ifail)
     IF (ifail/=0) THEN
        ifail=1
        GOTO 1
     END IF
     zfile=xfile
     zfield(:)=xfield(:)
!
! Check dataset structure
  ELSE
     zfile%ffmt%iver=file_version(zfile%ffile)
     IF (zfile%ffmt%iver/=xfile%ffmt%iver) THEN
        ifail=2
        carg2=xfile%ffile
        GOTO 1
     END IF
     CALL get_structure (zfile,zfield,xmiss,rlatz,rlngz,cstnz,rlatdz,rlngdz,cstndz,idomz,kaz,ifail)
     IF (ifail/=0) GOTO 2
! - check for consistency with xfile -
     IF (zfile%igrid/=xfile%igrid) THEN
        ifail=3
        iarg1=zfile%igrid
        iarg2=xfile%igrid
        GOTO 1
     END IF
     iarg1=0
     iarg2=0
! - check for consistency of field settings -
     DO ifd=1,zfile%nfs
        IF (zfile%nfs>1) iarg1=ifd
        DO ilf=1,zfile%nls
           IF (zfile%nls>1) iarg2=ilf
           l=(ifd-1)*zfile%nls+ilf
           IF (zfield(l)%var/=xfield(l)%var) THEN
              ifail=4
              carg2='field'
              GOTO 1
           END IF
           IF (zfield(l)%unit/=xfield(l)%unit) THEN
              ifail=4
              carg2='unit'
              GOTO 1
           END IF
           IF (zfield(l)%nlt/=xfield(l)%nlt) THEN
              ifail=5
              carg2='latitudes'
              GOTO 1
           END IF
           IF (zfield(l)%nlg/=xfield(l)%nlg) THEN
              ifail=5
              carg2='longitudes'
              GOTO 1
           END IF
           IF ((zfield(l)%ln2s.AND..NOT.xfield(l)%ln2s).OR.(.NOT.zfield(l)%ln2s.AND.xfield(l)%ln2s)) THEN
              ifail=6
              GOTO 1
           END IF
           IF (ANY(rlatz(:,l)/=rlatx(:,l))) THEN
              ifail=7
              carg2='Latitudes'
              GOTO 1
           END IF
           IF (ANY(rlngz(:,l)/=rlngx(:,l))) THEN
              ifail=7
              carg2='Longitudes'
              GOTO 1
           END IF
        END DO
     END DO
! - free unnecessary array memory -
     CALL reset_grids (rlatz,rlngz,cstnz,rlatdz,rlngdz,cstndz,idomz,ifail)
! - date settings -
     zfile%cdate1=get_cdate(zfile%period1,2)
     zfile%cdaten=get_cdate(zfile%periodn,2)
     IF (zfile%iseq==0) zfile%iseq=xfile%iseq
     zfile%cssn=get_cssn(zfile%period1,zfile%iseq)
     IF (zfile%cssn/=xfile%cssn) THEN
        ifail=8
        GOTO 1
     END IF
     IF (zfile%igrid==3) zfield(:)%nv=zfield(:)%nlt
  END IF
!
! Set default first date of interest and numbers of forecasts
  IF (zfile%ffile==xfile%ffile) THEN
     IF (xfile%fdate+(nt-1)<xfile%periodn%sdate) THEN
        zfile%fdate=xfile%fdate+nt
        nf=date_diff(zfile%fdate,zfile%periodn%sdate,zfile%iseq)+1
     ELSE
        zfile%fdate=zfile%periodn%sdate
        nf=1
     END IF
  ELSE
     zfile%fdate=zfile%period1%sdate
     nf=zfile%nt
     zfield(:)%region=xfield(:)%region
  END IF
  nz=nx
  mza=0
!
! Set if successful
  zfile%cssn=xfile%cssn
  zfile%lset=.true.
  ifcast=1
  check_zfile=record_change()
  check_zfile=2
  ifail=0
  CLOSE(iin)
!
  RETURN
!
! Reset if error
1 CLOSE (UNIT=iin)
  CALL error (TRIM(cproc),ifail, &
       c_arg1=TRIM(zfile%ffile),c_arg2=TRIM(carg2),i_arg1=iarg1,i_arg2=iarg2)
!
! Clear memory
2 check_zfile=close_fcast()
  CALL init_ifile (zfile)
  ifcast=0
  ifc=0
  check_zfile=3
!
  RETURN
  END FUNCTION check_zfile
 END FUNCTION get_zfile
!
!
!
 SUBROUTINE get_ifile (cxy,afile,afield,bfile,nv,rmiss,rlat,rlng,rlatd,rlngd,cstn,cstnd,idom,ka,feof,ifail)
!
! Accesses an input file, and determines its structure
!
! On exit (if cproc='get_ifile'):
!    ifail =  0 Successful
!    ifail =  1 Gridded and station X input files not permitted for MLR
!    ifail =  2 Station and unreferenced X input files not permitted for GCM
!    ifail =  3 Multi-field X input files not permitted for GCM
!    ifail =  4 Insufficient number of cases
!    ifail =  5 Date sequencing is inconsistent with currently opened input file
!
! Modules
  USE analysis,      ONLY: ianal,iaction
  USE CPT_constants, ONLY: mnt
  USE errors,        ONLY: cproc, &
                           error
  USE fields,        ONLY: bkfield, &
                           reset_grids,get_area,init_field,full_domain
  USE IO_constants,  ONLY: lfil,lstn
  USE iofiles,       ONLY: bkfile, &
                           get_old_file,file_version
  USE labels,        ONLY: cg_dsds_l
  USE pcs,           ONLY: cca_opts
  USE settings,      ONLY: nt, &
                           record_change
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: rmiss ! - default missing value flag -
!
  CHARACTER(LEN=1), INTENT(IN) :: cxy ! - X / Y variables flag -
!
  TYPE(ifile), INTENT(IN) :: bfile ! - second input file -
!
! Pointer scalars
  INTEGER, POINTER :: nv ! - number of variables -
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail  ! - error indicator -
!
! Pointer arrays
  INTEGER, POINTER :: idom(:,:) ! - variable is within domain -
!
  REAL(KIND=rp), POINTER :: rlat(:,:)  ! - latitudes -
  REAL(KIND=rp), POINTER :: rlng(:,:)  ! - longitudes -
  REAL(KIND=rp), POINTER :: rlatd(:,:) ! - used latitudes -
  REAL(KIND=rp), POINTER :: rlngd(:,:) ! - realigned/used longitudes -
!
  CHARACTER(LEN=lstn), POINTER :: cstn(:,:)  ! - names of stations -
  CHARACTER(LEN=lstn), POINTER :: cstnd(:,:) ! - names of stations within domains -
!
  LOGICAL, POINTER :: ka(:,:) ! - available cases flags -
!
  TYPE(field), POINTER :: afield(:) ! - first field -
!
! Procedure arguments
  INTEGER, EXTERNAL :: feof ! - EOF settings function -
!
! Locals
!
! Local scalars
  INTEGER :: iarg1  ! - error optional argument -
  INTEGER :: iarg2  ! - error optional argument -
  INTEGER :: isq    ! - sequencing indicator -
  INTEGER :: l      ! - current field / lagged field -
  INTEGER :: nv_old ! - backup number of variables -
  INTEGER :: lag    ! - lag -
  INTEGER :: lagadj ! - lag adjustment -
!
  CHARACTER(LEN=lfil) :: carg2 ! - second character error message argument -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ASSOCIATED
  INTRINSIC MIN
  INTRINSIC SIZE
  INTRINSIC TRIM
!
! Executable Statements
!
! Initialise routine
  cproc='get_ifile'
!
! Backup old file
  bkfile=afile
  nv_old=nv
  ALLOCATE (bkfield(SIZE(afield)))
  bkfield=afield
!
! Select input file
  CALL get_old_file (cxy//' Input File',afile%ffile,afile%fdir,afile%fname,ifail)
  IF (ifail/=0) THEN
     afile=bkfile
     nv=nv_old
     afield=bkfield
     DEALLOCATE (bkfield)
     RETURN
  END IF
!
! Check for version 10+ format
  afile%ffmt%iver=file_version(afile%ffile)
!
! Determine dataset structure
  CALL get_structure (afile,afield,rmiss,rlat,rlng,cstn,rlatd,rlngd,cstnd,idom,ka,ifail)
  IF (ifail/=0) GOTO 2
  cproc='get_ifile'
  IF (afile%nt<mnt) THEN
     iarg1=afile%iseq
     ifail=4
     GOTO 1
  END IF
!
! Check for consistent file sequences
  IF (bfile%lset) THEN
     IF (afile%iseq/=bfile%iseq) THEN
        carg2=bfile%ffile
        iarg1=afile%iseq
        iarg2=bfile%iseq
        ifail=5
        GOTO 1
     END IF
  END IF
  iseq=afile%iseq
!
! Check for valid input file formats
  IF (cxy=='X') THEN
     IF ((afile%igrid<=2).AND.(ianal==3)) THEN ! - disable gridded and station X input files for MLR -
        carg2=' '
        ifail=1
        GOTO 1
     ELSE IF ((afile%igrid/=1).AND.(ianal==4)) THEN ! - disable station and unreferenced X input files for GCM -
        carg2=' '
        ifail=2
        GOTO 1
     END IF
  END IF
  IF ((afile%nfl>1).AND.(ianal==4)) THEN ! - disable multi-field X input files for GCM -
     carg2=' '
     ifail=3
     GOTO 1
  END IF
!
! Prompt for domain if relevant
  SELECT CASE (afile%igrid)
! - gridded and station data -
   CASE (1,2)
     IF (SIZE(afield)==SIZE(bkfield)) THEN
        afield(:)%region%alim=bkfield(:)%region%alim
     ELSE
        DO l=1,afile%nfl
           afield(:)%region%alim=bkfield(1)%region%alim
        END DO
     END IF
     CALL get_area (cxy,afile%igrid,afile%nfs,afile%nls,afield,rlat,rlng,cstn,&
                      nv,rlatd,rlngd,cstnd,idom,ifail)
     IF (ifail/=0) GOTO 2
! - unreferenced data -
   CASE (3)
     nv=afield(1)%nlt
     afield(:)%nv=nv
     cstnd(:,:)=cstn(:,:)
    DO l=1,afile%nfl
       CALL full_domain (afield(l),idom(:,l))
    END DO
    ifail=0
! - cancel -
   CASE DEFAULT
     nv=nv_old
     afile=bkfile
     afield=bkfield
     DEALLOCATE (bkfield)
     RETURN
  END SELECT
!
! Prompt for numbers of modes if relevant
  SELECT CASE (ianal)
   CASE (1) ! - CCA -
     ifail=feof()
     IF (ifail==3) GOTO 2
     IF (bfile%lset) ifail=cca_opts()
     IF (ifail==3) GOTO 2
   CASE (2) ! - PCR -
     SELECT CASE (cxy)
      CASE ('X','x')
        ifail=feof()
      CASE DEFAULT
        CONTINUE
     END SELECT
   CASE DEFAULT
     CONTINUE
  END SELECT
!
! Update file settings
  afile%lset=.true.
  afile%cgss=TRIM(cg_dsds_l(afile%igrid))
  afile%cdate1=get_cdate(afile%period1,2)
  afile%cdaten=get_cdate(afile%periodn,2)
  afile%cssn=get_cssn(afile%period1,afile%iseq)
  DO l=1,afile%nfl
     afield(l)%cssn=get_cssn(afield(l)%tdate,afile%iseq)
  END DO
! - delete any old first date extraneous information -
  IF (afile%iseq==1) THEN
     afile%fdate%idy=afile%period1%sdate%idy
     afile%fdate%imn=afile%period1%sdate%imn
  END IF
!
! Set default first year of interest
! - retain prior date unless it is invalid -
  IF ((afile%fdate<afile%period1%sdate).OR.(afile%fdate>afile%periodn%sdate)) afile%fdate=afile%period1%sdate
! - set appropriate lag if the second input file is open -
  IF (bfile%lset) THEN
! - check lag -
     SELECT CASE (iseq)
      CASE (1)
        isq=2
      CASE DEFAULT
        isq=afile%iseq
     END SELECT
     SELECT CASE (cxy)
      CASE ('X')
        lag=date_diff(afile%fdate,bfile%fdate,isq)
        lagadj=-1
      CASE ('Y')
        lag=date_diff(bfile%fdate,afile%fdate,isq)
        lagadj=1
     END SELECT
! - if lag is negative set to a minimum positive lag, if viable -
     IF (lag<0) THEN
        IF ((bfile%fdate>afile%period1%sdate).AND.(bfile%fdate<afile%period1%sdate)) afile%fdate%iyr=afile%fdate%iyr+lagadj
     ELSE
        SELECT CASE (iseq)
         CASE (1)
           IF (lag>nmn) afile%fdate=afile%fdate-(lag/nmn)*lagadj
         CASE DEFAULT
           IF (lag>1) afile%fdate=afile%fdate-lag*lagadj
        END SELECT
     END IF
! - reset if invalid -
    IF ((afile%fdate<afile%period1%sdate).OR.(afile%fdate>afile%periodn%sdate)) afile%fdate=afile%period1%sdate
!
! Set default length of training period
     IF (nt==0) THEN
        nt=1+MIN(date_diff(afile%fdate,afile%periodn%sdate,afile%iseq), &
                 date_diff(bfile%fdate,bfile%periodn%sdate,bfile%iseq))
     ELSE
        nt=MIN(1+date_diff(afile%fdate,afile%periodn%sdate,afile%iseq),nt)
        nt=MIN(1+date_diff(bfile%fdate,bfile%periodn%sdate,bfile%iseq),nt)
     END IF
  END IF
! - check for file change -
  IF (afile%ffile/=bkfile%ffile) THEN
     ifail=record_change()
! - check for both files open -
     IF (bfile%lset) iaction=1
  END IF
  DEALLOCATE (bkfield)
  ifail=0
!
  RETURN
!
! Identify errors
1 CALL error (TRIM(cproc),ifail, &
       c_arg1=TRIM(afile%ffile),c_arg2=TRIM(carg2),i_arg1=iarg1,i_arg2=iarg2)
!
! Reset old file
2 afile=bkfile
  nv=nv_old
  IF (.NOT.ASSOCIATED(afield)) THEN
     CALL init_field (afield,1,(/rmiss/),ifail)
  ELSE
     CALL init_field (afield,SIZE(bkfield),bkfield(:)%rmiss,ifail)
     IF (ifail==0) THEN
        afield=bkfield
     ELSE
        CALL init_field (afield,1,(/rmiss/),ifail)
     END IF
  END IF
  DEALLOCATE (bkfield)
  IF (.NOT.afile%lset) CALL reset_grids (rlat,rlng,cstn,rlatd,rlngd,cstnd,idom,ifail)
  ifail=1
!
  RETURN
 END SUBROUTINE get_ifile
!
!
!
 SUBROUTINE get_structure (afile,afield,rmiss,rlat,rlng,cstn,rlatd,rlngd,cstnd,idom,ka,ifail)
!
! Determines structure of an input file, including its size, field settings, and missing cases
!
! On exit:
!    ifail =  0 Successful
!    ifail /= 1 Depends on value of cproc
!
! Arguments
!
! Modules
  USE errors,       ONLY: cproc, &
                          error
  USE IO_constants, ONLY: iin,lprd,lstn
  USE iofiles,      ONLY: open_infile
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: rmiss ! - missing value flag -
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Pointer arrays
  INTEGER, POINTER :: idom(:,:) ! - variables within domain -
!
  REAL(KIND=rp), POINTER :: rlat(:,:)  ! - latitudes -
  REAL(KIND=rp), POINTER :: rlng(:,:)  ! - longitudes -
  REAL(KIND=rp), POINTER :: rlatd(:,:) ! - realigned used latitudes -
  REAL(KIND=rp), POINTER :: rlngd(:,:) ! - realigned used longitudes -
!
  CHARACTER(LEN=lstn), POINTER :: cstn(:,:)  ! - names of stations -
  CHARACTER(LEN=lstn), POINTER :: cstnd(:,:) ! - names of stations within domain -
!
  LOGICAL, POINTER :: ka(:,:) ! - available cases flags -
!
  TYPE(field), POINTER :: afield(:)  ! - field -
!
! Locals
!
! Local scalars
  INTEGER :: iarg1 ! - error optional argument -
  INTEGER :: ifd   ! - current field -
!
  CHARACTER(LEN=lprd) :: cldate ! - last data read -
  CHARACTER(LEN=lprd) :: cfail  ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC TRIM
!
! Executable Statements
!
! Open file
  CALL open_infile (iin,afile%ffile,.true.,.true.,ifail)
  IF (ifail/=0) THEN
     cproc='open_infile'
     GOTO 1
  END IF
!
! Determine file structure
  SELECT CASE (afile%ffmt%iver)
   CASE (9)
     CALL get_structure_v9 (afile,afield,rmiss,rlat,rlng,cstn,rlatd,rlngd,cstnd,idom,ka,ifail,cproc,cldate,cfail)
     iarg1=0
   CASE (10)
     CALL get_structure_v10 (afile,afield,rmiss,rlat,rlng,cstn,rlatd,rlngd,cstnd,idom,ka,ifd,ifail,cproc,cldate,cfail)
     iarg1=ifd
   CASE DEFAULT
     cproc='file_version'
     ifail=5
     iarg1=afile%ffmt%iver
     cldate=' '
     cfail=' '
  END SELECT
!
! Errors
1 IF (ifail/=0) THEN
     CALL error (TRIM(cproc),ifail, &
          c_arg1=TRIM(afile%ffile),c_arg2=TRIM(cldate),c_arg3=TRIM(cfail),i_arg1=iarg1)
  END IF
  CLOSE (UNIT=iin)
!
! Set number of additional lags
  afile%nal=afile%nls-1
!
  RETURN
 END SUBROUTINE get_structure
!
!
!
 SUBROUTINE get_structure_v9 (afile,afield,rmiss,rlat,rlng,cstn,rlatd,rlngd,cstnd,idom,ka,ifail,cproc,cldate,cfail)
!
! Determines structure of a version-9 input file, including its size, field settings, and missing cases
!
! On exit (if cproc='get_structure_v9'):
!    ifail =  0 Successful
!    ifail =  1 Problem allocating memory
!    ifail =  2 Problem reading file. Data up to cldate read successfully
!    ifail =  3 Premature end of file reached. Data up to cldate read successfully
!    ifail =  4 File is not in a valid CPT format
!
! Modules
  USE fields,         ONLY: init_field
  USE gui,            ONLY: upcase
  USE IO_constants,   ONLY: iin,lprd,lstn
  USE time_constants, ONLY: lmon
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: rmiss ! - missing value flag -
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
  CHARACTER(LEN=*), INTENT(OUT) :: cproc  ! - procedure in error -
  CHARACTER(LEN=*), INTENT(OUT) :: cldate ! - last date read -
  CHARACTER(LEN=*), INTENT(OUT) :: cfail  ! - error indicator -
!
! Pointer arrays
  INTEGER, POINTER :: idom(:,:) ! - variables within domain -
!
  REAL(KIND=rp), POINTER :: rlat(:,:)  ! - latitudes -
  REAL(KIND=rp), POINTER :: rlng(:,:)  ! - longitudes -
  REAL(KIND=rp), POINTER :: rlatd(:,:) ! - realigned used latitudes -
  REAL(KIND=rp), POINTER :: rlngd(:,:) ! - realigned used longitudes -
!
  CHARACTER(LEN=lstn), POINTER :: cstn(:,:)  ! - names of stations -
  CHARACTER(LEN=lstn), POINTER :: cstnd(:,:) ! - names of stations within domain -
!
  LOGICAL, POINTER :: ka(:,:) ! - available cases flags -
!
  TYPE(field), POINTER :: afield(:)  ! - field -
!
! Locals
!
! Local scalars
  INTEGER :: idy ! - current day -
!
  CHARACTER(LEN=lmon) :: mycmon ! - current month -
  CHARACTER(LEN=lprd) :: ctag ! - tags -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC TRIM
!
! Executable Statements
!
! Initialise routine
  cproc='get_structure_v9'
  cldate=' '
  cfail=' '
!
! Clear field information
  afile%nfs=1
  afile%nls=1
  afile%nfl=1
  CALL init_field (afield,1,(/rmiss/),ifail)
  IF (ifail/=0) THEN
     ifail=1
     GOTO 3
  END IF
!
! Determine structure of dataset
  READ (UNIT=iin,FMT=*,ERR=1,END=2) ctag
  CALL upcase(ctag)
  SELECT CASE (TRIM(ctag))
   CASE ('STATION','STATIONS','STN') ! - station dataset -
     afile%igrid=2
     afile%igeog=1
   CASE ('YEARS','YEAR','NAME') ! - unreferenced -
     afile%igrid=3
     afile%igeog=0
   CASE DEFAULT ! - gridded -
     REWIND (UNIT=iin)
     READ (UNIT=iin,FMT=*,ERR=1,END=2) idy,mycmon
     IF (get_month(mycmon)==0) THEN
        ifail=4
        GOTO 3
     END IF
     afile%igrid=1
     afile%igeog=1
  END SELECT
!
! Determine structure of gridded dataset
  SELECT CASE (afile%igrid)
   CASE (1)
! - determine numbers of latitudes and longitudes -
     CALL get_gridded_v9 (iin,afile,afield(1),rlat,rlng,cstn,rlatd,rlngd,cstnd,idom,ka,ifail,cproc,cldate,cfail)
!
! Determine structure of station and unreferenced dataset
   CASE (2,3)
     CALL get_nongridded_v9 (iin,afile,afield(1),rlat,rlng,cstn,rlatd,rlngd,cstnd,idom,ka,ifail,cproc,cldate,cfail)
  END SELECT
  IF (ifail/=0) GOTO 3
!
! Set field information and number of tag lines
  afield(1)%tdate=afile%period1
  afield(1)%mdate=0
  afile%ntag=0
!
  CLOSE (UNIT=iin)
  ifail=0
  RETURN
!
! Errors
! - problem reading file -
1 CLOSE (UNIT=iin)
  ifail=2
  RETURN
! - problem reading file -
2 CLOSE (UNIT=iin)
  ifail=3
  RETURN
!
! - other errors -
3 CLOSE (UNIT=iin)
  RETURN
 END SUBROUTINE get_structure_v9
!
!
!
 SUBROUTINE get_gridded_v9 (iin,afile,afield,rlat,rlng,cstn,rlatd,rlngd,cstnd,idom,ka,ifail,cproc,cldate,cfail)
!
! Determines structure of a version-9 gridded input file, including its size, field settings, and missing cases
!
! On exit (if cproc='get_gridded_v9'):
!    ifail =  0 Successful
!    ifail =  1 Problem allocating memory
!
! Modules
  USE fields,       ONLY: reset_grids
  USE IO_constants, ONLY: lstn
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iin ! - input unit number -
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
  TYPE(field), INTENT(INOUT) :: afield ! - field -
!
  CHARACTER(LEN=*), INTENT(OUT) :: cproc  ! - procedure in error -
  CHARACTER(LEN=*), INTENT(OUT) :: cldate ! - last date read -
  CHARACTER(LEN=*), INTENT(OUT) :: cfail  ! - error indicator -
!
! Pointer arrays
  INTEGER, POINTER :: idom(:,:) ! - variables within domain -
!
  REAL(KIND=rp), POINTER :: rlat(:,:)  ! - latitudes -
  REAL(KIND=rp), POINTER :: rlng(:,:)  ! - longitudes -
  REAL(KIND=rp), POINTER :: rlatd(:,:) ! - realigned used latitudes -
  REAL(KIND=rp), POINTER :: rlngd(:,:) ! - realigned used longitudes -
!
  CHARACTER(LEN=lstn), POINTER :: cstn(:,:)  ! - names of stations -
  CHARACTER(LEN=lstn), POINTER :: cstnd(:,:) ! - names of stations within domain -
!
  LOGICAL, POINTER :: ka(:,:) ! - available cases flags -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC SIZE
!
! Executable Statements
!
! Initialise routine
  cproc='get_gridded_v9'
!
! Determine numbers of latitudes and longitudes
  CALL get_gridded_dimensions_v9 (iin,afield%nlg,afield%nlt,afield%ln2s,ifail,cfail)
  IF (ifail/=0) THEN
     cproc='get_gridded_dimensions_v9'
     RETURN
  END IF
!
! Read latitudes and longitudes
  CALL reset_grids (rlat,rlng,cstn,rlatd,rlngd,cstnd,idom,ifail, &
       igrid=afile%igrid,nfl=1,nlt=(/afield%nlt/),nlg=(/afield%nlg/))
  IF (ifail/=0) THEN
     ifail=1
     RETURN
  END IF
  CALL get_gridded_latlons_v9 (iin,afield%nlg,afield%nlt,afield%ln2s,rlng(:,1),rlat(:,1),ifail)
  IF (ifail/=0) THEN
     cproc='get_gridded_latlons_v9'
     RETURN
  END IF
!
! Determine number of time steps and sequencing
  IF (SIZE(rlatd,DIM=1)>SIZE(rlngd,DIM=1)) THEN
     CALL get_gridded_nt_v9 (iin,afile,afield%nlt,afield%nlg,rlat(:,1),rlng(:,1),ka,rlatd(:,1),ifail,cproc,cldate,cfail)
  ELSE
     CALL get_gridded_nt_v9 (iin,afile,afield%nlt,afield%nlg,rlat(:,1),rlng(:,1),ka,rlngd(:,1),ifail,cproc,cldate,cfail)
  END IF
!
  RETURN
!
 CONTAINS
!
!
  SUBROUTINE get_gridded_dimensions_v9 (iin,nlg,nlt,ln2s,ifail,cfail)
!
! Determines numbers of latitudes and longitudes in a version-9 gridded dataset
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 First line of data is too long. Data are probably truncated
!    ifail =  2 Unable to determine number of latitudes/longitudes (as indicated by cfail)
!    ifail =  3 Problem reading file
!
! Modules
  USE IO_constants,   ONLY: lfli
  USE time_constants, ONLY: lmon
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iin ! - input unit number -
!
! Output scalars
  INTEGER, INTENT(OUT) :: nlg   ! - number of longitudes -
  INTEGER, INTENT(OUT) :: nlt   ! - number of latitudes -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
  CHARACTER(LEN=*), INTENT(OUT) :: cfail ! - error indicator -
!
  LOGICAL, INTENT(OUT) :: ln2s ! - north to south latitudes flag -
!
! Locals
!
! Local scalars
  INTEGER :: j   ! - longitude index -
  INTEGER :: idy ! - day -
  INTEGER :: iyr ! - year -
  INTEGER :: inc ! - increment -
  INTEGER :: ll  ! - line length -
!
  REAL(KIND=rp) :: dum  ! - dummy argument -
  REAL(KIND=rp) :: rlt1 ! - first latitude -
  REAL(KIND=rp) :: rlt2 ! - last latitude -
!
  CHARACTER(LEN=lfli) :: fli  ! - first line of input -
  CHARACTER(LEN=lmon) :: mycmon ! - current month -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC LEN_TRIM
!
! Executable Statements
!
! Initialise routine
  cfail=' '
!
! Determine length of first line of data
  REWIND (UNIT=iin)
  ll=lfli
  READ (UNIT=iin,FMT='(A)',ERR=4,END=4) fli(1:ll)
  ll=LEN_TRIM(fli(1:ll))
! - return if line is too long -
  IF (ll==lfli) THEN
     ifail=1
     RETURN
  END IF
!
! Determine number of longitudes
  nlg=1
  inc=64
  DO
     READ (UNIT=fli(1:ll),FMT=*,ERR=1,END=1) idy,mycmon,iyr,(dum,j=1,nlg)
     nlg=nlg+inc
     CYCLE
1    nlg=nlg-inc
     IF (inc==1) EXIT
     inc=inc/8
  END DO
! - return if unsuccessful -
  IF (nlg<1) THEN
     ifail=2
     cfail='longitudes'
     RETURN
  END IF
!
! Determine number of latitudes
  nlt=0
  READ (UNIT=iin,FMT=*,ERR=3,END=2) rlt1,mycmon
  nlt=1
! - return if unsuccessful -
2 IF (nlt==0) THEN
     ifail=2
     cfail='latitudes'
     RETURN
  END IF
! - return if next line indicates a new date, implying no data -
  IF (get_month(mycmon)>0) THEN
     ifail=2
     cfail='latitudes'
     RETURN
  END IF
! - read new lines until next date is reached -
  DO
     READ (UNIT=iin,FMT=*,ERR=4,END=3) dum,mycmon
     IF (get_month(mycmon)>0) EXIT
     rlt2=dum
     nlt=nlt+1
  END DO
!
! Check latitude ordering
3 IF (nlt>1) THEN
     IF (rlt1>rlt2) THEN
        ln2s=.true.
     ELSE
        ln2s=.false.
     END IF
  ELSE
     ln2s=.true.
  END IF
  ifail=0
  RETURN
!
! Errors
4 ifail=3
  RETURN
  END SUBROUTINE get_gridded_dimensions_v9
!
!
!
  SUBROUTINE get_gridded_latlons_v9 (iin,nlg,nlt,ln2s,rlng,rlat,ifail)
!
! Determines latitudes and longitudes in a version-9 gridded dataset
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file reached
!    ifail =  3 Latitudes are not consecutive
!    ifail =  4 Duplicate latitudes
!
! Modules
  USE time_constants, ONLY: lmon
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iin ! - input unit number -
  INTEGER, INTENT(IN) :: nlg ! - number of longitudes -
  INTEGER, INTENT(IN) :: nlt ! - number of latitudes -
!
  LOGICAL, INTENT(IN) :: ln2s ! - north to south latitudes flag -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: rlng(:) ! - longitudes -
  REAL(KIND=rp), INTENT(OUT) :: rlat(:) ! - latitudes -
!
! Locals
!
! Local scalars
  INTEGER :: i    ! - latitude index -
  INTEGER :: i1   ! - first latitude index -
  INTEGER :: i2   ! - last latitude index -
  INTEGER :: j    ! - longitude index -
  INTEGER :: idy  ! - day -
  INTEGER :: iyr  ! - year -
  INTEGER :: iinc ! - latitude increment -
!
  CHARACTER(LEN=lmon) :: mycmon ! - current month -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ANY
!
! Executable Statements
!
! Read longitudes
  REWIND (UNIT=iin)
  READ (UNIT=iin,FMT=*,ERR=1,END=2) idy,mycmon,iyr,(rlng(j),j=1,nlg)
!
! Read latitudes
! - set latitude ordering -
  IF (ln2s) THEN
     i1=1
     i2=nlt
     iinc=1
  ELSE
     i1=nlt
     i2=1
     iinc=-1
  END IF
! - read latitudes -
  DO i=i1,i2,iinc
     READ (UNIT=iin,FMT=*,ERR=1,END=2) rlat(i)
  END DO
! - check latitudes -
  IF (ANY(rlat(1:nlt-1)<rlat(2:nlt))) ifail=3
  IF (ifail==3) THEN
     IF (ANY(rlat(1:nlt-1)==rlat(2:nlt))) ifail=4
     RETURN
  END IF
  ifail=0
  RETURN
!
! Errors
! - problem reading file -
1 ifail=1
  RETURN
!
! - end of file -
2 ifail=2
  RETURN
!
  END SUBROUTINE get_gridded_latlons_v9
!
!
!
  SUBROUTINE get_gridded_nt_v9 (iin,afile,nlt,nlg,rlat,rlng,ka,dwk,ifail,cproc,cldate,cfail)
!
! Determines numbers of cases in a gridded dataset
!
! On exit (if cproc='get_gridded_nt_v9'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file. Data up to cldate read successfully
!    ifail =  2 Premature end of file reached. Data up to cldate read successfully
!    ifail =  3 Inconsistent latitudes or longitudes (depending on cfail). Data up to cldate read successfully
!    ifail =  4 Invalid date sequencing (as indicated by cfail)
!    ifail =  5 Problem reading date (possible incorrect number of latitudes). Data up to cldate read successfully
!    ifail =  6 Problem allocating memory
!
! Modules
  USE time_constants, ONLY: lmon
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iin ! - input unit number -
  INTEGER, INTENT(IN) :: nlt ! - number of latitudes -
  INTEGER, INTENT(IN) :: nlg ! - number of longitudes -
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: rlat(:) ! - latitudes -
  REAL(KIND=rp), INTENT(IN) :: rlng(:) ! - longitudes -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
  CHARACTER(LEN=*), INTENT(OUT) :: cproc  ! - procedure in error -
  CHARACTER(LEN=*), INTENT(OUT) :: cldate ! - last date read -
  CHARACTER(LEN=*), INTENT(OUT) :: cfail  ! - error indicator -
!
! Pointer arrays
  LOGICAL, POINTER :: ka(:,:) ! - available cases flags -
!
! Workspace arrays
  REAL(KIND=rp), INTENT(INOUT) :: dwk(:) ! - workspace -
!
! Locals
!
! Local scalars
  INTEGER :: i     ! - latitude index -
  INTEGER :: j     ! - longitude index -
  INTEGER :: k     ! - case index -
  INTEGER :: isq   ! - time sequence indicator -
  INTEGER :: njump ! - number of jumped timesteps -
!
  CHARACTER(LEN=lmon) :: mycmon ! - current month -
!
  TYPE(period) :: periodp ! - previous date -
  TYPE(period) :: periodc ! - current date -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ANY
!
! Executable Statements
!
! Initialise routine
  cproc='get_gridded_nt_v9'
  cfail=' '
  cldate=' '
!
! Rewind to first date
  REWIND (UNIT=iin)
!
! Read first date
  READ (UNIT=iin,FMT=*,ERR=2,END=3) afile%period1%sdate%idy,mycmon,afile%period1%sdate%iyr
  afile%period1%sdate%imn=get_month(mycmon)
  ifail=valid_date(afile%period1%sdate)
  IF (ifail/=0) THEN
     cproc='valid_date'
     RETURN
  END IF
  afile%period1%edate=afile%period1%sdate
! - skip data -
  DO i=1,nlt
     READ (UNIT=iin,FMT=*,ERR=2,END=3)
  END DO
  afile%periodn=afile%period1
  afile%nt=1
!
! Read second date
  READ (UNIT=iin,FMT=*,ERR=2,END=1) periodc%sdate%idy,mycmon,periodc%sdate%iyr,(dwk(j),j=1,nlg)
  periodc%sdate%imn=get_month(mycmon)
  ifail=valid_date(periodc%sdate)
  IF (ifail/=0) THEN
     cproc='valid_date'
     RETURN
  END IF
  periodc%edate=periodc%sdate
! - check for consistency of longitudes -
  IF (ANY(dwk(1:nlg)/=rlng(1:nlg))) THEN
     ifail=3
     cfail='Longitudes'
     RETURN
  END IF
! - read latitudes -
  DO i=1,nlt
     READ (UNIT=iin,FMT=*,ERR=2,END=3) dwk(i)
  END DO
! - check for consistency of latitudes -
  IF (ANY(dwk(1:nlt)/=rlat(1:nlt))) THEN
     ifail=3
     cfail='Latitudes'
     RETURN
  END IF
  afile%nt=afile%nt+1
!
! Identify date sequencing
  CALL get_sequence (afile%period1,periodc,afile%iseq,ifail)
  SELECT CASE (ifail)
   CASE (0)
     IF (afile%iseq==2) THEN ! - block monthly sequencing in version 9 files -
        ifail=4
        cfail='Monthly'
        RETURN
     ELSE IF (afile%iseq==4) THEN ! - block seasonal sequencing in version 9 files -
        ifail=4
        cfail='Seasonal'
        RETURN
     END IF
     afile%periodn=periodc  
   CASE DEFAULT
     cproc='get_sequence'
     RETURN
  END SELECT
!
! Read subsequent dates
  get_nt: DO
     periodp=afile%periodn
     READ (UNIT=iin,FMT=*,ERR=2,END=1) periodc%sdate%idy,mycmon,periodc%sdate%iyr,(dwk(j),j=1,nlg)
     periodc%sdate%imn=get_month(mycmon)
     IF (periodc%sdate%imn==0) THEN
        ifail=5
        RETURN
     END IF
     periodc%edate=periodc%sdate
     afile%periodn=periodc
! - check for consistency of longitudes -
     IF (ANY(dwk(1:nlg)/=rlng(1:nlg))) THEN
        ifail=3
        cfail='Longitudes'
        RETURN
     END IF
! - read latitudes -
     DO i=1,nlt
        READ (UNIT=iin,FMT=*,ERR=2,END=3) dwk(i)
     END DO
! - check for consistency of latitudes -
     IF (ANY(dwk(1:nlt)/=rlat(1:nlt))) THEN
        ifail=3
        cfail='Latitudes'
        RETURN
     END IF
  END DO get_nt
! - determine total number of cases from difference between start and end dates -
1 afile%nt=date_diff(afile%period1%sdate,afile%periodn%sdate,afile%iseq)+1
!
! Assign memory for available cases flags
  IF (ASSOCIATED(ka)) THEN
     DEALLOCATE (ka)
     NULLIFY (ka)
  END IF
  ALLOCATE (ka(afile%nt,1),STAT=ifail)
  IF (ifail/=0) THEN
     ifail=6
     RETURN
  END IF
  ka(1,:)=.true.
!
! Return if there is only one case
  afile%periodn=afile%period1
  IF (afile%nt==1) RETURN
!
! Rewind to second date
  REWIND (UNIT=iin)
  DO k=1,nlt+1
     READ (UNIT=iin,FMT=*,ERR=2,END=3)
  END DO
!
! Check sequencing
  njump=0
  DO k=2,afile%nt
     SELECT CASE (njump) ! - identify whether to account for missing records -
! - read next record -
      CASE (0)
        periodp=afile%periodn
        READ (UNIT=iin,FMT=*,ERR=2,END=3) periodc%sdate%idy,mycmon,periodc%sdate%iyr
        DO i=1,nlt
           READ (UNIT=iin,FMT=*,ERR=2,END=3)
        END DO
        periodc%sdate%imn=get_month(mycmon)
        IF (periodc%sdate%imn==0) THEN
           ifail=5
           GOTO 4
        END IF
        periodc%edate=periodc%sdate
        CALL get_sequence (periodp,periodc,isq,ifail) ! - check whether next record is in sequence -
        IF ((ifail/=0).OR.(isq/=afile%iseq)) THEN
           IF (((afile%iseq==1).AND.(ifail==1)).OR. &                  ! - year missing in annual sequencing -
               ((afile%iseq==3).AND.((ifail>=4).AND.(ifail<=7)))) THEN ! - day(s) mssing in daily sequencing -
              njump=date_diff(periodp%sdate,periodc%sdate,afile%iseq)-1 ! - check whether dates are advancing -
              IF (njump>0) THEN
                 ka(k,1)=.false.
                 CYCLE
              END IF
           END IF
           ifail=7
           GOTO 4
        ELSE
           ka(k,1)=.true.
           afile%periodn=periodc
        END IF
! - current record is missing, but prepare to read next record -
      CASE (1)
        ka(k,1)=.true.
        afile%periodn=periodc
        njump=njump-1
! - current record is missing -
      CASE (2:)
        ka(k,1)=.false.
        afile%periodn=periodc
        njump=njump-1
     END SELECT
  END DO
  ifail=0
  RETURN
!
! Errors
! - problem reading file -
2 ifail=1
  GOTO 4
!
! - end of file -
3 ifail=2
!
! Free workspace
4 IF (ASSOCIATED(ka)) THEN
     DEALLOCATE (ka)
     NULLIFY (ka)
  END IF
  cldate=get_cdate(afile%periodn,2)
!
  RETURN
  END SUBROUTINE get_gridded_nt_v9
 END SUBROUTINE get_gridded_v9
!
!
!
 SUBROUTINE get_nongridded_v9 (iin,afile,afield,rlat,rlng,cstn,rlatd,rlngd,cstnd,idom,ka,ifail,cproc,cldate,cfail)
!
! Determines structure of a version-9 gridded input file, including its size, field settings, and missing cases
!
! On exit (if cproc='get_nongridded_v9'):
!    ifail =  0 Successful
!    ifail =  1 Problem allocating memory
!
! Modules
  USE fields,       ONLY: reset_grids
  USE IO_constants, ONLY: lstn
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iin ! - input unit number -
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
  TYPE(field), INTENT(INOUT) :: afield ! - field -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
  CHARACTER(LEN=*), INTENT(OUT) :: cproc  ! - procedure in error -
  CHARACTER(LEN=*), INTENT(OUT) :: cldate ! - last date read -
  CHARACTER(LEN=*), INTENT(OUT) :: cfail  ! - error indicator -
!
! Pointer arrays
  INTEGER, POINTER :: idom(:,:) ! - variables within domain -
!
  REAL(KIND=rp), POINTER :: rlat(:,:)  ! - latitudes -
  REAL(KIND=rp), POINTER :: rlng(:,:)  ! - longitudes -
  REAL(KIND=rp), POINTER :: rlatd(:,:) ! - realigned used latitudes -
  REAL(KIND=rp), POINTER :: rlngd(:,:) ! - realigned used longitudes -
!
  CHARACTER(LEN=lstn), POINTER :: cstn(:,:)  ! - names of stations -
  CHARACTER(LEN=lstn), POINTER :: cstnd(:,:) ! - names of stations within domain -
!
  LOGICAL, POINTER :: ka(:,:) ! - available cases flags -
!
! Executable Statements
!
! Initialise routine
  cproc='get_nongridded_v9'
  cldate=' '
  cfail=' '
!
! Determine numbers of stations
  CALL get_nongridded_dimensions_v9 (iin,afield%nlt,ifail)
  IF (ifail/=0) THEN
     cproc='get_nongridded_dimensions_v9'
     IF (afile%igrid==2) THEN
        cfail='stations'
     ELSE
        cfail='indices'
     END IF
     RETURN
  END IF
  afield%nlg=afield%nlt
!
! Read coordinates
  CALL reset_grids (rlat,rlng,cstn,rlatd,rlngd,cstnd,idom,ifail, &
       igrid=afile%igrid,nfl=1,nlt=(/afield%nlt/),nlg=(/afield%nlg/))
  IF (ifail/=0) THEN
     ifail=1
     RETURN
  END IF
  IF (afile%igrid==2) THEN
     CALL get_nongridded_latlons_v9 (iin,afield%nlt,cstn(:,1),ifail, &
          rlng=rlng(:,1),rlat=rlat(:,1))
  ELSE
     CALL get_nongridded_latlons_v9 (iin,afield%nlt,cstn(:,1),ifail)
  END IF
  IF (ifail/=0) THEN
     cproc='get_nongridded_latlons_v9'
     RETURN
  END iF
!
! Determine number of time steps and sequencing
! - station data -
  IF (afile%igrid==2) THEN
     CALL get_nongridded_nt_v9 (iin,3,afile%nt,afile%period1,afile%periodn,afile%iseq,ka,ifail,cproc,cldate,cfail)
! - unreferenced data -
  ELSE
     CALL get_nongridded_nt_v9 (iin,1,afile%nt,afile%period1,afile%periodn,afile%iseq,ka,ifail,cproc,cldate,cfail)
  END IF
  RETURN
!
 CONTAINS
!
!
  SUBROUTINE get_nongridded_dimensions_v9 (iin,nst,ifail)
!
! Determines numbers of stations in a version-9 nongridded dataset
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 First line of data is too long. Data are probably truncated
!    ifail =  2 Unable to determine number of stations
!    ifail =  3 Problem reading file
!
! Modules
  USE IO_constants, ONLY: lfli,ltag
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iin ! - input unit number -
!
! Output scalars
  INTEGER, INTENT(OUT) :: nst   ! - number of stations -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
  INTEGER :: i   ! - station index -
  INTEGER :: inc ! - increment -
  INTEGER :: ll  ! - line length -
!
  REAL(KIND=rp) :: dum  ! - dummy argument -
!
  CHARACTER(LEN=lfli) :: fli  ! - first line of input -
  CHARACTER(LEN=ltag) :: ctag ! - current month -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC LEN_TRIM
!
! Executable Statements
!
! Determine length of line of data of latitudes
  REWIND (UNIT=iin)
  READ (UNIT=iin,FMT=*,ERR=2,END=2)
  ll=lfli
  READ (UNIT=iin,FMT='(A)',ERR=2,END=2) fli(1:ll)
  ll=LEN_TRIM(fli(1:ll))
! - return if line is too long -
  IF (ll==lfli) THEN
     ifail=1
     RETURN
  END IF
!
! Determine number of stations
  nst=1
  inc=64
  DO
  READ (UNIT=fli(1:ll),FMT=*,ERR=1,END=1) ctag,(dum,i=1,nst)
     nst=nst+inc
     CYCLE
1    nst=nst-inc
     IF (inc==1) EXIT
     inc=inc/8
  END DO
  IF (nst>=1) THEN
     ifail=0
  ELSE
     ifail=2
  END IF
  RETURN
!
! Errors
2 ifail=3
  RETURN
  END SUBROUTINE get_nongridded_dimensions_v9
!
!
!
  SUBROUTINE get_nongridded_latlons_v9 (iin,nst,cstn,ifail,rlng,rlat)
!
! Determines latitudes and longitudes in a version-9 nongridded dataset
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file reached
!
! Modules
  USE IO_constants, ONLY: ldat
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iin ! - input unit number -
  INTEGER, INTENT(IN) :: nst ! - number of stations -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Output arrays
  CHARACTER(LEN=*), INTENT(OUT) :: cstn(:) ! - names of stations / indices -
! - optional output arrays
  REAL(KIND=rp), INTENT(OUT), OPTIONAL :: rlng(:) ! - longitudes -
  REAL(KIND=rp), INTENT(OUT), OPTIONAL :: rlat(:) ! - latitudes -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - station index -
!
  CHARACTER(LEN=ldat) :: ctag ! - tags -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC PRESENT
!
! Executable Statements
!
! Read stations
  REWIND (iin)
  READ (UNIT=iin,FMT=*,ERR=1,END=2) ctag,(cstn(i),i=1,nst)
  IF (PRESENT(rlat)) READ (UNIT=iin,FMT=*,ERR=1,END=2) ctag,(rlat(i),i=1,nst)
  IF (PRESENT(rlng)) READ (UNIT=iin,FMT=*,ERR=1,END=2) ctag,(rlng(i),i=1,nst)
  ifail=0
  RETURN
!
! Errors
! - problem reading file -
1 ifail=1
  RETURN
!
! - end of file -
2 ifail=2
  RETURN
!
  END SUBROUTINE get_nongridded_latlons_v9
!
!
!
  SUBROUTINE get_nongridded_nt_v9 (iin,iskip,nt,period1,periodn,iseqo,ka,ifail,cproc,cldate,cfail)
!
! Determines numbers of cases in a nongridded dataset
!
! On exit (if cproc='get_nongridded_nt_v9'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file. Data up to cldate read successfully
!    ifail =  2 Premature end of file reached. Data up to cldate read successfully
!    ifail =  3 Inconsistent latitudes or longitudes (depending on cfail). Data up to cldate read successfully
!    ifail =  4 Invalid date sequencing (as indicated by cfail)
!    ifail =  5 Problem reading date (possible incorrect number of latitudes). Data up to cldate read successfully
!    ifail =  6 Problem allocating memory
!
! Modules
  USE IO_constants, ONLY: lprd
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iin   ! - input unit number -
  INTEGER, INTENT(IN) :: iskip ! - number of lines to skip -
!
! Output scalars
  INTEGER, INTENT(OUT) :: nt    ! - number of time steps -
  INTEGER, INTENT(OUT) :: iseqo  ! - time sequence indicator -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
  CHARACTER(LEN=*), INTENT(OUT) :: cproc  ! - procedure in error -
  CHARACTER(LEN=*), INTENT(OUT) :: cldate ! - procedure in error -
  CHARACTER(LEN=*), INTENT(OUT) :: cfail  ! - error indicator -
!
  TYPE(period), INTENT(OUT) :: period1 ! - first date -
  TYPE(period), INTENT(OUT) :: periodn ! - last date -
!
! Pointer arrays
  LOGICAL, POINTER :: ka(:,:) ! - available cases flags -
!
! Locals
!
! Local scalars
  INTEGER :: k     ! - case index -
  INTEGER :: isq   ! - time sequence indicator -
  INTEGER :: njump ! - number of jumped timesteps -
!
  CHARACTER(LEN=lprd) :: ctag1 ! - date tags -
  CHARACTER(LEN=lprd) :: ctag2 ! - date tags -
!
  TYPE(period) :: periodp ! - previous date -
  TYPE(period) :: periodc ! - current date -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ADJUSTL
  INTRINSIC ASSOCIATED
  INTRINSIC TRIM
  INTRINSIC LEN_TRIM
!
! Executable Statements
!
! Initialise routine
  cproc='get_nongridded_nt_v9'
  cldate=' '
  cfail=' '
!
! Rewind to first date
  REWIND (UNIT=iin)
  periodn=0
  IF (iskip>0) THEN
     DO k=1,iskip
        READ (UNIT=iin,FMT=*,ERR=3,END=3)
     END DO
  END IF
!
! Read first date
  nt=0
  READ (UNIT=iin,FMT='(A)',ERR=3,END=3) ctag1
  ctag1=ADJUSTL(ctag1)
  CALL get_date (' ',TRIM(ctag1),period1%sdate,ifail, &
       edate=period1%edate)
  IF (ifail/=0) THEN
     cproc='get_date'
     RETURN
  END IF
  periodn=period1
  nt=nt+1
!
! Read second date
  READ (UNIT=iin,FMT='(A)',ERR=3,END=2) ctag1
  ctag1=ADJUSTL(ctag1)
  CALL get_date (' ',TRIM(ctag1),periodc%sdate,ifail, &
       edate=periodc%edate)
  IF (ifail/=0) THEN
     cproc='get_date'
     GOTO 4
  END IF
  nt=nt+1
!
! Identify date sequencing
  CALL get_sequence (period1,periodc,iseqo,ifail)
  SELECT CASE (ifail)
   CASE (0)
     IF (iseqo==2) THEN ! - block monthly sequencing in version 9 files -
        ifail=2
        cfail='Monthly'
        RETURN
     ELSE IF (iseqo==4) THEN ! - block seasonal sequencing in version 9 files -
        ifail=2
        cfail='Seasonal'
        RETURN
     END IF
   CASE DEFAULT
     cproc='get_sequence'
     GOTO 4
  END SELECT
!
! Skip to last date
  periodn=periodc
  DO
     READ (UNIT=iin,FMT='(A)',ERR=1,END=1) ctag2
     IF (LEN_TRIM(ctag2)==0) EXIT
     ctag1=ctag2
  END DO
1 ctag1=ADJUSTL(ctag1)
  CALL get_date (' ',TRIM(ctag1),periodc%sdate,ifail, &
       edate=periodc%edate)
  IF (ifail/=0) GOTO 4
  periodn=periodc
! - determine total number of cases from difference between start and end dates -
  nt=date_diff(period1%sdate,periodn%sdate,iseqo)+1
!
! Assign memory for available cases flags
  IF (ASSOCIATED(ka)) THEN
     DEALLOCATE (ka)
     NULLIFY (ka)
  END IF
2 ALLOCATE (ka(nt,1),STAT=ifail)
  IF (ifail/=0) THEN
     ifail=3
     GOTO 4
  END IF
  ka(1,1)=.true.
! - return if there is only one case -
  periodn=period1
  IF (nt==1) RETURN
!
! Rewind to second date
  REWIND (UNIT=iin)
  IF (iskip>0) THEN
     DO k=1,iskip+1
        READ (UNIT=iin,FMT=*,ERR=3,END=3)
     END DO
  END IF
!
! Check sequencing
  njump=0
  DO k=2,nt
     SELECT CASE (njump) ! - identify whether to account for missing records -
! - read next record -
      CASE (0)
        periodp=periodn
        READ (UNIT=iin,FMT='(A)',ERR=3,END=3) ctag1
        ctag1=ADJUSTL(ctag1)
        CALL get_date (' ',TRIM(ctag1),periodc%sdate,ifail, &
             edate=periodc%edate)
        IF (ifail/=0) THEN
           cproc='get_date'
           GOTO 4
        END IF
        CALL get_sequence (periodp,periodc,isq,ifail) ! - check whether next record is in sequence -
        IF ((ifail/=0).OR.(isq/=iseqo)) THEN
           IF (((iseqo==1).AND.(ifail==1)).OR. &                  ! - year missing in annual sequencing -
               ((iseqo==3).AND.((ifail>=4).AND.(ifail<=7)))) THEN ! - day(s) mssing in daily sequencing -
              njump=date_diff(periodp%sdate,periodc%sdate,iseqo)-1 ! - check whether dates are advancing -
              IF (njump>0) THEN
                 ka(k,1)=.false.
                 CYCLE
              END IF
           END IF
           ifail=4
           GOTO 4
        ELSE
           ka(k,1)=.true.
           periodn=periodc
        END IF
! - current record is missing, but prepare to read next record -
      CASE (1)
        ka(k,1)=.true.
        periodn=periodc
        njump=njump-1
! - current record is missing -
      CASE (2:)
        ka(k,1)=.false.
        periodn=periodc
        njump=njump-1
     END SELECT
  END DO
  ifail=0
  RETURN
!
! Errors
! - problem reading file -
3 ifail=1
!
! Free workspace
4 IF (ASSOCIATED(ka)) THEN
     DEALLOCATE (ka)
     NULLIFY (ka)
  END IF
  cldate=get_cdate(periodn,1)
  RETURN
!
  END SUBROUTINE get_nongridded_nt_v9
 END SUBROUTINE get_nongridded_v9
!
!
!
 SUBROUTINE get_structure_v10 (afile,afield,rmiss,rlat,rlng,cstn,rlatd,rlngd,cstnd,idom,ka,ifd,ifail,cproc,cldate,cfail)
!
! Determines structure of a version 10 input file, including its size, field settings, and missing cases
!
! On exit (if cproc='get_structure_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!    ifail =  3 Missing cpt:nfields tag
!    ifail =  4 Unrecognized header line
!
! Modules
  USE IO_constants, ONLY: iin,lstn,ltag,cxmlns
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: rmiss ! - missing value flag -
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifd   ! - current field -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
  CHARACTER(LEN=*), INTENT(OUT) :: cproc  ! - procedure in error -
  CHARACTER(LEN=*), INTENT(OUT) :: cldate ! - last date read -
  CHARACTER(LEN=*), INTENT(OUT) :: cfail  ! - error indicator -
!
! Pointer arrays
  INTEGER, POINTER :: idom(:,:) ! - variables within domain -
!
  REAL(KIND=rp), POINTER :: rlat(:,:)  ! - latitudes -
  REAL(KIND=rp), POINTER :: rlng(:,:)  ! - longitudes -
  REAL(KIND=rp), POINTER :: rlatd(:,:) ! - used longitudes -
  REAL(KIND=rp), POINTER :: rlngd(:,:) ! - realigned used longitudes -
!
  CHARACTER(LEN=lstn), POINTER :: cstn(:,:)  ! - names of stations -
  CHARACTER(LEN=lstn), POINTER :: cstnd(:,:) ! - names of stations within domain -
!
  LOGICAL, POINTER :: ka(:,:) ! - available cases flags -
!
  TYPE(field), POINTER :: afield(:)  ! - field -
!
! Locals
!
! Local scalars
  INTEGER :: i1   ! - locator -
  INTEGER :: igrd ! - data structure indicator -
  INTEGER :: nt   ! - number of time steps -
  INTEGER :: nlh  ! - number of header lines -
!
  CHARACTER(LEN=ltag) :: cline ! - line -
!
  LOGICAL :: lni ! - nfield initialized flag -
!
  TYPE(field) :: tfield ! - field information -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ASSOCIATED
  INTRINSIC INDEX
  INTRINSIC LEN
  INTRINSIC TRIM
!
! Executable Statements
!
! Initialise routine
  cproc='get_structure_v10'
  cldate=' '
  cfail=' '
  ifd=0
!
! Initialise
  IF (ASSOCIATED(afield)) NULLIFY (afield)
  afile%nfs=1
  lni=.false.
  nlh=0
  afile%ntag=0
!
! Read header lines
  headers: DO
     READ (UNIT=iin,FMT='(A)',ERR=1,END=2) cline
     nlh=nlh+1
!
! Read XML namespace headers
     IF (INDEX(cline,cxmlns//':')>0) THEN
        afile%ntag=afile%ntag+1
!
! Read number of fields
     ELSE IF (INDEX(cline,'cpt:nfields=')>0) THEN
        i1=INDEX(cline,'cpt:nfields=')+LEN('cpt:nfields=')
        READ (UNIT=cline(i1:),FMT=*,ERR=3) afile%nfs
        lni=.true.
        afile%ntag=afile%ntag+1
!
! First field
     ELSE IF (INDEX(cline,'cpt:field=')>0) THEN
        IF (lni) EXIT headers
        ifail=3
        GOTO 3
!
! Skip unused tags
     ELSE IF (INDEX(cline,'cpt:')>0) THEN
        afile%ntag=afile%ntag+1
!
! Unrecognised lines
     ELSE
        ifail=4
        GOTO 3
     END IF
  END DO headers
!
! Determine structure of dataset
  CALL get_tags (TRIM(cline)//' ',6,(/'T      ','col    ','row    ','ncol   ','nrow   ','field  '/),tfield,ifail,cfail, &
       igrid=igrd,nt=nt)
  IF (ifail/=0) THEN
     cproc='get_tags'
     GOTO 3
  END IF
! - disable multiple fields for unreferenced data -
  afile%igrid=igrd
  IF (afile%igrid/=3) THEN
     afile%igeog=1
  ELSE
     afile%igeog=0
  END IF
!
! Determine structure of gridded dataset
  SELECT CASE (afile%igrid)
   CASE (1)
     CALL get_gridded_v10 (iin,afile,afield,rmiss,rlat,rlng,cstn,rlatd,rlngd,cstnd,idom,ka,ifail,ifd,cproc,cldate,cfail)
!
! Determine structure of station and unreferenced datasets
   CASE (2,3)
     CALL get_nongridded_v10 (iin,nt,afile,afield,rmiss,rlat,rlng,cstn,rlatd,rlngd,cstnd,idom,ka,ifail,ifd,cproc,cldate,cfail)
  END SELECT
!
  GOTO 3
!
! Errors
! - problem reading file -
1 ifail=1
  GOTO 3
!
! - end of file -
2 ifail=2
!
! - other errors -
3 IF (afile%nfs==1) ifd=0
!
  RETURN
 END SUBROUTINE get_structure_v10
!
!
!
 SUBROUTINE get_gridded_v10 (iin,afile,afield,rmiss,rlat,rlng,cstn,rlatd,rlngd,cstnd,idom,ka,ifail,ifd,cproc,cldate,cfail)
!
! Determines structure of a version-10 gridded input file, including its size, field settings, and missing cases
!
! Modules
  USE fields,       ONLY: reset_grids,init_field
  USE IO_constants, ONLY: lstn
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iin ! - input unit number -
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: rmiss ! - missing value flag -
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
  INTEGER, INTENT(OUT) :: ifd   ! - field at error -
!
  CHARACTER(LEN=*), INTENT(OUT) :: cproc  ! - procedure in error -
  CHARACTER(LEN=*), INTENT(OUT) :: cldate ! - last data read -
  CHARACTER(LEN=*), INTENT(OUT) :: cfail  ! - error indicator -
!
! Pointer arrays
  INTEGER, POINTER :: idom(:,:) ! - variables within domain -
!
  REAL(KIND=rp), POINTER :: rlat(:,:)  ! - latitudes -
  REAL(KIND=rp), POINTER :: rlng(:,:)  ! - longitudes -
  REAL(KIND=rp), POINTER :: rlatd(:,:) ! - used latitudes -
  REAL(KIND=rp), POINTER :: rlngd(:,:) ! - realigned/used longitudes -
!
  CHARACTER(LEN=lstn), POINTER :: cstn(:,:)  ! - names of stations -
  CHARACTER(LEN=lstn), POINTER :: cstnd(:,:) ! - names of stations within domain -
!
  LOGICAL, POINTER :: ka(:,:) ! - available cases flags -
!
  TYPE(field), POINTER :: afield(:) ! - field information -
!
! Locals
!
! Local scalars
  INTEGER :: iseqbk ! - backup time sequence indicator -
!
  LOGICAL :: lfend ! - end of file flag -
!
! Executable Statements
!
! Initialise routine
  cproc='get_gridded_v10'
  cldate=' '
  cfail=' '
!
! Check for stacked fields and lagged fields
  CALL get_gridded_nls_v10 (iin,afile,ifail,ifd,cproc,cldate,cfail)
  IF (ifail/=0) RETURN
!
! Initialise fields
  CALL init_field (afield,afile%nfl,(/rmiss/),ifail)
  IF (ifail/=0) THEN
     ifail=1
     RETURN
  END IF
!
! Get fields and maximum number of cases
  CALL get_gridded_fields_v10 (iin,afile,afield,lfend,ifail,cproc,ifd,cldate,cfail)
  iseqbk=iseq
  iseq=afile%iseq
  IF (ifail/=0) GOTO 1
!
! Initialise latitudes and longitudes
  CALL reset_grids (rlat,rlng,cstn,rlatd,rlngd,cstnd,idom,ifail, &
       igrid=afile%igrid,nfl=afile%nfl,nlt=afield(:)%nlt,nlg=afield(:)%nlg)
  IF (ifail/=0) THEN
     ifail=1
     GOTO 1
  END IF
!
! Assign memory for available cases flags
  IF (ASSOCIATED(ka)) THEN
     DEALLOCATE (ka)
     NULLIFY (ka)
  END IF
  ALLOCATE (ka(afile%nt,afile%nfl),STAT=ifail)
  IF (ifail/=0) THEN
     ifail=1
     GOTO 1
  END IF
  ka(1,:)=.true.
  IF (afile%nt>1) ka(2:,:)=.false.
!
! Get latitudes and longitudes, and identify available cases
  CALL get_gridded_info_v10 (iin,afile,afield,ka,rlat,rlng,rlatd,rlngd,lfend,ifail,cproc,ifd,cldate,cfail)
!
! Free workspace
  IF (ifail/=0) THEN
     IF (ASSOCIATED(ka)) THEN
        DEALLOCATE (ka)
        NULLIFY (ka)
     END IF
  END IF
!
! Restore sequence
1 iseq=iseqbk
  RETURN
!
 CONTAINS
!
!
  SUBROUTINE get_gridded_nls_v10 (iin,afile,ifail,ifd,cproc,cldate,cfail)
!
! Determines stacking of any multiple fields, and number of lagged fields in a version-10 gridded input file
!
! On exit (if cproc='get_gridded_nls_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iin ! - input unit number -
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
  INTEGER, INTENT(OUT) :: ifd   ! - field at error -
!
  CHARACTER(LEN=*), INTENT(OUT) :: cproc  ! - procedure in error -
  CHARACTER(LEN=*), INTENT(OUT) :: cldate ! - last data read -
  CHARACTER(LEN=*), INTENT(OUT) :: cfail  ! - error indicator -
!
! Locals
!
! Local scalars
  INTEGER :: k ! - time index -
!
  TYPE(field) :: fieldp ! - previous field -
  TYPE(field) :: fieldc ! - current field -
!
  TYPE(period) :: period0 ! - null period -
!
! Executable Statements
!
! Initialise routine
  cproc='get_gridded_nls_v10'
  cldate=' '
  cfail=' '
  ifd=0
  afile%periodn=0
  period0=0
!
! Rewind to first date
  REWIND (UNIT=iin)
  IF (afile%ntag>0) THEN
     DO k=1,afile%ntag
        READ (UNIT=iin,FMT=*,ERR=1,END=2)
     END DO
  END IF
!
! Read first block
  ifd=1
  CALL get_gridded_block_v10 (iin,fieldp,ifail,cproc,cfail)
  IF (ifail/=0) RETURN
  afile%period1=fieldp%tdate
  afile%periodn=fieldp%tdate
!
! Read subsequent blocks
  afile%nt=0
  afile%nls=1
  get_nls: DO
     CALL get_gridded_block_v10 (iin,fieldc,ifail,cproc,cfail)
     SELECT CASE (ifail)
      CASE (0)
        CONTINUE
      CASE (-2) ! - check for end of file -
        IF (afile%nfs==1) THEN
           fieldc=fieldp
           afile%nt=1
           cproc='get_gridded_nls_v10'
           ifail=0
        ELSE
           RETURN
        END IF
      CASE DEFAULT
        RETURN
     END SELECT
!
! Check for stacked fields
     IF (afile%nls==1) THEN
        IF (afile%nfs>1) THEN
           IF (fieldp%member==fieldc%member) THEN
              afile%lensemble=.false.
              IF ((fieldp%var/=fieldc%var).OR.(fieldp%z%hght/=fieldc%z%hght)) THEN
                 afile%lstack=.false.
              ELSE
                 afile%lstack=.true.
              END IF
           ELSE
              afile%lensemble=.true.
              afile%lstack=.false.
           END IF
        ELSE
           afile%lensemble=.false.
           afile%lstack=.true.
        END IF
     END IF
!
! Skip remaining unstacked fields to next time step
     IF (.NOT.afile%lstack) THEN
        IF (afile%nfs>2) THEN
           DO ifd=3,afile%nfs
              CALL get_gridded_block_v10 (iin,fieldc,ifail,cproc,cfail)
              IF (ifail/=0) RETURN
           END DO
        END IF
        CALL get_gridded_block_v10 (iin,fieldc,ifail,cproc,cfail)
        SELECT CASE (ifail)
         CASE (0)
           CONTINUE
         CASE (-2) ! - check for end of file -
           IF (afile%nfs==1) THEN
              fieldc=fieldp
              afile%nt=1
              cproc='get_gridded_nls_v10'
              ifail=0
           ELSE
              RETURN
           END IF
         CASE DEFAULT
           RETURN
        END SELECT
     END IF
!
! Identify date sequencing
     CALL get_sequence (fieldp%tdate,fieldc%tdate,afile%iseq,ifail)
     SELECT CASE (ifail)
      CASE (0)
        afile%periodn=fieldc%tdate
        cldate=get_cdate(afile%periodn,1)
        IF ((afile%iseq==2).OR.(afile%iseq==4)) THEN ! - assume a lagged field if sequencing is monthly or seasonal -
           afile%nls=afile%nls+1
        ELSE
           EXIT get_nls
        END IF
      CASE DEFAULT
        cproc='get_sequence'
        GOTO 3
     END SELECT
  END DO get_nls
  afile%nfl=afile%nfs*afile%nls
!
  ifail=0
  RETURN
!
! Errors
! - problem reading file -
1 ifail=1
  GOTO 3
!
! - end of file -
2 ifail=2
!
! Identify last date read
3 IF (.NOT.afile%periodn==period0) cldate=get_cdate(afile%periodn,1)
!
  RETURN
  END SUBROUTINE get_gridded_nls_v10
!
!
!
  SUBROUTINE get_gridded_fields_v10 (iin,afile,afield,lfend,ifail,cproc,ifd,cldate,cfail)
!
! Determines field settings and number of cases
!
! On exit (if cproc='get_gridded_fields_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file. Data up to cldate in ifd field read successfully
!    ifail =  2 Premature end of file. Data up to cldate in ifd field read successfully
!    ifail =  3 Season for last date of ifd field is inconsistent with earlier dates
!    ifail =  4 Last date for ifd field is not in sequence
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iin  ! - input unit number -
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
  INTEGER, INTENT(OUT) :: ifd   ! - field index -
!
  CHARACTER(LEN=*), INTENT(OUT) :: cproc  ! - procedure in error -
  CHARACTER(LEN=*), INTENT(OUT) :: cldate ! - last data read -
  CHARACTER(LEN=*), INTENT(OUT) :: cfail  ! - error indicator -
!
  LOGICAL, INTENT(OUT) :: lfend ! - end of file flag -
!
! Input/output arrays
  TYPE(field), INTENT(INOUT) :: afield(:) ! - field information -
!
! Locals
!
! Local scalars
  INTEGER :: k   ! - case index -
  INTEGER :: l   ! - field/lagged-field index -
  INTEGER :: ilf ! - lagged-field index -
  INTEGER :: ntn ! - number of time steps -
!
  TYPE(field) :: tfield ! - temporary field information -
  TYPE(field) :: pfield ! - previous field information -
!
  TYPE(period) :: period0 ! - null period -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MAX
!
! Executable Statements
!
! Initialise routine
  cproc='get_gridded_fields_v10'
  cldate=' '
  cfail=' '
  ifd=0
  afile%periodn=0
  period0=0
!
! Rewind to first date
  REWIND (UNIT=iin)
  DO k=1,afile%ntag
     READ (UNIT=iin,FMT=*,ERR=1,END=2)
  END DO
!
! Read first lagged fields and stacked fields
  IF (afile%lstack) THEN
     afile%nt=1
     DO ifd=1,afile%nfs
        DO ilf=1,afile%nls
           l=(ifd-1)*afile%nls+ilf
           IF ((ifd==1).OR.(ilf/=1)) THEN
              CALL get_gridded_block_v10 (iin,afield(l),ifail,cproc,cfail)
              IF (ifail/=0) RETURN
           END IF
           afile%periodn=afield(l)%tdate
        END DO
!
! Continue to read blocks until new stacked field is found
        l=(ifd-1)*afile%nls+1
        DO
           CALL get_gridded_block_v10 (iin,tfield,ifail,cproc,cfail)
           SELECT CASE (ifail)
            CASE (0)
              afile%periodn=tfield%tdate
              IF ((tfield%member/=afield(l)%member).OR.(tfield%var/=afield(l)%var).OR.(tfield%z%hght/=afield(l)%z%hght)) THEN
                 l=ifd*afile%nls+1
                 afield(l)=tfield
                 EXIT
              END IF
              pfield=tfield
            CASE (2)
              cproc='get_gridded_fields_v10'
              ifail=0
              EXIT
            CASE DEFAULT
              GOTO 3
           END SELECT
        END DO
!
! Check whether last case is for last lagged field
        lfend=.false.
        tfield=pfield
        IF (afile%iseq==1) THEN
           DO ilf=afile%nls,1,-1
              l=(ifd-1)*afile%nfs+ilf
              IF ((tfield%tdate%sdate%imn==afield(l)%tdate%sdate%imn).AND.(tfield%tdate%sdate%idy==afield(l)%tdate%sdate%idy).AND. &
                  (tfield%tdate%edate%imn==afield(l)%tdate%edate%imn).AND.(tfield%tdate%edate%idy==afield(l)%tdate%edate%idy)) THEN
                 EXIT
              ELSE IF (ilf>1) THEN
                 lfend=.true.
              ELSE
                 ifail=3
                 GOTO 3
              END IF
           END DO
        END IF
!
! Calculate number of cases
        ntn=date_diff(afield(l)%tdate%sdate,tfield%tdate%sdate,afile%iseq)+1
        IF (ntn<1) THEN
           ifail=4
           RETURN
        END IF
        afile%nt=MAX(afile%nt,ntn)
     END DO
!
! Read first unstacked fields and lagged fields
  ELSE
     DO ilf=1,afile%nls
        DO ifd=1,afile%nfs
           l=(ifd-1)*afile%nls+ilf
           CALL get_gridded_block_v10 (iin,afield(l),ifail,cproc,cfail)
           afile%periodn=afield(l)%tdate
           IF (ifail/=0) RETURN
        END DO
     END DO
!
! Read to end of file
     DO
        CALL get_gridded_block_v10 (iin,tfield,ifail,cproc,cfail)
        SELECT CASE (ifail)
         CASE (0)
           afile%periodn=tfield%tdate
           pfield=tfield
         CASE (2)
           cproc='get_gridded_fields_v10'
           ifail=0
           EXIT
         CASE DEFAULT
           RETURN
        END SELECT
     END DO
!
! Check whether last case is for last unstacked field
     tfield=pfield
     lfend=.false.
     DO ifd=afile%nfs,1,-1
        l=ifd*afile%nls
        IF ((tfield%member==afield(l)%member).OR.(tfield%var/=afield(l)%var).OR.(tfield%z%hght/=afield(l)%z%hght)) THEN
           EXIT
        ELSE IF (ifd>1) THEN
           lfend=.true.
        ELSE
           ifail=3
           GOTO 3
        END IF
     END DO
!
! Check whether last case is for last lagged field
     IF (.NOT.lfend) THEN
        IF (afile%iseq==1) THEN
           DO ilf=afile%nls,1,-1
              IF ((tfield%tdate%sdate%imn==afield(ilf)%tdate%sdate%imn).AND. &
                  (tfield%tdate%sdate%idy==afield(ilf)%tdate%sdate%idy).AND. &
                  (tfield%tdate%edate%imn==afield(ilf)%tdate%edate%imn).AND. &
                  (tfield%tdate%edate%idy==afield(ilf)%tdate%edate%idy)) THEN
                 EXIT
              ELSE IF (ilf>1) THEN
                 lfend=.true.
              ELSE
                 ifail=3
                 GOTO 3
              END IF
           END DO
        END IF
!
! Calculate number of cases
        afile%nt=date_diff(afield(ilf)%tdate%sdate,tfield%tdate%sdate,afile%iseq)+1
        IF (afile%nt<1) THEN
           ifail=4
           RETURN
        END IF
     END IF
  END IF
!
! Set number of cases
  ifail=0
  RETURN
!
! Errors
! - problem reading file -
1 ifail=1
  GOTO 3
!
! - problem reading file -
2 ifail=2
!
! Identify last date read
3 IF (.NOT.afile%periodn==period0) cldate=get_cdate(afile%periodn,1)
!
  RETURN
  END SUBROUTINE get_gridded_fields_v10
!
!
!
  SUBROUTINE get_gridded_info_v10 (iin,afile,afield,ka,rlat,rlng,rlatd,rlngd,lfend,ifail,cproc,ifd,cldate,cfail)
!
! Reads latitudes and longitudes, checking for consistency of fields. Identifies available cases
!
! On exit (if cproc='get_gridded_info_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file. Data up to cldate in ifd field read successfully
!    ifail =  2 Premature end of file. Data up to cldate in ifd field read successfully
!    ifail =  3 Unable to read latitudes and/or longitudes (as indicated by cfail) for ifd field
!    ifail =  4 Dates for ifd field are not in sequence. Data up to cldate in ifd field read successfully
!
! Modules
  USE fields, ONLY: init_field
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iin  ! - input unit number -
!
  LOGICAL, INTENT(IN) :: lfend ! - end of file flag -
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
  INTEGER, INTENT(OUT) :: ifd   ! - field index -
!
  CHARACTER(LEN=*), INTENT(OUT) :: cproc  ! - procedure in error -
  CHARACTER(LEN=*), INTENT(OUT) :: cldate ! - last data read -
  CHARACTER(LEN=*), INTENT(OUT) :: cfail  ! - error indicator -
!
! Input/output arrays
  TYPE(field), INTENT(INOUT) :: afield(:) ! - field information -
!
  LOGICAL, INTENT(INOUT) :: ka(:,:) ! - available cases flags -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: rlat(:,:)  ! - latitudes -
  REAL(KIND=rp), INTENT(OUT) :: rlng(:,:)  ! - longitudes -
!
! Workspace arrays
  REAL(KIND=rp), INTENT(INOUT) :: rlatd(:,:) ! - test latitudes -
  REAL(KIND=rp), INTENT(INOUT) :: rlngd(:,:) ! - test longitudes -
!
! Locals
!
! Local arrays
  TYPE(period) :: periodps(afile%nfl) ! - last successfully read periods -
!
  TYPE(field), POINTER :: tfield(:) ! - temporary field information -
!
! Local scalars
  INTEGER :: k   ! - case index -
  INTEGER :: l   ! - field/lagged-field index -
  INTEGER :: isq ! - time sequence indicator -
  INTEGER :: ilf ! - lagged-field index -
!
  LOGICAL :: lread ! - read current case flag -
!
  TYPE(period) :: period0 ! - null period -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ANY
!
! Executable Statements
!
! Initialise routine
  cproc='get_gridded_info_v10'
  cldate=' '
  cfail=' '
  ifd=0
  l=0
!
! Set initial dates
  periodps(:)=afield(:)%tdate
  period0=0
!
! Initialise temporary field
  NULLIFY (tfield)
  CALL init_field (tfield,1,afield(:)%rmiss,ifail)
!
! Rewind
  REWIND (UNIT=iin)
  DO k=1,afile%ntag
     READ (UNIT=iin,FMT=*,ERR=1,END=2)
  END DO
!
! Read latitudes and longitudes for stacked fields
  IF (afile%lstack) THEN
     DO ifd=1,afile%nfs
        afile%periodn=0
        lread=.true.
        DO k=1,afile%nt
           DO ilf=1,afile%nls
              l=(ifd-1)*afile%nls+ilf
              IF (lread) THEN
                 CALL get_gridded_block_v10 (iin,tfield(1),ifail,cproc,cfail, &
                      rlat=rlatd(:,l),rlng=rlngd(:,l))
                 IF (ifail/=0) GOTO 3
!
! Check latitude ordering
                 IF (k==1) THEN
                    IF (rlatd(1,l)>rlatd(afield(l)%nlt,l)) THEN
                       afield(l)%ln2s=.true.
                       IF (ANY(rlatd(1:afield(l)%nlt-1,l)<rlatd(2:afield(l)%nlt,l))) ifail=1
                    ELSE
                       afield(l)%ln2s=.false.
                       IF (ANY(rlatd(1:afield(l)%nlt-1,l)>rlatd(2:afield(l)%nlt,l))) ifail=1
                    END IF
                    rlat(1:afield(l)%nlt,l)=rlatd(1:afield(l)%nlt,l)
                    rlng(1:afield(l)%nlg,l)=rlngd(1:afield(l)%nlg,l)
!
! Check for consistency of fields
                 ELSE
                    CALL check_gridded_block_v10 (tfield(1),afield(l),rlatd(1:afield(l)%nlt,l),rlat(1:afield(l)%nlt,l), &
                         rlngd(1:afield(l)%nlg,l),rlng(1:afield(l)%nlg,l),cfail,cproc,ifail)
                    IF (ifail/=0) GOTO 3
                 END IF
              END IF
!
! Check for missing dates
              cproc='get_gridded_info_v10'
              IF (k>1) THEN
                 CALL get_sequence (periodps(l),tfield(1)%tdate,isq,ifail) ! - check whether next record is in sequence -
                 IF ((ifail/=0).OR.(isq/=afile%iseq)) THEN
                    IF (((afile%iseq==1).AND.((ifail==1).OR. &                      ! - year missing in annual sequencing -
                       ! - month missing if lagged-fields are present -
                       ((ifail==3).AND.(afile%nls>1)))).OR. & 
                       ! - day(s) mssing in daily sequencing -
                       ((afile%iseq==3).AND.((ifail>=4).AND.(ifail<=7)))) THEN     
                       IF (date_diff(periodps(l)%sdate,tfield(1)%tdate%sdate,afile%iseq)<0) THEN 
                           ! - check whether dates are advancing -
                          ifail=4
                          GOTO 3
                       END IF
                       periodps(l)=periodps(l)+1
                       lread=.false.
                    ELSE
                       ifail=4
                       GOTO 3
                    END IF
                 ELSE
                    ka(k,l)=.true.
                    periodps(l)=tfield(1)%tdate
                    afile%periodn=tfield(1)%tdate
                    lread=.true.
                 END IF
              END IF
           END DO
        END DO
     END DO
!
! Read latitudes and longitudes for unstacked fields
  ELSE
     afile%periodn=0
     lread=.true.
     DO k=1,afile%nt
        DO ilf=1,afile%nls
           DO ifd=1,afile%nfs
              l=(ifd-1)*afile%nls+ilf
              IF (lread) THEN
                 CALL get_gridded_block_v10 (iin,tfield(1),ifail,cproc,cfail, &
                      rlat=rlatd(:,l),rlng=rlngd(:,l))
                 IF (ifail/=0) GOTO 3
!
! Check latitude ordering
                 IF (k==1) THEN
                    IF (rlatd(1,l)>rlatd(afield(l)%nlt,l)) THEN
                       afield(l)%ln2s=.true.
                       IF (ANY(rlatd(1:afield(l)%nlt-1,l)<rlatd(2:afield(l)%nlt,l))) ifail=1
                    ELSE
                       afield(l)%ln2s=.false.
                       IF (ANY(rlatd(1:afield(l)%nlt-1,l)>rlatd(2:afield(l)%nlt,l))) ifail=1
                    END IF
                    rlat(1:afield(l)%nlt,l)=rlatd(1:afield(l)%nlt,l)
                    rlng(1:afield(l)%nlg,l)=rlngd(1:afield(l)%nlg,l)
!
! Check for consistency of fields
                 ELSE
                    CALL check_gridded_block_v10 (tfield(1),afield(l),rlatd(1:afield(l)%nlt,l),rlat(1:afield(l)%nlt,l), &
                         rlngd(1:afield(l)%nlg,l),rlng(1:afield(l)%nlg,l),cfail,cproc,ifail)
                    IF (ifail/=0) GOTO 3
                 END IF
              END IF
!
! Check for missing dates
              cproc='get_gridded_info_v10'
              IF (k>1) THEN
                 CALL get_sequence (periodps(l),tfield(1)%tdate,isq,ifail) ! - check whether next record is in sequence -
                 IF ((ifail/=0).OR.(isq/=afile%iseq)) THEN
                    IF (((afile%iseq==1).AND.((ifail==1).OR. &                      ! - year missing in annual sequencing -
                       ((ifail==3).AND.(afile%nls>1)))).OR. & ! - month missing if lagged-fields are present -
                       ((afile%iseq==3).AND.((ifail>=4).AND.(ifail<=7)))) THEN     ! - day(s) mssing in daily sequencing -
                       ! - check whether dates are advancing -
                       IF (date_diff(periodps(l)%sdate,tfield(1)%tdate%sdate,afile%iseq)<0) THEN 
                          ifail=4
                          GOTO 3
                       END IF
                       periodps(l)=periodps(l)+1
                       lread=.false.
                    ELSE
                       ifail=4
                       GOTO 3
                    END IF
                 ELSE
                    ka(k,l)=.true.
                    periodps(l)=tfield(1)%tdate
                    afile%periodn=tfield(1)%tdate
                    lread=.true.
                 END IF
              END IF
           END DO
        END DO
     END DO
  END IF
!
! Invert latitudes of south to north fields
  DO l=1,afile%nfl
     IF (.NOT.afield(l)%ln2s) THEN
        rlatd(afield(l)%nlt:1:-1,l)=rlat(1:afield(l)%nlt,l)
        rlat(1:afield(l)%nlt,l)=rlatd(1:afield(l)%nlt,l)
     END IF
  END DO
  IF (lread) THEN
     ifail=0
  ELSE
     ifail=4
     GOTO 3
  END IF
  RETURN
!
! Errors
! - problem reading file -
1 ifail=1
  GOTO 3
!
! - problem reading file -
2 IF ((lfend).AND.(l==afile%nfl)) THEN
     ifail=0
     RETURN
  ELSE
     ifail=2
  END IF
!
! Identify last date read
3 IF (.NOT.afile%periodn==period0) cldate=get_cdate(afile%periodn,1)
!
  RETURN
  END SUBROUTINE get_gridded_info_v10
!
!
!
  SUBROUTINE get_gridded_block_v10 (iin,afield,ifail,cproc,cfail, &
             rlat,rlng)
!
! Reads field information for a block of data
!
! On exit (if cproc='get_gridded_block_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
  USE IO_constants, ONLY: ltag
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iin   ! - input unit number -
!
! Input/output scalars
  TYPE(field), INTENT(INOUT) :: afield ! - field information -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
  CHARACTER(LEN=*), INTENT(OUT) :: cproc ! - procedure in error -
  CHARACTER(LEN=*), INTENT(OUT) :: cfail ! - error indicator -
!
! Output arrays
! - optional output arrays -
  REAL(KIND=rp), INTENT(OUT), OPTIONAL :: rlat(:) ! - latitudes -
  REAL(KIND=rp), INTENT(OUT), OPTIONAL :: rlng(:) ! - longitudes -
!
! Locals
!
! Local scalars
  INTEGER :: i    ! - latitude index -
  INTEGER :: j    ! - longitude index -
  INTEGER :: igrd ! - structure -
!
  CHARACTER(LEN=ltag) :: ctag ! - tag line -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC TRIM
  INTRINSIC PRESENT
!
! Executable Statements
!
! Initialise routine
  cproc='get_gridded_block_v10'
  cfail=' '
!
! Read tag line
  READ (UNIT=iin,FMT='(A)',ERR=1,END=2) ctag
  IF (LEN_TRIM(ctag)==0) GOTO 2
  IF (INDEX(ctag,'cpt:field=')==0) THEN
     ifail=3
     RETURN
  END IF
!
! Parse tag line
  CALL get_tags (TRIM(ctag)//' ',11,                                                                                      &
       (/'T      ','S      ','Z      ','M      ','col    ','row    ','ncol   ','nrow   ','field  ','units  ','missing'/), &
       afield,ifail,cfail, &
       igrid=igrd)
  IF (ifail/=0) THEN
     cproc='get_tags'
     RETURN
  END IF
!
! Return if field is not gidded
  IF (igrd/=1) THEN
     ifail=4
     RETURN
  END IF
!
! Read longitudes
  IF (PRESENT(rlng)) THEN
     READ (UNIT=iin,FMT=*,ERR=1,END=2) (rlng(j),j=1,afield%nlg)
  ELSE
     READ (UNIT=iin,FMT=*,ERR=1,END=2)
  END IF
!
! Read latitudes
  IF (PRESENT(rlat)) THEN
     DO i=1,afield%nlt
        READ (UNIT=iin,FMT=*,ERR=1,END=2) rlat(i)
     END DO
  ELSE
     DO i=1,afield%nlt
        READ (UNIT=iin,FMT=*,ERR=1,END=2)
     END DO
  END IF
  RETURN
!
! Errors
! - problem reading file -
1 ifail=1
  RETURN
!
! - end of file -
2 ifail=2
  RETURN
!
  END SUBROUTINE get_gridded_block_v10
!
!
!
  SUBROUTINE check_gridded_block_v10 (tfield,rfield,tlat,rlat,tlng,rlng,cfail,cproc,ifail)
!
! Checks for consistency of field information
!
! On exit (if cproc='check_gridded_block_v10'):
!    ifail =  0 Successful
!    ifail =  1 Inconsistent tag as indicated by cfail
!    ifail =  2 Inconsistent latitudes or longitudes as indicated by cfail
!
! Arguments
!
! Input scalars
  TYPE(field), INTENT(IN) :: tfield ! - test field -
  TYPE(field), INTENT(IN) :: rfield ! - reference field -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: tlat(:) ! - test latitudes -
  REAL(KIND=rp), INTENT(IN) :: rlat(:) ! - reference longitudes -
  REAL(KIND=rp), INTENT(IN) :: tlng(:) ! - test latitudes -
  REAL(KIND=rp), INTENT(IN) :: rlng(:) ! - reference longitudes -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
  CHARACTER(LEN=*), INTENT(OUT) :: cproc ! - procedure in error -
  CHARACTER(LEN=*), INTENT(OUT) :: cfail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ANY
!
! Executable Statements
!
! Initialise routine
  cproc='check_gridded_block_v10'
  cfail=' '
!
! Check for consistent field settings
! - number of latitudes -
  IF (tfield%nlt/=rfield%nlt) THEN
     cfail='ncol'
     ifail=1
     RETURN
  END IF
! - number of longitudes -
  IF (tfield%nlg/=rfield%nlg) THEN
     cfail='nrow'
     ifail=1
     RETURN
  END IF
! - ensemble member -
  IF (tfield%member/=rfield%member) THEN
     cfail='member'
     ifail=1
     RETURN
  END IF
! - missing values -
  IF (tfield%rmiss/=rfield%rmiss) THEN
     cfail='missing'
     ifail=1
     RETURN
  END IF
! - variable -
  IF (tfield%var/=rfield%var) THEN
     cfail='var'
     ifail=1
     RETURN
  END IF
! - units -
  IF (tfield%unit/=rfield%unit) THEN
     cfail='units'
     ifail=1
     RETURN
  END IF
! - geopotential height -
  IF (tfield%z%hght/=rfield%z%hght) THEN
     cfail='z'
     ifail=1
     RETURN
  END IF
!
! Compare latitudes
  IF (ANY(tlat(:)/=rlat(:))) THEN
     cfail='latitudes'
     ifail=2
     RETURN
  END IF
!
! Compare longitudes
  IF (ANY(tlng(:)/=rlng(:))) THEN
     cfail='latitudes'
     ifail=2
     RETURN
  END IF
  ifail=0
!
  RETURN
  END SUBROUTINE check_gridded_block_v10
 END SUBROUTINE get_gridded_v10
!
!
!
 SUBROUTINE get_nongridded_v10 (iin,nt,afile,afield,rmiss,rlat,rlng,cstn,rlatd,rlngd,cstnd,idom,ka,ifail,ifd,cproc,cldate,cfail)
!
! Determines structure of a version-10 station or unreferenced input file, including its size, field settings, and missing cases
!
! On exit (if cproc='get_nongridded_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem allocating memory
!
! Modules
  USE fields,       ONLY: reset_grids,init_field
  USE IO_constants, ONLY: lstn
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iin ! - input unit number -
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: rmiss ! - missing value flag -
!
! Input/output scalars
  INTEGER, INTENT(INOUT) :: nt ! - number of time steps -
!
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
  INTEGER, INTENT(OUT) :: ifd   ! - field at error -
!
  CHARACTER(LEN=*), INTENT(OUT) :: cproc  ! - procedure in error -
  CHARACTER(LEN=*), INTENT(OUT) :: cldate ! - last data read -
  CHARACTER(LEN=*), INTENT(OUT) :: cfail  ! - error indicator -
!
! Pointer arrays
  INTEGER, POINTER :: idom(:,:) ! - variables within domain -
!
  REAL(KIND=rp), POINTER :: rlat(:,:)  ! - latitudes -
  REAL(KIND=rp), POINTER :: rlng(:,:)  ! - longitudes -
  REAL(KIND=rp), POINTER :: rlatd(:,:) ! - used latitudes -
  REAL(KIND=rp), POINTER :: rlngd(:,:) ! - realigned/used longitudes -
!
  CHARACTER(LEN=lstn), POINTER :: cstn(:,:)  ! - names of stations/indices -
  CHARACTER(LEN=lstn), POINTER :: cstnd(:,:) ! - names of stations within domains -
!
  LOGICAL, POINTER :: ka(:,:) ! - available cases flags -
!
  TYPE(field), POINTER :: afield(:) ! - field information -
!
! Locals
!
! Local scalars
  INTEGER :: ntag   ! - number of tag lines -
  INTEGER :: iseqbk ! - backup time sequence indicator -
!
  LOGICAL :: lfend ! - end of file flag -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ASSOCIATED
!
! Executable Statements
!
! Initialise routine
  cproc='get_nongridded_v10'
  cldate=' '
  cfail=' '
  ifd=0
!
! Check for lagged fields
  CALL get_nongridded_nls_v10 (iin,nt,afile,ntag,ifail,cproc,cldate)
  IF (ifail/=0) RETURN
!
! Initialise fields
  CALL init_field (afield,afile%nfl,(/rmiss/),ifail)
  IF (ifail/=0) THEN
     ifail=1
     RETURN
  END IF
!
! Get fields and maximum number of cases
  CALL get_nongridded_fields_v10 (iin,ntag,afile,afield,lfend,ifail,cproc,ifd,cldate,cfail)
  iseqbk=iseq
  iseq=afile%iseq
  IF (ifail/=0) GOTO 1
!
! Initialise latitudes and longitudes
  CALL reset_grids (rlat,rlng,cstn,rlatd,rlngd,cstnd,idom,ifail, &
       igrid=afile%igrid,nfl=afile%nfl,nlt=afield(:)%nlt,nlg=afield(:)%nlg)
  IF (ifail/=0) THEN
     ifail=1
     GOTO 2
  END IF
!
! Assign memory for available cases flags
  IF (ASSOCIATED(ka)) THEN
     DEALLOCATE (ka)
     NULLIFY (ka)
  END IF
  ALLOCATE (ka(afile%nt,afile%nfl),STAT=ifail)
  IF (ifail/=0) THEN
     ifail=1
     GOTO 2
  END IF
  ka(1,:)=.true.
  IF (afile%nt>1) ka(2:,:)=.false.
!
! Get station/index names and coordinates, and identify available cases
  CALL get_nongridded_info_v10 (iin,ntag,afile,afield,ka,rlat,rlng,cstn,rlatd,lfend,ifail,cproc,ifd,cldate,cfail)
  IF (ifail==0) GOTO 2
!
! Last date read
1 cldate=get_cdate(afile%periodn,1)
!
! Free workspace
  IF (ASSOCIATED(ka)) THEN
     DEALLOCATE (ka)
     NULLIFY (ka)
  END IF
!
! Restore sequencing
2 iseq=iseqbk
  afile%ntag=ntag
  RETURN
!
 CONTAINS
!
!
  SUBROUTINE get_nongridded_nls_v10 (iin,nt,afile,ntag,ifail,cproc,cldate)
!
! Determines number of lagged fields in a version-10 station or unreferenced input file
!
! On exit (if cproc='get_nongridded_nls_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
  USE IO_constants, ONLY: ltag
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iin ! - input unit number -
!
! Input/output scalars
  INTEGER, INTENT(INOUT) :: nt ! - number of time steps -
!
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ntag  ! - number of tag lines -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
  CHARACTER(LEN=*), INTENT(OUT) :: cproc  ! - procedure in error -
  CHARACTER(LEN=*), INTENT(OUT) :: cldate ! - last data read -
!
! Locals
!
! Local scalars
  INTEGER :: k ! - case index -
!
  CHARACTER(LEN=ltag) :: ctag ! - tag line -
!
  TYPE(period) :: periodc ! - current date -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ADJUSTL
  INTRINSIC INDEX
  INTRINSIC TRIM
!
! Executable Statements
!
! Initialise routine
  cproc='get_nongridded_nls_v10'
  cldate=' '
!
! Skip station names and tags
  READ (UNIT=iin,FMT=*,ERR=1,END=2)
  ntag=1
  DO
     READ (UNIT=iin,FMT='(A)',ERR=1,END=2) ctag
     IF (INDEX(ctag,'cpt:')==0) EXIT
     ntag=ntag+1
  END DO
!
! Read first date
  ctag=ADJUSTL(ctag)
  CALL get_date (' ',TRIM(ctag),afile%period1%sdate,ifail, &
       edate=afile%period1%edate)
  IF (ifail/=0) THEN
     cproc='get_date'
     RETURN
  END IF
  afile%periodn=afile%period1
!
! Check for lagged fields
! - read subsequent dates -
  afile%nls=1
  IF (nt>1) THEN
     get_nls: DO k=1,nt
        READ (UNIT=iin,FMT='(A)',ERR=1,END=2) ctag
        ctag=ADJUSTL(ctag)
        CALL get_date (' ',TRIM(ctag),periodc%sdate,ifail, &
             edate=periodc%edate)
        IF (ifail/=0) THEN
           cproc='get_date'
           GOTO 3
        END IF
! - identify date sequencing -
        CALL get_sequence (afile%period1,periodc,afile%iseq,ifail)
        SELECT CASE (ifail)
         CASE (0)
           afile%periodn=periodc
           IF ((afile%iseq==2).OR.(afile%iseq==4)) THEN ! - assume a lagged field if sequencing is monthly or seasonal -
              afile%nls=afile%nls+1
           ELSE
              EXIT get_nls
           END IF
         CASE DEFAULT
           cproc='get_sequence'
           GOTO 3
        END SELECT
     END DO get_nls
  END IF
  afile%nfl=afile%nfs*afile%nls
!
  ifail=0
  RETURN
!
! Errors
! - problem reading file -
1 ifail=1
  GOTO 3
! - end of file -
2 ifail=2
!
! Last date read
3 cldate=get_cdate(afile%periodn,1)
!
  RETURN
  END SUBROUTINE get_nongridded_nls_v10
!
!
!
  SUBROUTINE get_nongridded_fields_v10 (iin,ntag,afile,afield,lfend,ifail,cproc,ifd,cldate,cfail)
!
! Determines field settings and maximum number of cases
!
! On exit (if cproc='get_nongridded_fields_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file. Data up to cldate in ifd field read successfully
!    ifail =  2 Premature end of file. Data up to cldate in ifd field read successfully
!    ifail =  3 Season for last date of ifd field is inconsistent with earlier dates
!    ifail =  4 Last date for ifd field is not in sequence
!
! Modules
  USE IO_constants, ONLY: ltag
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iin  ! - input unit number -
  INTEGER, INTENT(IN) :: ntag ! - number of tag lines -
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
  INTEGER, INTENT(OUT) :: ifd   ! - field index -
!
  CHARACTER(LEN=*), INTENT(OUT) :: cproc  ! - procedure in error -
  CHARACTER(LEN=*), INTENT(OUT) :: cldate ! - last data read -
  CHARACTER(LEN=*), INTENT(OUT) :: cfail  ! - error indicator -
!
  LOGICAL, INTENT(OUT) :: lfend ! - end of file flag -
!
! Input/output arrays
  TYPE(field), INTENT(INOUT) :: afield(:) ! - field information -
!
! Locals
!
! Local scalars
  INTEGER :: k   ! - case index -
  INTEGER :: l   ! - field/lagged-field index -
  INTEGER :: ilf ! - lagged-field index -
  INTEGER :: ntn ! - number of time steps -
!
  CHARACTER(LEN=ltag) :: ctag ! - tag line -
!
  TYPE(period) :: periodc ! - current date -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ADJUSTL
  INTRINSIC MAX
  INTRINSIC TRIM
!
! Executable Statements
!
! Initialise routine
  cproc='get_nongridded_fields_v10'
  cldate=' '
  cfail=' '
  ifd=0
!
! Rewind to first field
  REWIND (UNIT=iin)
  DO k=1,afile%ntag
     READ (UNIT=iin,FMT=*,ERR=1,END=2)
  END DO
!
! Read fields
  afile%nt=1
  DO ifd=1,afile%nfs
     l=(ifd-1)*afile%nls+1
     READ (UNIT=iin,FMT='(A)',ERR=1,END=2) ctag
     ctag=ADJUSTL(ctag)
     CALL get_tags (TRIM(ctag)//' ',7,(/'col    ','row    ','ncol   ','nrow   ','field  ','units  ','missing'/), &
          afield(l),ifail,cfail, &
          nt=ntn)
     IF (ifail/=0) THEN
        cproc='get_tags'
        RETURN
     END IF
!
! Skip additional tag lines
     DO k=1,ntag
        READ (UNIT=iin,FMT=*,ERR=1,END=2)
     END DO
!
! Set lagged fields
     DO ilf=1,afile%nls
        l=(ifd-1)*afile%nls+ilf
        IF (ilf>1) afield(l)=afield(l-1)
        READ (UNIT=iin,FMT='(A)',ERR=1,END=2) ctag
        ctag=ADJUSTL(ctag)
        CALL get_date (' ',TRIM(ctag),periodc%sdate,ifail, &
             edate=periodc%edate)
        IF (ifail/=0) THEN
           cproc='get_date'
           IF (ilf==1) THEN
              RETURN
           ELSE
              GOTO 3
           END IF
        END IF
        afield(l)%tdate=periodc
        afile%periodn=periodc
     END DO
!
! Read last case
     IF (ntn>afile%nls) THEN
        IF (ntn-2*afile%nls>0) THEN
           DO k=afile%nls+1,ntn-1
              READ (UNIT=iin,FMT=*,ERR=1,END=2)
           END DO
        END IF
        l=ifd*afile%nls
        READ (UNIT=iin,FMT='(A)',ERR=1,END=2) ctag
        ctag=ADJUSTL(ctag)
        CALL get_date (' ',TRIM(ctag),periodc%sdate,ifail, &
             edate=periodc%edate)
        IF (ifail/=0) THEN
           cproc='get_date'
           GOTO 3
        END IF
!
! Check whether last case is for last lagged field
        lfend=.false.
        IF (afile%iseq==1) THEN
           DO ilf=afile%nls,1,-1
              IF ((periodc%sdate%imn==afield(l)%tdate%sdate%imn).AND.(periodc%sdate%idy==afield(l)%tdate%sdate%idy).AND. &
                  (periodc%edate%imn==afield(l)%tdate%edate%imn).AND.(periodc%edate%idy==afield(l)%tdate%edate%idy)) THEN
                 EXIT
              ELSE IF (ilf>1) THEN
                 lfend=.true.
                 l=l-1
              ELSE
                 ifail=3
                 GOTO 3
              END IF
           END DO
        END IF
!
! Calculate number of cases
        ntn=date_diff(afield(l)%tdate%sdate,periodc%sdate,afile%iseq)+1
        IF (ntn<1) THEN
           ifail=4
           RETURN
        END IF
        afile%nt=MAX(afile%nt,ntn)
        afile%periodn=periodc
     END IF
  END DO
!
! Set number of cases
  ifail=0
  RETURN
!
! Errors
! - problem reading file -
1 ifail=1
  GOTO 3
!
! - problem reading file -
2 ifail=2
!
! Identify last date read
3 cldate=get_cdate(afile%periodn,1)
!
  RETURN
  END SUBROUTINE get_nongridded_fields_v10
!
!
!
  SUBROUTINE get_nongridded_info_v10 (iin,ntag,afile,afield,ka,rlat,rlng,cstn,rwk,lfend,ifail,cproc,ifd,cldate,cfail)
!
! Determines variable names and coordinates of station of stations. Identifies available cases
!
! On exit (if cproc='get_nongridded_info_v10'):
!    ifail =  0 Successful
!    ifail =  1 Problem reading file. Data up to cldate in ifd field read successfully
!    ifail =  2 Premature end of file. Data up to cldate in ifd field read successfully
!    ifail =  3 Unable to read latitudes and/or longitudes (as indicated by cfail) for ifd field
!    ifail =  4 Dates for ifd field are not in sequence. Data up to cldate in ifd field read successfully
!
! Modules
  USE IO_constants, ONLY: lstn,ltag
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iin  ! - input unit number -
  INTEGER, INTENT(IN) :: ntag ! - number of tag lines -
!
  LOGICAL, INTENT(IN) :: lfend ! - end of file flag -
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
  INTEGER, INTENT(OUT) :: ifd   ! - field index -
!
  CHARACTER(LEN=*), INTENT(OUT) :: cproc  ! - procedure in error -
  CHARACTER(LEN=*), INTENT(OUT) :: cldate ! - last data read -
  CHARACTER(LEN=*), INTENT(OUT) :: cfail  ! - error indicator -
!
! Input arrays
  TYPE(field), INTENT(IN) :: afield(:) ! - field information -
!
! Input/output arrays
  LOGICAL, INTENT(INOUT) :: ka(:,:) ! - available cases flags -
!
! Output arrays
  REAL(KIND=rp), POINTER :: rlat(:,:) ! - latitudes -
  REAL(KIND=rp), POINTER :: rlng(:,:) ! - longitudes -
!
  CHARACTER(LEN=lstn), INTENT(OUT) :: cstn(:,:) ! - names of stations/indices -
!
! Workspace arrays
  REAL(KIND=rp), POINTER :: rwk(:,:) ! - workspace -
!
! Locals
!
! Local arrays
  TYPE(period) :: periodps(afile%nfl) ! - last successfully read periods -
!
! Local scalars
  INTEGER :: i     ! - station index -
  INTEGER :: k     ! - case index -
  INTEGER :: l     ! - field/lagged-field index -
  INTEGER :: isq   ! - time sequence indicator -
  INTEGER :: ilf   ! - lagged-field index -
  INTEGER :: nskip ! - number of lines to skip -
!
  LOGICAL :: llx ! - longitudes read flag -
  LOGICAL :: lly ! - longitudes read flag -
!
  CHARACTER(LEN=ltag) :: ctag ! - tag line -
!
  LOGICAL :: lread ! - read current case flag -
!
  TYPE(period) :: periodc ! - current date -
  TYPE(period) :: period0 ! - null period -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ADJUSTL
  INTRINSIC INDEX
  INTRINSIC TRIM
!
! Executable Statements
!
! Initialise routine
  cproc='get_nongridded_info_v10'
  cldate=' '
  cfail=' '
  ifd=0
!
! Set initial dates
  periodps(:)=afield(:)%tdate
  period0=0
!
! Rewind
  REWIND (UNIT=iin)
  DO k=1,afile%ntag
     READ (UNIT=iin,FMT=*,ERR=1,END=2)
  END DO
!
! Read station names
  lread=.true.
  read_fields: DO ifd=1,afile%nfs
     afile%periodn=0
     l=(ifd-1)*afile%nls+1
     IF (lread) READ (UNIT=iin,FMT=*,ERR=1,END=2)
     READ (UNIT=iin,FMT=*,ERR=1,END=2) (cstn(i,l),i=1,afield(l)%nlt)
!
! Read latitudes and longitudes
     nskip=1
     IF (afile%igeog==1) THEN
        lly=.false.
        llx=.false.
        DO
           READ (UNIT=iin,FMT=*,ERR=1,END=2) ctag,(rwk(i,l),i=1,afield(l)%nlt)
           IF (INDEX(ctag,'cpt:Y')>0) THEN
              rlat(1:afield(l)%nlt,l)=rwk(1:afield(l)%nlt,l)
              lly=.true.
              nskip=nskip+1
              IF (llx) EXIT
           ELSE IF (INDEX(ctag,'cpt:X')>0) THEN
              rlng(1:afield(l)%nlt,l)=rwk(1:afield(l)%nlt,l)
              llx=.true.
              nskip=nskip+1
              IF (lly) EXIT
           ELSE IF (INDEX(ctag,'cpt:')>0) THEN
              nskip=nskip+1
           ELSE
              EXIT
           END IF
        END DO
!
! Check for missing coordinates
        IF (llx.AND.lly) THEN
           CONTINUE
        ELSE IF (.NOT.(llx.AND.lly)) THEN
           cfail='latitudes and longitudes'
           ifail=3
           RETURN
        ELSE IF (.NOT.lly) THEN
           cfail='latitudes'
           ifail=3
           RETURN
        ELSE IF (.NOT.llx) THEN
           cfail='longitudes'
           ifail=3
           RETURN
        END IF
!
! Skip tag lines in unreferenced files
     ELSE
        nskip=nskip+1
        DO
           READ (UNIT=iin,FMT=*,ERR=1,END=2) ctag
           IF (INDEX(ctag,'cpt:')>0) THEN
              nskip=nskip+1
           ELSE
              EXIT
           END IF
        END DO
     END IF
!
! Copy field information for lagged variables
     IF (afile%nls>1) THEN
        DO ilf=2,afile%nls
           l=(ifd-1)*afile%nls+ilf
           IF (ilf>1) THEN
              IF (afile%igeog==1) THEN
                 rlat(:,l)=rlat(:,l+1-ilf)
                 rlng(:,l)=rlng(:,l+1-ilf)
              END IF
              cstn(:,l)=cstn(:,l+1-ilf)
           END IF
        END DO
     END IF
!
! Skip past first dates
     IF (afile%nt>1) THEN 
        DO k=1,afile%nls+ntag-nskip
           READ (UNIT=iin,FMT=*,ERR=1,END=2)
        END DO
!
! Check for missing dates
        lread=.true.
        DO k=2,afile%nt
           DO ilf=1,afile%nls
              l=(ifd-1)*afile%nls+ilf
              IF (lread) THEN
                 READ (UNIT=iin,FMT='(A)',ERR=1,END=2) ctag
                 ctag=ADJUSTL(ctag)
                 IF (INDEX(ctag,'cpt:')>0) THEN ! - check for next field in case of missing last values -
                    lread=.false.
                    CYCLE read_fields
                 END IF
                 CALL get_date (' ',TRIM(ctag),periodc%sdate,ifail, &
                      edate=periodc%edate)
                 IF (ifail/=0) THEN
                    cproc='get_date'
                    GOTO 3
                 END IF
              END IF
              CALL get_sequence (periodps(l),periodc,isq,ifail) ! - check whether next record is in sequence -
              IF ((ifail/=0).OR.(isq/=afile%iseq)) THEN
                 IF (((afile%iseq==1).AND.((ifail==1).OR. &                      ! - year missing in annual sequencing -
                                          ((ifail==3).AND.(afile%nls>1)))).OR. & ! - month missing if lagged-fields are present -
                     ((afile%iseq==3).AND.((ifail>=4).AND.(ifail<=7)))) THEN     ! - day(s) mssing in daily sequencing -
                    IF (date_diff(periodps(l)%sdate,periodc%sdate,afile%iseq)<0) THEN ! - check whether dates are advancing -
                       ifail=4
                       GOTO 3
                    END IF
                    periodps(l)=periodps(l)+1
                    lread=.false.
                 ELSE
                    ifail=4
                    GOTO 3
                 END IF
              ELSE
                 ka(k,l)=.true.
                 periodps(l)=periodc
                 afile%periodn=periodc
                 lread=.true.
              END IF
           END DO
        END DO
     END IF
  END DO read_fields
  IF (lread) THEN
     ifail=0
  ELSE
     ifail=4
     GOTO 3
  END IF
  RETURN
!
! Errors
! - problem reading file -
1 ifail=1
  GOTO 3
!
! - problem reading file -
2 IF ((lfend).AND.(l==afile%nfl)) THEN
     ifail=0
     RETURN
  ELSE
     ifail=2
  END IF
!
! Identify last date read
3 IF (.NOT.afile%periodn==period0) cldate=get_cdate(afile%periodn,1)
!
  RETURN
  END SUBROUTINE get_nongridded_info_v10
 END SUBROUTINE get_nongridded_v10
!
!
!
 SUBROUTINE get_tags (ctag,ntags,ctags,afield,ifail,cfail, &
                      igrid,nt)
!
! Reads CPT file tag values
!
! On exit:
!    ifail =  0 Successful
!    ifail = -1 Invalid combination of tags
!    ifail = -2 Missing optional argument nt for nrow and/or ncol tags
!    ifail =  1 Unable to read cfail tag
!    ifail =  2 Unable to find cfail tag (if tag is a required field, otherwise ifail=0 and default value is specified)
!    ifail =  3 Invalid value for cfail tag
!    ifail =  4 Value for crow is icompatible with ccol='X'
!    ifail =  5 Value for crow is icompatible with ccol='station'
!    ifail =  6 Value for crow is icompatible with ccol='index'
!    ifail =  7 Unknown value for ccol
!
! Modules
  USE IO_constants, ONLY: ldat
  USE numbers,      ONLY: zero
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: ntags ! - number of desired tags -
!
  CHARACTER(LEN=*), INTENT(IN) :: ctag ! - tag line -
!
! Input arrays
  CHARACTER(LEN=*), INTENT(IN) :: ctags(:) ! - desired tags -
!
! Input/output scalars
! - optional input/output scalars -
  INTEGER, INTENT(INOUT), OPTIONAL :: nt ! - number of time steps -
!
  TYPE(field), INTENT(INOUT) :: afield ! - field -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
! - optional output scalars -
  INTEGER, INTENT(OUT), OPTIONAL :: igrid ! - dataset structure indicator -
!
  CHARACTER(LEN=*), INTENT(OUT) :: cfail ! - problem tag -
!
! Locals
  INTEGER :: i    ! - tag index -
  INTEGER :: i1   ! - locator -
  INTEGER :: i2   ! - locator -
  INTEGER :: i3   ! - trimmed length -
  INTEGER :: igrd ! - data structure indicator -
  INTEGER :: ncol ! - number of columns -
  INTEGER :: nrow ! - number of rows -
!
  REAL(KIND=rp) :: r ! - real input value -
!
  CHARACTER(LEN=ldat) :: ccol  ! - columns -
  CHARACTER(LEN=ldat) :: crow  ! - rows -
!
  LOGICAL :: lcol  ! - columns set -
  LOGICAL :: lrow  ! - rows set -
  LOGICAL :: lncol ! - number of columns set -
  LOGICAL :: lnrow ! - number of rows set -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC INDEX
  INTRINSIC LEN
  INTRINSIC LEN_TRIM
  INTRINSIC NINT
  INTRINSIC PRESENT
  INTRINSIC SCAN
  INTRINSIC TRIM
!
! Executable Statements
!
! Initialise
  lcol=.false.
  lrow=.false.
  lncol=.false.
  lnrow=.false.
  i3=LEN_TRIM(ctag)
!
! Repeat for each tag
  ifail=1
  DO i=1,ntags
     cfail=ctags(i)
     SELECT CASE (TRIM(ctags(i)))
!
! Current date
      CASE ('T')
        CALL get_date ('T',ctag,afield%tdate%sdate,ifail, &
             edate=afield%tdate%edate)
        SELECT CASE (ifail)
         CASE (0,1)
           CONTINUE
         CASE (2)
           ifail=1
           GOTO 1
         CASE (3)
           GOTO 1
        END SELECT
!
! Start date for model data
      CASE ('S')
        CALL get_date ('S',ctag,afield%mdate,ifail)
        SELECT CASE (ifail)
         CASE (0,1)
           CONTINUE
         CASE (2)
           ifail=1
           GOTO 1
         CASE (3)
           GOTO 1
        END SELECT
!
! Level
      CASE ('Z')
        i1=INDEX(ctag(1:),'cpt:Z=')
        IF (i1>0) THEN
           i1=i1+LEN('cpt:Z=')
           i2=i1+SCAN(ctag(i1:),',')-2
           IF (i2<i1) i2=i3
           READ (UNIT=ctag(i1:i2),FMT=*,ERR=1) afield%z%hght,afield%z%unit
        ELSE
           afield%z%hght=zero
           afield%z%unit='none'
        END IF
!
! Ensemble member
      CASE ('M')
        lcol=.true.
        i1=INDEX(ctag(1:),'cpt:M=')
        IF (i1>0) THEN
           i1=i1+LEN('cpt:M=')
           i2=i1+SCAN(ctag(i1:),',')-2
           IF (i2<i1) i2=i3
           READ (UNIT=ctag(i1:i2),FMT=*,ERR=1) r
           afield%member=NINT(r)
        ELSE
           afield%member=0
        END IF
!
! Data structure
! - columns -
      CASE ('col')
        lcol=.true.
        i1=INDEX(ctag(1:),'cpt:col=')
        IF (i1>0) THEN
           i1=i1+LEN('cpt:col=')
           i2=i1+SCAN(ctag(i1:),',')-2
           IF (i2<i1) i2=i3
           READ (UNIT=ctag(i1:i2),FMT='(A)',ERR=1) ccol
        ELSE
           ifail=-2
           GOTO 1
        END IF
! - rows -
      CASE ('row')
        lrow=.true.
        i1=INDEX(ctag(1:),'cpt:row=')
        IF (i1>0) THEN
           i1=i1+LEN('cpt:row=')
           i2=i1+SCAN(ctag(i1:),',')-2
           IF (i2<i1) i2=i3
           READ (UNIT=ctag(i1:i2),FMT='(A)',ERR=1) crow
        ELSE
           ifail=-2
           GOTO 1
        END IF
!
! Identify number of latitudes and longitudes / number of stations / indices
! - number of columns -
      CASE ('ncol')
        lncol=.true.
        i1=INDEX(ctag(1:),'cpt:ncol=')
        IF (i1>0) THEN
           i1=i1+LEN('cpt:ncol=')
           READ (UNIT=ctag(i1:),FMT=*,ERR=1) ncol
        ELSE
           ifail=2
           GOTO 1
        END IF
! - number of rows -
      CASE ('nrow')
        lnrow=.true.
        i1=INDEX(ctag(1:),'cpt:nrow=')
        IF (i1>0) THEN
           i1=i1+LEN('cpt:nrow=')
           READ (UNIT=ctag(i1:),FMT=*,ERR=1) nrow
        ELSE
           ifail=2
           GOTO 1
        END IF
!
! Field variable
      CASE ('field')
        i1=INDEX(ctag(1:),'cpt:field=')
        IF (i1>0) THEN
           i1=i1+LEN('cpt:field=')
           i2=i1+SCAN(ctag(i1:),',')-2
           IF (i2<i1) i2=i3
           READ (UNIT=ctag(i1:i2),FMT='(A)',ERR=1) afield%var
        ELSE
           afield%var='undefined'
        END IF
!
! Variable units
      CASE ('units')
        i1=INDEX(ctag(1:),'cpt:units=')
        IF (i1>0) THEN
           i1=i1+LEN('cpt:units=')
           i2=i1+SCAN(ctag(i1:),',')-2
           IF (i2<i1) i2=i3
           READ (UNIT=ctag(i1:i2),FMT='(A)',ERR=1) afield%unit
        ELSE
           afield%unit='undefined'
        END IF
!
! Missing value flag
      CASE ('missing')
        i1=INDEX(ctag(1:),'cpt:missing=')
        IF (i1>0) THEN
           i1=i1+LEN('cpt:missing=')
           READ (UNIT=ctag(i1:),FMT=*,ERR=1) afield%rmiss
        END IF
     END SELECT
  END DO
  cfail=' '
!
! Check for valid combination of requested tags
  IF (((lrow.AND.lcol).OR..NOT.(lrow.OR.lcol)).AND.((lnrow.AND.lncol).OR..NOT.(lnrow.OR.lncol))) THEN
     IF (lcol) THEN
        CALL get_igrid (ccol,crow,igrd,ifail)
        IF (ifail/=0) THEN
           ifail=ifail+3
           GOTO 1
        END IF
        IF (PRESENT(igrid)) igrid=igrd
        SELECT CASE (igrd)
         CASE (1) ! - gridded data -
           IF (lncol) THEN
              afield%nlt=nrow
              afield%nlg=ncol
           END IF
         CASE (2) ! - station data -
           afield%nlt=ncol
           afield%nlg=ncol
           nt=nrow
         CASE (3) ! - station and unreferenced data -
           afield%nlt=ncol
           afield%nlg=ncol
           IF (.NOT.PRESENT(nt)) THEN
              ifail=-2
              GOTO 1
           END IF
           nt=nrow
        END SELECT
        ifail=0
     ELSE IF (lncol) THEN
        ifail=-1
     ELSE
        ifail=0
     END IF
  ELSE
     ifail=-1
  END IF
!
! Errors
1 RETURN
!
 CONTAINS
!
!
 SUBROUTINE get_igrid (ccol,crow,igrid,ifail)
!
! Determines file structure
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Value for crow is icompatible with ccol='X'
!    ifail =  2 Value for crow is icompatible with ccol='station'
!    ifail =  3 Value for crow is icompatible with ccol='index'
!    ifail =  4 Unknown value for ccol
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=ldat), INTENT(IN) :: ccol ! - columns -
  CHARACTER(LEN=ldat), INTENT(IN) :: crow ! - rows -
!
! Output scalars
  INTEGER, INTENT(OUT) :: igrid ! - file structure -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC TRIM
!
! Executable Statements
!
! Determine structure
  igrid=0
  SELECT CASE (TRIM(ccol))
   CASE ('X')
     SELECT CASE (TRIM(crow))
      CASE ('Y') ! - gridded file -
        igrid=1
      CASE DEFAULT
        ifail=1
        RETURN
     END SELECT
   CASE ('station')
     SELECT CASE (TRIM(crow))
      CASE ('T') ! - station file -
        igrid=2
      CASE DEFAULT
        ifail=2
        RETURN
     END SELECT
   CASE ('index')
     SELECT CASE (TRIM(crow))
      CASE ('T') ! - unreferenced file -
        igrid=3
      CASE DEFAULT
        ifail=3
        RETURN
     END SELECT
   CASE DEFAULT
     ifail=4
     RETURN
  END SELECT
  ifail=0
!
  RETURN
  END SUBROUTINE get_igrid
 END SUBROUTINE get_tags
END MODULE get_input_file
