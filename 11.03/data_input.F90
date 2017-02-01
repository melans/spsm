MODULE data_input
!
! Modules
  USE fields,  ONLY: field,domain,area
  USE iofiles, ONLY: ifile
  USE numbers, ONLY: rp=>dp
  USE time
!
! Implicit declarations
  IMPLICIT NONE
!
! Local parameter
#ifdef PGI
 INTEGER, PARAMETER :: lmax=2**22 ! maximum pgf90 will accept
#elif IFORT
 INTEGER, PARAMETER :: lmax=2**21 ! maximum ifort will accept
#elif GFORTRAN
 INTEGER, PARAMETER :: lmax=2**24 ! maximum gfortran will accept
#else
 INTEGER, PARAMETER :: lmax=2**24 ! default
#endif
!
CONTAINS
!
!
 FUNCTION get_data()
!
! Gets X and Y data and sets up default forecast file
!
! On exit returns:
!    0 if no errors
!   -1 if cancelled
!    1 if insufficient memory to read data
!    2 if first year of X training period is before first year in file
!    3 if first year of Y training period is before first year in file
!
! Modules
  USE analysis,   ONLY: dprog
  USE arrays,     ONLY: x,y,kax,kay
  USE errors,     ONLY: error
  USE fields,     ONLY: xfield,idomx, &
                        yfield,idomy
  USE iofiles,    ONLY: xfile,yfile
  USE numbers,    ONLY: one
  USE settings,   ONLY: nt,nx,ny,isem
!
! Function type
  INTEGER :: get_data
!
! Locals
!
! Local scalars
  INTEGER :: isq   ! - sequence flag -
  INTEGER :: ifail ! - error indicator -
!
  CHARACTER(LEN=10) :: cproc ! - procedure -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC SUM
  INTRINSIC TRIM
!
! Executable Statements
!
! Initialise reading
  get_data=-1
  ifail=init_read()
  IF (ifail/=0) THEN
     cproc='init_read'
     GOTO 1
  END IF
!
! Check read settings
  ifail=check_read()
  SELECT CASE (ifail)
   CASE (0)
     isq=1
   CASE (1)
     cproc='check_read'
     isq=xfile%iseq
     GOTO 1
   CASE (2)
     cproc='check_read'
     isq=yfile%iseq
     GOTO 1
   CASE DEFAULT
  END SELECT
!
! Initialise progress increment for reading data
  dprog=one/(num_read(xfile%igrid,nt,SUM(xfield(:)%nlt),xfile%nfs,xfile%nls,xfile%it1)+ &
             num_read(yfile%igrid,nt,SUM(yfield(:)%nlt),yfile%nfs,yfile%nls,yfile%it1)+1)
!
! Read X data
  WRITE (UNIT=*,FMT='(A)') 'Reading '//TRIM(xfile%ffile)//' ...'
  SELECT CASE (xfile%igrid)
   CASE (1)
     CALL read_grid (xfile,xfield,nt,isem,idomx,kax,x,ifail)
   CASE (2)
     CALL read_stns (xfile,xfield,nt,idomx,kax,x,ifail)
   CASE (3)
     CALL read_unrf (xfile,xfield,nt,idomx,kax,x,ifail)
  END SELECT
  IF (ifail/=0) RETURN
!
! Read Y data
  WRITE (UNIT=*,FMT='(A)') 'Reading '//TRIM(yfile%ffile)//' ...'
  SELECT CASE (yfile%igrid)
   CASE (1)
     CALL read_grid (yfile,yfield,nt,isem,idomy,kay,y,ifail)
   CASE (2)
     CALL read_stns (yfile,yfield,nt,idomy,kay,y,ifail)
   CASE (3)
     CALL read_unrf (yfile,yfield,nt,idomy,kay,y,ifail)
  END SELECT
  IF (ifail/=0) RETURN
  get_data=0
!
  RETURN
!
! Errors
1 get_data=ifail
  CALL error (cproc,ifail, &
       i_arg1=isq)
!
  RETURN
!
 CONTAINS
!
!
  FUNCTION init_read()
!
! Initialises memory and settings for reading data
!
! On exit, returns:
!    0 if no errors
!    1 if problem allocating memory
!
! Modules
  USE arrays, ONLY: iusex,iusey,kuse,dwk
!
! Function type
  INTEGER :: init_read
!
! Locals
!
! Local scalars
  INTEGER :: mlgx  ! - maximum number of X longitudes -
  INTEGER :: mlgy  ! - maximum number of Y longitudes -
  INTEGER :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MAX
  INTRINSIC MAXVAL
!
! Executable Statements
!
! Allocate data space
  init_read=1
! - X data -
  ALLOCATE (x(nx,nt),STAT=ifail)
  IF (ifail/=0) RETURN
! - Y data -
  ALLOCATE (y(ny,nt),STAT=ifail)
  IF (ifail/=0) RETURN
! - X variables flags -
  ALLOCATE (iusex(nx),STAT=ifail)
  IF (ifail/=0) RETURN
! - Y variables flags -
  ALLOCATE (iusey(ny),STAT=ifail)
  IF (ifail/=0) RETURN
! - cases flags -
  ALLOCATE (kuse(nt),STAT=ifail)
  IF (ifail/=0) RETURN
! - workspace -
  mlgx=MAXVAL(xfield(:)%nlg)
  mlgy=MAXVAL(yfield(:)%nlg)
  ALLOCATE (dwk(MAX(nt,MAX(mlgx,mlgy))),STAT=ifail)
  IF (ifail/=0) RETURN
!
  init_read=0
!
  RETURN
  END FUNCTION init_read
!
!
!
  FUNCTION check_read()
!
! Checks reading settings
!
! On exit returns:
!      0 if all checks passed
!      1 if xfile%fdate < xfield(:)%tdate%sdate, xfile%fdate reset to xfield(:)%tdate%sdate
!      2 if yfile%fdate < yfield(:)%tdate%sdate, yfile%fdate reset to yfield(:)%tdate%sdate
!     -1 if negative lag
!     -2 if large lag
!     -3 if inconsistent target period
!
! Modules
  USE settings, ONLY: check_it1,check_lag
!
! Function type
  INTEGER :: check_read
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
!
! Executable Statements
!
! Check date availability
  check_read=1
  CALL check_it1 (xfile%iseq,xfile%period1%sdate,xfile%fdate,xfile%it1,ifail)
  IF (ifail/=0) THEN
     check_read=1
     RETURN
  END IF
  CALL check_it1 (yfile%iseq,yfile%period1%sdate,yfile%fdate,yfile%it1,ifail)
  IF (ifail/=0) THEN
     check_read=2
     RETURN
  END IF
!
! Check for negative lags
  CALL check_lag (xfile%fdate,yfile%fdate,xfield(1)%mdate,xfile%it1,xfile%iseq,ifail)
  check_read=-ifail
!
  RETURN
  END FUNCTION check_read
 END FUNCTION get_data
!
!
!
 FUNCTION num_read(igrid,nt,nlt,nfs,nls,it1)
!
! Estimates number of steps to read data for progress meter
!
! Function type
  REAL(KIND=rp) :: num_read
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: igrid ! - grid identifier -
  INTEGER, INTENT(IN) :: nt    ! - number of cases in training period -
  INTEGER, INTENT(IN) :: nlt   ! - number of latitudes -
  INTEGER, INTENT(IN) :: nfs   ! - number of fields -
  INTEGER, INTENT(IN) :: nls   ! - number of lagged fields -
  INTEGER, INTENT(IN) :: it1   ! - index of first year of interest in file -
!
! Locals
!
! Local scalars
  INTEGER :: n  ! - number of time steps -
  INTEGER :: nr ! - number of input steps -
!
! Executable Statements
!
! Calculate number of input steps
  n=it1-1+nt
  SELECT CASE (igrid)
   CASE (1)
     nr=1+n*(nfs*nls+nlt)
   CASE (2)
     nr=nfs*(4+nls*2*n)
   CASE (3)
     nr=1+2*n*nls
  END SELECT
  num_read=nr+nt
!
  RETURN
 END FUNCTION num_read
!
!
!
 SUBROUTINE read_grid (afile,afield,n,isem,idom,ka,v,ifail)
!
! Reads gridded data
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
  USE analysis,     ONLY: prog,dprog
  USE arrays,       ONLY: dwk, &
                          insertion_sort
  USE errors,       ONLY: error
  USE IO_constants, ONLY: iin,lprd
  USE iofiles,      oNLY: open_infile
  USE labels,       ONLY: cg_seqs_l
  USE maths,        ONLY: magnitude
!#ifndef ONLY_FORTRAN
!  USE FortranCMix,  ONLY: read_grid_v9_c_wrapper,read_grid_v10_c_wrapper
!#endif
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n    ! - number of cases -
  INTEGER, INTENT(IN) :: isem ! - sort ensemble members flag -
!
  TYPE(ifile), INTENT(IN) :: afile ! - input file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: idom(:,:) ! - used gridpoints -
!
  LOGICAL, INTENT(IN) :: ka(:,:) ! - available cases flags -
!
  TYPE(field), INTENT(IN) :: afield(:) ! - fields -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: v(:,:) ! - data -
!
! Locals
!
! Local scalars
  INTEGER :: i     ! - variable index -
  INTEGER :: ie    ! - ensemble member index -
  INTEGER :: ifd   ! - field index -
  INTEGER :: ilf   ! - lagged field index -
  INTEGER :: iv    ! - current variable -
  INTEGER :: iv1   ! - variable offset -
  INTEGER :: k     ! - time index -
  INTEGER :: l     ! - field/lagged field index -
  INTEGER :: nread ! - number of records read from training period -
  INTEGER :: im    ! - order of magnitude -
!
  CHARACTER(LEN=  10) :: cfmt  ! - format statement -
  CHARACTER(LEN= 128) :: cprog ! - progress -
  CHARACTER(LEN=lprd) :: cdate ! - date -
!
  TYPE(period) :: period0 ! - date of last successfully read period -
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
     CALL error ('open_infile',ifail, &
          c_arg1=TRIM(afile%ffile))
     RETURN
  END IF
!
! Read gridded data
  period0=0
  SELECT CASE (afile%ffmt%iver)
   CASE (9)
     CALL read_grid_v9 (afile,afield,n,idom,ka,v,nread,period0,ifail)
   CASE (10)
     IF (.NOT.afile%lstack) THEN
        CALL read_grid_v10_nostack (afile,afield,n,idom,ka,v,nread,period0,ifail)
     ELSE
        CALL read_grid_v10_stack (afile,afield,n,idom,ka,v,nread,period0,ifail)
     END IF
  END SELECT
  CLOSE (iin)
!
! Errors
  IF (ifail/=0) THEN
     IF (period0%sdate%iyr>0) THEN
        cdate=get_cdate(period0,2)
        IF (nread>0) THEN
           im=magnitude(nread)
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(3A,I',im,',3A)'
           WRITE (UNIT=cprog,FMT=cfmt) &
              'Data up to ',TRIM(cdate),' (',nread,' ',TRIM(cg_seqs_l(afile%iseq)),' of training period) read successfully.'
        ELSE
           WRITE (UNIT=cprog,FMT='(3A)') &
              'Data up to ',TRIM(cdate),' read successfully.'
        END IF
        CALL error ('read_grid',ifail, &
             i_arg1=nread,c_arg1=TRIM(afile%ffile),c_arg2=TRIM(cprog))
     ELSE
        CALL error ('read_grid',ifail, &
             c_arg1=TRIM(afile%ffile))
     END IF
  END IF
!
! Sort ensemble members
  IF (afile%lensemble.AND.(isem==1)) THEN
     iv1=0
     DO ifd=1,afile%nfs/afile%nem
        DO ilf=1,afile%nls
           l=(ifd-1)*afile%nem*afile%nls+ilf
           DO i=1,afield(l)%nv
              DO k=1,n
                 DO ie=1,afile%nem
                    iv=iv1+((ie-1)*afile%nls+(ilf-1))*afield(l)%nv+i
                    dwk(ie)=v(iv,k)
                 END DO
                 CALL insertion_sort (dwk(:),afile%nem,'a')
                 DO ie=1,afile%nem
                    iv=iv1+((ie-1)*afile%nls+(ilf-1))*afield(l)%nv+i
                    v(iv,k)=dwk(ie)
                 END DO
              END DO
           END DO
        END DO
        l=(ifd-1)*afile%nem*afile%nls
        iv1=iv1+afile%nem*afile%nls*afield(l)%nv
     END DO
  END IF
!
  RETURN
!
 CONTAINS
!
!
  SUBROUTINE read_grid_v9 (afile,afield,n,idom,ka,v,nread,period0,ifail)
!
! Reads formatted gridded data
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
  USE time_constants, ONLY: lmon
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
  TYPE(ifile), INTENT(IN) :: afile ! - file -
!
! Input/output scalars
  TYPE(period), INTENT(INOUT) :: period0 ! - last successfully read period -
!
! Input arrays
  INTEGER, INTENT(IN) :: idom(:,:) ! - used gridpoints -
!
  LOGICAL, INTENT(IN) :: ka(:,:) ! - available cases flags -
!
  TYPE(field), INTENT(IN) :: afield(:) ! - fields -
!
! Output scalars
  INTEGER, INTENT(OUT) :: nread ! - number of cases in training period read successfully -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: v(:,:) ! - data -
!
! Locals
!
! Local scalars
  INTEGER :: i      ! - latitude index -
  INTEGER :: i1     ! - first latitude index -
  INTEGER :: i2     ! - last latitude index -
  INTEGER :: iinc   ! - latitude index increment -
  INTEGER :: j      ! - longitude index -
  INTEGER :: j0     ! - initial longitude offset -
  INTEGER :: jj     ! - grid index -
  INTEGER :: k      ! - time index -
  INTEGER :: irskip ! - number of records to skip -
!
  REAL(KIND=rp) :: rlat ! - latitudes -
!
  CHARACTER(LEN=lmon) :: mycmon ! - current month -
!
  TYPE(date) :: date1 ! - current date -
!
! Executable Statements
!
! Skip first few records if required
  nread=0
  irskip=afile%it1-1
  prog=prog+dprog
  IF (irskip>0) THEN
     DO k=1,irskip
        IF (.NOT.ka(k,1)) CYCLE
        READ (UNIT=iin,FMT=*,ERR=1,END=2) date1%idy,mycmon,date1%iyr
        date1%imn=get_month(mycmon)
        prog=prog+dprog
        DO i=1,afield(1)%nlt
           READ (UNIT=iin,FMT=*,ERR=1,END=2)
           prog=prog+dprog
        END DO
        period0%sdate=date1
     END DO
  END IF
!
! Determine direction
  IF (afield(1)%ln2s) THEN
     i1=1
     i2=afield(1)%nlt
     iinc=1
     j0=0
  ELSE
     i1=afield(1)%nlt
     i2=1
     iinc=-1
     j0=afield(1)%region%nlgs*(afield(1)%region%nlts-1)
  END IF
!
! Read data
  IF (irskip==afile%nt) THEN
     period0%edate=period0%sdate
     ifail=0
     RETURN
  END IF
  DO k=1,n
     IF (ka(irskip+k,1)) THEN
        READ (UNIT=iin,FMT=*,ERR=1,END=2) date1%idy,mycmon,date1%iyr
        date1%imn=get_month(mycmon)
        prog=prog+dprog
        jj=j0
        DO i=i1,i2,iinc
           IF ((i>=afield(1)%region%nlt1).AND.(i<=afield(1)%region%nlt2)) THEN
              READ (UNIT=iin,FMT=*,ERR=1,END=2) rlat,(dwk(j),j=1,afield(1)%nlg)
              v(jj+1:jj+afield(1)%region%nlgs,k)=dwk(idom(1:afield(1)%region%nlgs,1))
              jj=jj+afield(1)%region%nlgs*iinc
           ELSE
              READ (UNIT=iin,FMT=*,ERR=1,END=2)
           END IF
           prog=prog+dprog
        END DO
        period0%sdate=date1
        nread=nread+1
     ELSE
        v(1:afield(1)%nv,k)=afield(1)%rmiss
     END IF
  END DO
!
! No errors
  ifail=0
  RETURN
!
! Error
1 ifail=1
  period0%edate=period0%sdate
  RETURN
!
! End of file
2 ifail=2
  period0%edate=period0%sdate
  RETURN
!
  END SUBROUTINE read_grid_v9
!
!
!
  SUBROUTINE read_grid_v10_nostack (afile,afield,n,idom,ka,v,nread,period0,ifail)
!
! Reads formatted gridded data with unstacked fields
!
! On exit:
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
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
  TYPE(ifile), INTENT(IN) :: afile ! - file -
!
! Input/output scalars
  TYPE(period), INTENT(INOUT) :: period0 ! - last successfully read period -
!
! Output scalars
  INTEGER, INTENT(OUT) :: nread ! - number of cases read successfully -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: idom(:,:) ! - used gridpoints -
!
  LOGICAL, INTENT(IN) :: ka(:,:) ! - available cases flags -
!
  TYPE(field), INTENT(IN) :: afield(:) ! - fields -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: v(:,:) ! - data -
!
! Locals
!
! Local scalars
  INTEGER :: i      ! - latitude index -
  INTEGER :: i1     ! - first latitude index -
  INTEGER :: i2     ! - last latitude index -
  INTEGER :: iinc   ! - latitude index increment -
  INTEGER :: j      ! - longitude index -
  INTEGER :: jj     ! - grid index -
  INTEGER :: k      ! - time index -
  INTEGER :: l      ! - field / lag index -
  INTEGER :: irskip ! - number of records to skip -
!
  REAL(KIND=rp) :: rlat ! - latitudes -
!
  CHARACTER(LEN=ltag) :: ctag0 ! - previous tag line -
  CHARACTER(LEN=ltag) :: ctag1 ! - current tag line -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC SUM
!
! Executable Statements
!
! Skip v10 XML namespace headers and tags
  xml: DO k=1,afile%ntag
     READ (UNIT=iin,FMT=*,ERR=1,END=2)
  END DO xml
  prog=prog+dprog
!
! Skip first few records if required
  nread=0
  ctag0='None'
  irskip=afile%it1-1
  IF (irskip>0) THEN
     DO k=1,irskip
        DO l=1,afile%nfl
           IF (.NOT.ka(k,l)) CYCLE
           READ (UNIT=iin,FMT='(A)',ERR=1,END=2) ctag1
           READ (UNIT=iin,FMT=*,ERR=1,END=2)
           prog=prog+dprog
           DO i=1,afield(l)%nlt
              READ (UNIT=iin,FMT=*,ERR=1,END=2)
              prog=prog+dprog
           END DO
           ctag0=ctag1
        END DO
     END DO
  END IF
!
! Read data
  IF (irskip==afile%nt) THEN !FIXME: This should not happen
     IF (ctag0(1:4)/='None') THEN
        CALL get_date ('T',ctag0,period0%sdate,ifail, &
                       edate=period0%edate)
     END IF
     RETURN
  END IF
  DO k=1,n
     DO ilf=1,afile%nls
        DO ifd=1,afile%nfs
           l=(ifd-1)*afile%nls+ilf
           IF (ka(irskip+k,1)) THEN
              READ (UNIT=iin,FMT='(A)',ERR=1,END=2) ctag1
              READ (UNIT=iin,FMT=*,ERR=1,END=2)
              prog=prog+dprog
              IF (afield(l)%ln2s) THEN
                 i1=1
                 i2=afield(l)%nlt
                 iinc=1
                 jj=0
                 IF (l>1) jj=jj+SUM(afield(1:l-1)%region%nlts*afield(1:l-1)%region%nlgs)
              ELSE
                 i1=afield(l)%nlt
                 i2=1
                 iinc=-1
                 jj=afield(l)%region%nlgs*(afield(l)%region%nlts-1)
                 IF (l>1) jj=jj+SUM(afield(1:l-1)%region%nlts*afield(1:l-1)%region%nlgs)
              END IF
              DO i=i1,i2,iinc
                 IF ((i>=afield(l)%region%nlt1).AND.(i<=afield(l)%region%nlt2)) THEN
                    READ (UNIT=iin,FMT=*,ERR=1,END=2) rlat,(dwk(j),j=1,afield(l)%nlg)
                    v(jj+1:jj+afield(l)%region%nlgs,k)=dwk(idom(1:afield(l)%region%nlgs,l))
                    jj=jj+afield(l)%region%nlgs*iinc
                 ELSE
                    READ (UNIT=iin,FMT=*,ERR=1,END=2)
                 END IF
                 prog=prog+dprog
              END DO
              ctag0=ctag1
           ELSE
              jj=0
              IF (l>1) jj=jj+SUM(afield(1:l-1)%region%nlts*afield(1:l-1)%region%nlgs)
              v(jj+1:jj+afield(l)%region%nlts*afield(l)%region%nlgs,k)=afield(l)%rmiss
           END IF
        END DO
        nread=nread+1
     END DO
  END DO
!
! No errors
  ifail=0
  RETURN
!
! Error reading file
1 ifail=1
  GOTO 3
!
! End of file
2 ifail=2
!
! Get latest date
3 IF (ctag0(1:4)/='None') THEN
     CALL get_date ('T',ctag0,period0%sdate,ifail, &
          edate=period0%edate)
  END IF
!
  END SUBROUTINE read_grid_v10_nostack
!
!
!
  SUBROUTINE read_grid_v10_stack (afile,afield,n,idom,ka,v,nread,period0,ifail)
!
! Reads formatted gridded data with stacked fields
!
! On exit:
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
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
  TYPE(ifile), INTENT(IN) :: afile ! - file -
!
! Input/output scalars
  TYPE(period), INTENT(INOUT) :: period0 ! - last successfully read period -
!
! Output scalars
  INTEGER, INTENT(OUT) :: nread ! - number of cases read successfully -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: idom(:,:) ! - used gridpoints -
!
  LOGICAL, INTENT(IN) :: ka(:,:) ! - available cases flags -
!
  TYPE(field), INTENT(IN) :: afield(:) ! - fields -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: v(:,:) ! - data -
!
! Locals
!
! Local scalars
  INTEGER :: i      ! - latitude index -
  INTEGER :: i1     ! - first latitude index -
  INTEGER :: i2     ! - last latitude index -
  INTEGER :: iinc   ! - latitude index increment -
  INTEGER :: j      ! - longitude index -
  INTEGER :: jj     ! - grid index -
  INTEGER :: k      ! - time index -
  INTEGER :: l      ! - field / lag index -
  INTEGER :: irskip ! - number of records to skip -
!
  REAL(KIND=rp) :: rlat ! - latitudes -
!
  CHARACTER(LEN=ltag) :: ctag0 ! - previous tag line -
  CHARACTER(LEN=ltag) :: ctag1 ! - current tag line -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC SUM
!
! Executable Statements
!
! Skip v10 XML namespace headers and tags
  xml: DO k=1,afile%ntag
     READ (UNIT=iin,FMT=*,ERR=1,END=2)
  END DO xml
  prog=prog+dprog
!
! Skip first few records if required
  nread=0
  ctag0='None'
  irskip=afile%it1-1
  DO ifd=1,afile%nfs
     IF (irskip>0) THEN
        DO k=1,irskip
           DO ilf=1,afile%nls
              l=(ifd-1)*afile%nls+ilf
              IF (.NOT.ka(k,l)) CYCLE
              READ (UNIT=iin,FMT='(A)',ERR=1,END=2) ctag1
              READ (UNIT=iin,FMT=*,ERR=1,END=2)
              prog=prog+dprog
              DO i=1,afield(l)%nlt
                 READ (UNIT=iin,FMT=*,ERR=1,END=2)
                 prog=prog+dprog
              END DO
              ctag0=ctag1
           END DO
        END DO
     END IF
!
! Read data
  IF (irskip==afile%nt) THEN
     IF (ctag0(1:4)/='None') THEN
        CALL get_date ('T',ctag0,period0%sdate,ifail, &
                        edate=period0%edate)
     END IF
     RETURN
  END IF
     DO k=1,n
        DO ilf=1,afile%nls
           l=(ifd-1)*afile%nls+ilf
           IF (ka(irskip+k,1)) THEN
              READ (UNIT=iin,FMT='(A)',ERR=1,END=2) ctag1
              READ (UNIT=iin,FMT=*,ERR=1,END=2)
              prog=prog+dprog
              IF (afield(l)%ln2s) THEN
                 i1=1
                 i2=afield(l)%nlt
                 iinc=1
                 jj=0
                 IF (l>1) jj=jj+SUM(afield(1:l-1)%region%nlts*afield(1:l-1)%region%nlgs)
              ELSE
                 i1=afield(l)%nlt
                 i2=1
                 iinc=-1
                 jj=afield(l)%region%nlgs*(afield(l)%region%nlts-1)
                 IF (l>1) jj=jj+SUM(afield(1:l-1)%region%nlts*afield(1:l-1)%region%nlgs)
              END IF
              DO i=i1,i2,iinc
                 IF ((i>=afield(l)%region%nlt1).AND.(i<=afield(l)%region%nlt2)) THEN
                    READ (UNIT=iin,FMT=*,ERR=1,END=2) rlat,(dwk(j),j=1,afield(l)%nlg)
                    v(jj+1:jj+afield(l)%region%nlgs,k)=dwk(idom(1:afield(l)%region%nlgs,l))
                    jj=jj+afield(l)%region%nlgs*iinc
                 ELSE
                    READ (UNIT=iin,FMT=*,ERR=1,END=2)
                 END IF
                 prog=prog+dprog
              END DO
              ctag0=ctag1
           ELSE
              jj=0
              IF (l>1) jj=jj+SUM(afield(1:l-1)%region%nlts*afield(1:l-1)%region%nlgs)
              v(jj+1:jj+afield(l)%region%nlts*afield(l)%region%nlgs,k)=afield(l)%rmiss
           END IF
        END DO
        nread=nread+1
     END DO
  END DO
!
! No errors
  ifail=0
  RETURN
!
! Error reading file
1 ifail=1
  GOTO 3
!
! End of file
2 ifail=2
!
! Get latest date
3 IF (ctag0(1:4)/='None') THEN
     CALL get_date ('T',ctag0,period0%sdate,ifail, &
          edate=period0%edate)
  END IF
!
  END SUBROUTINE read_grid_v10_stack
 END SUBROUTINE read_grid
!
!
!
 SUBROUTINE read_stns (afile,afield,n,idom,ka,v,ifail)
!
! Selects appropriate reading routine based on file format
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
  USE analysis,     ONLY: prog,dprog
  USE arrays,       ONLY: dwk
  USE errors,       ONLY: error
  USE IO_constants, ONLY: iin,lprd
  USE iofiles,      ONLY: open_infile
  USE labels,       ONLY: cg_seqs_l
  USE maths,        ONLY: magnitude
!#ifndef ONLY_FORTRAN
!  USE FortranCMix,  ONLY: read_stns_v9_c_wrapper,read_stns_v10_c_wrapper
!#endif
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
  TYPE(ifile), INTENT(IN) :: afile ! - input file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: idom(:,:) ! - used gridpoints -
!
  LOGICAL, INTENT(IN) :: ka(:,:) ! - available cases flags -
!
  TYPE(field), INTENT(IN) :: afield(:) ! - fields -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: v(:,:) ! - data -
!
! Locals
!
! Local scalars
  INTEGER :: nread ! - number of records read from training period -
  INTEGER :: im    ! - order of magnitude -
!
  CHARACTER(LEN=  10) :: cfmt  ! - format statement -
  CHARACTER(LEN= 128) :: cprog ! - progress -
  CHARACTER(LEN=lprd) :: cdate ! - date -
!
  TYPE(period) :: period0 ! - date of last successfully read data -
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
     CALL error ('open_infile',ifail, &
          c_arg1=TRIM(afile%ffile))
     RETURN
  END IF
!
! Read station data
  period0=0
  SELECT CASE (afile%ffmt%iver)
   CASE (9)
     CALL read_stns_v9 (afile,afield,n,idom,ka,v,nread,period0,ifail)
   CASE (10)
     CALL read_nongrid_v10 (afile,afield,n,idom,ka,v,nread,period0,ifail)
  END SELECT
  CLOSE (iin)
  IF (ifail/=0) THEN
     IF (period0%sdate%iyr>0) THEN
        cdate=get_cdate(period0,2)
        IF (nread>0) THEN
           im=magnitude(nread)
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(3A,I',im,',3A)'
           WRITE (UNIT=cprog,FMT=cfmt) &
              'Data up to ',TRIM(cdate),' (',nread,' ',TRIM(cg_seqs_l(afile%iseq)),' of training period) read successfully.'
        ELSE
           WRITE (UNIT=cprog,FMT='(3A)') &
              'Data up to ',TRIM(cdate),' read successfully.'
        END IF
        CALL error ('read_stns',ifail, &
             i_arg1=nread,c_arg1=TRIM(afile%ffile),c_arg2=cprog)
     ELSE
        CALL error ('read_stns',ifail, &
             c_arg1=TRIM(afile%ffile))
     END IF
  END IF
!
  RETURN
!
 CONTAINS
!
!
  SUBROUTINE read_stns_v9 (afile,afield,n,idom,ka,v,nread,period0,ifail)
!
! Reads formatted station data
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
  USE IO_constants, ONLY: lprd,ltag
  USE numbers,      ONLY: digits
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
  TYPE(ifile), INTENT(IN) :: afile ! - input file -
!
! Input/output scalars
  TYPE(period), INTENT(INOUT) :: period0 ! - last successfully read period -
!
! Output scalars
  INTEGER, INTENT(OUT) :: nread ! - number of cases read successfully -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: idom(:,:) ! - used gridpoints -
!
  LOGICAL, INTENT(IN) :: ka(:,:) ! - available cases flags -
!
  TYPE(field), INTENT(IN) :: afield(:) ! - fields -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: v(:,:) ! - data -
!
! Locals
!
! Local scalars
  INTEGER :: i      ! - station index -
  INTEGER :: k      ! - time index -
  INTEGER :: i1     ! - locator -
  INTEGER :: irskip ! - number of records to skip -
!
  CHARACTER(LEN=ltag) :: ctag0 ! - previous tag line -
  CHARACTER(LEN=ltag) :: ctag1 ! - current tag line -
  CHARACTER(LEN=lmax) :: cline ! - line -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ADJUSTL
  INTRINSIC TRIM
  INTRINSIC VERIFY
!
! Executable Statements
!
! Read station labels and coordinates
  nread=0
  ctag0='None'
  READ (UNIT=iin,FMT=*,ERR=1,END=2)
  prog=prog+dprog
  READ (UNIT=iin,FMT=*,ERR=1,END=2)
  prog=prog+dprog
  READ (UNIT=iin,FMT=*,ERR=1,END=2)
  prog=prog+dprog
!
! Skip first few records if required
  irskip=afile%it1-1
  IF (irskip>0) THEN
     DO k=1,irskip
        IF (.NOT.ka(k,1)) CYCLE
        READ (UNIT=iin,FMT='(A)',ERR=1,END=2) ctag1
        ctag0=ctag1
        prog=prog+dprog
     END DO
  END IF
!
! Read data
  IF (irskip==afile%nt) THEN !FIXME: This should not happen.
     IF (ctag0(1:4)/='None') THEN
        CALL get_date (' ',ctag0,period0%sdate,ifail, &
                       edate=period0%edate)
     END IF
     RETURN
  END IF
  DO k=1,n
     IF (ka(irskip+k,1)) THEN
        READ (UNIT=iin,FMT='(A)',ERR=1,END=2) cline
        cline=ADJUSTL(cline)
        i1=VERIFY(cline,digits//'/-T:')
        READ (UNIT=cline(i1:),FMT=*,ERR=1) (dwk(i),i=1,afield(1)%nlt)
        nread=nread+1
        DO i=1,afield(1)%region%nlts
           v(i,k)=dwk(idom(i,1))
        END DO
        ctag0=cline(1:lprd)
        prog=prog+dprog
     ELSE
        v(1:afield(1)%region%nlts,k)=afield(1)%rmiss
     END IF
  END DO
!
! No errors
  ifail=0
  RETURN
!
! Error reading file
1 ifail=1
  GOTO 3
!
! End of file
2 ifail=2
!
! Get latest date
3 IF (ctag0(1:4)/='None') THEN
     CALL get_date (' ',TRIM(ctag0),period0%sdate,ifail, &
          edate=period0%edate)
  END IF
!
  END SUBROUTINE read_stns_v9
 END SUBROUTINE read_stns
!
!
!
 SUBROUTINE read_unrf (afile,afield,n,idom,ka,v,ifail)
!
! Selects appropriate reading routine based on file format
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
  USE analysis,     ONLY: prog,dprog
  USE errors,       ONLY: error
  USE IO_constants, ONLY: iin,lprd
  USE iofiles,      ONLY: open_infile
  USE labels,       ONLY: cg_seqs_l
  USE maths,        ONLY: magnitude
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
  TYPE(ifile), INTENT(IN) :: afile ! - input file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: idom(:,:) ! - used gridpoints -
!
  LOGICAL, INTENT(IN) :: ka(:,:) ! - available cases flags -
!
  TYPE(field), INTENT(IN) :: afield(:) ! - fields -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: v(:,:) ! - data -
!
! Locals
!
! Local scalars
  INTEGER :: nread ! - number of records read from training period -
  INTEGER :: im    ! - order of magnitude -
!
  CHARACTER(LEN=  10) :: cfmt  ! - format statement -
  CHARACTER(LEN= 128) :: cprog ! - progress -
  CHARACTER(LEN=lprd) :: cdate ! - date -
!
  TYPE(period) :: period0 ! - date of last successfully read data -
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
     CALL error ('open_infile',ifail, &
          c_arg1=TRIM(afile%ffile))
     RETURN
  END IF
!
! Read ungridded data
  period0=0
  SELECT CASE (afile%ffmt%iver)
   CASE (9)
     CALL read_unrf_v9 (afile,afield(1)%nv,n,afield(1)%rmiss,ka,v,nread,period0,ifail)
   CASE (10)
     CALL read_nongrid_v10 (afile,afield,n,idom,ka,v,nread,period0,ifail)
  END SELECT
  CLOSE (iin)
  IF (ifail/=0) THEN
     IF (period0%sdate%iyr>0) THEN
        cdate=get_cdate(period0,2)
        IF (nread>0) THEN
           im=magnitude(nread)
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(3A,I',im,',3A)'
           WRITE (UNIT=cprog,FMT=cfmt) &
              'Data up to ',TRIM(cdate),' (',nread,' ',TRIM(cg_seqs_l(afile%iseq)),' of training period) read successfully.'
        ELSE
           WRITE (UNIT=cprog,FMT='(3A)') &
              'Data up to ',TRIM(cdate),' read successfully.'
        END IF
        CALL error ('read_unrf',ifail, &
             i_arg1=nread,c_arg1=TRIM(afile%ffile),c_arg2=cprog)
     ELSE
        CALL error ('read_unrf',ifail, &
             c_arg1=TRIM(afile%ffile))
     END IF
  END IF
!
  RETURN
!
 CONTAINS
!
!
  SUBROUTINE read_unrf_v9 (afile,nv,n,rmiss,ka,v,nread,period0,ifail)
!
! Reads formatted unreferenced data
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
  USE IO_constants, ONLY: lprd,ltag
  USE numbers,      ONLY: digits
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nv ! - number of variables -
  INTEGER, INTENT(IN) :: n  ! - number of cases -
!
  REAL(KIND=rp), INTENT(IN) :: rmiss ! - missing values -
!
  TYPE(ifile), INTENT(IN) :: afile ! - input file -
!
! Input/output scalars
  TYPE(period), INTENT(INOUT) :: period0 ! - last successfully read period -
!
! Output scalars
  INTEGER, INTENT(OUT) :: nread ! - number of cases read successfully -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  LOGICAL, INTENT(IN) :: ka(:,:) ! - available cases flags -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: v(:,:) ! - data -
!
! Locals
!
! Local scalars
  INTEGER :: i      ! - station index -
  INTEGER :: k      ! - time index -
  INTEGER :: i1     ! - locator -
  INTEGER :: irskip ! - number of records to skip -
!
  CHARACTER(LEN=ltag) :: ctag0 ! - previous tag line -
  CHARACTER(LEN=ltag) :: ctag1 ! - current tag line -
  CHARACTER(LEN=lmax) :: cline ! - line -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ADJUSTL
  INTRINSIC TRIM
  INTRINSIC VERIFY
!
! Executable Statements
!
! Read variable names
  nread=0
  ctag0='None'
  READ (UNIT=iin,FMT=*,ERR=1,END=2)
  prog=prog+dprog
!
! Skip first few records if required
  irskip=afile%it1-1
  IF (irskip>0) THEN
     DO k=1,irskip
        IF (.NOT.ka(k,1)) CYCLE
        READ (UNIT=iin,FMT='(A)',ERR=1,END=2) ctag1
        ctag0=ctag1
        prog=prog+dprog
     END DO
  END IF
!
! Read data
  IF (irskip==afile%nt) THEN
     IF (ctag0(1:4)/='None') THEN
        CALL get_date (' ',ctag0,period0%sdate,ifail, &
                       edate=period0%edate)
     END IF
     RETURN
  END IF
  DO k=1,n
     IF (ka(k,1)) THEN
        READ (UNIT=iin,FMT='(A)',ERR=1,END=2) cline
        cline=ADJUSTL(cline)
        i1=VERIFY(cline,digits//'/-T:')
        READ (UNIT=cline(i1:),FMT=*,ERR=1) (v(i,k),i=1,nv)
        ctag0=cline(1:lprd)
        nread=nread+1
        prog=prog+dprog
     ELSE
        v(1:nv,k)=rmiss
     END IF
  END DO
!
! No errors
  ifail=0
  RETURN
!
! Error reading file
1 ifail=1
  GOTO 3
!
! End of file
2 ifail=2
!
! Get latest date
3 IF (ctag0(1:4)/='None') THEN
     CALL get_date (' ',TRIM(ctag0),period0%sdate,ifail, &
          edate=period0%edate)
  END IF
!
  END SUBROUTINE read_unrf_v9
 END SUBROUTINE read_unrf
!
!
!
 SUBROUTINE read_nongrid_v10 (afile,afield,n,idom,ka,v,nread,period0,ifail)
!
! Reads formatted station or unreferenced data
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Problem reading file
!    ifail =  2 Premature end of file
!
! Modules
  USE analysis,     ONLY: prog,dprog
  USE arrays,       ONLY: dwk
  USE IO_constants, ONLY: iin,lprd,ltag
  USE numbers,      ONLY: digits
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
  TYPE(ifile), INTENT(IN) :: afile ! - input file -
!
! Input/output scalars
  TYPE(period), INTENT(INOUT) :: period0 ! - last successfully read period -
!
! Output scalars
  INTEGER, INTENT(OUT) :: nread ! - number of cases read successfully -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: idom(:,:) ! - used gridpoints -
!
  LOGICAL, INTENT(IN) :: ka(:,:) ! - available cases flags -
!
  TYPE(field), INTENT(IN) :: afield(:) ! - fields -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: v(:,:)     ! - data -
!
! Locals
!
! Local scalars
  INTEGER :: i      ! - station/variable index -
  INTEGER :: k      ! - time index -
  INTEGER :: l      ! - field / lagged field index -
  INTEGER :: ifd    ! - field index -
  INTEGER :: ilf    ! - lagged field index -
  INTEGER :: i1     ! - locator -
  INTEGER :: ij     ! - available station index -
  INTEGER :: irskip ! - number of records to skip -
!
  CHARACTER(LEN=ltag) :: ctag0 ! - previous tag line -
  CHARACTER(LEN=ltag) :: ctag1 ! - current tag line -
  CHARACTER(LEN=lmax) :: cline ! - line -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ADJUSTL
  INTRINSIC INDEX
  INTRINSIC SUM
  INTRINSIC VERIFY
!
! Executable Statements
!
! Read v10 XML namespace headers and tags
  xml: DO
     READ (UNIT=iin,FMT='(A)',ERR=1,END=2) ctag1
     IF (INDEX(ctag1,'cpt:field=')>0) THEN
        EXIT xml
     ELSE
        CYCLE xml
     END IF
  END DO xml
!
! Skip station/index names and tags
  nread=0
  ctag0='None'
  irskip=afile%it1-1
  DO ifd=1,afile%nfs
     l=(ifd-1)*afile%nls+1
     IF (ifd==1) THEN
        DO k=1,afile%ntag
           READ (UNIT=iin,FMT=*,ERR=1,END=2)
        END DO
     ELSE
        DO k=1,afile%ntag+1
           READ (UNIT=iin,FMT=*,ERR=1,END=2)
        END DO
     END IF
     prog=prog+dprog
!
! Skip first few records if required
     IF (irskip>0) THEN
        DO k=1,irskip
           DO ilf=1,afile%nls
              l=(ifd-1)*afile%nls+ilf
              IF (.NOT.ka(k,l)) CYCLE
              READ (UNIT=iin,FMT='(A)',ERR=1,END=2) ctag1
              ctag0=ctag1
              prog=prog+dprog
           END DO
        END DO
     END IF
!
! Read data
  IF (irskip==afile%nt) THEN
     IF (ctag0(1:4)/='None') THEN
        CALL get_date (' ',TRIM(ctag0),period0%sdate,ifail, &
                       edate=period0%edate)
     END IF
     RETURN
  END IF
     nread=0
     DO k=1,n
        DO ilf=1,afile%nls
           l=(ifd-1)*afile%nls+ilf
           IF (l>1) THEN
              ij=SUM(afield(1:l-1)%region%nlts)
           ELSE
              ij=0
           END IF
           IF (ka(irskip+k,l)) THEN
              READ (UNIT=iin,FMT='(A)',ERR=1,END=2) cline
              cline=ADJUSTL(cline)
              i1=VERIFY(cline,digits//'/-T:')
              READ (UNIT=cline(i1:),FMT=*,ERR=1) (dwk(i),i=1,afield(l)%nlt)
!
! Extract stations within domain
              DO i=1,afield(l)%region%nlts
                 v(ij+i,k)=dwk(idom(i,l))
              END DO
              ctag0=cline(1:lprd)
              prog=prog+dprog
           ELSE
              v(ij+1:ij+afield(l)%region%nlts,k)=afield(l)%rmiss
           END IF
        END DO
        nread=nread+1
     END DO
  END DO
!
! No errors
  ifail=0
  RETURN
!
! Error reading file
1 ifail=1
  GOTO 3
!
! End of file
2 ifail=2
!
! Get latest date
3 IF (ctag0(1:4)/='None') THEN
     CALL get_date (' ',TRIM(ctag0),period0%sdate,ifail, &
          edate=period0%edate)
  END IF
!
 END SUBROUTINE read_nongrid_v10
END MODULE data_input
