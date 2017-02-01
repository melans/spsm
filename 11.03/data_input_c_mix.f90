! $Id: data_input_c_mix.f90 1215 2011-02-25 21:30:20Z simon $
FUNCTION copy_c_line(one_line,line_len)
!
! Implicit declarations
  IMPLICIT NONE

! IN/OUT parameters
  CHARACTER(LEN=1), DIMENSION(line_len), INTENT(OUT) :: one_line     ! - testing ctags -
  INTEGER ,INTENT(IN) :: line_len                         ! - length of one_line -
!
! Local scalars
!
! Function type
  INTEGER :: copy_c_line
!
! External C functions
  EXTERNAL copy_c_line_c
     CALL copy_c_line_c(one_line,line_len)

     copy_c_line=0
     RETURN ;
END FUNCTION copy_c_line
!
!
!
SUBROUTINE get_fileinfov10_from_f(xyz,dtype,nlt,nlg,&
                                  firstYear,firstMonth,firstDay,&
                                  endYear,endMonth,endDay,totalYears,&
                                  totalVariables,totalUsedVariables,&
                                  minLat,maxLat,minLon,maxLon) BIND(C)
!
! Modules
  USE, INTRINSIC :: ISO_C_BINDING
  USE iofiles,  ONLY: xfile,yfile,zfile
  USE fields,   ONLY: xfield,rlatx,rlngx,&
                      yfield,rlaty,rlngy,&
                      zfield,rlatz,rlngz
  USE settings, ONLY : nx,ny,nz,mxa,mya,mza
!
! IMPLICIT NONE
  IMPLICIT NONE
!
!
! Arguments
!
! Input scalars
  INTEGER(KIND=C_INT), INTENT(IN) :: xyz ! - 1->'X' file, 2->'Y' file, 3->'Z' -
!
! Output scalars
  INTEGER(KIND=C_INT), INTENT(OUT) :: dtype ! - 
  INTEGER(KIND=C_INT), INTENT(OUT) :: firstYear ! - 
  INTEGER(KIND=C_INT), INTENT(OUT) :: firstMonth ! - 
  INTEGER(KIND=C_INT), INTENT(OUT) :: firstDay ! - 
  INTEGER(KIND=C_INT), INTENT(OUT) :: endYear ! - 
  INTEGER(KIND=C_INT), INTENT(OUT) :: endMonth ! - 
  INTEGER(KIND=C_INT), INTENT(OUT) :: endDay ! - 
  INTEGER(KIND=C_INT), INTENT(OUT) :: totalYears ! - 
  INTEGER(KIND=C_INT), INTENT(OUT) :: totalVariables ! - 
  INTEGER(KIND=C_INT), INTENT(OUT) :: totalUsedVariables ! - 
  INTEGER(KIND=C_INT), INTENT(OUT) :: nlt(xfile%nfs*xfile%nls) ! - 
  INTEGER(KIND=C_INT), INTENT(OUT) :: nlg(xfile%nfs*xfile%nls) ! -  - 
!
  REAL(KIND=C_DOUBLE), INTENT(OUT) :: minLat(xfile%nfs*xfile%nls) ! -   - 
  REAL(KIND=C_DOUBLE), INTENT(OUT) :: maxLat(xfile%nfs*xfile%nls) ! -   - 
  REAL(KIND=C_DOUBLE), INTENT(OUT) :: minLon(xfile%nfs*xfile%nls) ! -   - 
  REAL(KIND=C_DOUBLE), INTENT(OUT) :: maxLon(xfile%nfs*xfile%nls) ! -   - 

!
! Local variables
  INTEGER :: i ! - index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MAXVAL
  INTRINSIC MINVAL

  totalVariables=0
  totalUsedVariables=0

  IF ( xyz == 1) THEN 
      dtype = xfile%igrid
      firstYear=xfile%period1%sdate%iyr
      firstMonth=xfile%period1%sdate%imn
      firstDay =xfile%period1%sdate%idy
      endYear=xfile%periodn%edate%iyr
      endMonth=xfile%periodn%edate%imn
      endDay =xfile%periodn%edate%idy
      totalYears=xfile%nt ! number of time steps FIXME
      DO i=1,xfile%nfs*xfile%nls
         nlt = xfield(i)%nlt
         nlg = xfield(i)%nlg
         totalVariables=totalVariables+xfield(i)%nlt*xfield(i)%nlg
         minLat(i)=MINVAL(rlatx(1:xfield(i)%nlt,i))
         maxLat(i)=MAXVAL(rlatx(1:xfield(i)%nlt,i))
         minLon(i)=MINVAL(rlngx(1:xfield(i)%nlg,i))
         maxLon(i)=MAXVAL(rlngx(1:xfield(i)%nlg,i))
      END DO
  ELSE IF ( xyz == 2) THEN 
      dtype=yfile%igrid
      firstYear=yfile%period1%sdate%iyr
      firstMonth=yfile%period1%sdate%imn
      firstDay =yfile%period1%sdate%idy
      endYear=yfile%periodn%edate%iyr
      endMonth=yfile%periodn%edate%imn
      endDay =yfile%periodn%edate%idy
      totalYears=yfile%nt ! number of time steps FIXME
      DO i=1,yfile%nfs*yfile%nls
         nlt = yfield(i)%nlt
         nlg = yfield(i)%nlg
         totalVariables=totalVariables+yfield(i)%nlt*yfield(i)%nlg
         minLat(i)=MINVAL(rlaty(1:yfield(i)%nlt,i))
         maxLat(i)=MAXVAL(rlaty(1:yfield(i)%nlt,i))
         minLon(i)=MINVAL(rlngy(1:yfield(i)%nlg,i))
         maxLon(i)=MAXVAL(rlngy(1:yfield(i)%nlg,i))
      END DO
  ELSE IF ( xyz == 3) THEN
      dtype=zfile%igrid
      firstYear=zfile%period1%sdate%iyr
      firstMonth=zfile%period1%sdate%imn
      firstDay =zfile%period1%sdate%idy
      endYear=zfile%periodn%edate%iyr
      endMonth=zfile%periodn%edate%imn
      endDay =zfile%periodn%edate%idy
      totalYears=zfile%nt ! number of time steps FIXME
      DO i=1,yfile%nfs*yfile%nls
         nlt = zfield(i)%nlt
         nlg = zfield(i)%nlg
         totalVariables=totalVariables+zfield(i)%nlt*zfield(i)%nlg
         minLat(i)=MINVAL(rlatz(1:zfield(i)%nlt,i))
         maxLat(i)=MAXVAL(rlatz(1:zfield(i)%nlt,i))
         minLon(i)=MINVAL(rlngz(1:zfield(i)%nlg,i))
         maxLon(i)=MAXVAL(rlngz(1:zfield(i)%nlg,i))
      END DO
  END IF

  RETURN
END SUBROUTINE get_fileinfov10_from_f
!
!
!
SUBROUTINE get_structure_v10_c_wrapper(xyz,fname,lenfname,vmiss,&
                    numFields,numLags,ifail,cfail,cfailen) BIND(C)
!
! Modules
  USE, INTRINSIC :: ISO_C_BINDING
  USE IO_constants,   ONLY: ldat,lprd
  USE iofiles,        ONLY: xfile,yfile,zfile
  USE fields,         ONLY: xfield,rlatx,rlngx,rrlatx,rrlngx,cstnx, &
                            yfield,rlaty,rlngy,rrlaty,rrlngy,cstny, &
                            zfield,rlatz,rlngz,rrlatz,rrlngz,cstnz
  USE FortranCMix,   ONLY: get_structure_v10_c_mix
!
! IMPLICIT NONE
  IMPLICIT NONE
!
!
! Arguments
!
! Input scalars
  INTEGER(KIND=C_INT), INTENT(IN) :: xyz ! ! - 1->'X' file, 2->'Y' file, 3->'Z' -
  INTEGER(KIND=C_INT), INTENT(IN) :: lenfname ! - length of file name -
  INTEGER(KIND=C_INT), INTENT(IN) :: cfailen ! - length of cfail -

  CHARACTER(KIND=C_CHAR), INTENT(IN) :: fname(lenfname) ! - file name -
!
! Input/Output scalar
  REAL(KIND=C_DOUBLE), INTENT(INOUT) :: vmiss ! - missing value flag -
!
! Output scalars
  INTEGER(KIND=C_INT), INTENT(OUT) :: numFields ! - 
  INTEGER(KIND=C_INT), INTENT(OUT) :: numLags ! - 
  INTEGER(KIND=C_INT), INTENT(OUT) :: ifail ! - error indicator -

  CHARACTER(KIND=C_CHAR),INTENT(OUT) :: cfail(cfailen) ! - error message -

! Locals
!
! Local scalars
  INTEGER :: i    ! - latitude index -
  INTEGER :: ifd   ! - current field -
  INTEGER :: ilf   ! - current lagged field -

  CHARACTER(LEN=lprd) :: mycfail ! - problem tag -

  mycfail=' '

  IF ( xyz == 1) THEN 
     DO i=1,lenfname
        xfile%ffile(i:i)=fname(i)
     END DO
     CALL get_structure_v10_c_mix (xfile,xfield,rlatx,rlngx,rrlatx,rrlngx,cstnx,&
                                   ifd,ilf,ifail,mycfail,vmiss)
     numFields=xfile%nfs
     numLags=xfile%nls
  ELSE IF (xyz == 2) THEN 
     DO i=1,lenfname
        yfile%ffile(i:i)=fname(i)
     END DO
     CALL get_structure_v10_c_mix (yfile,yfield,rlaty,rlngy,rrlaty,rrlngy,cstny,&
                                   ifd,ilf,ifail,mycfail,vmiss)
     numFields=yfile%nfs
     numLags=yfile%nls
  ELSE IF (xyz == 3) THEN 
     DO i=1,lenfname
        zfile%ffile(i:i)=fname(i)
     END DO
     CALL get_structure_v10_c_mix (zfile,zfield,rlatz,rlngz,rrlatz,rrlngz,cstnz,&
                                   ifd,ilf,ifail,mycfail,vmiss)
     numFields=zfile%nfs
     numLags=zfile%nls
  END IF
  DO i=1,cfailen
     cfail(i) = mycfail(i:i)
  END DO
  RETURN
END SUBROUTINE get_structure_v10_c_wrapper
!
!
!
SUBROUTINE get_tags_c_wrapper ( ctag,ntags,ctags_in,&
            afield_period1_sdate_iyr, afield_period1_sdate_imn, afield_period1_sdate_idy,&
            afield_period1_edate_iyr, afield_period1_edate_imn, afield_period1_edate_idy,&
            afield_mdate_iyr, afield_mdate_imn, afield_mdate_idy,&
            afield_nlt, afield_nlg, &
            ifail,igrid,nt, afield_var, afield_unit, cfail&
           )
!
! Reads CPT file tag values
!
! On exit:
!    ifail =  0 Successful
!    ifail = -1 Invalid combination of tags
!    ifail = -2 Missing optional argument nt for nrow and/or ncol tags
!    ifail =  1 Unable to read cfail tag
!    ifail =  2 Unable to find cfail tag (if tag is a required field, other wise ifail=0 and default value is specified)
!    ifail =  3 Invalid value for cfail tag
!    ifail =  4 Value for crow is icompatible with ccol='X'
!    ifail =  5 Value for crow is icompatible with ccol='station'
!    ifail =  6 Value for crow is icompatible with ccol='index'
!    ifail =  7 Unknown value for ccol
!
! Modules
  USE IO_constants,   ONLY: ldat,lvar
  USE time,           ONLY: get_date
  USE fields,         ONLY: field 
  USE get_input_file, ONLY: get_tags
  USE numbers,        ONLY: rp
!
! Implicit declarations
  IMPLICIT NONE
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: ntags ! - number of desired tags -
!
  CHARACTER(LEN=800), INTENT(IN) :: ctag ! - tag line -
!
! Input/output scalars
! - optional input/output scalars -
  INTEGER, INTENT(INOUT), OPTIONAL :: nt   ! - number of time steps -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
! - optional output scalars -
  INTEGER, INTENT(OUT), OPTIONAL :: igrid ! - dataset structure indicator -
!
  CHARACTER(LEN=1),   INTENT(OUT) :: cfail(ldat)       ! - problem tag -
  CHARACTER(LEN=1),   INTENT(OUT) :: afield_var(lvar)
  CHARACTER(LEN=1),   INTENT(OUT) :: afield_unit(lvar)
!
  INTEGER, INTENT(OUT) :: afield_period1_sdate_iyr      ! - field -
  INTEGER, INTENT(OUT) :: afield_period1_sdate_imn      ! - field -
  INTEGER, INTENT(OUT) :: afield_period1_sdate_idy      ! - field -
  INTEGER, INTENT(OUT) :: afield_period1_edate_iyr      ! - field -
  INTEGER, INTENT(OUT) :: afield_period1_edate_imn      ! - field -
  INTEGER, INTENT(OUT) :: afield_period1_edate_idy      ! - field -
  INTEGER, INTENT(OUT) :: afield_mdate_iyr              ! - field -
  INTEGER, INTENT(OUT) :: afield_mdate_imn ! - field -
  INTEGER, INTENT(OUT) :: afield_mdate_idy ! - field -
  INTEGER, INTENT(OUT) :: afield_nlt
  INTEGER, INTENT(OUT) :: afield_nlg
!
! Local Scalars
  TYPE(field) :: afield ! - field -
!
! Input arrays
  CHARACTER(LEN=800), INTENT(IN) :: ctags_in ! - desired tags (A C String which is ',' seperated and terminated.)
! 
! Local scalars
  CHARACTER(LEN=16), ALLOCATABLE :: ctags(:) ! - desired tags -
  INTEGER ::           i,i1,i2
  CHARACTER(LEN=ldat) :: cfail_local ! - problem tag -
!
! Intrinsic functions
  INTRINSIC INDEX
  INTRINSIC REPEAT
!
  ALLOCATE(ctags(ntags))

  i1=0;
  DO i=1,ntags
  i2=INDEX(ctags_in((i1+1):),',')
  READ(UNIT=ctags_in((i1+1):(i1+i2-1)), FMT=* ) ctags(i)
  i1=i2
  END DO

  IF( nt.eq.-99 ) THEN
      IF ( igrid .eq. -99 ) THEN
          CALL get_tags (ctag, ntags, ctags, afield, ifail, cfail_local,igrid=igrid,nt=nt)
      ELSE
          CALL get_tags (ctag, ntags, ctags, afield, ifail, cfail_local, nt=nt)
      END IF
  ELSE
      IF ( igrid .eq. -99 ) THEN
          CALL get_tags (ctag, ntags, ctags, afield, ifail, cfail_local, igrid=igrid)
      ELSE
          CALL get_tags (ctag, ntags, ctags, afield, ifail, cfail_local )
      END IF
  END IF

  afield_period1_sdate_iyr = afield%tdate%sdate%iyr
  afield_period1_sdate_imn = afield%tdate%sdate%imn
  afield_period1_sdate_idy = afield%tdate%sdate%idy
  afield_period1_edate_iyr = afield%tdate%edate%iyr
  afield_period1_edate_imn = afield%tdate%edate%imn
  afield_period1_edate_idy = afield%tdate%edate%idy
  afield_mdate_iyr = afield%mdate%iyr
  afield_mdate_imn = afield%mdate%imn
  afield_mdate_idy = afield%mdate%idy
  afield_nlt   = afield%nlt
  afield_nlg   = afield%nlg

  DEALLOCATE(ctags)
  
  DO i=1,lvar
     afield_var(i) = afield%var(i:i)
     afield_unit(i) = afield%unit(i:i)
  END DO
  DO i=1,ldat
     cfail(i) = cfail_local(i:i)
  END DO

END SUBROUTINE get_tags_c_wrapper
!
!
!
SUBROUTINE read_grid_v10_c_wrapper (afile,afield,n,v,rlat,rlng,nread,period0,ifail,cfail)
!
! Reads formatted gridded data
!
! Modules
  USE analysis,      ONLY: prog,dprog
  USE arrays,        ONLY: dwk
  USE CPT_constants, ONLY: mnt
  USE IO_constants,  ONLY: iin,ltag,ldat
  USE fields,        ONLY: field,domain
  USE iofiles,       ONLY: ifile
  USE numbers,       ONLY: rp
  USE time,          ONLY: period,date
!
! Implicit declarations
  IMPLICIT NONE
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: nread ! - number of cases read successfully -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
  CHARACTER(LEN=ldat), INTENT(OUT) :: cfail ! - problem tag -
!
  TYPE(period), INTENT(OUT) :: period0 ! - last successfully read period -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: rlat(:,:) ! - latitudes -
  REAL(KIND=rp), INTENT(OUT) :: rlng(:,:) ! - longitudes -
  REAL(KIND=rp), INTENT(OUT) :: v(:,:)    ! - data -
!
! Pointer arrays
  TYPE(field), POINTER :: afield(:) ! - fields -
!
! Local Scalars
  INTEGER, ALLOCATABLE       :: len_var(:);
  INTEGER, ALLOCATABLE       :: len_unit(:);
  INTEGER                    :: totalVar;
  INTEGER                    :: totalUnit;
  INTEGER                    :: i;
  CHARACTER(LEN=32), ALLOCATABLE :: vars(:)
  CHARACTER(LEN=32), ALLOCATABLE :: units(:)

! C function
  EXTERNAL read_grid_v10_c
!
! Intrinsic functions
  INTRINSIC TRIM
  INTRINSIC LEN_TRIM

  totalVar  = SIZE(afield(:))
  totalUnit = SIZE(afield(:))

  ALLOCATE( len_var(totalVar) )
  ALLOCATE( len_unit(totalUnit) )
  ALLOCATE( vars(totalUnit) )
  ALLOCATE( units(totalUnit) )

  DO i=1,totalVar
      len_var(i) = LEN_TRIM(afield(i)%var);
      vars(i)    = afield(i)%var
  END DO

  DO i=1,totalUnit
      len_unit(i) = LEN_TRIM(afield(i)%unit);
      units(i)    = afield(i)%unit
  END DO

  CALL read_grid_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
       n, v, rlat, rlng, nread, afile%it1, afile%nfs, afile%nls,&
       afile%period1%sdate%iyr, afile%period1%sdate%imn, afile%period1%sdate%idy, &
       afile%period1%edate%iyr, afile%period1%edate%imn, afile%period1%edate%idy,&
       vars, len_var, units, len_unit,&
       period0%sdate%iyr, period0%sdate%imn, period0%sdate%idy,&
       period0%edate%iyr, period0%edate%imn, period0%edate%idy,&
       afile%nt, afield(:)%nlt, afield(:)%nlg, afield(:)%region%nlts, afield(:)%region%nlgs, &
       afield(:)%region%nlt1, afield(:)%region%nlt2, afield(:)%region%nlg1, afield(:)%region%nlg2,&
       prog, dprog, afile%iseq, ifail, cfail, LEN_TRIM(cfail), afile%igeog & 
     )
  DEALLOCATE( len_var );
  DEALLOCATE( len_unit );
  DEALLOCATE( vars );
  DEALLOCATE( units );

END SUBROUTINE read_grid_v10_c_wrapper
!
!
!
SUBROUTINE read_grid_v9_c_wrapper (afile,afield,n,v,rlat,rlng,nread,date0,ifail)
!
! Reads formatted gridded data
!
! Modules
  USE analysis,       ONLY: prog,dprog
  USE arrays,         ONLY: dwk
  USE IO_constants,   ONLY: iin
  USE time_constants, ONLY: lmon
  USE fields,         ONLY: field,domain
  USE iofiles,        ONLY: ifile
  USE numbers,        ONLY: rp
  USE time,           ONLY: date,period
!
! Implicit declarations
  IMPLICIT NONE
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: nread ! - number of cases in training period read successfully -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
  TYPE(date), INTENT(OUT) :: date0 ! - last successfully read date -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: rlat(:) ! - latitudes -
  REAL(KIND=rp), INTENT(OUT) :: rlng(:) ! - longitudes -
  REAL(KIND=rp), INTENT(OUT) :: v(:,:)  ! - data -
!
! Pointer arrays
  TYPE(field), POINTER :: afield(:) ! - fields -
!
! C function
  EXTERNAL read_grid_v9_c
!
! Intrinsic functions
  INTRINSIC TRIM
  INTRINSIC LEN_TRIM
!
  CALL read_grid_v9_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile),  n, v, rlat, rlng, nread, afile%it1,&
                       date0%iyr,  date0%imn,  date0%idy, &
                       afield(1)%nlt, afield(1)%nlg, afield(1)%region%nlts, afield(1)%region%nlgs, &
                       afield(1)%region%nlt1, afield(1)%region%nlt2, afield(1)%region%nlg1, afield(1)%region%nlg2,&
                       prog, dprog, afile%iseq, ifail&
                  );

END SUBROUTINE read_grid_v9_c_wrapper
!
!
!
SUBROUTINE read_stns_v9_c_wrapper (afile,afield,n,v,rlat,rlng,rrlat,rrlng,cstn,nread,iyr0,ifail)
!
! Reads formatted station data
!
! Modules
  USE analysis,     ONLY: prog,dprog
  USE arrays,       ONLY: dwk
  USE gui,          ONLY: upcase
  USE IO_constants, ONLY: iin,lstn
  USE numbers,      ONLY: rp,r360
  USE fields,       ONLY: field,domain
  USE iofiles,      ONLY: ifile
!
! Implicit declarations
  IMPLICIT NONE
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: nread ! - number of cases read successfully -
  INTEGER, INTENT(OUT) :: iyr0  ! - last successfully read year -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: rlat(:)  ! - latitudes -
  REAL(KIND=rp), INTENT(OUT) :: rlng(:)  ! - longitudes -
  REAL(KIND=rp), INTENT(OUT) :: rrlat(:) ! - used latitudes -
  REAL(KIND=rp), INTENT(OUT) :: rrlng(:) ! - used longitudes -
  REAL(KIND=rp), INTENT(OUT) :: v(:,:)   ! - data -
!
  CHARACTER(LEN=lstn), INTENT(OUT) :: cstn(:) ! - station labels -
!
! Pointer arrays
  TYPE(field), POINTER :: afield(:) ! - fields -
!
! C function
  EXTERNAL read_stns_v9_c
!
! Intrinsic functions
  INTRINSIC TRIM
  INTRINSIC LEN_TRIM
!
  CALL read_stns_v9_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                       afield(1)%nlt, n, afile%it1,&
                       afield(1)%region%alim%rltn,afield(1)%region%alim%rlts, &
                       afield(1)%region%alim%rlgw,afield(1)%region%alim%rlge, &
                       nread, iyr0, &
                       v, rlat, rlng, rrlat, rrlng, cstn, prog, dprog, ifail&
                  )
  RETURN
END SUBROUTINE read_stns_v9_c_wrapper
!
!
!
SUBROUTINE read_unrf_v9_c_wrapper (afile,nv,n,v,cstn,nread,iyr0,ifail)
!
! Reads formatted unreferenced data
!
! Modules
  USE analysis,     ONLY: prog,dprog
  USE IO_constants, ONLY: lstn
  USE iofiles,      ONLY: ifile
  USE numbers,      ONLY: rp
!
! Implicit declarations
  IMPLICIT NONE
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nv ! - number of variables -
  INTEGER, INTENT(IN) :: n  ! - number of cases -
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: nread ! - number of cases read successfully -
  INTEGER, INTENT(OUT) :: iyr0  ! - last successfully read year -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: v(:,:) ! - data -
!
  CHARACTER(LEN=lstn), INTENT(OUT) :: cstn(:) ! - variable labels -
!
! C function
  EXTERNAL read_unrf_v9_c
!
! Intrinsic functions
  INTRINSIC TRIM
  INTRINSIC LEN_TRIM

  CALL read_unrf_v9_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                       afile%it1, nv, n, v, &
                       cstn, nread, iyr0, ifail, prog, dprog &
                     )

END SUBROUTINE read_unrf_v9_c_wrapper

SUBROUTINE read_unrf_v10_c_wrapper (afile,nv,n,v,cstn,nread,period0,ifail)
!
! Reads formatted unreferenced data
!
! Modules
  USE analysis,     ONLY: prog,dprog
  USE IO_constants, ONLY: lstn
  USE numbers,      ONLY: rp
  USE iofiles,      ONLY: ifile
  USE time,         ONLY: period
!
  IMPLICIT NONE
!
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nv ! - number of series -
  INTEGER, INTENT(IN) :: n  ! - number of cases -
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: nread ! - number of cases read successfully -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
  TYPE(period), INTENT(OUT) :: period0 ! - last successfully read period -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: v(:,:) ! - data -
!
  CHARACTER(LEN=lstn), INTENT(OUT) :: cstn(:) ! - station labels -
!
! C function
  EXTERNAL read_unrf_v10_c
!
! Intrinsic functions
  INTRINSIC TRIM
  INTRINSIC LEN_TRIM

  CALL read_unrf_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                       afile%it1, afile%nfs, afile%nls, afile%ntag,&
                       nv, n, v, &
                       cstn, nread, afile%iseq,&
                       period0%sdate%iyr, period0%sdate%imn, period0%sdate%idy, &
                       period0%edate%iyr, period0%edate%imn, period0%edate%idy, &
                       ifail, prog, dprog &
                      )

END SUBROUTINE read_unrf_v10_c_wrapper

SUBROUTINE read_stns_v10_c_wrapper (afile,afield,n,v,rlat,rlng,rrlat,rrlng,cstn,nread,period0,ifail)
!
! Reads formatted station data
!
! Modules
  USE analysis,     ONLY: prog,dprog
  USE IO_constants, ONLY: lstn
  USE numbers,      ONLY: rp
  USE iofiles,      ONLY: ifile
  USE fields,       ONLY: field
  USE time,         ONLY: period
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n ! - number of cases -
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: nread ! - number of cases read successfully -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
  TYPE(period), INTENT(OUT) :: period0 ! - last successfully read period -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: rlat(:,:)  ! - latitudes -
  REAL(KIND=rp), INTENT(OUT) :: rlng(:,:)  ! - longitudes -
  REAL(KIND=rp), INTENT(OUT) :: rrlat(:,:) ! - used latitudes -
  REAL(KIND=rp), INTENT(OUT) :: rrlng(:,:) ! - used longitudes -
  REAL(KIND=rp), INTENT(OUT) :: v(:,:)   ! - data -
!
  CHARACTER(LEN=lstn), INTENT(OUT) :: cstn(:,:) ! - station labels -
!
! Pointer arrays
  TYPE(field), POINTER :: afield(:) ! - fields -
!
! C function
  EXTERNAL read_stns_v10_c
!
! Local varialbes
  CHARACTER(LEN=lstn), ALLOCATABLE :: cstn_loc(:) ! - station labels -
  INTEGER                          :: total_nlt   ! total station names in file
  INTEGER                          :: maxl        ! afield dimenension 
  INTEGER                          :: i           ! index 
  INTEGER                          :: j           ! index 

! Intrinsic functions
  INTRINSIC TRIM
  INTRINSIC LEN_TRIM

  total_nlt=0;
  maxl = SIZE(afield(:));
  DO i=1,maxl
     total_nlt = total_nlt+afield(i)%nlt;
  END DO
  
  ALLOCATE( cstn_loc(total_nlt) );

  CALL read_stns_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                        afile%nfs,afile%nls,afile%ntag,&
                        afile%it1,afile%nt,afield(:)%nlt,n,&
                        afield(:)%region%alim%rltn,afield(:)%region%alim%rlts, &
                        afield(:)%region%alim%rlgw,afield(:)%region%alim%rlge, &
                        afield(:)%nv, &
                        v, rlat, rlng, rrlat, rrlng, & 
                        cstn_loc, maxl, nread, &
                        period0%sdate%iyr, period0%sdate%imn, period0%sdate%idy,&
                        period0%edate%iyr, period0%edate%imn, period0%edate%idy,&
                        afile%iseq, ifail, prog, dprog &
                      )
  DO i=1,maxl
    DO j=1,afield(i)%nlt
       cstn(j,i) = cstn_loc((i-1)*maxl+j);
    END DO
  END DO

  DEALLOCATE(cstn_loc)

END SUBROUTINE read_stns_v10_c_wrapper
