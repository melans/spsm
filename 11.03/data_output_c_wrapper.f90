! $Id: data_output_c_wrapper.f90 1215 2011-02-25 21:30:20Z simon $
  SUBROUTINE write_grid_v10_c_wrapper (afile,afield,nfs,nes,nt,nv,v,iuse,kuse,miss,rlat,rlng,ifail, &
             it0)
!
! Outputs data in gridded format
!
! Modules
  USE fields,       ONLY: field
  USE IO_constants, ONLY: ffmts,faccs,cprcs
  USE iofiles,      ONLY: ofile
  USE time,         ONLY: date,period
  USE numbers,      ONLY: rp
!
  IMPLICIT NONE
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nfs  ! - number of fields -
  INTEGER, INTENT(IN) :: nes  ! - number of EOF extensions -
  INTEGER, INTENT(IN) :: nt   ! - number of cases -
  INTEGER, INTENT(IN) :: nv   ! - number of grids -
!
  REAL(KIND=rp), INTENT(IN) :: miss ! - missing value flag -
!
  TYPE(field), INTENT(IN) :: afield(:) ! - field -
!
! - optional input scalars -
  INTEGER, INTENT(IN), OPTIONAL :: it0 ! - time 0 -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: iuse(:) ! - used variables indices -
  REAL(KIND=rp), INTENT(IN) :: v(:,:)  ! - data -
  REAL(KIND=rp), INTENT(IN) :: rlat(:,:) ! - latitudes -
  REAL(KIND=rp), INTENT(IN) :: rlng(:,:) ! - longitudes -
!
  LOGICAL, INTENT(IN) :: kuse(:) ! - used cases flags -
!
  TYPE(ofile), INTENT(IN) :: afile ! - output file -
!
! Local variables
  INTEGER :: my_it0;
  INTEGER :: cpt_m;

  REAL(KIND=rp) :: cpt_z_hght

  CHARACTER(LEN=16) :: cpt_z_unit

! EXTERNAL C function
  EXTERNAL  write_grid_v10_c

! Functions
  INTRINSIC PRESENT
  INTRINSIC TRIM
  INTRINSIC LEN_TRIM

  IF(PRESENT(it0)) THEN
      my_it0=it0
      cpt_m=afield(1)%member
      cpt_z_hght=afield(1)%z%hght
      cpt_z_unit=afield(1)%z%unit
  ELSE
      my_it0=-99
      cpt_m =-99
      cpt_z_hght=-99.0
      cpt_z_unit=" "
  END IF

  !PRINT *, '-----call write_grid_v10_c_wrapper '
  !PRINT *, 'afile==>',afile
  !PRINT *, 'afield==>',afield
  !PRINT *, 'nt==>',nt
  !PRINT *, 'nv==>',nv
  !PRINT *, 'v==>',v
  !PRINT *, 'iuse==>',iuse
  !PRINT *, 'kuse==>',kuse
  !PRINT *, 'miss==>',miss
  !PRINT *, 'rlat==>',rlat
  !PRINT *, 'rlng==>',rlng

  CALL write_grid_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                      TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                      TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                      TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                      nt, nv, v,  iuse, kuse, miss, rlat, rlng,  SIZE(v,1), 0, &
                      afield(1)%region%nlts, afield(1)%region%nlgs, nfs, nes,&
                      TRIM(afield(1)%var), LEN_TRIM(afield(1)%var), &
                      TRIM(afield(1)%unit), LEN_TRIM(afield(1)%unit), my_it0, &
                      cpt_m,-99.0,"NULL",4,"NULL",4,"NULL",4,"Y",1,"X",1,cpt_z_hght,cpt_z_unit,16,&
                      afield(:)%tdate%sdate%iyr, afield(:)%tdate%sdate%imn, afield(:)%tdate%sdate%idy,&
                      afield(:)%tdate%edate%iyr, afield(:)%tdate%edate%imn, afield(:)%tdate%edate%idy,&
                      afield(:)%mdate%iyr,  afield(:)%mdate%imn,   afield(:)%mdate%idy,&
                      ifail&
                    );

  END SUBROUTINE write_grid_v10_c_wrapper
!
!
  SUBROUTINE write_grids_v10_c_wrapper (ctype,afile,afield,nfs,nes,it0,nt,n3,v,iuse,kuse,miss,rlat,rlng,ifail, &
             cl)
!
! Outputs data in gridded format
!
! Modules
  USE fields,       ONLY: field
  USE IO_constants, ONLY: ffmts,faccs,cprcs
  USE iofiles,      ONLY: ofile
  USE time,         ONLY: date,period
  USE numbers,      ONLY: rp
!
  IMPLICIT NONE
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nfs  ! - number of fields -
  INTEGER, INTENT(IN) :: nes  ! - number of EOF extensions -
  INTEGER, INTENT(IN) :: it0  ! - time 0 -
  INTEGER, INTENT(IN) :: nt   ! - number of cases -
  INTEGER, INTENT(IN) :: n3   ! - third dimension -
!
  REAL(KIND=rp), INTENT(IN) :: miss ! - missing value flag -
!
  TYPE(field), INTENT(IN) :: afield(:) ! - field -
!
  CHARACTER(LEN=1), INTENT(IN) :: ctype ! - output type -
!
! - optional input scalars -
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: cl! - confidence level-
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: iuse(:) ! - used variables indices -
  REAL(KIND=rp), INTENT(IN) :: v(:,:,:)  ! - data -
  REAL(KIND=rp), INTENT(IN) :: rlat(:,:) ! - latitudes -
  REAL(KIND=rp), INTENT(IN) :: rlng(:,:) ! - longitudes -
!
  LOGICAL, INTENT(IN) :: kuse(:) ! - used cases flags -
!
  TYPE(ofile), INTENT(IN) :: afile ! - output file -
!
! Local variables
  REAL(KIND=rp) :: my_cl;

! EXTERNAL C function
  EXTERNAL  write_grids_v10_c

! Functions
  INTRINSIC PRESENT
  INTRINSIC TRIM
  INTRINSIC LEN_TRIM

  IF(PRESENT(cl)) THEN
      my_cl=cl
  ELSE
      my_cl=-99.0
  END IF

  CALL write_grids_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                      TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                      TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                      TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                      ctype,&
                      nt, afield(:)%nv,n3, v,  iuse, kuse, miss, rlat, rlng,  SIZE(v,1), 0, &
                      afield(:)%region%nlts, afield(:)%region%nlgs, nfs, nes,afield(:)%nva,&
                      TRIM(afield(1)%var), LEN_TRIM(afield(1)%var), &
                      TRIM(afield(1)%unit), LEN_TRIM(afield(1)%unit), it0, &
                      afield(:)%member,my_cl,"NULL",4,"NULL",4,"NULL",4,"Y",1,"X",1,afield(:)%z%hght,afield(:)%z%unit,16,&
                      afield(:)%tdate%sdate%iyr, afield(:)%tdate%sdate%imn, afield(:)%tdate%sdate%idy,&
                      afield(:)%tdate%edate%iyr, afield(:)%tdate%edate%imn, afield(:)%tdate%edate%idy,&
                      afield(:)%mdate%iyr,  afield(:)%mdate%imn,   afield(:)%mdate%idy,&
                      ifail&
                    );

  END SUBROUTINE write_grids_v10_c_wrapper
!
!
  SUBROUTINE write_stns_v10_c_wrapper(afile,afield,nfs,nes, it0,nt,nv,v,iuse,kuse,miss,rlat,rlng,cstn,ifail)
!
! Outputs data in station format
!
! Modules
  USE fields,       ONLY: field
  USE IO_constants, ONLY: ffmts,faccs,cprcs,lstn
  USE iofiles,      ONLY: ofile
  USE numbers,      ONLY: rp
  USE time,         ONLY: period
!
  IMPLICIT NONE
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nfs  ! - number of fields -
  INTEGER, INTENT(IN) :: nes  ! - number of EOF extensions -
  INTEGER, INTENT(IN) :: it0  ! - time 0 -
  INTEGER, INTENT(IN) :: nt   ! - number of cases -
  INTEGER, INTENT(IN) :: nv   ! - number of stations -
!
  REAL(KIND=rp), INTENT(IN) :: miss ! - missing value flag -
!
  TYPE(ofile), INTENT(IN) :: afile  ! - output file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: iuse(:) ! - error indicator -
!
  REAL(KIND=rp), INTENT(IN) :: v(:,:)  ! - data -
  REAL(KIND=rp), INTENT(IN) :: rlat(:,:) ! - latitudes -
  REAL(KIND=rp), INTENT(IN) :: rlng(:,:) ! - longitudes -
!
  CHARACTER(LEN=lstn), INTENT(IN) :: cstn(:,:) ! - station names -
!
  LOGICAL, INTENT(IN) :: kuse(:) ! - variable flags -
!
  TYPE(field), INTENT(IN) :: afield(:) ! - field -

! EXTERNAL C function
  EXTERNAL  write_stns_v10_c

! Functions
  INTRINSIC PRESENT
  INTRINSIC TRIM
  INTRINSIC LEN_TRIM
!
! Local variables
  INTEGER :: cpt_m

  REAL(KIND=rp) :: cpt_z_hght

  CHARACTER(LEN=16) :: cpt_z_unit

  cpt_m=afield(1)%member
  cpt_z_hght=afield(1)%z%hght
  cpt_z_unit=afield(1)%z%unit

  CALL write_stns_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                      TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                      TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                      TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                      nt, nv, v,  iuse, kuse, miss, rlat, rlng,  SIZE(v,1), &
                      0, nfs, nes, afield(:)%nv,afield(:)%nva, &
                      TRIM(afield(1)%var), LEN_TRIM(afield(1)%var), &
                      TRIM(afield(1)%unit), LEN_TRIM(afield(1)%unit), it0, &
                      cpt_m,-99.0,"NULL",4,"NULL",4,"NULL",4,"Y",1,"X",1,cpt_z_hght,cpt_z_unit,16,&
                      afield(:)%tdate%sdate%iyr, afield(:)%tdate%sdate%imn, afield(:)%tdate%sdate%idy,&
                      afield(:)%tdate%edate%iyr, afield(:)%tdate%edate%imn, afield(:)%tdate%edate%idy,&
                      afield(:)%mdate%iyr,  afield(:)%mdate%imn,   afield(:)%mdate%idy,&
                      ifail, cstn(:,1) &
                    );

  END SUBROUTINE write_stns_v10_c_wrapper
!
!
  SUBROUTINE write_stnss_v10_c_wrapper(ctype,afile,afield,nfs,nes,it0,nt,nv,n3,v,iuse,kuse,miss,&
                                       rlat,rlng,cstn,ifail,cl)
!
! Outputs data in station format
!
! Modules
  USE fields,       ONLY: field
  USE IO_constants, ONLY: ffmts,faccs,cprcs,lstn
  USE iofiles,      ONLY: ofile
  USE numbers,      ONLY: rp
  USE time,         ONLY: period
!
  IMPLICIT NONE
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nfs  ! - number of fields -
  INTEGER, INTENT(IN) :: nes  ! - number of EOF extensions -
  INTEGER, INTENT(IN) :: it0  ! - time 0 -
  INTEGER, INTENT(IN) :: nt   ! - number of cases -
  INTEGER, INTENT(IN) :: nv   ! - number of stations -
  INTEGER, INTENT(IN) :: n3   ! - third dimension -
!
  REAL(KIND=rp), INTENT(IN) :: miss ! - missing value flag -
!
  TYPE(ofile), INTENT(IN) :: afile  ! - output file -
!
  CHARACTER(LEN=1), INTENT(IN) :: ctype ! - output type -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! - optional input scalars -
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: cl! - confidence level-
!
! Input arrays
  INTEGER, INTENT(IN) :: iuse(:) ! - error indicator -
!
  REAL(KIND=rp), INTENT(IN) :: v(:,:,:)  ! - data -
  REAL(KIND=rp), INTENT(IN) :: rlat(:,:) ! - latitudes -
  REAL(KIND=rp), INTENT(IN) :: rlng(:,:) ! - longitudes -
!
  CHARACTER(LEN=lstn), INTENT(IN) :: cstn(:,:) ! - station names -
!
  LOGICAL, INTENT(IN) :: kuse(:) ! - variable flags -
!
  TYPE(field), INTENT(IN) :: afield(:) ! - field -

! EXTERNAL C function
  EXTERNAL  write_stns_v10_c

! Functions
  INTRINSIC PRESENT
  INTRINSIC TRIM
  INTRINSIC LEN_TRIM
!
! Local variables
  REAL(KIND=rp) :: my_cl

  IF(PRESENT(cl)) THEN
      my_cl=cl
  ELSE
      my_cl=-99.0
  END IF

  CALL write_stnss_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                      TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                      TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                      TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                      ctype,nt, nv,n3,v,  iuse, kuse, miss, rlat, rlng,  SIZE(v,1), &
                      0, nfs, nes, afield(:)%nv,afield(:)%nva, &
                      TRIM(afield(1)%var), LEN_TRIM(afield(1)%var), &
                      TRIM(afield(1)%unit), LEN_TRIM(afield(1)%unit), it0, &
                      my_cl,&
                      afield(:)%tdate%sdate%iyr, afield(:)%tdate%sdate%imn, afield(:)%tdate%sdate%idy,&
                      afield(:)%tdate%edate%iyr, afield(:)%tdate%edate%imn, afield(:)%tdate%edate%idy,&
                      afield(:)%mdate%iyr,  afield(:)%mdate%imn,   afield(:)%mdate%idy,&
                      ifail, cstn(:,1) &
                    );

  END SUBROUTINE write_stnss_v10_c_wrapper
!
!
  SUBROUTINE write_unrf_v10_c_wrapper(afile,afield,nfs,nes,it0,nt,nv,v,iuse,kuse,miss,cstn,ifail,b0)
!
! Outputs data in station format
!
! Modules
  USE fields,       ONLY: field
  USE IO_constants, ONLY: ffmts,faccs,cprcs,lstn
  USE iofiles,      ONLY: ofile
  USE numbers,      ONLY: rp
  USE time,         ONLY: period
!
  IMPLICIT NONE
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nfs  ! - number of fields -
  INTEGER, INTENT(IN) :: nes  ! - number of EOF extensions -
  INTEGER, INTENT(IN) :: it0  ! - time 0 -
  INTEGER, INTENT(IN) :: nt   ! - number of cases -
  INTEGER, INTENT(IN) :: nv   ! - number of stations -
!
  REAL(KIND=rp), INTENT(IN) :: miss ! - missing value flag -
!
  TYPE(ofile), INTENT(IN) :: afile  ! - output file -
!
  INTEGER, INTENT(IN), OPTIONAL ::b0  ! - regression constant -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: iuse(:) ! - error indicator -
!
  REAL(KIND=rp), INTENT(IN) :: v(:,:)  ! - data -
!
  CHARACTER(LEN=lstn), INTENT(IN) :: cstn(:,:) ! - station names -
!
  LOGICAL, INTENT(IN) :: kuse(:) ! - variable flags -
!
  TYPE(field), INTENT(IN) :: afield(:) ! - field -

! EXTERNAL C function
  EXTERNAL  write_unrf_v10_c

! Functions
  INTRINSIC PRESENT
  INTRINSIC TRIM
  INTRINSIC LEN_TRIM
!
! Local variables
  INTEGER :: my_b0
  INTEGER :: cpt_m
  REAL(KIND=rp) ::z_hght 

  my_b0=-99
  z_hght=-99.0
  cpt_m=afield(1)%member
  IF(PRESENT(b0)) my_b0=0

  CALL write_unrf_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                      TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                      TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                      TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                      nt, nv, v,  iuse, kuse, miss, SIZE(v,1), &
                      0, nfs, nes, afield(1)%nv,&
                      TRIM(afield(1)%var), LEN_TRIM(afield(1)%var), &
                      TRIM(afield(1)%unit), LEN_TRIM(afield(1)%unit), it0, my_b0,&
                      cpt_m,-99.0,"NULL",4,"NULL",4,"NULL",4,"NULL",4,"NULL",4,z_hght,"NULL",4,&
                      afield(:)%tdate%sdate%iyr, afield(:)%tdate%sdate%imn, afield(:)%tdate%sdate%idy,&
                      afield(:)%tdate%edate%iyr, afield(:)%tdate%edate%imn, afield(:)%tdate%edate%idy,&
                      ifail, cstn(:,1) &
                    );

  END SUBROUTINE write_unrf_v10_c_wrapper
!
!
  SUBROUTINE write_unrfs_v10_c_wrapper(ctype,afile,afield,nfs,nes,it0,nt,nv,n3,v,iuse,kuse,miss,cstn,ifail,cl)
!
! Outputs data in station format
!
! Modules
  USE fields,       ONLY: field
  USE IO_constants, ONLY: ffmts,faccs,cprcs,lstn
  USE iofiles,      ONLY: ofile
  USE numbers,      ONLY: rp
  USE time,         ONLY: period
!
  IMPLICIT NONE
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nfs  ! - number of fields -
  INTEGER, INTENT(IN) :: nes  ! - number of EOF extensions -
  INTEGER, INTENT(IN) :: it0  ! - time 0 -
  INTEGER, INTENT(IN) :: nt   ! - number of cases -
  INTEGER, INTENT(IN) :: nv   ! - number of stations -
  INTEGER, INTENT(IN) :: n3   ! - third dimension -
!
  REAL(KIND=rp), INTENT(IN) :: miss ! - missing value flag -
!
  TYPE(ofile), INTENT(IN) :: afile  ! - output file -
!
  CHARACTER(LEN=1), INTENT(IN) :: ctype ! - output type -
!
  REAL(KIND=rp), INTENT(IN), OPTIONAL ::cl  ! - regression constant -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  INTEGER, INTENT(IN) :: iuse(:) ! - error indicator -
!
  REAL(KIND=rp), INTENT(IN) :: v(:,:,:)  ! - data -
!
  CHARACTER(LEN=lstn), INTENT(IN) :: cstn(:,:) ! - station names -
!
  LOGICAL, INTENT(IN) :: kuse(:) ! - variable flags -
!
  TYPE(field), INTENT(IN) :: afield(:) ! - field -

! EXTERNAL C function
  EXTERNAL  write_unrfs_v10_c

! Functions
  INTRINSIC PRESENT
  INTRINSIC TRIM
  INTRINSIC LEN_TRIM
!
! Local variables
  REAL(KIND=rp) :: my_cl

  IF(PRESENT(cl)) THEN
      my_cl=cl
  ELSE
      my_cl=-99.0
  END IF

  !PRINT *,'SIZE(v,1)',SIZE(v,1),' SIZE(v,2)',SIZE(v,2),' SIZE(v,3)',SIZE(v,3)
  CALL write_unrfs_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                      TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                      TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                      TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                      ctype, nt, nv, n3, v,  iuse, kuse, miss, SIZE(v,1), &
                      0, nfs, nes, afield(:)%nv,&
                      TRIM(afield(1)%var), LEN_TRIM(afield(1)%var), &
                      TRIM(afield(1)%unit), LEN_TRIM(afield(1)%unit), it0, &
                      my_cl,&
                      afield(:)%tdate%sdate%iyr, afield(:)%tdate%sdate%imn, afield(:)%tdate%sdate%idy,&
                      afield(:)%tdate%edate%iyr, afield(:)%tdate%edate%imn, afield(:)%tdate%edate%idy,&
                      ifail, cstn(:,1) &
                    );

  END SUBROUTINE write_unrfs_v10_c_wrapper
!
!
!
!
  SUBROUTINE write_load_v10_c_wrapper(afile,afield,igrid,nfs,nes,&
                    ne,nv,iuse,kuse,eof,miss,cfld,ifail,rlat,rlng,cstn)
!
! Prints spatial loadings
!
! Modules
  USE fields,       ONLY: field
  USE IO_constants, ONLY: lstn,ffmts,faccs,cprcs,lstn
  USE iofiles,      ONLY: ofile
  USE numbers,      ONLY: rp
!
  IMPLICIT NONE
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: igrid ! - gridded data flag -
  INTEGER, INTENT(IN) :: nfs   ! - number of fields -
  INTEGER, INTENT(IN) :: nes   ! - number of EOF dimensions -
  INTEGER, INTENT(IN) :: ne    ! - number of EOF modes -
  INTEGER, INTENT(IN) :: nv    ! - number of variables -
!
  REAL(KIND=rp), INTENT(IN) :: miss ! - missing value flag -
!
  CHARACTER(LEN=*), INTENT(IN) :: cfld ! - field description -
!
  TYPE(ofile), INTENT(IN) :: afile ! - output file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
!
  INTEGER, INTENT(IN) :: iuse(:) ! - variable flags -
  LOGICAL, INTENT(IN) :: kuse(:) ! - variable flags -
!
  REAL(KIND=rp), INTENT(IN) :: eof(:,:) ! - spatial loadings -
!
  TYPE(field), INTENT(IN) :: afield(:)  ! - field
!
! - optional input arrays -
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: rlat(:,:) ! - latitudes -
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: rlng(:,:) ! - longitudes -
!
  CHARACTER(LEN=lstn), INTENT(IN), OPTIONAL :: cstn(:,:) ! - station names -

! 
! EXTERNAL C function
  EXTERNAL  write_load_v10_c

! Functions
  INTRINSIC PRESENT
  INTRINSIC TRIM
  INTRINSIC LEN_TRIM
! Local variables
  INTEGER             :: i               ! - index
  CHARACTER(LEN=lstn) :: mycstn(afield(1)%nlt)    ! - station names -
  REAL(KIND=rp)       :: myrlat(afield(1)%nlt) ! - latitudes -
  REAL(KIND=rp)       :: myrlng(afield(1)%nlt) ! - longitudes -
  INTEGER             :: varlen(nfs*nes)   ! var length
  INTEGER             :: cssnlen(nfs*nes)   ! var length

  DO i=1,afield(1)%nlt
    mycstn(i)="NO STATION"
    myrlat(i)=0.0
    myrlng(i)=0.0
  END DO
  DO i=1,nfs*nes
    varlen(i)=LEN_TRIM(afield(i)%var)
    cssnlen(i)=LEN_TRIM(afield(i)%cssn)
  END DO
  IF(PRESENT(cstn)) THEN
    IF(PRESENT(rlat)) THEN
      IF(PRESENT(rlng)) THEN
          CALL write_load_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                      TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                      TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                      TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                      nfs,nes, nv, ne, iuse, kuse, eof, miss, igrid, &
                      afield(:)%region%nlts, afield(:)%region%nlgs, rlat, rlng,&
                      cstn, SIZE(eof,1), 0, TRIM(cfld), LEN_TRIM(cfld),&
                      afield(:)%var, varlen,      afield(:)%cssn, cssnlen,&
                      afield(:)%nv,&
                      ifail&
                    )
      ELSE
          CALL write_load_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                      TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                      TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                      TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                      nfs,nes, nv, ne, iuse, kuse, eof, miss, igrid, &
                      afield(:)%region%nlts, afield(:)%region%nlgs, rlat, myrlng,&
                      cstn, SIZE(eof,1), 0, TRIM(cfld), LEN_TRIM(cfld),&
                      afield(:)%var,       afield(:)%cssn,cssnlen, &
                      afield(:)%nv,&
                      ifail&
                      )

      END IF
    ELSE
      IF(PRESENT(rlng) ) THEN
          CALL write_load_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                      TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                      TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                      TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                      nfs,nes, nv, ne, iuse, kuse, eof, miss, igrid, &
                      afield(:)%region%nlts, afield(:)%region%nlgs, myrlat, rlng,&
                      cstn, SIZE(eof,1), 0, TRIM(cfld), LEN_TRIM(cfld),&
                      afield(:)%var,varlen,       afield(:)%cssn,cssnlen, &
                      afield(:)%nv,&
                      ifail&
                    )
      ELSE
          CALL write_load_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                      TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                      TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                      TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                      nfs,nes, nv, ne, iuse, kuse, eof, miss, igrid, &
                      afield(:)%region%nlts, afield(:)%region%nlgs, myrlat, myrlng,&
                      cstn, SIZE(eof,1), 0, TRIM(cfld), LEN_TRIM(cfld),&
                      afield(:)%var,varlen,       afield(:)%cssn,cssnlen, &
                      afield(:)%nv,&
                      ifail&
                      )

      END IF

    END IF

  ELSE
    IF(PRESENT(rlat) ) THEN
      IF(PRESENT(rlng) ) THEN
          CALL write_load_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                      TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                      TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                      TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                      nfs,nes, nv, ne, iuse, kuse, eof, miss, igrid, &
                      afield(:)%region%nlts, afield(:)%region%nlgs, rlat, rlng,&
                      mycstn, SIZE(eof,1), 0, TRIM(cfld), LEN_TRIM(cfld),&
                      afield(:)%var, varlen,      afield(:)%cssn,cssnlen, &
                      afield(:)%nv,&
                      ifail&
                    )
      ELSE
          CALL write_load_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                      TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                      TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                      TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                      nfs,nes, nv, ne, iuse, kuse, eof, miss, igrid, &
                      afield(:)%region%nlts, afield(:)%region%nlgs, rlat, myrlng,&
                      mycstn, SIZE(eof,1), 0, TRIM(cfld), LEN_TRIM(cfld),&
                      afield(:)%var, varlen,      afield(:)%cssn,cssnlen, &
                      afield(:)%nv,&
                      ifail&
                      )
      END IF
    ELSE
      IF(PRESENT(rlng) ) THEN
          CALL write_load_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                      TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                      TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                      TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                      nfs,nes, nv, ne, iuse, kuse, eof, miss, igrid, &
                      afield(:)%region%nlts, afield(:)%region%nlgs, myrlat, rlng,&
                      mycstn, SIZE(eof,1), 0, TRIM(cfld), LEN_TRIM(cfld),&
                      afield(:)%var, varlen,      afield(:)%cssn,cssnlen, &
                      afield(:)%nv,&
                      ifail&
                    )
      ELSE
          CALL write_load_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                      TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                      TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                      TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                      nfs,nes, nv, ne, iuse, kuse, eof, miss, igrid, &
                      afield(:)%region%nlts, afield(:)%region%nlgs, myrlat, myrlng,&
                      mycstn, SIZE(eof,1), 0, TRIM(cfld), LEN_TRIM(cfld),&
                      afield(:)%var, varlen,      afield(:)%cssn,cssnlen, &
                      afield(:)%nv,&
                      ifail&
                      )
      END IF

    END IF

  END IF

  END SUBROUTINE write_load_v10_c_wrapper
!
!
SUBROUTINE write_eigs_v10_c_wrapper (afile,ne,sv,nt,ifail)
!
! Modules
  USE IO_constants, ONLY: faccs,cprcs,ffmts
  USE iofiles,      ONLY: ofile
  USE numbers,      ONLY: sp,oneh,rp
!
  IMPLICIT NONE
!
! Arguments
! Output scalars
 INTEGER, INTENT(OUT) :: ifail   ! - total number of eigenvalues -
! Input scalars
  INTEGER, INTENT(IN) :: ne   ! - total number of eigenvalues -
  INTEGER, INTENT(IN) :: nt   ! - number of cases -
!
  TYPE(ofile), INTENT(IN) :: afile ! - output file -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: sv(:) ! - singular values -
! 
! EXTERNAL C function
  EXTERNAL  write_eigs_v10_c

! Functions
  INTRINSIC PRESENT
  INTRINSIC TRIM
  INTRINSIC LEN_TRIM
! Hard-Coded nfs to 1 
  CALL write_eigs_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                         TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                         TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                         TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                         ne, sv, nt, &
                         1,  ifail &
                       )

  END SUBROUTINE write_eigs_v10_c_wrapper
!
!
  SUBROUTINE write_scor_v10_c_wrapper (cscore,afile,nfs,nes,nt,it0,&
                                       fperiods,period1,ne,kuse,miss,ts,ifail)
!
! Modules
  USE IO_constants, ONLY: faccs,cprcs,ffmts
  USE iofiles,      ONLY: ofile
  USE numbers,      ONLY: rp
  USE time,         ONLY: period 
!
  IMPLICIT NONE
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nt   ! - number of cases -
  INTEGER, INTENT(IN) :: ne   ! - number of EOF modes -
  INTEGER, INTENT(IN) :: it0  ! - index of 0th case -
  INTEGER, INTENT(IN) :: nfs  ! - number of fields -
  INTEGER, INTENT(IN) :: nes  ! - number of EOF dimensions -
!
  REAL(KIND=rp), INTENT(IN) :: miss !- missing value flag -
!
  CHARACTER(LEN=*), INTENT(IN)  :: cscore ! - score description
!
  TYPE(ofile), INTENT(IN) :: afile   ! - output file -
  TYPE(period),INTENT(IN) :: period1
  TYPE(period),INTENT(IN) :: fperiods(:)
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: ts(:,:) ! - time scores -
!
  LOGICAL, INTENT(IN)  :: kuse(:)      ! - used cases flags
! 
! EXTERNAL C function
  EXTERNAL  write_scor_v10_c

! Functions
  INTRINSIC TRIM
  INTRINSIC LEN_TRIM

  CALL write_scor_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                         TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                         TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                         TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                         nfs, nes, nt, it0, ne, ts, SIZE(ts,1), 0,&
                         kuse, miss, cscore, LEN_TRIM(cscore),&
                         fperiods%sdate%iyr, fperiods%sdate%imn, fperiods%sdate%idy,&
                         fperiods%edate%iyr, fperiods%edate%imn, fperiods%edate%idy,&
                         period1%sdate%iyr, period1%sdate%imn, period1%sdate%idy,&
                         period1%edate%iyr, period1%edate%imn, period1%edate%idy,&
                         ifail &
                       )

  END SUBROUTINE write_scor_v10_c_wrapper
!
  SUBROUTINE write_canc_v10_c_wrapper (afile,nc,mu,ifail)
!
! Modules
  USE IO_constants, ONLY: ffmts,faccs,cprcs
  USE iofiles,      ONLY: ofile
  USE numbers,      ONLY: rp
!
  IMPLICIT NONE
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nc   ! - number of correlations -
!
  TYPE(ofile), INTENT(IN) :: afile ! - output file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: mu(:) ! - singular values -
! 
! EXTERNAL C function
  EXTERNAL  write_canc_v10_c

! Functions
  INTRINSIC TRIM
  INTRINSIC LEN_TRIM

  CALL write_canc_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                         TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                         TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                         TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                         nc, mu, 0, &
                         ifail &
                       )
  END SUBROUTINE write_canc_v10_c_wrapper
!
!
  SUBROUTINE write_roc_v10_c_wrapper (afile,nb,ng,hit,far,roca,ifail)
!
! Modules
  USE IO_constants, ONLY: faccs,cprcs,ffmts
  USE iofiles,      ONLY: ofile
  USE numbers,      ONLY: rp
!
  IMPLICIT NONE
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nb   ! - number of bins -
  INTEGER, INTENT(IN) :: ng   ! - number of categories -
!
  TYPE(ofile), INTENT(IN) :: afile ! - output file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: roca(:)  ! - ROC areas -
  REAL(KIND=rp), INTENT(IN) :: hit(:,:) ! - hit rates -
  REAL(KIND=rp), INTENT(IN) :: far(:,:) ! - false-alarm rates -
! 
! EXTERNAL C function
  EXTERNAL  write_roc_v10_c

! Functions
  INTRINSIC TRIM
  INTRINSIC LEN_TRIM

  CALL write_roc_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                         TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                         TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                         TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                         nb, ng, hit, far, roca, 0, &
                         ifail &
                       )
  END SUBROUTINE write_roc_v10_c_wrapper
!
!
  SUBROUTINE write_regr_v10_c_wrapper (afile,xfile,xfield,nx,iusex,yfile,yfield,ny,iusey,&
                              b,dmiss,ifail,  rlatx,rlngx,rlaty,rlngy,cstx,csty,b0)
!
! Modules
  USE arrays,       ONLY: dwk,swk
  USE fields,       ONLY: field
  USE IO_constants, ONLY: faccs,cprcs,lstn,ffmts
  USE iofiles,      ONLY: ofile,ifile
  USE numbers,      ONLY: rp
!
  IMPLICIT NONE
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nx     ! - number of X variables -
  INTEGER, INTENT(IN) :: ny     ! - number of Y variables -
!
  REAL(KIND=rp), INTENT(IN) :: dmiss ! - missing value flag -
!
  TYPE(ofile), INTENT(IN) :: afile ! - output file -
!
  TYPE(ifile), INTENT(IN) :: xfile ! - intput file -
  TYPE(ifile), INTENT(IN) :: yfile ! - intput file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
!
  INTEGER, INTENT(IN) :: iusex(:) ! - X variable flags -
  INTEGER, INTENT(IN) :: iusey(:) ! - Y variable flags -
! Input arrays
  REAL(KIND=rp), INTENT(IN) :: b(:,:) ! - regression coefficients -
!
  TYPE(field), INTENT(IN) :: xfield(:) ! - X field 
  TYPE(field), INTENT(IN) :: yfield(:) ! - Y field
!
! - optional input arrays -
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: rlatx(:,:) ! - X latitudes -
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: rlngx(:,:) ! - X longitudes -
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: rlaty(:,:) ! - Y latitudes -
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: rlngy(:,:) ! - Y longitudes -
  REAL(KIND=rp), INTENT(IN), OPTIONAL :: b0(:)    ! - regression constants -
!
  CHARACTER(LEN=lstn), INTENT(IN), OPTIONAL :: cstx(:,:) ! - X station names -
  CHARACTER(LEN=lstn), INTENT(IN), OPTIONAL :: csty(:,:) ! - Y station names -
!
!Local Scalars
  INTEGER             :: i                        ! - index
  CHARACTER(LEN=lstn) :: mycstnx(xfield(1)%region%nlts)    ! - station names -
  REAL(KIND=rp)       :: myrlatx(xfield(1)%region%nlts)             ! - X latitudes -
  REAL(KIND=rp)       :: myrlngx(xfield(1)%region%nlgs)             ! - X longitudes -
  CHARACTER(LEN=lstn) :: mycstny(yfield(1)%region%nlts)    ! - station names -
  REAL(KIND=rp)       :: myrlaty(yfield(1)%region%nlts)             ! - Y latitudes -
  REAL(KIND=rp)       :: myrlngy(yfield(1)%region%nlgs)             ! - Y longitudes -
  REAL(KIND=rp)       :: myb0(2)                  ! - regression constants -

  INTEGER             :: xvarlen(xfile%nfs*xfile%nls)          ! - var length
  INTEGER             :: yvarlen(yfile%nfs*yfile%nls)          ! - var length
  INTEGER             :: xcssnlen(xfile%nfs*xfile%nls)         ! - var length
  INTEGER             :: ycssnlen(yfile%nfs*yfile%nls)         ! - var length

! 
! EXTERNAL C function
  EXTERNAL  :: write_regr_v10_c

! Functions
  INTRINSIC PRESENT
  INTRINSIC TRIM
  INTRINSIC LEN_TRIM
  INTRINSIC SIZE

  !PRINT *,'11111111111111111111111'
  myb0(1) =0.0
  myb0(2) =0.0
  !PRINT *,'----11111111111111111111111'
  DO i=1,xfield(1)%region%nlts
    mycstnx(i)="none"
    myrlatx(i)=0.0
    myrlngx(i)=0.0
  END DO
  DO i=1,xfield(1)%region%nlgs
    myrlngx(i)=0.0
  END DO

  !PRINT *,'2222222222222222222222'
  DO i=1,yfield(1)%region%nlts
    mycstny(i)="none"
    myrlaty(i)=0.0
    myrlngy(i)=0.0
  END DO
  DO i=1,yfield(1)%region%nlgs
    myrlngy(i)=0.0
  END DO

  !PRINT *,'333333333333333333333333333'
  DO i=1,xfile%nfs*xfile%nls
    xvarlen(i) = LEN_TRIM(xfield(i)%var)
    xcssnlen(i)= LEN_TRIM(xfield(i)%cssn)
  END DO

  !PRINT *,'44444444444444444444444444444'
  DO i=1,yfile%nfs*yfile%nls
    yvarlen(i) = LEN_TRIM(yfield(i)%var)
    ycssnlen(i)= LEN_TRIM(yfield(i)%cssn)
  END DO

  !PRINT *,'555555555555555555555555555555'
  IF(PRESENT(b0)) THEN
      IF(PRESENT(rlatx) .AND. PRESENT(rlngx) ) THEN
          IF(PRESENT(rlaty) .AND. PRESENT(rlngy) ) THEN
              IF(PRESENT(cstx)) THEN
                !PRINT *," ! b0 rlatx, rlngx,rlaty and rlngy are presented ---------- "
                CALL write_regr_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                             TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                             TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                             TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                             nx,iusex,ny,iusey,&
                             b,dmiss,xfile%igrid,yfile%igrid,&
                             xfile%nfs,xfile%nls,yfile%nfs,yfile%nls,&
                             xfield(:)%region%nlts,xfield(:)%region%nlgs,SIZE(b,1),&
                             yfield(:)%region%nlts,yfield(:)%region%nlgs,&
                             rlatx, rlngx, cstx, &
                             rlaty, rlngy, csty, 0,&
                             yfield(:)%var,yvarlen,yfield(:)%cssn,ycssnlen,&
                             xfield(:)%var,xvarlen,xfield(:)%cssn,xcssnlen,&
                             b0,1,&
                             xfield(:)%nv,yfield(:)%nv,xfield(:)%nva,yfield(:)%nva,&
                             ifail&
                           )
              ELSE
                  !PRINT *, "@@@@@@@@@@@@@@@@@@@ "
                CALL write_regr_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                             TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                             TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                             TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                             nx,iusex,ny,iusey,&
                             b,dmiss,xfile%igrid,yfile%igrid,&
                             xfile%nfs,xfile%nls,yfile%nfs,yfile%nls,&
                             xfield(:)%region%nlts,xfield(:)%region%nlgs,SIZE(b,1),&
                             yfield(:)%region%nlts,yfield(:)%region%nlgs,&
                             rlatx, rlngx, mycstnx, &
                             rlaty, rlngy, csty, 0,&
                             yfield(:)%var,yvarlen,yfield(:)%cssn,ycssnlen,&
                             xfield(:)%var,xvarlen,xfield(:)%cssn,xcssnlen,&
                             b0,1,&
                             xfield(:)%nv,yfield(:)%nv,xfield(:)%nva,yfield(:)%nva,&
                             ifail&
                           )
              END IF
          ELSE
              !---------- rlaty and rlngy are not presented ----------
              CALL write_regr_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                             TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                             TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                             TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                             nx,iusex,ny,iusey,&
                             b,dmiss,xfile%igrid,yfile%igrid,&
                             xfile%nfs,xfile%nls,yfile%nfs,yfile%nls,&
                             xfield(:)%region%nlts,xfield(:)%region%nlgs,SIZE(b,1),&
                             yfield(:)%region%nlts,yfield(:)%region%nlgs,&
                             rlatx, rlngx, cstx, &
                             myrlaty, myrlngy, mycstny, 0,&
                             yfield(:)%var,yvarlen,yfield(:)%cssn,ycssnlen,&
                             xfield(:)%var,xvarlen,xfield(:)%cssn,xcssnlen,&
                             b0,1,&
                             xfield(:)%nv,yfield(:)%nv,xfield(:)%nva,yfield(:)%nva,&
                             ifail&
                           )

              !---------- rlaty and rlngy are not presented ----------
          END IF
      ELSE
      !---------- rlatx and rlngx are not presented ----------
          IF(PRESENT(rlaty) .AND. PRESENT(rlngy) ) THEN
          ELSE
              !---------- rlaty and rlngy are not presented ----------
              !---------- rlaty and rlngy are not presented ----------
          ENDIF
      !---------- rlatx and rlngx are not presented ----------
      END IF
  ELSE
! b0 is not presented ----------
      IF(PRESENT(rlatx) .AND. PRESENT(rlngx) ) THEN
          IF(PRESENT(rlaty) .AND. PRESENT(rlngy) ) THEN
              !PRINT *, " b0 is not presented, rlatx, rlngx, rlaty, rlngy are presented"
              IF(PRESENT(cstx) ) THEN
                IF(PRESENT(csty) ) THEN
                CALL write_regr_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                             TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                             TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                             TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                             nx,iusex,ny,iusey,&
                             b,dmiss,xfile%igrid,yfile%igrid,&
                             xfile%nfs,xfile%nls,yfile%nfs,yfile%nls,&
                             xfield(:)%region%nlts,xfield(:)%region%nlgs,SIZE(b,1),&
                             yfield(:)%region%nlts,yfield(:)%region%nlgs,&
                             rlatx, rlngx, cstx, &
                             rlaty, rlngy, csty, 0,&
                             yfield(:)%var,yvarlen,yfield(:)%cssn,ycssnlen,&
                             xfield(:)%var,xvarlen,xfield(:)%cssn,xcssnlen,&
                             myb0,0,&
                             xfield(:)%nv,yfield(:)%nv,xfield(:)%nva,yfield(:)%nva,&
                             ifail&
                           )
                ELSE
                CALL write_regr_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                             TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                             TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                             TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                             nx,iusex,ny,iusey,&
                             b,dmiss,xfile%igrid,yfile%igrid,&
                             xfile%nfs,xfile%nls,yfile%nfs,yfile%nls,&
                             xfield(:)%region%nlts,xfield(:)%region%nlgs,SIZE(b,1),&
                             yfield(:)%region%nlts,yfield(:)%region%nlgs,&
                             rlatx, rlngx, cstx, &
                             rlaty, rlngy, mycstny, 0,&
                             yfield(:)%var,yvarlen,yfield(:)%cssn,ycssnlen,&
                             xfield(:)%var,xvarlen,xfield(:)%cssn,xcssnlen,&
                             myb0,0,&
                             xfield(:)%nv,yfield(:)%nv,xfield(:)%nva,yfield(:)%nva,&
                             ifail&
                           )
                END IF
              ELSE
                IF(PRESENT(csty) ) THEN
                  !PRINT *, "#########"
                CALL write_regr_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                             TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                             TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                             TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                             nx,iusex,ny,iusey,&
                             b,dmiss,xfile%igrid,yfile%igrid,&
                             xfile%nfs,xfile%nls,yfile%nfs,yfile%nls,&
                             xfield(:)%region%nlts,xfield(:)%region%nlgs,SIZE(b,1),&
                             yfield(:)%region%nlts,yfield(:)%region%nlgs,&
                             rlatx, rlngx, mycstnx, &
                             rlaty, rlngy, csty, 0,&
                             yfield(:)%var,yvarlen,yfield(:)%cssn,ycssnlen,&
                             xfield(:)%var,xvarlen,xfield(:)%cssn,xcssnlen,&
                             myb0,0,&
                             xfield(:)%nv,yfield(:)%nv,xfield(:)%nva,yfield(:)%nva,&
                             ifail&
                             )
                ELSE
                PRINT *, "######### no b0 cstnx cstny"
                CALL write_regr_v10_c( TRIM(afile%ffile), LEN_TRIM(afile%ffile), &
                             TRIM(ffmts(afile%ffmt%ifmt)),LEN_TRIM(ffmts(afile%ffmt%ifmt)),&
                             TRIM(faccs(afile%ffmt%iacc)),LEN_TRIM(faccs(afile%ffmt%iacc)),&
                             TRIM(cprcs(afile%ffmt%iprc)),LEN_TRIM(cprcs(afile%ffmt%iprc)),&
                             nx,iusex,ny,iusey,&
                             b,dmiss,xfile%igrid,yfile%igrid,&
                             xfile%nfs,xfile%nls,yfile%nfs,yfile%nls,&
                             xfield(:)%region%nlts,xfield(:)%region%nlgs,SIZE(b,1),&
                             yfield(:)%region%nlts,yfield(:)%region%nlgs,&
                             rlatx, rlngx, mycstnx, &
                             rlaty, rlngy, mycstny, 0,&
                             yfield(:)%var,yvarlen,yfield(:)%cssn,ycssnlen,&
                             xfield(:)%var,xvarlen,xfield(:)%cssn,xcssnlen,&
                             myb0,0,&
                             xfield(:)%nv,yfield(:)%nv,xfield(:)%nva,yfield(:)%nva,&
                             ifail&
                             )
                END IF
              END IF
          ELSE
              !---------- rlaty and rlngy are not presented ----------
              !---------- rlaty and rlngy are not presented ----------
          END IF
      ELSE

      END IF
! b0 is not presented ----------
  END IF

  END SUBROUTINE write_regr_v10_c_wrapper
