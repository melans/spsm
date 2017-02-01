! $Id: fortran_c_mix_interface.f90 1215 2011-02-25 21:30:20Z simon $
MODULE FortranCMix
!
! Modules
  USE numbers, ONLY: rp
!
! Implicit declarations
  IMPLICIT NONE

INTERFACE
    SUBROUTINE get_structure_v10_c_mix (afile,afield,rlat,rlng,rrlat,rrlng,cstn,ifd,ilf,ifail,cfail,miss_v)
!
! Modules
  USE fields,        ONLY: field,init_field,reset_grids
  USE iofiles,       ONLY: ifile 
  USE IO_constants,  ONLY: iin,lfli,lprd,lstn,ltag,cxmlns
!
! Arguments
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
! - optional input/output scalars -
  REAL(KIND=rp), INTENT(INOUT), OPTIONAL :: miss_v ! - missing value flag -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifd  ! - current field -
  INTEGER, INTENT(OUT) :: ilf  ! - current lagged field -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
  CHARACTER(LEN=*), INTENT(OUT) :: cfail ! - problem tag -
!
! Pointer arrays
  REAL(KIND=rp), POINTER :: rlat(:,:)  ! - latitudes -
  REAL(KIND=rp), POINTER :: rlng(:,:)  ! - longitudes -
  REAL(KIND=rp), POINTER :: rrlat(:,:) ! - used longitudes -
  REAL(KIND=rp), POINTER :: rrlng(:,:) ! - realigned used longitudes -
!
  CHARACTER(LEN=lstn), POINTER :: cstn(:,:) ! - station labels -
!
  TYPE(field), POINTER :: afield(:)  ! - field -
 END SUBROUTINE get_structure_v10_c_mix
!
 SUBROUTINE get_structure_v9_c_mix (afile,afield,rlat,rlng,rrlat,rrlng,cstn,ifail)
!
! Modules
  USE fields,         ONLY: field
  USE IO_constants,   ONLY: lstn
  USE iofiles,        ONLY: ifile
!
  IMPLICIT NONE
!
! Arguments
!
! Arguments
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Pointer arrays
  REAL(KIND=rp), POINTER :: rlat(:,:)  ! - latitudes -
  REAL(KIND=rp), POINTER :: rlng(:,:)  ! - longitudes -
  REAL(KIND=rp), POINTER :: rrlat(:,:) ! - realigned used latitudes -
  REAL(KIND=rp), POINTER :: rrlng(:,:) ! - realigned used longitudes -
!
  CHARACTER(LEN=lstn), POINTER :: cstn(:,:) ! - station labels -
!
  TYPE(field), POINTER :: afield(:)  ! - field -
!
! External C functions
  EXTERNAL get_structure_v9_c
  EXTERNAL get_lat_lon_cstn_v9_c
!
 END SUBROUTINE get_structure_v9_c_mix

 FUNCTION copy_c_line(one_line,line_len)
!
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
 END FUNCTION copy_c_line

  SUBROUTINE read_grid_v10_c_wrapper (afile,afield,n,v,rlat,rlng,nread,period0,ifail,cfail)
!
! Reads formatted gridded data
!
! Modules
  USE IO_constants,  ONLY: ltag
  USE iofiles,       ONLY: ifile
  USE fields,        ONLY: field
  USE time,          ONLY: period
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
  CHARACTER(LEN=*), INTENT(OUT) :: cfail ! - problem tag -
!
  TYPE(period), INTENT(OUT) :: period0 ! - last successfully read period -
!
! Output arrays
  REAL(KIND=rp), INTENT(OUT) :: rlat(:,:) ! - latitudes -
  REAL(KIND=rp), INTENT(OUT) :: rlng(:,:) ! - longitudes -
  REAL(KIND=rp), INTENT(OUT) :: v(:,:)  ! - data -
!
! Pointer arrays
  TYPE(field), POINTER :: afield(:) ! - fields -
  END SUBROUTINE read_grid_v10_c_wrapper

  SUBROUTINE read_grid_v9_c_wrapper(afile,afield,n,v,rlat,rlng,nread,date0,ifail)
!
! Reads formatted gridded data
!
! Modules
  USE iofiles,       ONLY: ifile
  USE fields,        ONLY: field
  USE time,          ONLY: date 
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

  END SUBROUTINE read_grid_v9_c_wrapper

SUBROUTINE read_stns_v9_c_wrapper (afile,afield,n,v,rlat,rlng,rrlat,rrlng,cstn,nread,iyr0,ifail)
!
! Reads formatted station data
!
! Modules
  USE IO_constants, ONLY: lstn
  USE fields,       ONLY: field
  USE iofiles,      ONLY: ifile
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n   ! - number of cases -
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

END SUBROUTINE read_stns_v9_c_wrapper
!
!
SUBROUTINE read_unrf_v9_c_wrapper (afile,nv,n,v,cstn,nread,iyr0,ifail)
!
! Reads formatted unreferenced data
!
! Modules
  USE IO_constants, ONLY: lstn
  USE iofiles,      ONLY: ifile
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
END SUBROUTINE read_unrf_v9_c_wrapper

SUBROUTINE read_unrf_v10_c_wrapper (afile,nv,n,v,cstn,nread,period0,ifail)
!
! Reads formatted unreferenced data
!
! Modules
  USE IO_constants, ONLY: lstn
  USE iofiles,      ONLY: ifile
  USE time,         ONLY: period
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
END SUBROUTINE read_unrf_v10_c_wrapper

SUBROUTINE read_stns_v10_c_wrapper (afile,afield,n,v,rlat,rlng,rrlat,rrlng,cstn,nread,period0,ifail)
!
! Reads formatted station data
!
! Modules
  USE IO_constants, ONLY: lstn
  USE iofiles,      ONLY: ifile
  USE fields,       ONLY: field
  USE time,         ONLY: period
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: n   ! - number of cases -
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

END SUBROUTINE read_stns_v10_c_wrapper

  SUBROUTINE write_grid_v10_c_wrapper (afile,afield,nfs,nes,nt,nv,v,iuse,kuse,miss,rlat,rlng,ifail, &
             it0)
!
! Outputs data in gridded format
!
! Modules
  USE fields,       ONLY: field
  USE iofiles,      ONLY: ofile
  USE time,         ONLY: period
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
  INTEGER, INTENT(OUT) :: ifail ! - error indicator - !  
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

! EXTERNAL C function
  EXTERNAL  write_grid_v10_c

  END SUBROUTINE write_grid_v10_c_wrapper
!
  SUBROUTINE write_grids_v10_c_wrapper (ctype,afile,afield,nfs,nes,it0,nt,nv,n3,v,iuse,kuse,miss,rlat,rlng,ifail, &
             cl)
!
! Outputs data in gridded format
!
! Modules
  USE fields,       ONLY: field
  USE IO_constants, ONLY: ffmts,faccs,cprcs
  USE iofiles,      ONLY: ofile
  USE time,         ONLY: date,period
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
  INTEGER, INTENT(IN) :: nv   ! - number of grids -
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
  END SUBROUTINE write_grids_v10_c_wrapper
!
!
  SUBROUTINE write_stns_v10_c_wrapper(afile,afield,nfs,nes, it0,nt,nv,v,iuse,kuse,miss,rlat,rlng,cstn,ifail)
!
! Outputs data in station format
!
! Modules
  USE fields,       ONLY: field
  USE iofiles,      ONLY: ofile
  USE IO_constants, ONLY: lstn 
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

END SUBROUTINE write_stnss_v10_c_wrapper

!
!
  SUBROUTINE write_unrf_v10_c_wrapper(afile,afield,nfs,nes, it0,nt,nv,v,iuse,kuse,miss,cstn,ifail,b0)
!
! Outputs data in station format
!
! Modules
  USE fields,       ONLY: field
  USE IO_constants, ONLY: ffmts,faccs,cprcs,lstn
  USE iofiles,      ONLY: ofile
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

  END SUBROUTINE write_unrf_v10_c_wrapper
!
!
  SUBROUTINE write_unrfs_v10_c_wrapper(ctype,afile,afield,nfs,nes,it0,nt,nv,n3,&
             v,iuse,kuse,miss,cstn,ifail,cl)
!
! Outputs data in station format
!
! Modules
  USE fields,       ONLY: field
  USE IO_constants, ONLY: ffmts,faccs,cprcs,lstn
  USE iofiles,      ONLY: ofile
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

  END SUBROUTINE write_unrfs_v10_c_wrapper

!
!
  SUBROUTINE write_load_v10_c_wrapper(afile,afield,igrid,nfs,nes,ne,nv,&
                                      iuse,kuse,eof,miss,cfld,ifail,rlat,rlng,cstn)
!
! Prints spatial loadings
!
! Modules
  USE fields,       ONLY: field
  USE IO_constants, ONLY: lstn,ffmts,faccs,cprcs,lstn
  USE iofiles,      ONLY: ofile
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

  END SUBROUTINE write_eigs_v10_c_wrapper
!
!
  SUBROUTINE write_scor_v10_c_wrapper (cscore,afile,nfs,nes,nt,it0,fperiods,period1,ne,kuse,miss,ts,ifail)
!
! Modules
  USE IO_constants, ONLY: faccs,cprcs,ffmts
  USE iofiles,      ONLY: ofile
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
  INTEGER, INTENT(IN) :: nes  ! - number of EOF extensions -
!
  REAL(KIND=rp), INTENT(IN) :: miss !- missing value flag -
!
  CHARACTER(LEN=*), INTENT(IN)  :: cscore ! - score description
!
  TYPE(ofile), INTENT(IN) :: afile ! - output file -
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

  END SUBROUTINE write_scor_v10_c_wrapper
!
!
  SUBROUTINE write_canc_v10_c_wrapper (afile,nc,mu,ifail)
!
! Modules
  USE iofiles, ONLY: ofile
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

  END SUBROUTINE write_canc_v10_c_wrapper
!
!
  SUBROUTINE write_roc_v10_c_wrapper (afile,nb,ng,hit,far,roca,ifail)
!
! Modules
  USE iofiles, ONLY: ofile
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

  END SUBROUTINE write_roc_v10_c_wrapper
!
!
  SUBROUTINE write_regr_v10_c_wrapper (afile,xfile,xfield,nx,iusex,yfile,yfield,&
                   ny,iusey,b,dmiss,ifail,rlatx,rlngx,rlaty,rlngy,cstx,csty,b0)
!
! Modules
  USE arrays,       ONLY: dwk,swk
  USE fields,       ONLY: field
  USE IO_constants, ONLY: faccs,cprcs,lstn,ffmts
  USE iofiles,      ONLY: ofile,ifile
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

  END SUBROUTINE write_regr_v10_c_wrapper

END INTERFACE

END MODULE FortranCMix
