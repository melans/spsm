MODULE gui_constants
!
! Modules
  USE numbers, ONLY: rp=>dp,one
!
! Parameters
!
! Integer parameters
  INTEGER, PARAMETER, PUBLIC :: mcol=18   ! - number of shading colours -
  INTEGER, PARAMETER, PUBLIC :: nfont=3   ! - number of default fonts -
  INTEGER, PARAMETER, PUBLIC :: mtitle=52 ! - maximum length of title -
!
! Real parameters
  REAL(KIND=rp), PARAMETER, PUBLIC :: dleft  =0.15_rp ! - default left margin -
  REAL(KIND=rp), PARAMETER, PUBLIC :: dright =0.05_rp ! - default right margin -
  REAL(KIND=rp), PARAMETER, PUBLIC :: dbottom=0.10_rp ! - default bottom margin -
  REAL(KIND=rp), PARAMETER, PUBLIC :: dtop   =0.10_rp ! - default top margin -
  REAL(KIND=rp), PARAMETER, PUBLIC :: djqual=one      ! - default JPEG quality -
!
! Character parameters
  CHARACTER(LEN=15), PARAMETER, PUBLIC :: dfont(nfont)= & ! - default fonts -
     (/'Arial          ','MS Sans Serif  ','Times New Roman'/)
!
END MODULE gui_constants