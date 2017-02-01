!$Id: CPT_constants.f90 1180 2011-01-31 20:32:13Z lsong $
! Author: Simon Mason
MODULE CPT_constants
!
! Parameters
!
! Integer parameters
  INTEGER, PARAMETER, PUBLIC :: na=4         ! - number of analytical methods -
  INTEGER, PARAMETER, PUBLIC :: ng=3         ! - number of categories -
  INTEGER, PARAMETER, PUBLIC :: nts=ng-1     ! - number of thresholds -
  INTEGER, PARAMETER, PUBLIC :: mnt=4        ! - minimum length of training period -
  INTEGER, PARAMETER, PUBLIC :: nb=11        ! - number of probability bins -
  INTEGER, PARAMETER, PUBLIC :: nds=3        ! - number of data structures -
  INTEGER, PARAMETER, PUBLIC :: nsq=3        ! - number of sequences -
  INTEGER, PARAMETER, PUBLIC :: npo=3        ! - number of principal component calculation options -
  INTEGER, PARAMETER, PUBLIC :: nmo=4        ! - number of missing value replacement options -
  INTEGER, PARAMETER, PUBLIC :: nstd=4       ! - number of standardization options -
  INTEGER, PARAMETER, PUBLIC :: nev=3        ! - number of error-variance options -
  INTEGER, PARAMETER, PUBLIC :: nwr=2        ! - number of weather roulette options -
  INTEGER, PARAMETER, PUBLIC :: ipm=100      ! - maximum percentage of missing values -
  INTEGER, PARAMETER, PUBLIC :: nskill=9+nts ! - number of skill scores -
  INTEGER, PARAMETER, PUBLIC :: nscore=16    ! - number of scores -
  INTEGER, PARAMETER, PUBLIC :: nps=3*ng+5   ! - number of probabilistic scores -
  INTEGER, PARAMETER, PUBLIC :: nep=1000     ! - number of points on exceedance probability curve -
!
! Logical parameters
  LOGICAL, PARAMETER, PUBLIC :: lscore(nscore) = & ! - positively oriented score flag -
     (/ .true., .true., .true., .true., .true.,.false.,.false.,.false., &
        .true., .true., .true., .true., .true., .true., .true., .true./)
!
END MODULE CPT_constants