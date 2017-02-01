MODULE iofiles
!
! Modules
  USE IO_constants
  USE numbers,        ONLY: rp,one
  USE time
  USE time_constants, ONLY: nmn
!
! Implicit declarations
  IMPLICIT NONE
!
! Derived type definitions
!
! - file format -
  TYPE fformat
     INTEGER :: iver ! - CPT version number -
     INTEGER :: ifmt ! - file format -
     INTEGER :: iacc ! - file access -
     INTEGER :: iprc ! - data precision -
     INTEGER :: lrec ! - record length -
  END TYPE fformat
!
! - input files -
  TYPE ifile
     CHARACTER(LEN= lfil) :: ffile  ! - file -
     CHARACTER(LEN= ldir) :: fdir   ! - file directory -
     CHARACTER(LEN= lnam) :: fname  ! - file name -
     CHARACTER(LEN= lext) :: fext   ! - file extension -
     CHARACTER(LEN= lstr) :: cgss   ! - field structure -
     CHARACTER(LEN= lprd) :: cdate1 ! - first date in file -
     CHARACTER(LEN= lprd) :: cdaten ! - last date in file -
     CHARACTER(LEN= lprd-4) :: cssn   ! - season -
!
     INTEGER :: igrid               ! - data structure flag -
     INTEGER :: igeog               ! - geographical reference flag -
     INTEGER :: iseq                ! - time sequence flag -
     INTEGER :: nfs                 ! - number of fields (including ensemble members) -
     INTEGER :: nem                 ! - number of ensemble members -
     INTEGER :: nls                 ! - number of lagged fields -
     INTEGER :: nfl                 ! - total number of fields and lagged fields -
     INTEGER :: nal                 ! - number of additional lagged fields -
     INTEGER :: nt                  ! - number of time steps -
     INTEGER :: nat                 ! - number of available time steps -
     INTEGER :: it1                 ! - index of first date of interest -
     INTEGER :: ntag                ! - number of additional tag lines -
     INTEGER :: mdate               ! - date modified -
!
     TYPE(fformat) :: ffmt          ! - file format -
!
     TYPE(period) :: period1        ! - period of first data -
     TYPE(period) :: periodn        ! - period of last data -
     TYPE(date) :: fdate            ! - first date of interest -
!
     LOGICAL :: lset                ! - file flag -
     LOGICAL :: lstack              ! - stacked fields flag -
     LOGICAL :: lensemble           ! - ensemble fields flag -
  END TYPE ifile
!
! - version 10 input files -
  TYPE ifile_v10
     CHARACTER(LEN=    lfil) :: ffile  ! - file -
     CHARACTER(LEN=    ldir) :: fdir   ! - file directory -
     CHARACTER(LEN=    lnam) :: fname  ! - file name -
     CHARACTER(LEN=    lext) :: fext   ! - file extension -
     CHARACTER(LEN=lstr_v10) :: cgss   ! - field structure -
     CHARACTER(LEN=    lprd) :: cdate1 ! - first date in file -
     CHARACTER(LEN=    lprd) :: cdaten ! - last date in file -
     CHARACTER(LEN=   nmn-1) :: cssn   ! - season -
!
     INTEGER :: igrid                  ! - data structure flag -
     INTEGER :: igeog                  ! - geographical reference flag -
     INTEGER :: iseq                   ! - time sequence flag -
     INTEGER :: nfs                    ! - number of fields -
     INTEGER :: nls                    ! - number of lagged fields -
     INTEGER :: nal                    ! - number of additional lagged fields -
     INTEGER :: nt                     ! - number of time steps -
     INTEGER :: it1                    ! - index of first date of interest -
     INTEGER :: ntag                   ! - number of additional tag lines -
!
     TYPE(fformat) :: ffmt             ! - file format -
!
     TYPE(period) :: period1           ! - period of first data -
     TYPE(period) :: periodn           ! - period of last data -
     TYPE(date) :: fdate               ! - first date of interest -
!
     LOGICAL :: lset                   ! - file flag -
  END TYPE ifile_v10
!
! - version 9input files -
  TYPE ifile_v9
     INTEGER :: igrid            ! - data structure flag -
     INTEGER :: igeog            ! - geographical reference flag -
     INTEGER :: iyr1             ! - first year in file -
     INTEGER :: imn1             ! - first month in file -
     INTEGER :: idy1             ! - first day in file -
     INTEGER :: it1              ! - index of first date of interest -
     INTEGER :: iseq             ! - sequence flag -
!
     TYPE(fformat) :: ffmt       ! - file format -
!
     CHARACTER(LEN=190) :: ffile ! - file -
     CHARACTER(LEN=150) :: fdir  ! - file directory -
     CHARACTER(LEN= 36) :: fname ! - file name -
     CHARACTER(LEN=  4) :: fext  ! - file extension -
!
     LOGICAL :: lset             ! - file flag -
  END TYPE ifile_v9
!
! - output files -
  TYPE ofile
     INTEGER :: nfile             ! - file number -
!
     CHARACTER(LEN=lfil) :: ffile ! - file -
     CHARACTER(LEN=ldir) :: fdir  ! - file directory -
     CHARACTER(LEN=lnam) :: fname ! - file name -
     CHARACTER(LEN=lext) :: fext  ! - file extension -
     CHARACTER(LEN=ldsc) :: desc  ! - file description -
!
     TYPE(fformat) :: ffmt        ! - file format -
!
     LOGICAL :: lset              ! - file flag -
  END TYPE ofile
!
! Scalars
!
! Integer scalars
  INTEGER, PUBLIC :: nfile ! - file number -
  INTEGER, PUBLIC :: ipic  ! - graphics type flag -
!
! Character scalars
  CHARACTER(LEN= 150), PUBLIC :: ddir      ! - default data file directory -
  CHARACTER(LEN= 150), PUBLIC :: ddir_old  ! - old data file directory -
  CHARACTER(LEN= 150), PUBLIC :: odir      ! - default output file directory -
  CHARACTER(LEN= 150), PUBLIC :: odir_old  ! - old output file directory -
  CHARACTER(LEN=  36), PUBLIC :: fname     ! - file name -
  CHARACTER(LEN=  36), PUBLIC :: fname_old ! - old file name -
  CHARACTER(LEN= 186), PUBLIC :: ffile     ! - file -
  CHARACTER(LEN= 186), PUBLIC :: ffile_old ! - old file -
  CHARACTER(LEN=   4), PUBLIC :: fext      ! - file extension -
!
  CHARACTER(LEN=1), PUBLIC :: cxy ! - current input file -
!
! Derived type scalars
  TYPE(ifile), PUBLIC :: xfile  ! - X data input file -
  TYPE(ifile), PUBLIC :: yfile  ! - Y data input file -
  TYPE(ifile), PUBLIC :: zfile  ! - X forecast data input file -
  TYPE(ifile), PUBLIC :: bkfile ! - backup input file -
!
  TYPE(ofile), PUBLIC :: xofile ! - X input data output file -
  TYPE(ofile), PUBLIC :: xifile ! - interpolated X data output file -
  TYPE(ofile), PUBLIC :: yofile ! - Y input data output file -
  TYPE(ofile), PUBLIC :: yhfile ! - cross-validated predictions output file -
  TYPE(ofile), PUBLIC :: yrfile ! - retroactive predictions output file -
  TYPE(ofile), PUBLIC :: rpfile ! - retroactive forecast probabilities output file -
  TYPE(ofile), PUBLIC :: rlfile ! - retroactive prediction limits output file -
  TYPE(ofile), PUBLIC :: xefile ! - X eigenvalues output file -
  TYPE(ofile), PUBLIC :: yefile ! - Y eigenvalues output file -
  TYPE(ofile), PUBLIC :: xlfile ! - X spatial loadings output file -
  TYPE(ofile), PUBLIC :: ylfile ! - Y spatial loadings output file -
  TYPE(ofile), PUBLIC :: xsfile ! - X temporal scores output file -
  TYPE(ofile), PUBLIC :: ysfile ! - Y temporal scores output file -
  TYPE(ofile), PUBLIC :: xmfile ! - X homogeneous covariance maps output file -
  TYPE(ofile), PUBLIC :: ymfile ! - Y homogeneous covariance maps output file -
  TYPE(ofile), PUBLIC :: xtfile ! - X homogeneous covariance maps time series output file -
  TYPE(ofile), PUBLIC :: ytfile ! - Y homogeneous covariance maps time series output file -
  TYPE(ofile), PUBLIC :: ccfile ! - canonical correlations output file -
  TYPE(ofile), PUBLIC :: rbfile ! - regression coefficients output file -
  TYPE(ofile), PUBLIC :: pbfile ! - PC regression coefficients output file -
  TYPE(ofile), PUBLIC :: fpfile ! - forecast probabilities output file -
  TYPE(ofile), PUBLIC :: fofile ! - forecast odds output file -
  TYPE(ofile), PUBLIC :: fvfile ! - forecasts output file -
  TYPE(ofile), PUBLIC :: fsfile ! - forecast ensembles output file -
  TYPE(ofile), PUBLIC :: fefile ! - prediction error variance output file -
  TYPE(ofile), PUBLIC :: flfile ! - prediction limits output file -
  TYPE(ofile), PUBLIC :: fxfile ! - X temporal scores for forecasts output file -
  TYPE(ofile), PUBLIC :: skfile ! - skill output file -
  TYPE(ofile), PUBLIC :: pvfile ! - p-values output file -
  TYPE(ofile), PUBLIC :: rofile ! - ROC output file (individual point) -
  TYPE(ofile), PUBLIC :: rrfile ! - ROC output file (all points) -
  TYPE(ofile), PUBLIC :: atfile ! - attributes diagram output file -
  TYPE(ofile), PUBLIC :: wrfile ! - weather roulette output file -
!
CONTAINS
!
!
 SUBROUTINE init_iofiles ()
!
! Initialises data structures and output file numbers and descriptions
!
! Executable Statements
!
! Initialise output files
! - X input data -
  xofile%nfile=1
  xofile%desc='X Input Data'
! - interpolated X data -
  xifile%nfile=2
  xifile%desc='Interpolated X Data'
! - Y input data -
  yofile%nfile=3
  yofile%desc='Y Input Data'
! - cross-validated predictions -
  yhfile%nfile=4
  yhfile%desc='Cross-Validated Predictions'
! - retroactive predictions -
  yrfile%nfile=5
  yrfile%desc='Retroactive Predictions'
! - retroactive forecast probabilities -
  rpfile%nfile=6
  rpfile%desc='Retroactive Forecast Probabilities'
! - retroactive prediction limits -
  rlfile%nfile=7
  rlfile%desc='Retroactive Lower Prediction Limits'
! - X eigenvalues -
  xefile%nfile=8
  xefile%desc='X Eigenvalues'
! - X spatial loadings -
  xlfile%nfile=9
  xlfile%desc='X Spatial Loadings'
! - X temporal scores -
  xsfile%nfile=10
  xsfile%desc='X Temporal Scores'
! - Y eigenvalues -
  yefile%nfile=11
  yefile%desc='Y Eigenvalues'
! - Y spatial loadings -
  ylfile%nfile=12
  ylfile%desc='Y Spatial Loadings'
! - Y temporal scores -
  ysfile%nfile=13
  ysfile%desc='Y Temporal Scores'
! - canonical correlation output file settings -
  ccfile%nfile=14
  ccfile%desc='Canonical Correlations'
! - X homogeneous maps -
  xmfile%nfile=15
  xmfile%desc='X CCA Map Loadings'
! - X homogeneous time series -
  xtfile%nfile=16
  xtfile%desc='X CCA Map Series'
! - Y homogeneous maps -
  ymfile%nfile=17
  ymfile%desc='Y CCA Map Loadings'
! - Y homogeneous time series -
  ytfile%nfile=18
  ytfile%desc='Y CCA Map Series'
! - regression output file settings -
  rbfile%nfile=19
  rbfile%desc='Regression Coefficients'
! - PC regression output file settings -
  pbfile%nfile=20
  pbfile%desc='PC Regression Coefficients'
! - forecast probabilities -
  fpfile%nfile=21
  fpfile%desc='Forecast Probabilities'
! - forecast odds -
  fofile%nfile=22
  fofile%desc='Forecast Odds'
! - forecasts -
  fvfile%nfile=23
  fvfile%desc='Forecasts'
! - forecast ensembles -
  fsfile%nfile=24
  fsfile%desc='Forecast Ensembles'
! - prediction error variances -
  fefile%nfile=25
  fefile%desc='Prediction Error Variances'
! - prediction limits -
  flfile%nfile=26
  flfile%desc='Prediction Limits'
! - predictor time scores -
  fxfile%nfile=27
  fxfile%desc='Predictor Time Scores'
! - skill scores file -
  skfile%nfile=28
  skfile%desc='Skill Scores'
! - p-values file -
  pvfile%nfile=29
  pvfile%desc='P-values'
! - ROC file -
  rofile%nfile=30
  rofile%desc='ROC Results'
! - ROC file -
  rrfile%nfile=31
  rrfile%desc='ROC Results'
! - attributes diagram file -
  atfile%nfile=32
  atfile%desc='Reliability Results'
! - weather roulette file -
  wrfile%nfile=33
  wrfile%desc='Weather Roulette'
!
  RETURN
 END SUBROUTINE init_iofiles
!
!
!
 SUBROUTINE init_ifile (afile,dir)
!
! Initialises an input file
!
! Modules
  USE labels, ONLY: cg_dsds_l
!
! Arguments
!
! Input scalars
! - optional input scalars -
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: dir ! - directory -
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC LEN
  INTRINSIC PRESENT
  INTRINSIC REPEAT
!
! Executable Statements
!
! Set input file settings
  afile%ffile=REPEAT(' ',LEN(afile%ffile))
  IF (PRESENT(dir)) afile%fdir=dir
  afile%fname=REPEAT(' ',LEN(afile%fname))
  afile%fext=REPEAT(' ',LEN(afile%fext))
  afile%cgss=cg_dsds_l(0)
  afile%cdate1='N/A'
  afile%cdaten='N/A'
  afile%cssn=REPEAT(' ',LEN(afile%cssn))
  afile%igrid=0
  afile%igeog=0
  afile%iseq=0
  afile%nfs=0
  afile%nem=0
  afile%nls=0
  afile%nfl=0
  afile%nal=0
  afile%nt=0
  afile%nat=0
  afile%it1=0
  afile%ntag=0
  afile%mdate=0
  afile%ffmt%iver=0
  afile%ffmt%iacc=1
  afile%ffmt%ifmt=2
  afile%ffmt%iprc=2
  afile%ffmt%lrec=0
  afile%period1=0
  afile%periodn=0
  afile%fdate=0
  afile%lset=.false.
  afile%lstack=.false.
  afile%lensemble=.false.
!
  RETURN
 END SUBROUTINE init_ifile
!
!
!
 SUBROUTINE init_ofiles ()
!
! Initialises output files
!
! Executable Statements
!
! Input files
! - X input data -
  CALL init_ofile (xofile)
! - interpolated X data -
  CALL init_ofile (xifile)
! - Y input data -
  CALL init_ofile (yofile)
!
! Prediction file settings
! - cross-validated predictions -
  CALL init_ofile (yhfile)
! - retroactive predictions -
  CALL init_ofile (yrfile)
! - retroactive forecast probabilities -
  CALL init_ofile (rpfile)
! - retroactive prediction limits -
  CALL init_ofile (rlfile)
!
! X output file settings
! - eigenvalues -
  CALL init_ofile (xefile)
! - spatial loadings -
  CALL init_ofile (xlfile)
! - temporal scores -
  CALL init_ofile (xsfile)
! - homogeneous maps -
  CALL init_ofile (xmfile)
! - homogeneous time series -
  CALL init_ofile (xtfile)
!
! Y output file settings
! - eigenvalues -
  CALL init_ofile (yefile)
! - spatial loadings -
  CALL init_ofile (ylfile)
! - temporal scores -
  CALL init_ofile (ysfile)
! - homogeneous maps -
  CALL init_ofile (ymfile)
! - homogeneous time series -
  CALL init_ofile (ytfile)
!
! Canonical correlation output file settings
  CALL init_ofile (ccfile)
!
! Regression output file settings
  CALL init_ofile (rbfile)
!
! PC regression output file settings
  CALL init_ofile (pbfile)
!
! Forecast files
! - forecast probabilities -
  CALL init_ofile (fpfile)
! - forecast odds -
  CALL init_ofile (fofile)
! - forecasts -
  CALL init_ofile (fvfile)
! - forecast ensembles -
  CALL init_ofile (fsfile)
! - prediction error variances -
  CALL init_ofile (fefile)
! - prediction limits -
  CALL init_ofile (flfile)
! - predictor time scores -
  CALL init_ofile (fxfile)
!
! Skill scores and p-values files
  CALL init_ofile (skfile)
  CALL init_ofile (pvfile)
!
! Verification files
! - ROC files -
  CALL init_ofile (rofile)
  CALL init_ofile (rrfile)
! - attributes diagram file -
  CALL init_ofile (atfile)
! - weather roulette file -
  CALL init_ofile (wrfile)
!
  RETURN
 END SUBROUTINE init_ofiles
!
!
!
 SUBROUTINE init_ofile (f)
!
! Initialises an output file
!
! Arguments
!
! Input/output scalars
  TYPE(ofile), INTENT(INOUT) :: f ! - output file -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC LEN
  INTRINSIC REPEAT
!
! Executable Statements
  f%lset=.false.
  f%fname=REPEAT(' ',LEN(f%fname))
  f%ffile=REPEAT(' ',LEN(f%ffile))
  f%fdir=odir
  f%fext=REPEAT(' ',LEN(f%fext))
  f%ffmt%iacc=1
  f%ffmt%ifmt=2
  f%ffmt%iprc=2
  f%ffmt%lrec=0
!
  RETURN
 END SUBROUTINE init_ofile
!
!
!
 SUBROUTINE file_reset (lfile,fname)
!
! Arguments
!
! Output scalars
  CHARACTER(LEN=*), INTENT(OUT) :: fname ! - file name -
!
  LOGICAL, INTENT(OUT) :: lfile ! - file flag -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC LEN
  INTRINSIC REPEAT
!
! Executable Statements
!
! Reset file flag
  lfile=.false.
!
! Reset file name
  fname=REPEAT(' ',LEN(fname))
!
  RETURN
 END SUBROUTINE file_reset
!
!
!
 SUBROUTINE files_reset ()
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC LEN_TRIM
!
! Executable Statements
!
! Input data with missing values estimated
! - X input data -
  IF ((xofile%lset).OR.(LEN_TRIM(xofile%fname)>0)) CALL file_reset (xofile%lset,xofile%fname)
! - interpolated X data -
  IF ((xifile%lset).OR.(LEN_TRIM(xifile%fname)>0)) CALL file_reset (xifile%lset,xifile%fname)
! - Y input data -
  IF ((yofile%lset).OR.(LEN_TRIM(yofile%fname)>0)) CALL file_reset (yofile%lset,yofile%fname)
!
! Predictions
! - cross-validated predictions -
  IF ((yhfile%lset).OR.(LEN_TRIM(yhfile%fname)>0)) CALL file_reset (yhfile%lset,yhfile%fname)
! - retroactive predictions -
  IF ((yrfile%lset).OR.(LEN_TRIM(yrfile%fname)>0)) CALL file_reset (yrfile%lset,yrfile%fname)
! - retroactive forecast probabilities -
  IF ((rpfile%lset).OR.(LEN_TRIM(rpfile%fname)>0)) CALL file_reset (rpfile%lset,rpfile%fname)
! - retroactive prediction limits -
  IF ((rlfile%lset).OR.(LEN_TRIM(rlfile%fname)>0)) CALL file_reset (rlfile%lset,rlfile%fname)
!
! Eigenvalues
! - X eigenvalues -
  IF ((xefile%lset).OR.(LEN_TRIM(xefile%fname)>0)) CALL file_reset (xefile%lset,xefile%fname)
! - Y eigenvalues -
  IF ((yefile%lset).OR.(LEN_TRIM(yefile%fname)>0)) CALL file_reset (yefile%lset,yefile%fname)
!
! Spatial loadings
! - X spatial loadings -
  IF ((xlfile%lset).OR.(LEN_TRIM(xlfile%fname)>0)) CALL file_reset (xlfile%lset,xlfile%fname)
! - Y spatial loadings -
  IF ((ylfile%lset).OR.(LEN_TRIM(ylfile%fname)>0)) CALL file_reset (ylfile%lset,ylfile%fname)
!
! Temporal scores
! - X scores -
  IF ((xsfile%lset).OR.(LEN_TRIM(xsfile%fname)>0)) CALL file_reset (xsfile%lset,xsfile%fname)
! - Y scores -
  IF ((ysfile%lset).OR.(LEN_TRIM(ysfile%fname)>0)) CALL file_reset (ysfile%lset,ysfile%fname)
!
! CCA results
! - canonical correlations -
  IF ((ccfile%lset).OR.(LEN_TRIM(ccfile%fname)>0)) CALL file_reset (ccfile%lset,ccfile%fname)
! - X homogeneous maps -
  IF ((xmfile%lset).OR.(LEN_TRIM(xmfile%fname)>0)) CALL file_reset (xmfile%lset,xmfile%fname)
! - Y homogeneous maps -
  IF ((ymfile%lset).OR.(LEN_TRIM(ymfile%fname)>0)) CALL file_reset (ymfile%lset,ymfile%fname)
! - X homogeneous map series -
  IF ((xtfile%lset).OR.(LEN_TRIM(xtfile%fname)>0)) CALL file_reset (xtfile%lset,xtfile%fname)
! - Y homogeneous map series -
  IF ((ytfile%lset).OR.(LEN_TRIM(ytfile%fname)>0)) CALL file_reset (ytfile%lset,ytfile%fname)
!
! Regression coefficients
! - regression coefficients -
  IF ((rbfile%lset).OR.(LEN_TRIM(rbfile%fname)>0)) CALL file_reset (rbfile%lset,rbfile%fname)
! - PC regression coefficients -
  IF ((pbfile%lset).OR.(LEN_TRIM(pbfile%fname)>0)) CALL file_reset (pbfile%lset,pbfile%fname)
!
! Forecasts
! - forecast probabilities -
  IF ((fpfile%lset).OR.(LEN_TRIM(fpfile%fname)>0)) CALL file_reset (fpfile%lset,fpfile%fname)
! - forecast odds -
  IF ((fofile%lset).OR.(LEN_TRIM(fofile%fname)>0)) CALL file_reset (fofile%lset,fofile%fname)
! - forecast values -
  IF ((fvfile%lset).OR.(LEN_TRIM(fvfile%fname)>0)) CALL file_reset (fvfile%lset,fvfile%fname)
! - forecast ensembles -
  IF ((fsfile%lset).OR.(LEN_TRIM(fsfile%fname)>0)) CALL file_reset (fsfile%lset,fsfile%fname)
! - prediction error variances -
  IF ((fefile%lset).OR.(LEN_TRIM(fefile%fname)>0)) CALL file_reset (fefile%lset,fefile%fname)
! - prediction limits -
  IF ((flfile%lset).OR.(LEN_TRIM(flfile%fname)>0)) CALL file_reset (flfile%lset,flfile%fname)
! - predictor forecast scores -
  IF ((fxfile%lset).OR.(LEN_TRIM(fxfile%fname)>0)) CALL file_reset (fxfile%lset,fxfile%fname)
!
! Skill scores and p-values
  IF ((skfile%lset).OR.(LEN_TRIM(skfile%fname)>0)) CALL file_reset (skfile%lset,skfile%fname)
  IF ((pvfile%lset).OR.(LEN_TRIM(pvfile%fname)>0)) CALL file_reset (pvfile%lset,pvfile%fname)
!
! Verification
! - ROC results -
  IF ((rofile%lset).OR.(LEN_TRIM(rofile%fname)>0)) CALL file_reset (rofile%lset,rofile%fname)
  IF ((rrfile%lset).OR.(LEN_TRIM(rrfile%fname)>0)) CALL file_reset (rrfile%lset,rrfile%fname)
! - reliability results -
  IF ((atfile%lset).OR.(LEN_TRIM(atfile%fname)>0)) CALL file_reset (atfile%lset,atfile%fname)
! - weather roulette -
  IF ((wrfile%lset).OR.(LEN_TRIM(wrfile%fname)>0)) CALL file_reset (wrfile%lset,wrfile%fname)
!
  RETURN
 END SUBROUTINE files_reset
!
!
!
 SUBROUTINE open_infile (iin,ffile,lfmt,lrecl,ifail)
!
! Modules
  USE IO_constants, ONLY: lfli
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iin ! - input file unit number -
!
  CHARACTER(LEN=*), INTENT(IN) :: ffile ! - input file -
!
  LOGICAL, INTENT(IN) :: lfmt  ! - formatted file flag -
  LOGICAL, INTENT(IN) :: lrecl ! - record length flag -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Locals
!
! Local scalars
  INTEGER :: ios ! - IO status -
!
  CHARACTER(LEN=11) :: cfmt ! - formatting -
!
! Executabel Statements
!
! Set formatting
  IF (lfmt) THEN
     cfmt='formatted'
  ELSE
     cfmt='unformatted'
  END IF
!
! Open file
  IF (lrecl) THEN
     OPEN (UNIT=iin,FILE=ffile,ACCESS='sequential',ACTION='read',FORM=cfmt,IOSTAT=ios,RECL=lfli,STATUS='old')
  ELSE
     OPEN (UNIT=iin,FILE=ffile,ACCESS='sequential',ACTION='read',FORM=cfmt,IOSTAT=ios,STATUS='old')
  END IF
  SELECT CASE (ios)
   CASE (0)
     ifail=0
   CASE (128)
     ifail=1
   CASE (134)
     ifail=2
   CASE DEFAULT
     ifail=3
  END SELECT
!
  RETURN
 END SUBROUTINE open_infile
!
!
!
 FUNCTION file_version(afile)
!
! Determines CPT file format version
!
! Modules
  USE IO_constants,  ONLY: iin,cxmlns,cxmlns_cpt,ltag
!
! Function type
  INTEGER :: file_version
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: afile ! - filename -
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
!
  CHARACTER(LEN=ltag) :: ctag ! - CPT file tag -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC INDEX
  INTRINSIC LEN
!
! Executable Statements
!
! Open file
  CALL open_infile (iin,afile,.true.,.false.,ifail)
  IF (ifail/=0) THEN
     file_version=-ifail
     RETURN
  END IF
!
! Identify file version
  READ (UNIT=iin,FMT='(A)',IOSTAT=ifail) ctag
  CLOSE (UNIT=iin)
  IF (ifail/=0) THEN
     file_version=-4
     RETURN
  END IF
!
! Check for CPT XML namespace flag
  IF (INDEX(ctag,cxmlns//':cpt='//cxmlns_cpt(1:LEN(cxmlns_cpt)-1))>0) THEN
     file_version=10
  ELSE
     file_version=9
  END IF
!
  RETURN
 END FUNCTION file_version
!
!
!
 SUBROUTINE ifile_v10_to_v11 (afile_v10,afile_v11)
!
! Converts version 10 input files to version 11
!
! Arguments
!
! Input scalars
  TYPE(ifile_v10), INTENT(IN) :: afile_v10 ! - version 10 file -
!
! Output scalars
  TYPE(ifile), INTENT(OUT) :: afile_v11 ! - version 11 file -
!
! Executable Statements
!
! Convert file
  afile_v11%ffile=afile_v10%ffile           ! - file -
  afile_v11%fdir=afile_v10%fdir             ! - file directory -
  afile_v11%fname=afile_v10%fname           ! - file name -
  afile_v11%fext=afile_v10%fext             ! - file extension -
  afile_v11%cgss=afile_v10%cgss             ! - file structure -
  afile_v11%cdate1=afile_v10%cdate1         ! - first date in file -
  afile_v11%cdaten=afile_v10%cdaten         ! - last date in file -
  afile_v11%cssn=afile_v10%cssn             ! - season -
  afile_v11%igrid=afile_v10%igrid           ! - data structure flag -
  afile_v11%igeog=afile_v10%igeog           ! - geographical reference flag -
  afile_v11%iseq=afile_v10%iseq             ! - time sequence flag -
  afile_v11%nfs=afile_v10%nfs               ! - number of fields -
  afile_v11%nem=1                           ! - number of ensemble members -
  afile_v11%nls=afile_v10%nls               ! - number of lagged fields -
  afile_v11%nfl=afile_v11%nfs*afile_v11%nls ! - total number of fields and lagged fields -
  afile_v11%nal=afile_v10%nal               ! - number of additional lagged fields -
  afile_v11%nt=afile_v10%nt                 ! - number of time steps -
  afile_v11%nat=afile_v10%nt                ! - number of available time steps -
  afile_v11%it1=afile_v10%it1               ! - index of first date of interest -
  afile_v11%ntag=afile_v10%ntag             ! - number of additional tag lines -
  afile_v11%mdate=0                         ! - date last modified -
  afile_v11%ffmt=afile_v10%ffmt             ! - file format -
  afile_v11%period1=afile_v10%period1       ! - period of first data -
  afile_v11%periodn=afile_v10%periodn       ! - period of last data -
  afile_v11%fdate=afile_v10%fdate           ! - first date of interest -
  afile_v11%lset=afile_v10%lset             ! - file flag -
  afile_v11%lstack=.false.                  ! - stacked fields flag -
  afile_v11%lensemble=.false.               ! - ensemble fields flag -
!
  RETURN
 END SUBROUTINE ifile_v10_to_v11
!
!
!
 SUBROUTINE ifile_v9_to_v11 (afile_v9,afile_v11)
!
! Converts version 9 input files to version 11
!
! Modules
  USE labels, ONLY: cg_dsds_l
  USE time,   ONLY: get_cdate,get_cssn
!
! Arguments
!
! Input scalars
  TYPE(ifile_v9), INTENT(IN) :: afile_v9 ! - version 9 file -
!
! Output scalars
  TYPE(ifile), INTENT(OUT) :: afile_v11 ! - version 11 file -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC TRIM
!
! Executable Statements
!
! Convert file
  afile_v11%ffile=afile_v9%ffile                            ! - file -
  afile_v11%fdir=afile_v9%fdir                              ! - file directory -
  afile_v11%fname=afile_v9%fname                            ! - file name -
  afile_v11%fext=afile_v9%fext                              ! - file extension -
  afile_v11%igrid=afile_v9%igrid                            ! - data structure flag -
  afile_v11%igeog=afile_v9%igeog                            ! - geographical reference flag -
  afile_v11%iseq=afile_v9%iseq                              ! - time sequence flag -
  afile_v11%nfs=1                                           ! - number of fields -
  afile_v11%nem=1                                           ! - number of ensemble members -
  afile_v11%nls=1                                           ! - number of lagged fields -
  afile_v11%nfl=1                                           ! - total number of fields and lagged fields -
  afile_v11%nal=0                                           ! - number of additional lagged fields -
  afile_v11%nt=1                                            ! - number of time steps -
  afile_v11%nat=1                                           ! - number of available time steps -
  afile_v11%it1=afile_v9%it1                                ! - index of first date of interest -
  afile_v11%ntag=0                                          ! - number of additional tag lines -
  afile_v11%mdate=0                                         ! - date last modified -
  afile_v11%ffmt=afile_v9%ffmt                              ! - file format -
  afile_v11%period1%sdate%iyr=afile_v9%iyr1                 ! - first year in file -
  afile_v11%period1%sdate%imn=afile_v9%imn1                 ! - first month in file -
  afile_v11%period1%sdate%idy=afile_v9%idy1                 ! - first day in file -
  afile_v11%period1%edate=afile_v11%period1%sdate           ! - end date of first period in file -
  afile_v11%periodn=0                                       ! - period of last data -
  afile_v11%fdate=0                                         ! - first date of interest -
  afile_v11%lset=afile_v9%lset                              ! - file flag -
  afile_v11%cgss=TRIM(cg_dsds_l(afile_v11%igrid))           ! - file structure -
  afile_v11%cdate1=get_cdate(afile_v11%period1,2)           ! - first date in file -
  afile_v11%cdaten=' '                                      ! - last date in file -
  afile_v11%cssn=get_cssn(afile_v11%period1,afile_v11%iseq) ! - season -
  afile_v11%lstack=.false.                                  ! - stacked fields flag -
  afile_v11%lensemble=.false.                               ! - ensemble fields flag -
!
  RETURN
 END SUBROUTINE ifile_v9_to_v11
!
!
!
 SUBROUTINE get_old_file (ftype,ffile,fdir,fname,ifail,fext)
!
! Modules
  USE IO_constants, ONLY: cdir
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: ftype ! - file type -
!
! Input/output scalars
  CHARACTER(LEN=*), INTENT(INOUT) :: fdir ! - file directory -
!
! - optional input/output scalars -
  CHARACTER(LEN=*), INTENT(INOUT), OPTIONAL :: fext ! - selected file extension -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
  CHARACTER(LEN=*), INTENT(OUT) :: ffile ! - selected file -
  CHARACTER(LEN=*), INTENT(OUT) :: fname ! - selected file name -
!
! Locals
!
! Local scalars
  INTEGER :: i1,i2,i3 ! - position of filename -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC INDEX
  INTRINSIC LEN
  INTRINSIC LEN_TRIM
  INTRINSIC PRESENT
  INTRINSIC REPEAT
!
! Executable Statements
!
! Create window
  ffile=REPEAT(' ',LEN(ffile))
  CALL get_filtered_file (ftype,ffile,1)
!
! Isolate file name
  i3=LEN_TRIM(ffile)
  IF (i3>0) THEN
     i1=INDEX(ffile,cdir,BACK=.true.)
     i2=INDEX(ffile,'.',BACK=.true.)
     IF (i2>i1) THEN
        IF (PRESENT(fext)) THEN
           fname=ffile(i1+1:i2-1)
           fext=ffile(i2+1:i3)
        ELSE
           fname=ffile(i1+1:i3)
        END IF
     ELSE
        fname=ffile(i1+1:i3)
        IF (PRESENT(fext)) fext=REPEAT(' ',LEN(fext))
     END IF
! - identify directory -
     fdir=ffile(1:i1)
     ifail=0
  ELSE
     ifail=1
  END IF
!
  RETURN
 END SUBROUTINE get_old_file
!
!
!
 SUBROUTINE get_new_file (ftype,ffile,fdir,fext,ffilt,fname,ifail)
!
! Modules
  USE IO_constants, ONLY: cdir
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: ftype ! - file type -
  CHARACTER(LEN=*), INTENT(IN) :: fext  ! - file extension -
  CHARACTER(LEN=*), INTENT(IN) :: ffilt ! - file filter -
!
! Input/output scalars
  CHARACTER(LEN=*), INTENT(INOUT) :: fdir ! - file directory -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
  CHARACTER(LEN=*), INTENT(OUT) :: ffile ! - selected file -
  CHARACTER(LEN=*), INTENT(OUT) :: fname ! - selected file name -
!
! Locals
!
! Local scalars
  INTEGER :: i1,i2,i3 ! - position of filename -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC INDEX
  INTRINSIC LEN
  INTRINSIC LEN_TRIM
  INTRINSIC REPEAT
  INTRINSIC TRIM
!
! Executable Statements
!
! Select file window
  DO
     ffile=REPEAT(' ',LEN(ffile))
     CALL get_filtered_file (ftype,ffile,0)
!
! Isolate file name
     i2=LEN_TRIM(ffile)
     IF (i2>0) THEN
        i1=INDEX(ffile,cdir,BACK=.true.)
! - identify directory -
        fdir=ffile(1:i1)
! - remove file extension if present -
        i3=INDEX(ffile(i1+1:i2),ffilt(2:LEN_TRIM(ffilt)),BACK=.true.)
        IF (i3>0) THEN
           i3=INDEX(ffile(1:i2),ffilt(2:LEN_TRIM(ffilt)),BACK=.true.)
           i2=i3-1
        ELSE
           ffile=TRIM(ffile)//fext(INDEX(fext,'.',BACK=.true.):LEN(fext))
        END IF
        fname=ffile(i1+1:i2)
        ifail=0
     ELSE
        ifail=1
        EXIT
     END IF
     ifail=check_new_file(ffile)
     IF (ifail==0) EXIT
  END DO
!
  RETURN
 END SUBROUTINE get_new_file
!
!
!
 FUNCTION check_new_file(ffile)
!
! Returns 0 if filename is OK to use.
!
! Modules
  USE errors, ONLY: error
  USE gui,    ONLY: print_warning
!
! Function type
  INTEGER :: check_new_file
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: ffile ! - file -
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
!
  LOGICAL :: le ! - file exist flag -
!
! Executable Statements
!
! Check file name
! - check for existence of file -
  INQUIRE (FILE=ffile,EXIST=le,IOSTAT=ifail)
  IF (ifail==0) THEN
     IF (le) THEN
        CALL print_warning ('File already exists. Overwriting old file',nopause=.true.)
     END IF
     check_new_file=0
!
! Invalid file name
  ELSE
     ifail=1
     CALL error ('check_new_file',ifail)
     check_new_file=1
  END IF
!
  RETURN
 END FUNCTION check_new_file
!
!
!
 SUBROUTINE get_filtered_file (title,ffile,oldnew)
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: oldnew ! - old file / new file flag -
! 
  CHARACTER(LEN=*), INTENT(IN) :: title ! - file title -
! 
! Output scalars
  CHARACTER(LEN=*), INTENT(OUT) :: ffile ! - file -
!
! Locals
!
! Local scalars
  LOGICAL :: le ! - file existence flag -
!
! Executable Statements
!
! Prompt for file
1 WRITE (UNIT=*,FMT='(2A)',ADVANCE='no') title,': '
  READ (UNIT=*,FMT='(A)',ERR=1, END=1) ffile
!
! Check for existence
  IF (oldnew==1) THEN
     INQUIRE (FILE=ffile,EXIST=le)
     IF (.NOT.le) GOTO 1
  END IF
!
  RETURN
 END SUBROUTINE get_filtered_file
!
!
!
 SUBROUTINE geto_gen (afile)
!
! Modules
  USE IO_constants, ONLY: ffmts
!
! Arguments
!
! Input/output scalars
  TYPE(ofile), INTENT(INOUT) :: afile ! - file -
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC TRIM
!
! Executable Statements
!
! Define output file
  SELECT CASE (TRIM(ffmts(afile%ffmt%ifmt)))
   CASE ('formatted')
     afile%fext='.txt'
     CALL get_new_file (TRIM(afile%desc)//' Output file',afile%ffile,afile%fdir,afile%fext,'Formatted output files', &
          afile%fname,ifail)
   CASE ('unformatted')
     afile%fext='.dat'
     CALL get_new_file (TRIM(afile%desc)//' Output file',afile%ffile,afile%fdir,afile%fext,'Unformatted output files', &
          afile%fname,ifail)
  END SELECT
!
! Confirm selection
  IF (ifail==0) THEN
     afile%lset=.true.
  END IF
!
  RETURN
 END SUBROUTINE geto_gen
END MODULE iofiles
