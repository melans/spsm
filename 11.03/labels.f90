MODULE labels
!
! Modules
  USE CPT_constants, ONLY: na,nds,ng,npo,nps,nscore,nskill,nsq,nstd,nts,nwr
  USE CPT_text,      ONLY: nlang,clang
  USE IO_constants,  ONLY: lstr
!
! Implicit declarations
  IMPLICIT NONE
!
! Arrays
! - labels -
  CHARACTER(LEN=     3), DIMENSION(    na), PUBLIC :: cg_atypes_a   ! - analysis abbreviations -
  CHARACTER(LEN=    39), DIMENSION(    na), PUBLIC :: cg_atypes_t   ! - analysis types -
  CHARACTER(LEN=     1), DIMENSION(    ng), PUBLIC :: cg_cat_a      ! - category abbreviations -
  CHARACTER(LEN=    10), DIMENSION(  0:ng), PUBLIC :: cg_cat_l      ! - category labels -
  CHARACTER(LEN=lstr+1), DIMENSION( 0:nds), PUBLIC :: cg_dsds       ! - data structure descriptions -
  CHARACTER(LEN=lstr+1), DIMENSION( 0:nds), PUBLIC :: cg_dsds_l     ! - data structure descriptions -
  CHARACTER(LEN=    19), DIMENSION(   nds), PUBLIC :: cg_ftype      ! - file structure types -
  CHARACTER(LEN=    50), DIMENSION(   npo), PUBLIC :: cg_pccos      ! - principal components calculation options -
  CHARACTER(LEN=    42), DIMENSION(   nps), PUBLIC :: cg_pvscores_t ! - probabilistic verification scores -
  CHARACTER(LEN=    40), DIMENSION(nscore), PUBLIC :: cg_scores     ! - verification measures -
  CHARACTER(LEN=     6), DIMENSION( 0:nsq), PUBLIC :: cg_seq        ! - date sequence types -
  CHARACTER(LEN=     7), DIMENSION( 0:nsq), PUBLIC :: cg_seqs       ! - date sequence types -
  CHARACTER(LEN=     6), DIMENSION( 0:nsq), PUBLIC :: cg_seq_l      ! - date sequence types -
  CHARACTER(LEN=     7), DIMENSION( 0:nsq), PUBLIC :: cg_seqs_l     ! - date sequence types -
  CHARACTER(LEN=    32), DIMENSION(nskill), PUBLIC :: cg_skill_t    ! - skill measures -
  CHARACTER(LEN=    25), DIMENSION(  nstd), PUBLIC :: cg_stds_t     ! - standardizations -
  CHARACTER(LEN=    11), DIMENSION(   nts), PUBLIC :: cg_thr_l      ! - threshold labels -
  CHARACTER(LEN=    26), DIMENSION(   nwr), PUBLIC :: cg_wrlts_t    ! - weather roulette options -
!
  CHARACTER(LEN=    13), DIMENSION(0:ng,2), PUBLIC :: cg_bss        ! - Brier, ranked probability score and skill scores -
!
! - menu labels -
  CHARACTER(LEN=    40), DIMENSION(    na), PUBLIC :: cm_atypes     ! - Analysis Types -
!
! Scalars
!
! - labels -
  CHARACTER(LEN= 9), PUBLIC :: cg_actions_u   ! - ACTIONS -
  CHARACTER(LEN= 5), PUBLIC :: cg_all         ! - All -
  CHARACTER(LEN=54), PUBLIC :: cg_approx      ! - Approximate data limits in brackets -
  CHARACTER(LEN=25), PUBLIC :: cg_attrib_t    ! - Attributes Diagram -
  CHARACTER(LEN=26), PUBLIC :: cg_attribs_t   ! - Attributes Diagrams -
  CHARACTER(LEN=11), PUBLIC :: cg_browse_l    ! - browse -
  CHARACTER(LEN=23), PUBLIC :: cg_cancor_t    ! - Canonical Correlation -
  CHARACTER(LEN=25), PUBLIC :: cg_ccacopts_t  ! - CCA Calculation Options -
  CHARACTER(LEN=17), PUBLIC :: cg_ccamaps_t   ! - CCA Maps -
  CHARACTER(LEN=18), PUBLIC :: cg_ccaopts_t   ! - CCA Options -
  CHARACTER(LEN=25), PUBLIC :: cg_climate     ! - Climatological period -
  CHARACTER(LEN=25), PUBLIC :: cg_climate_t   ! - Climatological Period -
  CHARACTER(LEN=20), PUBLIC :: cg_cv_t        ! - Cross-validated -
  CHARACTER(LEN= 8), PUBLIC :: cg_data        ! - Data -
  CHARACTER(LEN=17), PUBLIC :: cg_data1       ! - First data -
  CHARACTER(LEN=17), PUBLIC :: cg_datan       ! - Last data -
  CHARACTER(LEN=10), PUBLIC :: cg_done        ! - Done -
  CHARACTER(LEN= 6), PUBLIC :: cg_edit_u      ! - EDIT -
  CHARACTER(LEN= 8), PUBLIC :: cg_error       ! - Error -
  CHARACTER(LEN= 8), PUBLIC :: cg_error_u     ! - ERROR -
  CHARACTER(LEN=11), PUBLIC :: cg_exit        ! - Exit -
  CHARACTER(LEN=14), PUBLIC :: cg_fcast       ! - Forecast -
  CHARACTER(LEN=14), PUBLIC :: cg_fcasts      ! - Forecasts -
  CHARACTER(LEN=13), PUBLIC :: cg_fcasts_l    ! - forecasts -
  CHARACTER(LEN= 7), PUBLIC :: cg_field       ! - Field -
  CHARACTER(LEN= 8), PUBLIC :: cg_fields_l    ! - fields -
  CHARACTER(LEN=11), PUBLIC :: cg_file        ! - File -
  CHARACTER(LEN=11), PUBLIC :: cg_file_u      ! - FILE -
  CHARACTER(LEN=17), PUBLIC :: cg_filename    ! - File name -
  CHARACTER(LEN=30), PUBLIC :: cg_fsetting_t  ! - Forecast Settings -
  CHARACTER(LEN=16), PUBLIC :: cg_goodness_t  ! - Goodness Index -
  CHARACTER(LEN=20), PUBLIC :: cg_indata_t    ! - Input Data -
  CHARACTER(LEN=21), PUBLIC :: cg_infiles_t   ! - Input Files -
  CHARACTER(LEN=16), PUBLIC :: cg_lagfield    ! - Lagged field -
  CHARACTER(LEN=12), PUBLIC :: cg_lags_l      ! - lags -
  CHARACTER(LEN=10), PUBLIC :: cg_lat         ! - Latitude -
  CHARACTER(LEN=23), PUBLIC :: cg_latnmost    ! - Northernmost latitude -
  CHARACTER(LEN=23), PUBLIC :: cg_latsmost    ! - Southernmost latitude -
  CHARACTER(LEN=45), PUBLIC :: cg_lcv         ! - Length of cross-validation window -
  CHARACTER(LEN=11), PUBLIC :: cg_lng         ! - Longitude -
  CHARACTER(LEN=23), PUBLIC :: cg_lngemost    ! - Easternmost longitude -
  CHARACTER(LEN=23), PUBLIC :: cg_lngwmost    ! - Westernmost longitude -
  CHARACTER(LEN=10), PUBLIC :: cg_lower       ! - Lower -
  CHARACTER(LEN=38), PUBLIC :: cg_ltp         ! - Length of training period -
  CHARACTER(LEN=19), PUBLIC :: cg_maxnof      ! - Maximum number of -
  CHARACTER(LEN=19), PUBLIC :: cg_minnof      ! - Minimum number of -
  CHARACTER(LEN= 5), PUBLIC :: cg_mode        ! - Mode -
  CHARACTER(LEN=26), PUBLIC :: cg_modecopts_t ! - Mode Calculation Options -
  CHARACTER(LEN=18), PUBLIC :: cg_modeopts_l  ! - mode options -
  CHARACTER(LEN=18), PUBLIC :: cg_modeopts_t  ! - Mode Options -
  CHARACTER(LEN= 6), PUBLIC :: cg_modes_l     ! - modes -
  CHARACTER(LEN=53), PUBLIC :: cg_neglatlng   ! - Southern latitudes and western longitudes negatives -
  CHARACTER(LEN=10), PUBLIC :: cg_nof         ! - Number of -
  CHARACTER(LEN=18), PUBLIC :: cg_nused       ! - Number used -
  CHARACTER(LEN= 6), PUBLIC :: cg_odds        ! - Odds -
  CHARACTER(LEN=25), PUBLIC :: cg_options_u   ! - OPTIONS -
  CHARACTER(LEN=25), PUBLIC :: cg_pinterval_t ! - Prediction Interval -
  CHARACTER(LEN=16), PUBLIC :: cg_probs       ! - Probabilities -
  CHARACTER(LEN=11), PUBLIC :: cg_progress    ! - Progress -
  CHARACTER(LEN=22), PUBLIC :: cg_pscores_t   ! - Probabilistic Scores -
  CHARACTER(LEN=12), PUBLIC :: cg_ra          ! - Retroactive -
  CHARACTER(LEN=36), PUBLIC :: cg_resamples_t ! - Resampling Settings -
  CHARACTER(LEN=12), PUBLIC :: cg_reset       ! - Reset -
  CHARACTER(LEN=29), PUBLIC :: cg_results_t   ! - Output Results -
  CHARACTER(LEN=18), PUBLIC :: cg_roc_t       ! - ROC Diagram -
  CHARACTER(LEN=18), PUBLIC :: cg_rocs_t      ! - ROC Diagrams -
  CHARACTER(LEN=16), PUBLIC :: cg_rodds_t     ! - Relative Odds -
  CHARACTER(LEN=28), PUBLIC :: cg_screes_t    ! - Scree Plots -
  CHARACTER(LEN=47), PUBLIC :: cg_specdom     ! - Please specify domain limits -
  CHARACTER(LEN=13), PUBLIC :: cg_start       ! - Start at -
  CHARACTER(LEN=12), PUBLIC :: cg_threshs     ! - Thresholds -
  CHARACTER(LEN= 7), PUBLIC :: cg_to_l        ! - to -
  CHARACTER(LEN=12), PUBLIC :: cg_tools_u     ! - TOOLS -
  CHARACTER(LEN=25), PUBLIC :: cg_tperiod     ! - Training period -
  CHARACTER(LEN=25), PUBLIC :: cg_training    ! - Training data -
  CHARACTER(LEN=25), PUBLIC :: cg_training_t  ! - Training Data -
  CHARACTER(LEN=10), PUBLIC :: cg_upper       ! - Upper -
  CHARACTER(LEN= 7), PUBLIC :: cg_view_u      ! - VIEW -
  CHARACTER(LEN=15), PUBLIC :: cg_warning     ! - Warning -
  CHARACTER(LEN=15), PUBLIC :: cg_warning_u   ! - WARNING -
  CHARACTER(LEN=22), PUBLIC :: cg_wr_t        ! - Weather Roulette -
  CHARACTER(LEN=13), PUBLIC :: cg_xdomain_t   ! - X Domain -
  CHARACTER(LEN=29), PUBLIC :: cg_xs_t        ! - Explanatory (X) Variables -
  CHARACTER(LEN=23), PUBLIC :: cg_xscree_t    ! - X Scree Plot -
  CHARACTER(LEN=13), PUBLIC :: cg_ydomain_t   ! - Y Domain -
  CHARACTER(LEN=28), PUBLIC :: cg_ys_t        ! - Response (Y) Variables -
  CHARACTER(LEN=23), PUBLIC :: cg_yscree_t    ! - Y Scree Plot -
  CHARACTER(LEN=25), PUBLIC :: cg_zs_t        ! - Forecast Variables -
!
! Interfaces
!
! Generic interfaces
  INTERFACE read_labels
   MODULE PROCEDURE read_label
   MODULE PROCEDURE read_label1
   MODULE PROCEDURE read_label2
  END INTERFACE read_labels
!
CONTAINS
!
!
 SUBROUTINE init_labels (ilang,ifail)
!
! Initialises GUI labels
!
! Modules
  USE CPT_text,     ONLY: clang
  USE IO_constants, ONLY: iin,cdir
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: ilang ! - language identifier -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Executable Statements
!
! Open labels file
  OPEN (UNIT=iin,FILE='LABELS'//cdir//'labels_'//clang(ilang)(1:3)//'.dat', &
        ACCESS='sequential',ACTION='read',FORM='unformatted',IOSTAT=ifail,STATUS='old')
  SELECT CASE (ifail)
   CASE (0)
     ifail=0
   CASE (128)
     ifail=1
   CASE (134)
     ifail=2
   CASE DEFAULT
     ifail=3
  END SELECT
  IF (ifail/=0) RETURN
!
! Read labels
! - arrays -
  CALL read_labels (iin,'cg_atypes_a',cg_atypes_a(:),ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_atypes_t',cg_atypes_t(:),ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_bss',cg_bss(0:,:),ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_cat_a',cg_cat_a(:),ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_cat_l',cg_cat_l(0:),ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_dsds',cg_dsds(0:),ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_dsds_l',cg_dsds_l(0:),ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_ftype',cg_ftype(:),ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_pccos',cg_pccos(:),ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_pvscores_t',cg_pvscores_t(:),ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_scores',cg_scores(:),ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_seq',cg_seq(0:),ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_seq_l',cg_seq_l(0:),ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_seqs',cg_seqs(0:),ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_seqs_l',cg_seqs_l(0:),ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_skill_t',cg_skill_t(:),ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_stds_t',cg_stds_t(:),ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_thr_l',cg_thr_l(:),ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_wrlts_t',cg_wrlts_t(:),ifail)
  IF (ifail/=0) GOTO 1
!
! - scalars -
  CALL read_labels (iin,'cg_actions_u',cg_actions_u,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_all',cg_all,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_approx',cg_approx,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_attrib_t',cg_attrib_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_attribs_t',cg_attribs_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_browse_l',cg_browse_l,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_cancor_t',cg_cancor_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_ccacopts_t',cg_ccacopts_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_ccamaps_t',cg_ccamaps_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_ccaopts_t',cg_ccaopts_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_climate',cg_climate,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_climate_t',cg_climate_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_cv_t',cg_cv_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_data',cg_data,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_data1',cg_data1,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_datan',cg_datan,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_done',cg_done,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_edit_u',cg_edit_u,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_error',cg_error,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_error_u',cg_error_u,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_exit',cg_exit,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_fcast',cg_fcast,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_fcasts',cg_fcasts,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_fcasts_l',cg_fcasts_l,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_field',cg_field,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_fields_l',cg_fields_l,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_file',cg_file,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_file_u',cg_file_u,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_filename',cg_filename,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_fsetting_t',cg_fsetting_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_goodness_t',cg_goodness_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_indata_t',cg_indata_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_infiles_t',cg_infiles_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_lagfield',cg_lagfield,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_lags_l',cg_lags_l,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_lat',cg_lat,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_latnmost',cg_latnmost,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_latsmost',cg_latsmost,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_lcv',cg_lcv,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_lng',cg_lng,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_lngemost',cg_lngemost,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_lngwmost',cg_lngwmost,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_lower',cg_lower,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_ltp',cg_ltp,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_maxnof',cg_maxnof,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_minnof',cg_minnof,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_mode',cg_mode,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_modecopts_t',cg_modecopts_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_modeopts_l',cg_modeopts_l,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_modeopts_t',cg_modeopts_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_modes_l',cg_modes_l,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_neglatlng',cg_neglatlng,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_nof',cg_nof,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_nused',cg_nused,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_odds',cg_odds,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_options_u',cg_options_u,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_pinterval_t',cg_pinterval_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_probs',cg_probs,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_progress',cg_progress,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_pscores_t',cg_pscores_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_ra',cg_ra,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_resamples_t',cg_resamples_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_reset',cg_reset,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_results_t',cg_results_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_roc_t',cg_roc_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_rocs_t',cg_rocs_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_rodds_t',cg_rodds_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_screes_t',cg_screes_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_specdom',cg_specdom,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_start',cg_start,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_threshs',cg_threshs,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_to_l',cg_to_l,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_tools_u',cg_tools_u,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_tperiod',cg_tperiod,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_training',cg_training,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_training_t',cg_training_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_upper',cg_upper,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_view_u',cg_view_u,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_warning',cg_warning,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_warning_u',cg_warning_u,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_wr_t',cg_wr_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_xdomain_t',cg_xdomain_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_xs_t',cg_xs_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_xscree_t',cg_xscree_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_ydomain_t',cg_ydomain_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_ys_t',cg_ys_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_yscree_t',cg_yscree_t,ifail)
  IF (ifail/=0) GOTO 1
!
  CALL read_labels (iin,'cg_zs_t',cg_zs_t,ifail)
  IF (ifail/=0) GOTO 1
!
! Close file
1 CLOSE (UNIT=iin)
!
  RETURN
 END SUBROUTINE init_labels
!
!
!
 SUBROUTINE read_label (iin,ctag,clabel,ifail)
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iin ! - input file unit number -
!
  CHARACTER(LEN=*), INTENT(IN) :: ctag ! - label tag -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
  CHARACTER(LEN=*), INTENT(OUT) :: clabel ! - label tag -
!
! Locals
!
! Local scalars
  INTEGER :: lctag ! - tag length -
  INTEGER :: lclab ! - label length -
!
  CHARACTER(LEN=32) :: cintag ! - current tag -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC LEN
!
! Executable Statements
!
! Read labels for each language
  READ (UNIT=iin,ERR=1,END=2) lctag,lclab
  IF ((lctag/=LEN(ctag)).OR.(lclab/=LEN(clabel))) GOTO 3
  READ (UNIT=iin,ERR=1,END=2) cintag(1:lctag)
  IF (cintag(1:lctag)/=ctag) GOTO 3
  READ (UNIT=iin,ERR=1,END=2) clabel
!
  ifail=0
  RETURN
!
! Errors
1 ifail=4
  RETURN
!
2 ifail=5
  RETURN
!
3 ifail=6
  RETURN
!
 END SUBROUTINE read_label
!
!
!
 SUBROUTINE read_label1 (iin,ctag,clabel,ifail)
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iin ! - input file unit number -
!
  CHARACTER(LEN=*), INTENT(IN) :: ctag ! - label tag -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Output arrays
  CHARACTER(LEN=*), INTENT(OUT) :: clabel(:) ! - label tag -
!
! Locals
!
! Local scalars
  INTEGER :: lctag ! - tag length -
  INTEGER :: lclab ! - label length -
!
  CHARACTER(LEN=32) :: cintag ! - current tag -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC LEN
!
! Executable Statements
!
! Read labels for each language
  READ (UNIT=iin,ERR=1,END=2) lctag,lclab
  IF ((lctag/=LEN(ctag)).OR.(lclab/=LEN(clabel(1)))) GOTO 3
  READ (UNIT=iin,ERR=1,END=2) cintag(1:lctag)
  IF (cintag(1:lctag)/=ctag) GOTO 3
  READ (UNIT=iin,ERR=1,END=2) clabel(:)
!
  ifail=0
  RETURN
!
! Errors
1 ifail=4
  RETURN
!
2 ifail=5
  RETURN
!
3 ifail=6
  RETURN
!
 END SUBROUTINE read_label1
!
!
!
 SUBROUTINE read_label2 (iin,ctag,clabel,ifail)
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iin ! - input file unit number -
!
  CHARACTER(LEN=*), INTENT(IN) :: ctag ! - label tag -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Output arrays
  CHARACTER(LEN=*), INTENT(OUT) :: clabel(:,:) ! - label tag -
!
! Locals
!
! Local scalars
  INTEGER :: lctag ! - tag length -
  INTEGER :: lclab ! - label length -
!
  CHARACTER(LEN=32) :: cintag ! - current tag -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC LEN
!
! Executable Statements
!
! Read labels for each language
  READ (UNIT=iin,ERR=1,END=2) lctag,lclab
  IF ((lctag/=LEN(ctag)).OR.(lclab/=LEN(clabel(1,1)))) GOTO 3
  READ (UNIT=iin,ERR=1,END=2) cintag(1:lctag)
  IF (cintag(1:lctag)/=ctag) GOTO 3
  READ (UNIT=iin,ERR=1,END=2) clabel(:,:)
!
  ifail=0
  RETURN
!
! Errors
1 ifail=4
  RETURN
!
2 ifail=5
  RETURN
!
3 ifail=6
  RETURN
!
 END SUBROUTINE read_label2
END MODULE labels
