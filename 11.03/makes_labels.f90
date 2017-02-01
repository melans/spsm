PROGRAM makes_labels
!
! Modules
  USE CPT_constants, ONLY: na,nds,ng,npo,nps,nscore,nskill,nsq,nstd,nts,nwr
  USE CPT_text,      ONLY: nlang,clang
  USE IO_constants,  ONLY: iin
  USE labels,        ONLY: cg_atypes_a,cg_atypes_t,cg_bss,cg_cat_a,cg_cat_l,cg_dsds,cg_dsds_l,cg_ftype,cg_pccos,cg_pvscores_t,     &
                           cg_scores,cg_seq,cg_seq_l,cg_seqs,cg_seqs_l,cg_skill_t,cg_stds_t,cg_thr_l,cg_wrlts_t,                   &
                           cg_actions_u,cg_all,cg_approx,cg_attrib_t,cg_attribs_t,cg_browse_l,cg_cancor_t,cg_ccacopts_t,           &
                           cg_ccamaps_t,cg_ccaopts_t,cg_climate,cg_climate_t,cg_cv_t,cg_data,cg_data1,cg_datan,cg_done,cg_edit_u,  &
                           cg_error,cg_error_u,cg_exit,cg_fcasts,cg_fcasts_l,cg_field,cg_fields_l,cg_file,cg_file_u,cg_filename,   &
                           cg_fcast,cg_fsetting_t,cg_goodness_t,cg_indata_t,cg_infiles_t,cg_lagfield,cg_lags_l,cg_lat,cg_latnmost, &
                           cg_latsmost,cg_lcv,cg_lng,cg_lngemost,cg_lngwmost,cg_lower,cg_ltp,cg_maxnof,cg_minnof,cg_mode,          &
                           cg_modecopts_t,cg_modeopts_l,cg_modeopts_t,cg_modes_l,cg_neglatlng,cg_nof,cg_nused,cg_odds,             &
                           cg_options_u,cg_pinterval_t,cg_probs,cg_progress,cg_pscores_t,cg_ra,cg_resamples_t,cg_reset,            &
                           cg_results_t,cg_roc_t,cg_rocs_t,cg_rodds_t,cg_screes_t,cg_specdom,cg_start,cg_threshs,cg_to_l,          &
                           cg_tools_u,cg_tperiod,cg_training,cg_training_t,cg_upper,cg_view_u,cg_warning,cg_warning_u,cg_wr_t,     &
                           cg_xdomain_t,cg_xs_t,cg_xscree_t,cg_ydomain_t,cg_ys_t,cg_yscree_t,cg_zs_t
!
! Implicit declarations
  IMPLICIT NONE
!
! Parameters
  INTEGER, PARAMETER :: iout0=20 ! - output file unit number -
!
! Arrays
  CHARACTER(LEN=3), DIMENSION(nlang) :: clang_a ! - language abbreviations -
!
! Scalars
  INTEGER :: ilang ! - language index -
  INTEGER :: ifail ! - error indicator -
!
  CHARACTER(LEN=512) :: cfail ! - current input string -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC LEN
!
! Executable Statements
!
! Initialise language abbreviations
  DO ilang=1,nlang
     clang_a(ilang)=clang(ilang)(1:LEN(clang_a))
     CALL lcase (clang_a(ilang))
  END DO
!
! Open labels input and output files
  OPEN (UNIT=iin,FILE='LABELS/labels.txt',ACTION='read',FORM='formatted',STATUS='old')
  DO ilang=1,nlang
     OPEN (UNIT=iout0+ilang,FILE='LABELS/labels_'//clang(ilang)(1:3)//'.dat', &
           ACCESS='sequential',ACTION='write',FORM='unformatted',STATUS='unknown')
  END DO
!
! Read labels
  CALL read_label1 ('cg_atypes_a',cg_atypes_a(:),na,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label1 ('cg_atypes_t',cg_atypes_t(:),na,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label2 ('cg_bss',cg_bss(0:,:),ng+1,2,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label1 ('cg_cat_a',cg_cat_a(:),ng,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label1 ('cg_cat_l',cg_cat_l(0:),ng+1,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label1 ('cg_dsds',cg_dsds(0:),nds+1,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label1 ('cg_dsds_l',cg_dsds_l(0:),nds+1,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label1 ('cg_ftype',cg_ftype(:),nds,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label1 ('cg_pccos',cg_pccos(:),npo,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label1 ('cg_pvscores_t',cg_pvscores_t(:),nps,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label1 ('cg_scores',cg_scores(:),nscore,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label1 ('cg_seq',cg_seq(0:),nsq+1,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label1 ('cg_seq_l',cg_seq_l(0:),nsq+1,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label1 ('cg_seqs',cg_seqs(0:),nsq+1,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label1 ('cg_seqs_l',cg_seqs_l(0:),nsq+1,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label1 ('cg_skill_t',cg_skill_t(:),nskill,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label1 ('cg_stds_t',cg_stds_t(:),nstd,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label1 ('cg_thr_l',cg_thr_l(:),nts,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label1 ('cg_wrlts_t',cg_wrlts_t(:),nwr,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_actions_u',cg_actions_u,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_all',cg_all,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_approx',cg_approx,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_attrib_t',cg_attrib_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_attribs_t',cg_attribs_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_browse_l',cg_browse_l,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_cancor_t',cg_cancor_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_ccacopts_t',cg_ccacopts_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_ccamaps_t',cg_ccamaps_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_ccaopts_t',cg_ccaopts_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_climate',cg_climate,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_climate_t',cg_climate_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_cv_t',cg_cv_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_data',cg_data,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_data1',cg_data1,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_datan',cg_datan,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_done',cg_done,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_edit_u',cg_edit_u,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_error',cg_error,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_error_u',cg_error_u,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_exit',cg_exit,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_fcast',cg_fcast,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_fcasts',cg_fcasts,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_fcasts_l',cg_fcasts_l,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_field',cg_field,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_fields_l',cg_fields_l,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_file',cg_file,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_file_u',cg_file_u,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_filename',cg_filename,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_fsetting_t',cg_fsetting_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_goodness_t',cg_goodness_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_indata_t',cg_indata_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_infiles_t',cg_infiles_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_lagfield',cg_lagfield,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_lags_l',cg_lags_l,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_lat',cg_lat,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_latnmost',cg_latnmost,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_latsmost',cg_latsmost,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_lcv',cg_lcv,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_lng',cg_lng,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_lngemost',cg_lngemost,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_lngwmost',cg_lngwmost,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_lower',cg_lower,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_ltp',cg_ltp,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_maxnof',cg_maxnof,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_minnof',cg_minnof,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_mode',cg_mode,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_modecopts_t',cg_modecopts_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_modeopts_l',cg_modeopts_l,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_modeopts_t',cg_modeopts_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_modes_l',cg_modes_l,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_neglatlng',cg_neglatlng,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_nof',cg_nof,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_nused',cg_nused,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_odds',cg_odds,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_options_u',cg_options_u,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_pinterval_t',cg_pinterval_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_probs',cg_probs,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_progress',cg_progress,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_pscores_t',cg_pscores_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_ra',cg_ra,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_resamples_t',cg_resamples_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_reset',cg_reset,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_results_t',cg_results_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_roc_t',cg_roc_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_rocs_t',cg_rocs_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_rodds_t',cg_rodds_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_screes_t',cg_screes_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_specdom',cg_specdom,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_start',cg_start,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_threshs',cg_threshs,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_to_l',cg_to_l,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_tools_u',cg_tools_u,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_tperiod',cg_tperiod,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_training',cg_training,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_training_t',cg_training_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_upper',cg_upper,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_view_u',cg_view_u,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_warning',cg_warning,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_warning_u',cg_warning_u,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_wr_t',cg_wr_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_xdomain_t',cg_xdomain_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_xs_t',cg_xs_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_xscree_t',cg_xscree_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_ydomain_t',cg_ydomain_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_ys_t',cg_ys_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_yscree_t',cg_yscree_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
  CALL read_label ('cg_zs_t',cg_zs_t,ifail,cfail)
  IF (ifail/=0) GOTO 1
!
! Errors
1 SELECT CASE (ifail)
   CASE(1)
     PRINT *, 'ERROR: Problem reading labels.txt. Current label: '//TRIM(cfail)
   CASE (2)
     PRINT *, 'ERROR: Problem reading labels.txt. End of file.'
  END SELECT
!
! Close files
  CLOSE (UNIT=iin)
  DO ilang=1,nlang
     CLOSE (UNIT=iout0+ilang)
  END DO
!
CONTAINS
!
!
 SUBROUTINE lcase (c)
!
! Arguments
!
! Input / output scalars
  CHARACTER(LEN=*), INTENT(INOUT) :: c ! - text to be converted to lower case -
!
! Locals
!
! Local scalar
  INTEGER :: i ! - index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ACHAR
  INTRINSIC IACHAR
  INTRINSIC LEN_TRIM
!
! Executable Statements
!
! Convert to lower case
  DO i=1,LEN_TRIM(c)
     IF (IACHAR(c(i:i))>=IACHAR('A').AND.IACHAR(c(i:i))<=IACHAR('Z')) c(i:i)=ACHAR(IACHAR(c(i:i))+IACHAR('a')-IACHAR('A'))
  END DO
!
  RETURN
 END SUBROUTINE lcase
!
!
!
 SUBROUTINE read_label (ctag,clabel,ifail,cstring)
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: ctag ! - label tag -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
  CHARACTER(LEN=*), INTENT(OUT) :: clabel  ! - label tag -
  CHARACTER(LEN=*), INTENT(OUT) :: cstring ! - current input string -
!
! Locals
!
! Local scalars
  INTEGER :: ilang ! - language index -
  INTEGER :: i1    ! - starting point -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC INDEX
  INTRINSIC LEN
  INTRINSIC LEN_TRIM
!
! Executable Statements
!
! Read labels for each language
  DO ilang=1,nlang
     READ (UNIT=iin,FMT='(A)',ERR=1,END=2) cstring
! - error if current tag is not found -
     IF (INDEX(cstring,TRIM(ctag))==0) GOTO 1
! - error if current language is not found -
     IF (INDEX(cstring,clang_a(ilang))/=LEN_TRIM(ctag)+2) GOTO 1
     i1=INDEX(cstring,"=")+1
     READ (UNIT=cstring(i1:),FMT=*,ERR=1) clabel
     WRITE (UNIT=iout0+ilang) LEN(ctag),LEN(clabel)
     WRITE (UNIT=iout0+ilang) ctag
     WRITE (UNIT=iout0+ilang) clabel
  END DO
!
  ifail=0
  RETURN
!
! Errors
1 ifail=1
  RETURN
!
2 ifail=2
  RETURN
!
 END SUBROUTINE read_label
!
!
!
 SUBROUTINE read_label1 (ctag,clabel,n,ifail,cstring)
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: ctag ! - label tag -
!
! Input / output scalars
  INTEGER, INTENT(IN) :: n ! - number of labels -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
  CHARACTER(LEN=*), INTENT(OUT) :: cstring ! - current input string -
!
! Output arrays
  CHARACTER(LEN=*), INTENT(OUT) :: clabel(:) ! - label tag -
!
! Locals
!
! Local scalars
  INTEGER :: ilang ! - language index -
  INTEGER :: i1    ! - starting point -
  INTEGER :: i     ! - indices -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC INDEX
  INTRINSIC LEN_TRIM
  INTRINSIC TRIM
!
! Executable Statements
!
! Read labels for each language
  DO ilang=1,nlang
     READ (UNIT=iin,FMT='(A)',ERR=1,END=2) cstring
! - error if current tag is not found -
     IF (INDEX(cstring,TRIM(ctag))==0) GOTO 1
! - error if current language is not found -
     IF (INDEX(cstring,clang_a(ilang))/=LEN_TRIM(ctag)+2) GOTO 1
     i1=INDEX(cstring,"=")+1
     READ (UNIT=cstring(i1:),FMT=*,ERR=1) (clabel(i),i=1,n)
     WRITE (UNIT=iout0+ilang) LEN(ctag),LEN(clabel(1))
     WRITE (UNIT=iout0+ilang) ctag
     WRITE (UNIT=iout0+ilang) clabel
  END DO
!
  ifail=0
  RETURN
!
! Errors
1 ifail=1
  RETURN
!
2 ifail=2
  RETURN
!
 END SUBROUTINE read_label1
!
!
!
 SUBROUTINE read_label2 (ctag,clabel,n1,n2,ifail,cstring)
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: ctag ! - label tag -
!
! Input / output scalars
  INTEGER, INTENT(IN) :: n1 ! - number of labels -
  INTEGER, INTENT(IN) :: n2 ! - number of labels -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
  CHARACTER(LEN=*), INTENT(OUT) :: cstring ! - current input string -
!
! Output arrays
  CHARACTER(LEN=*), INTENT(OUT) :: clabel(:,:) ! - label tag -
!
! Locals
!
! Local scalars
  INTEGER :: ilang ! - language index -
  INTEGER :: i1    ! - starting point -
  INTEGER :: i,j   ! - indices -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC INDEX
  INTRINSIC LEN_TRIM
  INTRINSIC TRIM
!
! Executable Statements
!
! Read labels for each language
  DO ilang=1,nlang
     READ (UNIT=iin,FMT='(A)',ERR=1,END=2) cstring
! - error if current tag is not found -
     IF (INDEX(cstring,TRIM(ctag))==0) GOTO 1
! - error if current language is not found -
     IF (INDEX(cstring,clang_a(ilang))/=LEN_TRIM(ctag)+2) GOTO 1
     i1=INDEX(cstring,"=")+1
     READ (UNIT=cstring(i1:),FMT=*,ERR=1) ((clabel(j,i),j=1,n1),i=1,n2)
     WRITE (UNIT=iout0+ilang) LEN(ctag),LEN(clabel(1,1))
     WRITE (UNIT=iout0+ilang) ctag
     WRITE (UNIT=iout0+ilang) clabel
  END DO
!
  ifail=0
  RETURN
!
! Errors
1 ifail=1
  RETURN
!
2 ifail=2
  RETURN
!
 END SUBROUTINE read_label2
END PROGRAM makes_labels
