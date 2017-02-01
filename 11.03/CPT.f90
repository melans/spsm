! $Id: CPT.f90 1245 2011-03-04 21:14:47Z simon $
! Climate Predictability Tool for MOS Downscaling
!
! Written by Dr Simon J. Mason, Dr Michael K. Tippett, and Lulin Song
!
PROGRAM cpt
!
! Implicit declarations
  IMPLICIT NONE
!
! Functions and Subroutines
!
! External routines
  EXTERNAL init_cpt
  EXTERNAL title_page
  EXTERNAL CPT_window
!
! Executable Statements
!
! Initialise CPT
  CALL init_cpt ()
!
! Print out title page
  CALL title_page ()
!
! Prompt for program settings
  CALL CPT_window ()
!
END PROGRAM cpt
!
!
SUBROUTINE title_page ()
!
! Modules
  USE analysis,      ONLY: ianal
  USE CPT_constants, ONLY: na
  USE labels,        ONLY: cg_atypes_a,cg_atypes_t
  USE pcs,           ONLY: ieofx
  USE version,       ONLY: cyr1,cyr2,ver
!
! Implicit declarations
  IMPLICIT NONE
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC TRIM
!
! Executable Statements
!
! Print title page
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') '            C L I M A T E   P R E D I C T A B I L I T Y   T O O L'
  WRITE (UNIT=*,FMT='(A,F5.2)') '                                Version ',ver
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') '                 Evaluating seasonal climate predictability'
  WRITE (UNIT=*,FMT='(A)') '                        Designed for MOS applications'
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(5A)') ' Copyright ',cyr1,'-',cyr2,' International Research Institute for Climate and Society'
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A)') 'III RRRR  III'
  WRITE (UNIT=*,FMT='(A)') ' I  R   R  I'
  WRITE (UNIT=*,FMT='(A)') ' I  R   R  I'
  WRITE (UNIT=*,FMT='(A)') ' I  RRRR   I'
  WRITE (UNIT=*,FMT='(A)') ' I  R   R  I   I N T E R N A T I O N A L   R E S E A R C H   I N S T I T U T E'
  WRITE (UNIT=*,FMT='(A)') 'III R   R III  F O R         C L I M A T E         A N D         S O C I E T Y'
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT=*)
!
! Prompt for analysis type
1  DO ianal=1,na
     WRITE (UNIT=*,FMT='(I1,5A)') ianal,'.  ',TRIM(cg_atypes_t(ianal)),' (',cg_atypes_a(ianal),')'
  END DO
  WRITE (UNIT=*,FMT='(A)') '0.  Exit'
  READ (UNIT=*,FMT=*,ERR=1) ianal
  SELECT CASE (ianal)
! - quit -
   CASE (0)
     STOP
! - set analysis type -
   CASE (1:na)
     CONTINUE
     IF (ianal==3) ieofx=2
! - error -
   CASE DEFAULT
     GOTO 1
  END SELECT
!
  RETURN
END SUBROUTINE title_page
!
!
!
SUBROUTINE CPT_window ()
!
! Modules
  USE analysis,       ONLY: iaction,ianal,ianaln,icalc,jcalc, &
                            close_analysis,reset
  USE bootstrap,      ONLY: bootstrap_cv,bootstrap_ra,get_boot_opts
  USE categories,     ONLY: iclim, &
                            climatology,tailoring
  USE get_input_file, ONLY: get_xfile,get_yfile
  USE data_output,    ONLY: save_data
  USE fields,         ONLY: get_xarea,get_yarea
  USE forecast,       ONLY: get_fcst_file,fcst_fser,fcst_fens,fcst_fps,fcst_pexc,fcst_fval,get_fcast_opts,change_fcast
  USE gui,            ONLY: cwtitle,irv,iscree,jcca,jpcr,jmlr,jgcm,jgauss, &
                            set_greyflags
  USE iofiles,        ONLY: xfile,yfile,zfile
  USE labels,         ONLY: cg_actions_u,cg_atypes_a,cg_atypes_t,cg_edit_u,cg_fcast,cg_fcasts_l,cg_file_u,cg_lcv,cg_ltp,cg_nof, &
                            cg_options_u,cg_reset,cg_tools_u,cg_training_t,cg_view_u
  USE maps,           ONLY: skill_maps_cv,skill_maps_ra
  USE missing,        ONLY: missing_opts
  USE pcs,            ONLY: ieofx,ieofy, &
                            eofx_opts,eofy_opts,advanced_eof,cca_opts,gcm_opts
  USE scree,          ONLY: scree_plots
  USE settings,       ONLY: igauss,igauss_bk,izero,isem,istd,iretro,ifcast,ipval,nt,lcw,nf,nx,ny,mxa,mya
  USE statistics,     ONLY: goodness_opts
  USE tables,         ONLY: table_cv,table_ra
  USE validate,       ONLY: validate_cv,validate_ra,roc_cv,roc_ra
  USE verif,          ONLY: attrib_diagram,roc_diagram,prob_scores,wrlt_diagram
!
! Implicit declarations
  IMPLICIT NONE
!
! Locals
!
! Locals parameters
  INTEGER, PARAMETER :: iwide=127 ! - width of screen output -
!
! Locals scalars
  INTEGER :: iopt ! - analysis option -
  INTEGER :: ios  ! - IO status -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC TRIM
!
! External functions
  INTEGER, EXTERNAL :: change_view
  INTEGER, EXTERNAL :: change_language
  INTEGER, EXTERNAL :: run_analysis
!
! Executable Statements
!
! Set grey menu items
  CALL set_greyflags (ianal,xfile%igrid,yfile%igrid)
!
! Print title
  DO
     WRITE (UNIT=*,FMT=*)
     DO ios=1,iwide
        WRITE (UNIT=*,FMT='(A)',ADVANCE='no') '_'
     END DO
     WRITE (UNIT=*,FMT='(A)') '_'
     WRITE (UNIT=*,FMT=*)
     WRITE (UNIT=*,FMT='(A)') cwtitle
!
! Indicate current settings
     CALL file_settings (           'X',xfile,nx,mxa)
     CALL file_settings (           'Y',yfile,ny,mya)
     CALL file_settings (TRIM(cg_fcast),zfile,nx,mxa)
!
     WRITE (UNIT=*,FMT=*)
     WRITE (UNIT=*,FMT='(A)') TRIM(cg_training_t)                             ! 'Training Data'
     WRITE (UNIT=*,FMT='(A,T45,I10)') TRIM(cg_ltp),nt                         ! 'Length of training period: '
     WRITE (UNIT=*,FMT='(A,T45,I10)') TRIM(cg_lcv),lcw                        ! 'Length of cross-validation window: '
     WRITE (UNIT=*,FMT='(A,T45,I10)') TRIM(cg_nof)//' '//TRIM(cg_fcasts_l),nf ! 'Number of forecasts: '
     DO ios=1,iwide
        WRITE (UNIT=*,FMT='(A)',ADVANCE='no') '_'
     END DO
     WRITE (UNIT=*,FMT='(A)') '_'
!
! Indicate options  FIXME needs to internationalized
     WRITE (UNIT=*,FMT=*)
     WRITE (UNIT=*,FMT='(A)') 'Select option:'
     WRITE (UNIT=*,FMT='(A)')                        '  1.  Open X input file'
     WRITE (UNIT=*,FMT='(A)')                        '  2.  Open Y input file'
     IF (xfile%lset) WRITE (UNIT=*,FMT='(A)')        '  3.  Open forecast file'
     IF (xfile%lset) WRITE (UNIT=*,FMT='(A)')        '  4.  X training period settings'
     IF (yfile%lset) WRITE (UNIT=*,FMT='(A)')        '  5.  Y training period settings'
     IF (zfile%lset) WRITE (UNIT=*,FMT='(A)')        '  6.  Forecast period settings'
     WRITE (UNIT=*,FMT='(A)')                        '  7.  '//TRIM(cg_ltp)
     WRITE (UNIT=*,FMT='(A)')                        '  8.  '//TRIM(cg_lcv)
     IF (zfile%lset) WRITE (UNIT=*,FMT='(A)')        '  9.  Number '//TRIM(cg_nof)//' '//TRIM(cg_fcasts_l)
     IF (icalc==1) THEN
        WRITE (UNIT=*,FMT='(A)')                     TRIM(cg_file_u)
        WRITE (UNIT=*,FMT='(A)')                     '111.  Data output'
     END IF
     IF (jcalc==1) THEN
        WRITE (UNIT=*,FMT='(A)')                     TRIM(cg_edit_u)
        IF (xfile%igeog==1) WRITE (UNIT=*,FMT='(A)') '211.  Change X data domain'
        IF (yfile%igeog==1) WRITE (UNIT=*,FMT='(A)') '212.  Change Y data domain'
     END IF
     IF (iaction==1) THEN
        WRITE (UNIT=*,FMT='(A)')                     TRIM(cg_actions_u)
        IF (jcalc==1) THEN
           WRITE (UNIT=*,FMT='(A)')                  '311.  Perform cross-validated analysis'
           WRITE (UNIT=*,FMT='(A)')                  '312.  Perform retroactive analysis'
        END IF
        WRITE (UNIT=*,FMT='(A)')                     '321.  '//TRIM(cg_reset)
     END IF
     IF (icalc==1) THEN
        WRITE (UNIT=*,FMT='(A)')                     TRIM(cg_tools_u)
        WRITE (UNIT=*,FMT='(A)')                     '411.  Cross-validated performance measures'
        WRITE (UNIT=*,FMT='(A)')                     '412.  Cross-validated bootstrapping and significance testing'
        SELECT CASE (ipval)
         CASE (0)
           WRITE (UNIT=*,FMT='(A)')                  '413.  Cross-validated skill maps'
         CASE (1)
           WRITE (UNIT=*,FMT='(A)')                  '413.  Cross-validated p-value maps'
        END SELECT
        WRITE (UNIT=*,FMT='(A)')                     '414.  Cross-validated ROC'
        WRITE (UNIT=*,FMT='(A)')                     '415.  Cross-validated contingency tables'
        IF (iretro==1) THEN
           WRITE (UNIT=*,FMT='(A)')                  '421.  Retroactive performance measures'
           WRITE (UNIT=*,FMT='(A)')                  '422.  Retroactive bootstrapping and significance testing'
           SELECT CASE (ipval)
            CASE (0)
              WRITE (UNIT=*,FMT='(A)')               '423.  Retroactive skill maps'
            CASE (1)
              WRITE (UNIT=*,FMT='(A)')               '423.  Retroactive p-value maps'
           END SELECT
           WRITE (UNIT=*,FMT='(A)')                  '424.  Retroactive ROC'
           WRITE (UNIT=*,FMT='(A)')                  '425.  Retroactive contingency tables'
        END IF
        IF (irv==1) THEN
           WRITE (UNIT=*,FMT='(A)')                  '431.  Attributes diagram'
           WRITE (UNIT=*,FMT='(A)')                  '432.  ROC diagram'
           WRITE (UNIT=*,FMT='(A)')                  '433.  Probabilistic verification scores'
           WRITE (UNIT=*,FMT='(A)')                  '434.  Weather roulette'
        END IF
        IF (iscree==1) WRITE (UNIT=*,FMT='(A)')      '441.  Scree plots'
        IF (ifcast==1) THEN
           WRITE (UNIT=*,FMT='(A)')                  '451.  Forecast series'
           WRITE (UNIT=*,FMT='(A)')                  '452.  Forecast ensembles'
           WRITE (UNIT=*,FMT='(A)')                  '453.  Exceedance probabilities'
           WRITE (UNIT=*,FMT='(A)')                  '454.  Forecast values'
           WRITE (UNIT=*,FMT='(A)')                  '455.  Forecast probabilities'
        END IF
     END IF
     WRITE (UNIT=*,FMT='(A)')                        TRIM(cg_options_u)
     WRITE (UNIT=*,FMT='(A)')                        '511.  Change language'
     IF (ianal<=2) THEN
        WRITE (UNIT=*,FMT='(A)')                     '521.  Change numbers of X modes'
        WRITE (UNIT=*,FMT='(A)')                     '522.  Change X modes options'
        IF (ianal==1) THEN
           WRITE (UNIT=*,FMT='(A)')                  '523.  Change numbers of Y modes'
           WRITE (UNIT=*,FMT='(A)')                  '524.  Change Y modes options'
           WRITE (UNIT=*,FMT='(A)')                  '525.  Change numbers of CCA modes'
        END IF
     ELSE IF (ianal==4) THEN
        WRITE (UNIT=*,FMT='(A)')                     '526.  Change GCM options'
     END IF
     WRITE (UNIT=*,FMT='(A)')                        '531.  Goodness index options'
     IF (iclim==1) WRITE (UNIT=*,FMT='(A)')          '532.  Set climatological period'
     WRITE (UNIT=*,FMT='(A)')                        '533.  Tailoring'
     IF (jcalc==1) THEN
        IF (jgauss==1) THEN
           SELECT CASE (igauss)
            CASE (0)
              WRITE (UNIT=*,FMT='(A)')               '541.  Transform Y data (turn on)'
            CASE (1)
              WRITE (UNIT=*,FMT='(A)')               '541.  Transform Y data (turn off)'
           END SELECT
        END IF

        SELECT CASE (izero)
         CASE (0)
           WRITE (UNIT=*,FMT='(A)')                  '542.  Zero-bound (turn on)'
         CASE (1)
           WRITE (UNIT=*,FMT='(A)')                  '542.  Zero-bound (turn off)'
        END SELECT
        SELECT CASE (isem)
         CASE (0)
           WRITE (UNIT=*,FMT='(A)')                  '543.  Sort Ensemble Members (turn on)'
         CASE (1)
           WRITE (UNIT=*,FMT='(A)')                  '543.  Sort Ensemble Members (turn off)'
        END SELECT

        WRITE (UNIT=*,FMT='(A)')                     '544.  Missing value options'
     END IF
     WRITE (UNIT=*,FMT='(A)')                        '551.  Bootstrap settings'
     WRITE (UNIT=*,FMT='(A)')                        '552.  Forecast settings'
     SELECT CASE (ipval)
      CASE (0)
        WRITE (UNIT=*,FMT='(A)')                     '561.  Skill map p-values (turn on)'
      CASE (1)
        WRITE (UNIT=*,FMT='(A)')                     '561.  Skill map p-values (turn off)'
     END SELECT
     IF (jcalc==1) THEN
        WRITE (UNIT=*,FMT='(A)')                     TRIM(cg_view_u)
        IF (jcca==1) WRITE (UNIT=*,FMT='(A)')        '611.  '//cg_atypes_t(1)//' ('//cg_atypes_a(1)//')'
        IF (jpcr==1) WRITE (UNIT=*,FMT='(A)')        '612.  '//cg_atypes_t(2)//' ('//cg_atypes_a(2)//')'
        IF (jmlr==1) WRITE (UNIT=*,FMT='(A)')        '613.  '//cg_atypes_t(3)//' ('//cg_atypes_a(3)//')'
        IF (jgcm==1) WRITE (UNIT=*,FMT='(A)')        '614.  '//cg_atypes_t(4)//' ('//cg_atypes_a(4)//')'
     END IF

     WRITE (UNIT=*,FMT='(A)')                        ' '
     WRITE (UNIT=*,FMT='(A)')                        '  0.  Exit'
     READ (UNIT=*,FMT=*,IOSTAT=ios) iopt
     IF (ios/=0) CYCLE
!
! Perform action
     SELECT CASE (iopt)
      CASE (1) ! - Open X input file -
        iopt=get_xfile()
      CASE (2) ! - Open Y input file -
        iopt=get_yfile()
      CASE (3) ! - Open forecast file -
        IF (.NOT.xfile%lset) CYCLE
        iopt=get_fcst_file()
      CASE (4) ! - X training period settings -
        IF (.NOT.xfile%lset) CYCLE
        CALL get_fdate ('X',xfile)
      CASE (5) ! - Y training period settings -
        IF (.NOT.yfile%lset) CYCLE
        CALL get_fdate ('Y',yfile)
      CASE (6) ! - Forecast period settings -
        IF (.NOT.zfile%lset) CYCLE
        CALL get_fdate ('forecast',zfile)
        CALL change_fcast ()
      CASE (7) ! - Length of training period -
        CALL get_nt ()
      CASE (8) ! - Length of cross-validation window -
        CALL get_lcw ()
      CASE (9) ! - Number of forecasts -
        IF (.NOT.zfile%lset) CYCLE
        CALL get_nf ()
      CASE (111) ! - Data output -
        iopt=save_data()
      CASE (211) ! - Change X data domain -
        IF (jcalc==0) CYCLE
        IF (xfile%igeog==0) CYCLE
        ios=get_xarea()
      CASE (212) ! - Change Y data domain -
        IF (jcalc==0) CYCLE
        IF (yfile%igeog==0) CYCLE
        ios=get_yarea()
      CASE (311) ! - Perform cross-validated analysis -
        IF (iaction==0) CYCLE
        IF (jcalc==0) CYCLE
        iretro=0
        ios=run_analysis()
      CASE (312) ! - Perform retroactive analysis -
        IF (iaction==0) CYCLE
        IF (jcalc==0) CYCLE
        iretro=1
        ios=run_analysis()
      CASE (321) ! - Reset -
        IF (iaction==0) CYCLE
        ios=reset('Resetting')
      CASE (411) ! - Cross-validated performance measures -
        IF (icalc==0) CYCLE
        iopt=validate_cv()
      CASE (412) ! - Cross-validated bootstrapping and significance testing -
        IF (icalc==0) CYCLE
        iopt=bootstrap_cv()
      CASE (413) ! - Cross-validated skill maps -
        IF (icalc==0) CYCLE
        iopt=skill_maps_cv()
      CASE (414) ! - Cross-validated ROC -
        IF (icalc==0) CYCLE
        iopt=roc_cv()
      CASE (415) ! - Cross-validated contingency tables -
        IF (icalc==0) CYCLE
        iopt=table_cv()
      CASE (421) ! - Retroactive performance measures -
        IF (icalc==0) CYCLE
        IF (iretro/=1) CYCLE
        iopt=validate_ra()
      CASE (422) ! - Retroactive bootstrapping and significance testing -
        IF (icalc==0) CYCLE
        IF (iretro/=1) CYCLE
        iopt=bootstrap_ra()
      CASE (423) ! - Retroactive skill maps -
        IF (icalc==0) CYCLE
        IF (iretro/=1) CYCLE
        iopt=skill_maps_ra()
      CASE (424) ! - Retroactive ROC -
        IF (icalc==0) CYCLE
        IF (iretro/=1) CYCLE
        iopt=roc_ra()
      CASE (425) ! - Retroactive contingency tables -
        IF (icalc==0) CYCLE
        IF (iretro/=1) CYCLE
        iopt=table_ra()
      CASE (431) ! - Attributes diagram -
        IF (icalc==0) CYCLE
        IF (iretro/=1) CYCLE
        iopt=attrib_diagram()
      CASE (432) ! - ROC diagram -
        IF (icalc==0) CYCLE
        IF (iretro/=1) CYCLE
        iopt=roc_diagram()
      CASE (433) ! - Probabilistic verification scores -
        IF (icalc==0) CYCLE
        IF (iretro/=1) CYCLE
        iopt=prob_scores()
      CASE (434) ! - Weather roulette -
        IF (icalc==0) CYCLE
        IF (iretro/=1) CYCLE
        iopt=wrlt_diagram()
      CASE (441) ! - Scree plots -
        IF (icalc==0) CYCLE
        IF (iscree/=1) CYCLE
        iopt=scree_plots()
      CASE (451) ! - Forecast series -
        IF (icalc==0) CYCLE
        IF (ifcast/=1) CYCLE
        iopt=fcst_fser()
      CASE (452) ! - Forecast ensembles -
        IF (icalc==0) CYCLE
        IF (ifcast/=1) CYCLE
        iopt=fcst_fens()
      CASE (453) ! - Exceedance probabilities -
        IF (icalc==0) CYCLE
        IF (ifcast/=1) CYCLE
        iopt=fcst_pexc()
      CASE (454) ! - Forecast values -
        IF (icalc==0) CYCLE
        IF (ifcast/=1) CYCLE
        iopt=fcst_fval()
      CASE (455) ! - Forecast probabilities -
        IF (icalc==0) CYCLE
        IF (ifcast/=1) CYCLE
        iopt=fcst_fps()
      CASE (511) ! - Change language -
        iopt=change_language()
      CASE (521) ! - Change numbers of X modes -
        SELECT CASE (ianal)
         CASE (1,2)
           ios=eofx_opts()
         CASE (3)
           CYCLE
         CASE (4)
           ios=gcm_opts()
        END SELECT
      CASE (522) ! - Change X modes options -
        IF (ianal==3) CYCLE
        ios=advanced_eof('X',ieofx)
      CASE (523) ! - Change numbers of Y modes -
        IF (ianal/=1) CYCLE
        ios=eofy_opts()
      CASE (524) ! - Change Y modes options -
        IF (ianal/=1) CYCLE
        ios=advanced_eof('Y',ieofy)
      CASE (525) ! - Change numbers of CCA modes -
        IF (ianal/=1) CYCLE
        ios=cca_opts()
      CASE (526) ! - Change GCM options -
        IF (ianal/=4) CYCLE
        ios=gcm_opts()
      CASE (531) ! - Goodness index options
        iopt=goodness_opts()
      CASE (532) ! - Set climatological period -
        IF (iclim==0) CYCLE
        ios=climatology()
      CASE (533) ! - Tailoring -
        ios=tailoring()
      CASE (541) ! - Transform Y data -
        IF (jcalc==0) CYCLE
        igauss=1-igauss
        igauss_bk=igauss
      CASE (542) ! - Zero-bound -
        IF (jcalc==0) CYCLE
        izero=1-izero
        IF (istd==3) istd=0
      CASE (543) ! Set sort-ensemble members flag -
        IF (jcalc==0) CYCLE
        isem=1-isem
      CASE (544) ! - Missing value options -
        IF (jcalc==0) CYCLE
        ios=missing_opts()
      CASE (551) ! - Bootstrap settings -
        iopt=get_boot_opts()
      CASE (552) ! - Forecast settings -
        iopt=get_fcast_opts()
      CASE (553) ! - p-values -
        ipval=1-ipval
      CASE (611) ! - CCA -
        IF (jcalc==0) CYCLE
        IF (jcca==0) CYCLE
        ianaln=1
        ios=change_view()
      CASE (612) ! - PCR -
        IF (jcalc==0) CYCLE
        IF (jgcm==0) CYCLE
        ianaln=2
        ios=change_view()
      CASE (613) ! - MLR
        IF (jcalc==0) CYCLE
        IF (jmlr==0) CYCLE
        ianaln=3
        ios=change_view()
      CASE (614) ! - MLR
        IF (jcalc==0) CYCLE
        IF (jgcm==0) CYCLE
        ianaln=4
        ios=change_view()
      CASE (0) ! - Exit -
        ios=close_analysis()
        EXIT
      CASE DEFAULT
        CYCLE
     END SELECT
  END DO

  RETURN
!
CONTAINS
!
!
 SUBROUTINE file_settings (cxyz,afile,nv,mva)
!
! Modules
  USE IO_constants, ONLY: lprd
  USE iofiles,      ONLY: ifile
  USE labels,       ONLY: cg_lags_l,cg_data1,cg_datan,cg_dsds_l,cg_fields_l,cg_file,cg_indata_t,cg_nof,cg_nused,cg_start
  USE maths,        ONLY: magnitude
  USE time,         ONLY: get_cdate
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: nv  ! - number of variables -
  INTEGER, INTENT(IN) :: mva ! - number of variables -
!
  CHARACTER(LEN=*), INTENT(IN) :: cxyz ! - X/Y/Z flag -
!
  TYPE(ifile), INTENT(IN) :: afile ! - input file -
!
! Locals
!
! Local scalars
  CHARACTER(LEN=lprd) :: fsdate ! - file start date -
  CHARACTER(LEN=  24) :: cfmt   ! - format statement -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC ADJUSTL
  INTRINSIC TRIM
!
! Executable Statements
!
! Print file settings
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(  T45,A)') cxyz//' '//TRIM(cg_file)
  WRITE (UNIT=*,FMT=*)
  WRITE (UNIT=*,FMT='(A,T45,A)') cg_indata_t,TRIM(afile%fname)
  WRITE (UNIT=*,FMT='(A,T45,A)') TRIM(cg_data1),TRIM(afile%cdate1)
  WRITE (UNIT=*,FMT='(A,T45,A)') TRIM(cg_datan),TRIM(afile%cdaten)
  fsdate=get_cdate(afile%fdate,2)
  WRITE (UNIT=*,FMT='(A,T45,A)') TRIM(cg_start),ADJUSTL(fsdate)
  WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,T45,I',magnitude(afile%nfs),')'
  WRITE (UNIT=*,FMT=cfmt) TRIM(cg_nof)//' '//TRIM(cg_fields_l),afile%nfs
  WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,T45,I',magnitude(afile%nls),')'
  WRITE (UNIT=*,FMT=cfmt) TRIM(cg_nof)//' '//TRIM(cg_lags_l),afile%nls
  WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,T45,I',magnitude(nv),')'
  WRITE (UNIT=*,FMT=cfmt) TRIM(cg_nof)//' '//TRIM(cg_dsds_l(1)),nv
  WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,T45,I',magnitude(mva),')'
  WRITE (UNIT=*,FMT=cfmt) TRIM(cg_nused)//' variables: ',mva
!
  RETURN
 END SUBROUTINE file_settings
!
!
!
 SUBROUTINE get_nt ()
!
! Prompts for length of training period
!
! Modules
  USE CPT_constants, ONLY: mnt
  USE iofiles,       ONLY: xfile,yfile
  USE maths,         ONLY: magnitude
  USE settings,      ONLY: nt
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
  INTEGER :: mn    ! - maximum length of training period -
!
  CHARACTER(LEN=13) :: cfmt ! - format statement -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MIN
!
! Executable Statements
!
! Check for reset
  ifail=reset('Modifying length of training period')
  IF (ifail==1) RETURN
!
! Determine maximum length of training period
  mn=xfile%nt
  IF (xfile%nt>0) THEN
     IF (yfile%nt>0) THEN
        mn=MIN(xfile%nt,yfile%nt)
     ELSE
        mn=xfile%nt
     END IF
  ELSE 
     IF (yfile%nt>0) THEN
        mn=yfile%nt
     ELSE
        mn=0
     END IF
  END IF
  IF (mn>0) THEN
     WRITE (UNIT=cfmt,FMT='(A,2(I1,A))') '(A,I',magnitude(mnt),',A,I',magnitude(mn),',A)'
  ELSE
     WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(mnt),',A)'
  END IF
!
! Get training data settings
1 WRITE (UNIT=*,FMT=*)
  IF (mn>0) THEN
     WRITE (UNIT=*,FMT=cfmt,ADVANCE='no') 'Length of training period (minimum is ',mnt,'; maximum is ',mn,'): '
  ELSE
     WRITE (UNIT=*,FMT=cfmt,ADVANCE='no') 'Length of training period (minimum is ',mnt,'): '
  END IF
  READ (UNIT=*,FMT=*,ERR=1)  nt
  IF (nt<mnt) GOTO 1
!
  RETURN
 END SUBROUTINE get_nt
!
!
!
 SUBROUTINE get_lcw ()
!
! Prompts for length of cross-validation period
!
! Modules
  USE settings, ONLY: lcw, &
                      odd_lcw
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
!
! Executable Statements
!
! Check for reset
  ifail=reset('Modifying length of cross-validation window')
  IF (ifail==1) RETURN
!
! Get training data settings
  WRITE (UNIT=*,FMT=*)
1 WRITE (UNIT=*,FMT='(A)',ADVANCE='no') 'Length of cross-validation period (must be odd): '
  READ (UNIT=*,FMT=*,ERR=1)  lcw
  IF (lcw<1) GOTO 1
  CALL odd_lcw (lcw,ifail)
  IF (ifail/=0) GOTO 1
!
  RETURN
 END SUBROUTINE get_lcw
!
!
!
 SUBROUTINE get_nf ()
!
! Prompts for number of forecasts
!
! Modules
  USE forecast, ONLY: change_fcast
  USE settings, ONLY: nf
!
! Executable Statements
!
! Get training data settings
  WRITE (UNIT=*,FMT=*)
1 WRITE (UNIT=*,FMT='(A)',ADVANCE='no') 'Number of forecasts: '
  READ (UNIT=*,FMT=*,ERR=1)  nf
  IF (nf<1) GOTO 1
  CALL change_fcast ()
!
  RETURN
 END SUBROUTINE get_nf
!
!
!
 SUBROUTINE get_fdate (cxy,afile)
!
! Prompts for training period settings
!
! Modules
  USE labels,         ONLY: cg_seq_l
  USE errors,         ONLY: error
  USE iofiles,        ONLY: ifile
  USE time,           ONLY: date_diff,ndays
  USE time_constants, ONLY: nmn
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: cxy ! - X/Y flag -
!
! Input/output scalars
  TYPE(ifile), INTENT(INOUT) :: afile ! - input file -
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
!
  CHARACTER(LEN=23) :: cprmt ! - prompt -
!
! Functions and Subroutines
!
! Functions
  INTRINSIC TRIM
!
! Executable Statements
!
! Check for reset
  SELECT CASE (cxy)
   CASE ('X','x','Y','y')
     ifail=reset('Modifying start date of training period')
     IF (ifail==1) RETURN
!
! Create prompt
     cprmt=' of '//cxy//' training period'
   CASE DEFAULT
     cprmt=' from which to forecast'
  END SELECT 
!
! Get input data information
  WRITE (UNIT=*,FMT='(4A)') 'First date of data in ',cxy,' file: ',afile%cdate1
1 WRITE (UNIT=*,FMT='(4A)',ADVANCE='no') 'First ',TRIM(cg_seq_l(1)),TRIM(cprmt),': '
  READ (UNIT=*,FMT=*,ERR=1) afile%fdate%iyr
  IF (afile%iseq==3) THEN
2    WRITE (UNIT=*,FMT='(4A)',ADVANCE='no') 'First ',TRIM(cg_seq_l(2)),TRIM(cprmt),': '
     READ (UNIT=*,FMT=*,ERR=2) afile%fdate%imn
     IF ((afile%fdate%imn<1).OR.(afile%fdate%imn>nmn)) GOTO 2
3    WRITE (UNIT=*,FMT='(4A)',ADVANCE='no') 'First ',TRIM(cg_seq_l(3)),TRIM(cprmt),': '
     READ (UNIT=*,FMT=*,ERR=3) afile%fdate%idy
     IF ((afile%fdate%idy<1).OR.(afile%fdate%idy>ndays(afile%fdate%iyr,afile%fdate%imn))) GOTO 3
  END IF
  IF (date_diff(afile%period1%sdate,afile%fdate,afile%iseq)<0) THEN
     SELECT CASE (cxy)
      CASE ('X','x','Y','y')
        ifail=1
        CALL error ('get_fdate',ifail, &
             c_arg1=TRIM(cxy))
      CASE DEFAULT
        ifail=2
        CALL error ('get_fdate',ifail)
     END SELECT
     GOTO 1
  END IF
!
  RETURN
 END SUBROUTINE get_fdate
END SUBROUTINE CPT_window
!
!
!
FUNCTION change_language()
!
! Changes language of CPT prompts
!
! Modules
  USE CPT_text
  USE errors, ONLY: error
  USE labels, ONLY: cg_exit, &
                    init_labels
!
! Implicit declarations
  IMPLICIT NONE
!
! Function type
  INTEGER :: change_language
!
! Locals
!
! Local scalars
  INTEGER :: ifail ! - error indicator -
!
! Executable Statements
!
! Prompt for language preferences
  change_language=0
1 WRITE (UNIT=*,FMT=*)
  DO ilang=1,nlang
     WRITE (UNIT=*,FMT='(I1,2A)') ilang,'.  ',clang(ilang)
  END DO
  WRITE (UNIT=*,FMT='(A)') '0.  '//cg_exit
  READ (UNIT=*,FMT=*,ERR=1) ilang
  SELECT CASE (ilang)
! - quit -
   CASE (0)
     RETURN
! - set language -
   CASE (1:nlang)
     CALL init_labels (ilang,ifail)
     IF (ifail/=0) THEN
        CALL error ('init_labels',ifail)
        GOTO 1
     END IF
! - error -
   CASE DEFAULT
     GOTO 1
  END SELECT
!
END FUNCTION change_language
!
!
!
FUNCTION change_view()
!
! Changes analysis option
!
! Modules
  USE analysis, ONLY: ianal,ianaln
  USE gui,      ONLY: cwtitle
  USE labels,   ONLY: cg_atypes_t
  USE pcs,      ONLY: ieofx,ieofx_bk
  USE settings, ONLY: igauss,igauss_bk
  USE version,  ONLY: cver
!
! Implicit declarations
  IMPLICIT NONE
!
! Function type
  INTEGER :: change_view
!
! Executable Statements
!
! Update CPT window title
  change_view=0
  ianal=ianaln
  SELECT CASE (ianal)
   CASE (1,2) ! - CCA and PCR -
     ieofx=ieofx_bk
     igauss=igauss_bk
   CASE (3) ! - MLR -
     ieofx_bk=ieofx
     ieofx=2
     igauss=igauss_bk
   CASE (4) ! - GCM -
     ieofx_bk=ieofx
     ieofx=2
     igauss_bk=igauss
     igauss=0
  END SELECT
  cwtitle=cver//' - '//cg_atypes_t(ianal)
!
  RETURN
END FUNCTION change_view
