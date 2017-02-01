! $Id: errors.f90 1224 2011-03-03 16:28:24Z simon $
MODULE errors
!
! Implicit declarations
  IMPLICIT NONE
!
! Scalars
!
! Character scalars
  CHARACTER(LEN=32), PUBLIC :: cproc ! - procedure in error -
!
CONTAINS
!
!
 SUBROUTINE init_errors ()
!
! Modules
  USE labels, ONLY: cg_error,cg_error_u
!
! Executable Statements
!
! Initialise error labels
  cg_error='Error'
  cg_error_u='ERROR'
!
  RETURN
 END SUBROUTINE init_errors
!
!
!
 SUBROUTINE error (cproc,ifail,i_arg1,i_arg2,r_arg1,c_arg1,c_arg2,c_arg3)
!
! Modules
  USE gui,     ONLY: print_advisory,print_error,print_warning
  USE labels,  ONLY: cg_ftype,cg_seq,cg_seq_l,cg_seqs_l
  USE maths,   ONLY: magnitude,get_cordn
  USE numbers, ONLY: sp
  USE version, ONLY: CPT_email
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: cproc ! - procedure -
!
! - optional input scalars -
  INTEGER, INTENT(IN), OPTIONAL :: i_arg1 ! - integer argument -
  INTEGER, INTENT(IN), OPTIONAL :: i_arg2 ! - integer argument -
!
  REAL(KIND=sp), INTENT(IN), OPTIONAL :: r_arg1 ! - real argument -
!
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: c_arg1 ! - character argument -
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: c_arg2 ! - character argument -
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: c_arg3 ! - character argument -
!
! Input/output scalars
  INTEGER, INTENT(INOUT) :: ifail ! - error number -
!
!
! Locals
!
! Local scalars
  CHARACTER(LEN=   8) :: cfmt ! - format statement -
  CHARACTER(LEN=1024) :: msg  ! - error message -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC PRESENT
  INTRINSIC LEN_TRIM
  INTRINSIC TRIM
!
! Executable Statements
!
! Identify error
  SELECT CASE (cproc)
   CASE ('attrib_diagram')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Insufficient memory to construct attributes diagram', &
           nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('cca_opts')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Maximum number of CCA modes cannot be less than minimum number.', &
           nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('check_analogues')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_advisory &
             ('First analogue year has been reset.')
      CASE (2)
        CALL print_advisory &
             ('Second analogue year has been reset.')
      CASE (3:)
        CALL print_advisory &
             ('Analogue years have been reset.')
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('check_climate')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_warning &
             ('Last '//TRIM(cg_seq_l(i_arg1))//' of climatology is not after first '//TRIM(cg_seq_l(i_arg1))//'.', &
             msg2='Last '//TRIM(cg_seq_l(i_arg1))//' of climatology has been reset.')
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('check_ddom')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Southernmost X latitude is north of northernmost latitude.',&
             nopause=.false.)
      CASE (2)
        CALL print_error ('Southernmost Y latitude is north of northernmost latitude.',&
             nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('check_gridded_block_v10')
     SELECT CASE (ifail)
      CASE (1)
        IF (LEN_TRIM(c_arg2)>0) THEN
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file: '//c_arg1//&
                 &'Inconsistent ''cpt:'//TRIM(c_arg3)//''' tag in the ',i_arg1,get_cordn(i_arg1)//' field.'//&
                 &'Last successfully read data for '//TRIM(c_arg2)
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file: '//c_arg1//&
                 &'Inconsistent ''cpt:'//TRIM(c_arg3)//''' tag.'//&
                 &'Last successfully read data for '//TRIM(c_arg2)
           ENDIF
        ELSE
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                 &'Inconsistent ''cpt:'//TRIM(c_arg3)//''' tag in the ',i_arg1,get_cordn(i_arg1)//' field.'
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file:'//c_arg1//&
                 &'Inconsistent ''cpt:'//TRIM(c_arg3)//''' tag.'
           ENDIF
        END IF
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE (2)
        IF (LEN_TRIM(c_arg2)>0) THEN
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                 &'Inconsistent '//TRIM(c_arg3)//' in the ',i_arg1,get_cordn(i_arg1)//' field.'//&
                 &'Last successfully read data for '//TRIM(c_arg2)
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file:'//c_arg1//&
                 &'Inconsistent '//TRIM(c_arg3)//'.'//&
                 &'Last successfully read data for '//TRIM(c_arg2)
           ENDIF
        ELSE
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                 &'Inconsistent '//TRIM(c_arg3)//' in the ',i_arg1,get_cordn(i_arg1)//' field.'
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file:'//c_arg1//&
                 &'Inconsistent '//TRIM(c_arg3)//'.'
           ENDIF
        END IF
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('check_n')
     SELECT CASE (ifail)
      CASE (1)
        WRITE (cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
        WRITE (msg,FMT=cfmt) 'Length of training period must be at least ',i_arg1,'.'
        CALL print_error (TRIM(msg), &
           nopause=.false.)
      CASE (2)
        WRITE (cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
        WRITE (msg,FMT=cfmt) 'Retroactive calculations can only be performed if training period is at least ',i_arg1,'.'
        CALL print_error (TRIM(msg), &
           nopause=.false.)
      CASE (3)
        CALL print_warning ('Length of training period must be greater than cross-validation window length + 2.', &
           msg2='Cross-validation window length has been reset.')
      CASE (4)
        WRITE (cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
        WRITE (msg,FMT=cfmt) 'Retroactive calculations can only be performed if training period is at least ',i_arg1,&
           ' more than length of cross-validation window.Cross-validation window length has been reset.'
        CALL print_warning (TRIM(msg))
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('check_new_file')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Invalid file name. Please try another', &
           nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('check_nt1')
     SELECT CASE (ifail)
      CASE (1)
        WRITE (cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
        WRITE (msg,FMT=cfmt) &
           'Length of initial training period must be at least cross-validation window length + ',i_arg1,'.&
          &Initial length of training period has been reset.'
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('check_pcs')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_warning &
             ('Minimum number of X EOF modes must not be more than number of cases-1.'&
           &//'Minimum number of X EOF modes has been reset.')
      CASE (2)
        CALL print_warning &
             ('Minimum number of X EOF modes must not be more than number of variables.', &
              msg2='Minimum number of X EOF modes has been reset.')
      CASE (3)
        CALL print_warning &
             ('Maximum number of X EOF modes must not be less than minimum number.', &
              msg2='Maximum number of X EOF modes has been reset.')
      CASE (4)
        CALL print_warning &
             ('Maximum number of X EOF modes must not be more than number of cases - length of cross-validation window -1.', &
              msg2='Maximum number of X EOF modes has been reset.')
      CASE (5)
        CALL print_warning &
             ('Maximum number of X EOF modes must not be more than number of variables.', &
              msg2='Maximum number of X EOF modes has been reset.')
      CASE (6)
        CALL print_warning &
             ('Minimum number of Y EOF modes must not be more than number of cases - length of cross-validation window -1.', &
              msg2='Minimum number of Y EOF modes has been reset.')
      CASE (7)
        CALL print_warning &
             ('Minimum number of Y EOF modes must not be more than number of variables.', &
              msg2='Minimum number of Y EOF modes has been reset.')
      CASE (8)
        CALL print_warning &
             ('Maximum number of Y EOF modes must not be less than minimum number.', &
              msg2='Maximum number of Y EOF modes has been reset.')
      CASE (9)
        CALL print_warning &
             ('Maximum number of Y EOF modes must not be more than number of cases - length of cross-validation window -1.', &
              msg2='Maximum number of Y EOF modes has been reset.')
      CASE (10)
        CALL print_warning &
             ('Maximum number of Y EOF modes must not be more than number of variables.', &
              msg2='Maximum number of Y EOF modes has been reset.')
      CASE (11)
        CALL print_warning &
             ('Minimum number of CCA modes must not be more than minimum number of X EOF modes.', &
              msg2='Minimum number of CCA modes has been reset.')
      CASE (12)
        CALL print_warning &
             ('Minimum number of CCA modes must not be more than minimum number of Y EOF modes.', &
              msg2='Minimum number of CCA modes has been reset.')
      CASE (13)
        CALL print_warning &
             ('Maximum number of CCA modes must not be less than minimum number.', &
              msg2='Maximum number of CCA modes has been reset.')
      CASE (14)
        CALL print_warning &
             ('Maximum number of CCA modes must not be more than maximum number of X EOF modes.', &
              msg2='Maximum number of CCA modes has been reset.')
      CASE (15)
        CALL print_warning &
             ('Maximum number of CCA modes must not be more than maximum number of Y EOF modes.', &
              msg2='Maximum number of CCA modes has been reset.')
      CASE (16)
        CALL print_warning &
             ('Maximum number of X modes + maximum number of Y modes exceeds number of cases - length of cross-validation window.'&
           &//'Spurious CCA modes are likely. It is recommended that:'&
           &//'1. the advanced CCA option be changed to ignore over-fitted modes, and/or'&
           &//'2. the maximum number of X and / or Y modes has been reduced.')
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('check_read')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_warning &
             ('First '//TRIM(cg_seq_l(i_arg1))//' of X training period is before first '//TRIM(cg_seq_l(i_arg1))//' in file.', &
              msg2='First '//TRIM(cg_seq_l(i_arg1))//' of X training period has been reset.')
      CASE (2)
        CALL print_warning &
             ('First '//TRIM(cg_seq_l(i_arg1))//' of Y training period is before first '//TRIM(cg_seq_l(i_arg1))//' in file.', &
              msg2='First '//TRIM(cg_seq_l(i_arg1))//' of Y training period has been reset.')
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('check_zfile')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Problem allocating memory to read file:'//c_arg1,nopause=.false.)
      CASE (2)
        CALL print_error ('The forecast file must be in the same CPT version format as the X file.&
             &Forecast file:  '//c_arg1//'X file:  '//c_arg2,nopause=.false.)
      CASE (3)
        CALL print_error ('The forecast file must be in the same format as the X file.&
             &Forecast file:  '//cg_ftype(i_arg1)//'X file:  '//cg_ftype(i_arg2),nopause=.false.)
      CASE (4)
        IF ((i_arg1>0).AND.(i_arg2>0)) THEN
           WRITE (UNIT=cfmt,FMT='(A,2(I1,A))') '(A,I',magnitude(i_arg1),',A,I',magnitude(i_arg2),',A)'
           WRITE (UNIT=msg,FMT=cfmt) '''cpt:'//c_arg2//''' tags are inconsistent with X input file for the ', &
              i_arg1,get_cordn(i_arg1)//' field and the ',                                                   &
              i_arg2,get_cordn(i_arg2)//' lag in file:'//c_arg1
        ELSE IF (i_arg1>0) THEN
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
           WRITE (UNIT=msg,FMT=cfmt) '''cpt:'//c_arg2//''' tags are inconsistent with X input file for the ', &
              i_arg1,get_cordn(i_arg1)//' field in file:'//c_arg1
        ELSE
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg2),',A)'
           WRITE (UNIT=msg,FMT=cfmt) '''cpt:'//c_arg2//''' tags are inconsistent with X input file for the ', &
              i_arg2,get_cordn(i_arg2)//' lag in file:'//c_arg1
        END IF
        CALL print_error (TRIM(msg), &
           msg2=c_arg1,nopause=.false.)
      CASE (5)
        IF ((i_arg1>0).AND.(i_arg2>0)) THEN
           WRITE (UNIT=cfmt,FMT='(A,2(I1,A))') '(A,I',magnitude(i_arg1),',A,I',magnitude(i_arg2),',A)'
           WRITE (UNIT=msg,FMT=cfmt) 'Number of '//c_arg2//' is inconsistent with X input file for the ', &
              i_arg1,get_cordn(i_arg1)//' field and the ',                                               &
              i_arg2,get_cordn(i_arg2)//' lag in file:'//c_arg1
        ELSE IF (i_arg1>0) THEN
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
           WRITE (UNIT=msg,FMT=cfmt) 'Number of '//c_arg2//' is inconsistent with X input file for the ', &
              i_arg1,get_cordn(i_arg1)//' field in file:'//c_arg1
        ELSE
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg2),',A)'
           WRITE (UNIT=msg,FMT=cfmt) 'Number of '//c_arg2//' is inconsistent with X input file for the ', &
              i_arg2,get_cordn(i_arg2)//' lag in file:'//c_arg1
        END IF
        CALL print_error (TRIM(msg), &
           msg2=c_arg1,nopause=.false.)
      CASE (6)
        IF ((i_arg1>0).AND.(i_arg2>0)) THEN
           WRITE (UNIT=cfmt,FMT='(A,2(I1,A))') '(A,I',magnitude(i_arg1),',A,I',magnitude(i_arg2),',A)'
           WRITE (UNIT=msg,FMT=cfmt) 'Latitude ordering is inconsistent with X input file for the ', &
              i_arg1,get_cordn(i_arg1)//' field and the ',                                          &
              i_arg2,get_cordn(i_arg2)//' lag in file:'//c_arg1
        ELSE IF (i_arg1>0) THEN
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
           WRITE (UNIT=msg,FMT=cfmt) 'Latitude ordering is inconsistent with X input file for the ', &
              i_arg1,get_cordn(i_arg1)//' field in file:'//c_arg1
        ELSE
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg2),',A)'
           WRITE (UNIT=msg,FMT=cfmt) 'Latitude ordering is inconsistent with X input file for the ', &
              i_arg2,get_cordn(i_arg2)//' lag in file:'//c_arg1
        END IF
        CALL print_error (TRIM(msg), &
           msg2=c_arg1,nopause=.false.)
      CASE (7)
        IF ((i_arg1>0).AND.(i_arg2>0)) THEN
           WRITE (UNIT=cfmt,FMT='(A,2(I1,A))') '(A,I',magnitude(i_arg1),',A,I',magnitude(i_arg2),',A)'
           WRITE (UNIT=msg,FMT=cfmt) c_arg2//' are inconsistent with X input file for the ', &
              i_arg1,get_cordn(i_arg1)//' field and the ',                                  &
              i_arg2,get_cordn(i_arg2)//' lag in file:'//c_arg1
        ELSE IF (i_arg1>0) THEN
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
           WRITE (UNIT=msg,FMT=cfmt) c_arg2//' are inconsistent with X input file for the ', &
              i_arg1,get_cordn(i_arg1)//' field in file:'//c_arg1
        ELSE
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg2),',A)'
           WRITE (UNIT=msg,FMT=cfmt) c_arg2//' are inconsistent with X input file for the ', &
              i_arg2,get_cordn(i_arg2)//' lag in file:'//c_arg1
        END IF
        CALL print_error (TRIM(msg), &
           msg2=c_arg1,nopause=.false.)
      CASE (8)
        CALL print_error ('Season is inconsistent with X input file in file:', &
           msg2=c_arg1,nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('draw_map')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Problem reading boundaries file. Country boundaries incomplete.', &
           msg2='Try reinstalling boundaries files in the Bondaries subdirectory.',nopause=.true.)
      CASE (2)
        CALL print_error &
           ('The requested boundaries file does not exist in the Boundaires subdirectory. Country boundaries not drawn.', &
           msg2='Reinstal boundaries file in the Boundaries subdirectory.',nopause=.true.)
      CASE (3)
        CALL print_error ('Problem reading lakes file. Lakes incomplete.', &
           msg2='Try reinstalling lakes files in the Bondaries subdirectory.',nopause=.true.)
      CASE (4)
        CALL print_error ('The requested lakes file does not exist in the Boundaires subdirectory. Lakes not drawn.', &
           msg2='Reinstal lakes file in the Boundaries subdirectory.',nopause=.true.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('eof_opts')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Maximum number of '//c_arg1//' EOF modes cannot be less than minimum number.', &
           nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('file_version')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Problem opening file:'//c_arg1//&
           &'Cannot find file. Try checking file path and name.',nopause=.false.)
      CASE (2)
        CALL print_error ('Problem opening file:'//c_arg1//&
           &'File is protected. Try closing file in other applications.',nopause=.false.)
      CASE (3)
        CALL print_error ('Problem opening file:'//c_arg1,nopause=.false.)
      CASE (4)
        CALL print_error ('Problem reading file:'//c_arg1,nopause=.false.)
      CASE (5)
        WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
        WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//'File version number ',i_arg1,' is invalid.'
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('get_area')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Southern domain limit is north of northern domain limit.&
                          &Latitudinal limits have been reset. Please check new settings.',nopause=.false.)
      CASE (2)
        CALL print_error ('Northern domain limit is south of southernmost data.&
                          &Northern domain limit has been reset. Please check new settings.',nopause=.false.)
      CASE (3)
        CALL print_error ('Southern domain limit is north of northernmost data.&
                          &Southern domain limit has been reset. Please check new settings.',nopause=.false.)
      CASE (4)
        CALL print_error ('Longitudinal extent is too small.&
                          &Longitudinal limits have been reset. Please check new settings.',nopause=.false.)
      CASE (5)
        CALL print_error ('Invalid longitudinal extent.&
                          &Longitudinal limits have been reset. Please check new settings.',nopause=.false.)
     END SELECT
!
   CASE ('get_date')
     SELECT CASE (ifail)
      CASE (1)
        GOTO 1
      CASE (2)
        IF (LEN_TRIM(c_arg2)>0) THEN
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading date in file:'//c_arg1//&
                 &'Last successfully read date in the ',i_arg1,get_cordn(i_arg1)//' field: '//TRIM(c_arg2)
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading date in file:'//c_arg1//&
                 &'Last successfully read date: '//TRIM(c_arg2)
           ENDIF
        ELSE
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading first date in the ',i_arg1,get_cordn(i_arg1)//' field in file:'//c_arg1
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading first date in file:'//c_arg1
           ENDIF
        END IF
        CALL print_error (msg,nopause=.false.)
      CASE (3)
        IF (LEN_TRIM(c_arg2)>0) THEN
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Invalid month in file:'//c_arg1//&
                 &'Last successfully read date in the ',i_arg1,get_cordn(i_arg1)//' field: '//TRIM(c_arg2)
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Invalid month in file:'//c_arg1//&
                 &'Last successfully read date: '//TRIM(c_arg2)
           ENDIF
        ELSE
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Invalid month in first date in the ', &
                 i_arg1,get_cordn(i_arg1)//' field in file:'//c_arg1
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Invalid month in first date in file:'//c_arg1
           ENDIF
        END IF
        CALL print_error (msg,nopause=.false.)
      CASE (4)
        IF (LEN_TRIM(c_arg2)>0) THEN
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Invalid day of month in file:'//c_arg1//&
                 &'Last successfully read date in the ',i_arg1,get_cordn(i_arg1)//' field: '//TRIM(c_arg2)
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Invalid day of month in file: '//c_arg1//&
                 &'Last successfully read date: '//TRIM(c_arg2)
           ENDIF
        ELSE
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Invalid day of month in first date in the ', &
                 i_arg1,get_cordn(i_arg1)//' field in file:'//c_arg1
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Invalid day of month in first date in file:'//c_arg1
           ENDIF
        END IF
        CALL print_error (msg,nopause=.false.)
      CASE (5)
        IF (LEN_TRIM(c_arg2)>0) THEN
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Invalid month and day of month in file:'//c_arg1//&
                 &'Last successfully read date in the ',i_arg1,get_cordn(i_arg1)//' field: '//TRIM(c_arg2)
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Invalid month and day of month in file:'//c_arg1//&
                 &'Last successfully read date: '//TRIM(c_arg2)
           ENDIF
        ELSE
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Invalid month and day of month in first date in the ', &
                 i_arg1,get_cordn(i_arg1)//' field in file:'//c_arg1
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Invalid month and day of month in first date in file:'//c_arg1
           ENDIF
        END IF
        CALL print_error (msg,nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('get_forecast')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Insufficient memory to produce forecast',nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('get_gridded_block_v10')
     SELECT CASE (ifail)
      CASE (1)
        IF (LEN_TRIM(c_arg2)>0) THEN
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                 &'Last successfully read data for '//TRIM(c_arg2)//' in the ',i_arg1,get_cordn(i_arg1)//' field.'
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file:'//c_arg1//&
                 &'Last successfully read data for '//TRIM(c_arg2)
           ENDIF
        ELSE
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                 &'Problem reading the ',i_arg1,get_cordn(i_arg1)//' field.'
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file:'//c_arg1
           ENDIF
        END IF
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE (2)
        CALL print_error ('Problem reading file: '//c_arg1//'End of file reached.',nopause=.false.)
      CASE (3)
        IF (LEN_TRIM(c_arg2)>0) THEN
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                 &'Missing ''cpt:field'' tag. &
                 &Last successfully read data for '//TRIM(c_arg2)//' in the ',i_arg1,get_cordn(i_arg1)//' field.'
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file:'//c_arg1//&
                 &'Missing ''cpt:field'' tag. Last successfully read data for '//TRIM(c_arg2)
           ENDIF
        ELSE
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                 &'Missing ''cpt:field'' tag in the ',i_arg1,get_cordn(i_arg1)//' field.'
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file:'//c_arg1//'Missing ''cpt:field'' tag.'
           ENDIF
        END IF
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE (4)
        IF (LEN_TRIM(c_arg2)>0) THEN
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
           WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                &'The data structure for the ',i_arg1,get_cordn(i_arg1)//' field must be gridded.&
                &Last successfully read data for '//TRIM(c_arg2)
        ELSE
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
           WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                &'The data structure for the ',i_arg1,get_cordn(i_arg1)//' field must be gridded.'
        END IF
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('get_gridded_dimensions_v9')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_warning ('Maximum file width reached. File may have been truncated:'//c_arg1)
      CASE (2)
        CALL print_error ('Problem reading file:'//c_arg1//&
           &'Number of '//TRIM(c_arg3)//' appears to be zero.',nopause=.false.)
      CASE (3)
        CALL print_error ('Problem reading first line in file:'//c_arg1,nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('get_gridded_fields_v10')
     SELECT CASE (ifail)
      CASE (1)
        IF (LEN_TRIM(c_arg2)>0) THEN
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                 &'Last successfully read data for '//TRIM(c_arg2)//' in the ',i_arg1,get_cordn(i_arg1)//' field.'
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file:'//c_arg1//&
                 &'Last successfully read data for '//TRIM(c_arg2)
           ENDIF
        ELSE
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                 &'Problem reading the ',i_arg1,get_cordn(i_arg1)//' field.'
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file:'//c_arg1
           ENDIF
        END IF
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE (2)
        CALL print_error ('Problem reading file:'//c_arg1//'End of file reached.',nopause=.false.)
      CASE (3)
        IF (LEN_TRIM(c_arg2)>0) THEN
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                 &'Missing ''cpt:field'' tag. &
                 &Last successfully read data for '//TRIM(c_arg2)//' in the ',i_arg1,get_cordn(i_arg1)//' field.'
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file:'//c_arg1//&
                 &'Missing ''cpt:field'' tag. Last successfully read data for '//TRIM(c_arg2)
           ENDIF
        ELSE
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                 &'Missing ''cpt:field'' tag in the ',i_arg1,get_cordn(i_arg1)//' field.'
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file:'//c_arg1//'Missing ''cpt:field'' tag.'
           ENDIF
        END IF
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE (4)
        IF (LEN_TRIM(c_arg2)>0) THEN
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                 &'Dates are not in sequence in the ',i_arg1,get_cordn(i_arg1)//' field.'//&
                 &'Last successfully read data for '//TRIM(c_arg2)
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file:'//c_arg1//&
                 &'Dates are not in sequence.'//&
                 &'Last successfully read data for '//TRIM(c_arg2)
           ENDIF
        ELSE
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                 &'Dates are not in sequence in the ',i_arg1,get_cordn(i_arg1)//' field.'
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file:'//c_arg1//&
                 &'Dates are not in sequence.'
           ENDIF
        END IF
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('get_gridded_info_v10')
     SELECT CASE (ifail)
      CASE (1)
        IF (LEN_TRIM(c_arg2)>0) THEN
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                 &'Last successfully read data for '//TRIM(c_arg2)//' in the ',i_arg1,get_cordn(i_arg1)//' field.'
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file:'//c_arg1//&
                 &'Last successfully read data for '//TRIM(c_arg2)
           ENDIF
        ELSE
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                 &'Problem reading the ',i_arg1,get_cordn(i_arg1)//' field.'
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file:'//c_arg1
           ENDIF
        END IF
        CALL print_error (msg,nopause=.false.)
      CASE (2)
        CALL print_error ('Problem reading file:'//c_arg1//' End of file reached.',nopause=.false.)
      CASE (3)
        IF (LEN_TRIM(c_arg2)>0) THEN
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                 &'Unable to read '//TRIM(c_arg3)//' in the ',i_arg1,get_cordn(i_arg1)//' field.'//&
                 &'Last successfully read data for '//TRIM(c_arg2)
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file:'//c_arg1//&
                 &'Unable to read '//TRIM(c_arg3)//'.'//&
                 &'Last successfully read data for '//TRIM(c_arg2)
           ENDIF
        ELSE
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                 &'Unable to read '//TRIM(c_arg3)//' in the ',i_arg1,get_cordn(i_arg1)//' field.'
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file:'//c_arg1//&
                 &'Unable to read '//TRIM(c_arg3)//'.'
           ENDIF
        END IF
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE (4)
        IF (LEN_TRIM(c_arg2)>0) THEN
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                 &'Dates are not in sequence in the ',i_arg1,get_cordn(i_arg1)//' field.'//&
                 &'Last successfully read data for '//TRIM(c_arg2)
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file:'//c_arg1//&
                 &'Dates are not in sequence.'//&
                 &'Last successfully read data for '//TRIM(c_arg2)
           ENDIF
        ELSE
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                 &'Dates are not in sequence in the ',i_arg1,get_cordn(i_arg1)//' field.'
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file:'//c_arg1//&
                 &'Dates are not in sequence.'
           ENDIF
        END IF
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('get_gridded_latlons_v9')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Problem reading file:'//c_arg1,nopause=.false.)
      CASE (2)
        CALL print_error ('Problem reading file:'//c_arg1//&
           &'End of file reached.',nopause=.false.)
      CASE (3)
        CALL print_error ('Problem reading file:'//c_arg1//&
             &'Latitudes are not consecutive.',nopause=.false.)
      CASE (4)
        CALL print_error ('Problem reading file:'//c_arg1//&
             &'Latitudes are not consecutive. Try checking for duplicate latitudes.',nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('get_gridded_nt_v9')
     SELECT CASE (ifail)
      CASE (1)
        IF (LEN_TRIM(c_arg2)>0) THEN
           CALL print_error ('Problem reading file:'//c_arg1,&
              msg2='Last successfully read data for '//TRIM(c_arg2),nopause=.false.)
        ELSE
           CALL print_error ('Problem reading file:'//c_arg1,nopause=.false.)
        END IF
      CASE (2)
        IF (LEN_TRIM(c_arg2)>0) THEN
           CALL print_error ('Problem reading file:'//c_arg1,&
              msg2='End of file reached.Last successfully read data for '//TRIM(c_arg2),nopause=.false.)
        ELSE
           CALL print_error ('Problem reading file:'//c_arg1,&
              msg2='End of file reached.',nopause=.false.)
        END IF
      CASE (3)
        CALL print_error ('Problem reading file:'//c_arg1,&
           msg2=TRIM(c_arg3)//' for '//TRIM(c_arg2)//' are inconsistent with those for earlier dates.',nopause=.false.)
      CASE (4)
        CALL print_error ('Problem reading file:'//c_arg1,&
            msg2=TRIM(c_arg3)//' sequencing of data is not permitted in CPT version 9 input files.',nopause=.false.)
      CASE (5)
        IF (LEN_TRIM(c_arg2)>0) THEN
           CALL print_error ('Problem reading file: '//c_arg1,&
               msg2='Problem with date format. Last successfully read data for '//TRIM(c_arg2),nopause=.false.)
        ELSE
           CALL print_error ('Problem reading file: '//c_arg1,&
               msg2='Problem with date format for first date.',nopause=.false.)
        END IF
      CASE (6)
        CALL print_error ('Problem allocating memory to read file: '//c_arg1,nopause=.false.)
      CASE (7)
        CALL print_error ('Problem reading file:'//c_arg1,&
           msg2='Dates are not in sequence. Last successfully read data for '//TRIM(c_arg2),nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('get_gridded_v10')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Problem allocating memory to read file:'//c_arg1,nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('get_ifile')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Only unreferenced X input files can be used for MLR',nopause=.false.)
      CASE (2)
        CALL print_error ('Only gridded X input files can be used for GCM',nopause=.false.)
      CASE (3)
        CALL print_error ('Multi-field or multi-lagged X input files can be used for GCM',nopause=.false.)
      CASE (4)
        CALL print_error ('Insufficient number of '//TRIM(cg_seqs_l(i_arg1))//' in file: '//c_arg1,nopause=.false.)
      CASE (5)
        CALL print_error &
             ('File sequencing must be consistent'//&
              cg_seq(i_arg1)//' sequencing in '//c_arg1,&
              msg2=cg_seq(i_arg2)//' sequencing in '//c_arg2,nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('get_nongridded_dimensions_v9')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_warning ('Maximum file width reached. File may have been truncated:',msg2=c_arg1)
      CASE (2)
        CALL print_error ('Problem reading file:',msg2=c_arg1//&
           &'Number of '//TRIM(c_arg2)//' appears to be zero.',nopause=.false.)
      CASE (3)
        CALL print_error ('Problem reading first line in file:',msg2=c_arg1,nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('get_nongridded_fields_v10')
     SELECT CASE (ifail)
      CASE (1)
        IF (LEN_TRIM(c_arg2)>0) THEN
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file: '//c_arg1//&
                 &'Last successfully read data for '//TRIM(c_arg2)//' in the ',i_arg1,get_cordn(i_arg1)//' field.'
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file: '//c_arg1//&
                 &'Last successfully read data for '//TRIM(c_arg2)
           ENDIF
        ELSE
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file: '//c_arg1//&
                 &'Problem reading the ',i_arg1,get_cordn(i_arg1)//' field.'
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file:'//c_arg1
           ENDIF
        END IF
        CALL print_error (msg,nopause=.false.)
      CASE (2)
        CALL print_error ('Problem reading file:'//c_arg1,msg2='End of file reached.',nopause=.false.)
      CASE (3)
        IF (i_arg1>0) THEN
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
           WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file: '//c_arg1//&
              &'Season for last date in the ',i_arg1,get_cordn(i_arg1)//' field is inconsistent with earlier dates.'
           CALL print_error (msg,nopause=.false.)
        ELSE
           CALL print_error ('Problem reading file:'//c_arg1,&
                msg2='Season for last date is inconsistent with earlier dates.',nopause=.false.)
        ENDIF
      CASE (4)
        IF (i_arg1>0) THEN
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
           WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file: '//c_arg1//&
              &'Last date in the ',i_arg1,get_cordn(i_arg1)//' field is not in sequence.'
           CALL print_error (msg,nopause=.false.)
        ELSE
           CALL print_error ('Problem reading file: '//c_arg1,&
              msg2='Last date is not in sequence.',nopause=.false.)
        ENDIF
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('get_nongridded_info_v10')
     SELECT CASE (ifail)
      CASE (1)
        IF (LEN_TRIM(c_arg2)>0) THEN
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                 &'Last successfully read data for '//TRIM(c_arg2)//' in the ',i_arg1,get_cordn(i_arg1)//' field.'
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file:'//c_arg1//&
                 &'Last successfully read data for '//TRIM(c_arg2)
           ENDIF
        ELSE
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
                 &'Problem reading the ',i_arg1,get_cordn(i_arg1)//' field.'
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file:'//c_arg1
           ENDIF
        END IF
        CALL print_error (msg,nopause=.false.)
      CASE (2)
        CALL print_error ('Problem reading file:'//c_arg1,msg2='End of file reached.',nopause=.false.)
      CASE (3)
        IF (i_arg1>0) THEN
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
           WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
              &'Unable to read '//TRIM(c_arg3)//' for the ',i_arg1,get_cordn(i_arg1)//' field.'
           CALL print_error (msg,nopause=.false.)
        ELSE
           CALL print_error ('Problem reading file:'//c_arg1,&
                msg2='Unable to read '//c_arg3//'.',nopause=.false.)
        ENDIF
      CASE (4)
        IF (i_arg1>0) THEN
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
           WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file:'//c_arg1//&
              &'Dates are not consecutive in the ',i_arg1,get_cordn(i_arg1)//' field. Last successfully read data for: '//c_arg2
           CALL print_error (msg,nopause=.false.)
        ELSE
           CALL print_error ('Problem reading file:'//c_arg1,&
                msg2='Dates are not consecutive. Last successfully read data for: '//c_arg2,nopause=.false.)
        ENDIF
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('get_nongridded_latlons_v9')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Problem reading file: '//c_arg1,nopause=.false.)
      CASE (2)
        CALL print_error ('Problem reading file: '//c_arg1,&
           msg2='End of file reached.',nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('get_nongridded_nls_v10')
     SELECT CASE (ifail)
      CASE (1)
        IF (LEN_TRIM(c_arg2)>0) THEN
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file: '//c_arg1//&
                 &'Last successfully read data for '//TRIM(c_arg2)//' in the ',i_arg1,get_cordn(i_arg1)//' field.'
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file: '//c_arg1//&
                 &'Last successfully read data for '//TRIM(c_arg2)
           ENDIF
        ELSE
           IF (i_arg1>0) THEN
              WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
              WRITE (UNIT=msg,FMT=cfmt) 'Problem reading file: '//c_arg1//&
                 &'Problem reading the ',i_arg1,get_cordn(i_arg1)//' field.'
           ELSE
              WRITE (UNIT=msg,FMT='(A)') 'Problem reading file: '//c_arg1
           ENDIF
        END IF
        CALL print_error (msg,nopause=.false.)
      CASE (2)
        CALL print_error ('Problem reading file: '//c_arg1//' End of file reached.',nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('get_nongridded_nt_v9')
     SELECT CASE (ifail)
      CASE (1)
        IF (LEN_TRIM(c_arg2)>0) THEN
           CALL print_error ('Problem reading file: '//c_arg1//&
              &' Last successfully read data for '//TRIM(c_arg2),nopause=.false.)
        ELSE
           CALL print_error ('Problem reading file: '//c_arg1,nopause=.false.)
        END IF
      CASE (2)
        CALL print_error ('Problem reading file: '//c_arg1//&
           &' '//TRIM(c_arg3)//' sequencing of data is not permitted in CPT version 9 input files.',nopause=.false.)
      CASE (3)
        CALL print_error ('Problem allocating memory to read file: '//c_arg1,nopause=.false.)
      CASE (4)
        CALL print_error ('Problem reading file: '//c_arg1//&
           &' Dates are not in sequence. Last successfully read data for '//TRIM(c_arg2),nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('get_nongridded_v10')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Problem allocating memory to read file: '//c_arg1,nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('get_old_file')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Invalid file name. CPT cannot read files with the %% character in the file name.',nopause=.false.)
      CASE DEFAULT
        GOTO 1
      END SELECT
!
   CASE ('get_ptype')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Not a valid project file.',nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('get_sequence')
     SELECT CASE (ifail)
      CASE (1)
        IF (i_arg1>0) THEN
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
           WRITE (UNIT=msg,FMT=cfmt) 'Identical dates in file: '//c_arg1&
                &//' Last correct date read in the ',i_arg1,get_cordn(i_arg1)//' field: '//c_arg2
           CALL print_error (msg,nopause=.false.)
        ELSE
           CALL print_error ('Identical dates in file: '//c_arg1&
                &//' Last correct date read: '//c_arg2,nopause=.false.)
        END IF
      CASE (2)
        IF (i_arg1>0) THEN
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
           WRITE (UNIT=msg,FMT=cfmt) 'Years are not consecutive in file: '//c_arg1&
                &//' Last correct date read in the ',i_arg1,get_cordn(i_arg1)//' field: '//c_arg2
           CALL print_error (msg,nopause=.false.)
        ELSE
           CALL print_error ('Years are not consecutive in file: '//c_arg1,&
                msg2=' Last correct date read: '//c_arg2,nopause=.false.)
        END IF
      CASE (3)
        IF (i_arg1>0) THEN
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
           WRITE (UNIT=msg,FMT=cfmt) 'Months are not consecutive in file: '//c_arg1&
                &//' Last correct date read in the ',i_arg1,get_cordn(i_arg1)//' field: '//c_arg2
           CALL print_error (msg,nopause=.false.)
        ELSE
           CALL print_error ('Months are not consecutive in file: '//c_arg1,&
                msg2=' Last correct date read: '//c_arg2,nopause=.false.)
        END IF
      CASE (4)
        IF (i_arg1>0) THEN
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
           WRITE (UNIT=msg,FMT=cfmt) 'Years and months are not consecutive in file: '//c_arg1&
                &//' Last correct date read in the ',i_arg1,get_cordn(i_arg1)//' field: '//c_arg2
           CALL print_error (msg,nopause=.false.)
        ELSE
           CALL print_error ('Years and months are not consecutive in file: '//c_arg1,&
                msg2=' Last correct date read: '//c_arg2,nopause=.false.)
        END IF
      CASE (5)
        IF (i_arg1>0) THEN
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
           WRITE (UNIT=msg,FMT=cfmt) 'Days are not consecutive in file: '//c_arg1&
                &//' Last correct date read in the ',i_arg1,get_cordn(i_arg1)//' field: '//c_arg2
           CALL print_error (msg,nopause=.false.)
        ELSE
           CALL print_error ('Days are not consecutive in file: '//c_arg1,&
                msg2=' Last correct date read: '//c_arg2,nopause=.false.)
        END IF
      CASE (6)
        IF (i_arg1>0) THEN
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
           WRITE (UNIT=msg,FMT=cfmt) 'Years and days are not consecutive in file: '//c_arg1&
                &//' Last correct date read in the ',i_arg1,get_cordn(i_arg1)//' field: '//c_arg2
           CALL print_error (msg,nopause=.false.)
        ELSE
           CALL print_error ('Years and days are not consecutive in file: '//c_arg1&
                &//' Last correct date read: '//c_arg2,nopause=.false.)
        END IF
      CASE (7)
        IF (i_arg1>0) THEN
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
           WRITE (UNIT=msg,FMT=cfmt) 'Months and days are not consecutive in file: '//c_arg1&
                &//' Last correct date read in the ',i_arg1,get_cordn(i_arg1)//' field: '//c_arg2
           CALL print_error (msg,nopause=.false.)
        ELSE
           CALL print_error ('Months and days are not consecutive in file: '//c_arg1&
                &//' Last correct date read: '//c_arg2,nopause=.false.)
        END IF
      CASE (8)
        IF (i_arg1>0) THEN
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
           WRITE (UNIT=msg,FMT=cfmt) 'Years, months, and days are not consecutive in file: '//c_arg1&
                &//' Last correct date read in the ',i_arg1,get_cordn(i_arg1)//' field: '//c_arg2
           CALL print_error (msg,nopause=.false.)
        ELSE
           CALL print_error ('Years, months, and days are not consecutive in file: '//c_arg1,&
                msg2=' Last correct date read: '//c_arg2,nopause=.false.)
        END IF
      CASE (9)
        IF (i_arg1>0) THEN
           WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
           WRITE (UNIT=msg,FMT=cfmt) 'Inconsistent length of season in file: '//c_arg1&
                &//' Last correct date read in the ',i_arg1,get_cordn(i_arg1)//' field: '//c_arg2
           CALL print_error (msg,nopause=.false.)
        ELSE
           CALL print_error ('Inconsistent length of season in file: '//c_arg1,&
                msg2=' Last correct date read: '//c_arg2,nopause=.false.)
        END IF
     END SELECT
!
   CASE ('get_structure_v9')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Problem allocating memory to read file: '//c_arg1,nopause=.false.)
      CASE (2)
        CALL print_error ('Problem reading file: '//c_arg1,nopause=.false.)
      CASE (3)
        CALL print_error ('Problem reading file: '//c_arg1//&
           &' End of file reached.',nopause=.false.)
      CASE (4)
        CALL print_error ('Problem reading file: '//c_arg1//&
           &' File is not a valid CPT format.',nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('get_structure_v10')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Problem reading file: '//c_arg1,nopause=.false.)
      CASE (2)
        CALL print_error ('Problem reading file: '//c_arg1//&
           &' End of file reached.',nopause=.false.)
      CASE (3)
        CALL print_error ('Missing cpt:nfields tag in file: '//c_arg1,nopause=.false.)
      CASE (4)
        CALL print_error ('Unrecognised header line in file: '//c_arg1,nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('get_tags')
     SELECT CASE (ifail)
      CASE (-1)
        WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
        WRITE (UNIT=msg,FMT=cfmt) 'Invalid combination of tags in the ',i_arg1, &
           get_cordn(i_arg1)//' field in file: '//c_arg1
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE (-2)
        GOTO 1
      CASE (1)
        WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
        WRITE (UNIT=msg,FMT=cfmt) 'Problem reading ''cpt:'//c_arg3//''' tag for the ',i_arg1, &
           get_cordn(i_arg1)//' field in file: '//c_arg1
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE (2)
        WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
        WRITE (UNIT=msg,FMT=cfmt) 'Missing ''cpt:'//c_arg3//''' tag for the ',i_arg1, &
           get_cordn(i_arg1)//' field in file: '//c_arg1
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE (3)
        WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
        WRITE (UNIT=msg,FMT=cfmt) 'Invalid date at ''cpt:S'' tag for the ',i_arg1, &
           get_cordn(i_arg1)//' field in file: '//c_arg1
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE (4)
        WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
        WRITE (UNIT=msg,FMT=cfmt) 'Value for ''cpt:row'' tag is incompatible with ''cpt:col=X'' for the ',i_arg1, &
           get_cordn(i_arg1)//' field in file: '//c_arg1
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE (5)
        WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
        WRITE (UNIT=msg,FMT=cfmt) 'Value for ''cpt:row'' tag is incompatible with ''cpt:col=station'' for the ',i_arg1, &
           get_cordn(i_arg1)//' field in file: '//c_arg1
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE (6)
        WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
        WRITE (UNIT=msg,FMT=cfmt) 'Value for ''cpt:row'' tag is incompatible with ''cpt:col=index'' for the ',i_arg1, &
           get_cordn(i_arg1)//' field in file: '//c_arg1
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE (7)
        WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
        WRITE (UNIT=msg,FMT=cfmt) 'Unknown value for ''cpt:col'' tag for the ',i_arg1, &
           get_cordn(i_arg1)//' field in file: '//c_arg1
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('get_user')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Unable to create new user. Check permissions for '//c_arg1, &
           nopause=.true.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('init_boot')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Insufficient memory to perform bootstrapping', &
           nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('init_fcast')
     SELECT CASE (ifail)
      CASE (3)
        CALL print_warning &
           ('First '//TRIM(cg_seq_l(i_arg1))//' from which to forecast is before first '//TRIM(cg_seq_l(i_arg1))//' in file.', &
           msg2='First '//TRIM(cg_seq_l(i_arg1))//' from which to forecast has been reset.')
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('init_gui')
     SELECT CASE (ifail)
      CASE (1)
        WRITE (cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
        WRITE (msg,FMT=cfmt) 'Insufficient memory for results windows.&
          &//Maximum number of windows requested is ',i_arg1,'.'
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('init_labels')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Problem reading labels file.', &
           msg2='File does not exist. Try reinstalling the labels file in the Labels subdirectory.', &
           nopause=.false.)
      CASE (2,3)
        CALL print_error ('Problem opening labels file.', &
           msg2='Try reinstalling the labels file in the Labels subdirectory.', &
           nopause=.false.)
      CASE (4)
        CALL print_error ('Problem reading labels file.', &
           msg2='Try reinstalling the labels file in the Labels subdirectory.', &
           nopause=.false.)
      CASE (5)
        CALL print_error ('Problem reading labels file.', &
           msg2='Premature end of file. Try reinstalling the labels file in the Labels subdirectory.', &
           nopause=.false.)
      CASE (6)
        CALL print_error ('Problem reading labels file.', &
           msg2='Labels file is out of date. Try reinstalling the labels file in the Labels subdirectory.', &
           nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('init_perm')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Insufficient memory to calculate p-values', &
           nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('init_read')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Insufficient memory to read data', &
           nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('init_scores')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Insufficient memory to perform validation', &
           nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('init_sets')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Insufficient memory to initalise setting.', &
           msg2='Contact '//CPT_email,nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('init_skill')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Insufficient memory to construct skill maps', &
           nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('non_missing')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('The '//c_arg1//' file contains no non-missing series', &
           msg2=c_arg2,nopause=.false.)
      CASE (2)
        CALL print_error ('The '//c_arg1//' file contains at least one field with no non-missing series', &
           msg2=c_arg2,nopause=.false.)
      CASE (3)
        CALL print_advisory ('Insufficient series to use nearest neighbour; mean used instead')
      CASE (4)
        CALL print_advisory ('Insufficient memory to estimate missing values; mean used instead')
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('open_infile')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Problem opening file:'//c_arg1//&
           &'Cannot find file. Try checking file path and name.',nopause=.false.)
      CASE (2)
        CALL print_error ('Problem opening file:'//c_arg1//&
           &'File is protected. Try closing file in other applications.',nopause=.false.)
      CASE (3)
        CALL print_error ('Problem opening file:'//c_arg1,nopause=.false.)
      CASE (4)
        CALL print_error ('File has been modified. Please re-open input file:'//c_arg1,nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('open_project')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Not a valid project file.',nopause=.false.)
      CASE (2)
        CALL print_error ('Problem opening project file:'//c_arg1//&
           &'Cannot find file. Try checking file path and name.',nopause=.false.)
      CASE (3)
        CALL print_error ('Problem opening project file:'//c_arg1//&
           &'File is protected. Try closing file in other applications.',nopause=.false.)
      CASE (4)
        CALL print_error ('Problem opening project file:'//c_arg1,nopause=.false.)
      CASE (5)
        WRITE (UNIT=msg,FMT='(A,F6.2)') 'Project file is newer than CPT version. Project file version: ',r_arg1
        CALL print_error (msg,nopause=.false.)
      CASE (6)
        CALL print_error ('Problem reading project file.',nopause=.false.)
      CASE (7)
        CALL print_error ('End of project file reached.',nopause=.false.)
      CASE (8)
        CALL print_error ('Insufficient memory to initalise setting. Contact '//CPT_email,nopause=.false.)
      CASE (9)
        CALL print_error ('CPT version 9 project files are not fully supported.&
             &Please re-open input files.',nopause=.false.)
      CASE (10)
        CALL print_error ('Current version of Project File is old.'// &
             'Project File cannot be read.',nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('perform_CCA','perform_MLR','perform_PCR','perform_GCM')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Insufficient memory to perform '//c_arg1,nopause=.false.)
      CASE (2)
        CALL print_error ('Problem performing '//c_arg1,nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('prob_scores')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Insufficient memory to calculate probabilistic scores',nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('read_grid','read_stns','read_unrf')
     SELECT CASE (ifail)
      CASE (1)
        IF (PRESENT(c_arg2)) THEN
           CALL print_error ('Problem reading file:'//c_arg1,msg2=c_arg2,nopause=.false.)
        ELSE
           CALL print_error ('Problem reading file:'//c_arg1,nopause=.false.)
        END IF
      CASE (2)
        IF (PRESENT(c_arg2)) THEN
           CALL print_error ('End of file reached:'//c_arg1,msg2=c_arg2,nopause=.false.)
        ELSE
           CALL print_error ('End of file reached:'//c_arg1,nopause=.false.)
        END IF
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('read_ini')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Problem opening '//c_arg1//'file.', &
           msg2='Check that '//c_arg1//' is installed in the CPT directory:'//c_arg2,nopause=.true.)
      CASE (2)
        CALL print_error ('Problem reading '//c_arg1//'.', &
           msg2='Try reinstalling '//c_arg1//' in the CPT directory.',nopause=.true.)
      CASE (3)
        CALL print_error ('Problem reading '//c_arg1//'.', &
           msg2='Try deleting '//c_arg1//' to reinitialize current user.',nopause=.true.)
      CASE (4)
        CALL print_error ('Problem reading '//c_arg1//'.', &
           msg2='File version is inconsistent with the current version of CPT.'&
           &//'Try reinstalling '//c_arg1//' in the CPT directory.',nopause=.true.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('roc_diagram')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Insufficient memory to construct ROC diagram', &
           nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('run_analysis')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Only unreferenced X input files can be used for MLR',nopause=.false.)
      CASE (2)
        CALL print_error ('Only gridded X input files can be used for GCM',nopause=.false.)
      CASE (3)
        CALL print_error ('Only station or gridded Y input files can be used for GCM',nopause=.false.)
      CASE (4)
        CALL print_error &
             ('Data contain negative values when zero-bound is set',msg2=c_arg1,nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('save_graphic')
     SELECT CASE (ifail)
      CASE (0)
        CALL print_advisory ('Graphic saved as '//c_arg1, &
           msg2='In '//c_arg2)
      CASE (1)
        CALL print_error ('Unable to save graphic', &
           nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('save_project')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Problem opening project file: '//c_arg1//&
           &' Cannot find file. Try checking file path and name.',nopause=.false.)
      CASE (2)
        CALL print_error ('Problem opening project file: '//c_arg1//&
           &' File is protected. Try closing file in other applications.',nopause=.false.)
      CASE (3)
        CALL print_error ('Problem opening project file: '//c_arg1,nopause=.false.)
      CASE (4)
        CALL print_error ('Problem saving project file.',nopause=.false.)
      CASE (5)
        CALL print_advisory ('Project saved as '//c_arg1, &
           msg2='In '//c_arg2)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('set_nused')
     SELECT CASE (ifail)
      CASE (1)
        WRITE (cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
        WRITE (msg,FMT=cfmt) &
           'Length of initial training period must be at least cross-validation window length + ',i_arg1,'.&
          &Initial length of training period has been reset.'
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('set_zero')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_warning ('%% of average standardization option is not available when the zero-bound option is off.&
           & No standardization has been set.')
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('tailoring')
     SELECT CASE (ifail)
      CASE (1)
        WRITE (UNIT=cfmt,FMT='(A,I1,A)') '(A,I',magnitude(i_arg1),',A)'
        WRITE (UNIT=msg,FMT=cfmt) 'Invalid analogue: ',i_arg1,'. Date is omitted because of missing values.'
        CALL print_error (TRIM(msg),nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('threshold_opts')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('The sum of the probabilities of the above- and below-normal categories must be less than 1.0.&
           & Please reset.',nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('valid_date')
     SELECT CASE (ifail)
      CASE (1)
        IF (LEN_TRIM(c_arg3)>1) THEN
           CALL print_error ('Problem reading date in file: '//c_arg1//&
              &' Invalid month. Last correct date read: '//c_arg2,nopause=.false.)
        ELSE
           CALL print_error ('Problem reading date in file: '//c_arg1//&
              &' Invalid month for first date.',nopause=.false.)
        END IF
      CASE (2)
        IF (LEN_TRIM(c_arg3)>1) THEN
           CALL print_error ('Problem reading date in file: '//c_arg1//&
              &' Invalid day of month. Last correct date read: '//c_arg2,nopause=.false.)
        ELSE
           CALL print_error ('Problem reading date in file: '//c_arg1//&
              &' Invalid day of month for first date.',nopause=.false.)
        END IF
      CASE (3)
        IF (LEN_TRIM(c_arg3)>1) THEN
           CALL print_error ('Problem reading date in file: '//c_arg1//&
              &' Invalid month and day of month. Last correct date read: '//c_arg2,nopause=.false.)
        ELSE
           CALL print_error ('Problem reading date in file: '//c_arg1//&
              &' Invalid month and day of month for first date.',nopause=.false.)
        END IF
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('write_results')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Problem opening '//c_arg1//' file:', &
           msg2=c_arg2,nopause=.false.)
      CASE (2)
        CALL print_error ('Problem writing cpt:T tag in '//c_arg1//' file:', &
           msg2=c_arg2,nopause=.false.)
      CASE (3)
        CALL print_error ('Problem saving '//c_arg1//' file:', &
           msg2=c_arg2,nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE ('wrlt_diagram')
     SELECT CASE (ifail)
      CASE (1)
        CALL print_error ('Insufficient memory to construct profits diagram', &
           nopause=.false.)
      CASE DEFAULT
        GOTO 1
     END SELECT
!
   CASE DEFAULT
     GOTO 1
  END SELECT
  RETURN
!
! Incorrect call
1 WRITE (msg,'(A,I2)') &
        'Unknown error. Please contact '//CPT_email//' providing the following information:'&
     &//'Routine: '//cproc&
     &//'Error: ',ifail
  CALL print_error (TRIM(msg),nopause=.false.)
  RETURN
 END SUBROUTINE error
END MODULE errors
