! $Id: time_c_wrapper.f90 1160 2010-12-21 19:17:33Z lsong $
!
 FUNCTION valid_date_c_wrapper(adate_iyr,adate_imn,adate_idy) BIND(C)
!
! Checks that date is valid
!
! On exit:
!    valid_date =  0 Valid
!    valid_date =  1 Invalid month
!    valid_date =  2 Invalid day of month
!    valid_date =  3 Invalid month and day of month
!
! Modules
  USE, INTRINSIC:: ISO_C_BINDING
  USE time, ONLY: date, valid_date
!
  IMPLICIT NONE
!
! Function type
  INTEGER(KIND=C_INT) :: valid_date_c_wrapper
!
! Arguments
!
! Input scalars
  INTEGER(KIND=C_INT), INTENT(IN) :: adate_iyr
  INTEGER(KIND=C_INT), INTENT(IN) :: adate_imn
  INTEGER(KIND=C_INT), INTENT(IN) :: adate_idy

!
! Local scalars
  TYPE(date) :: adate ! - date -
!
! Executable Statements
!
  adate%iyr=adate_iyr
  adate%imn=adate_imn
  adate%idy=adate_idy

  valid_date_c_wrapper = valid_date(adate)
!
  RETURN
 END FUNCTION valid_date_c_wrapper
!
!
!
 SUBROUTINE check_date_c_wrapper( date1_iyr,date1_imn,date1_idy, &
                                  date2_iyr,date2_imn,date2_idy, &
                                  seq,ifail &
                                ) BIND(C)
!
! Checks that second date is immediately after first,
! If seq=1 checks that years are consecutive.
! If seq=2 checks that months are consecutive
! If seq=3 checks that days are consecutive.
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Years are not consecutive
!    ifail =  2 Months are not consecutive
!    ifail =  3 Years and months are not consecutive
!    ifail =  4 Days are not consecutive
!    ifail =  5 Years and days are not consecutive
!    ifail =  6 Months and days are not consecutive
!    ifail =  7 Years, months, and days are not consecutive
!    ifail =  8 The value of seq is invalid
!
! Modules
  USE, INTRINSIC:: ISO_C_BINDING
  USE time, Only:date,check_date
!
  IMPLICIT NONE
!
! Arguments
!
! Input scalars
  INTEGER(KIND=C_INT), INTENT(IN) :: seq ! - sequence indicator -
  INTEGER(KIND=C_INT), INTENT(IN) :: date1_iyr
  INTEGER(KIND=C_INT), INTENT(IN) :: date1_imn
  INTEGER(KIND=C_INT), INTENT(IN) :: date1_idy
  INTEGER(KIND=C_INT), INTENT(IN) :: date2_iyr
  INTEGER(KIND=C_INT), INTENT(IN) :: date2_imn
  INTEGER(KIND=C_INT), INTENT(IN) :: date2_idy
!
! Output scalars
  INTEGER(KIND=C_INT), INTENT(OUT) :: ifail ! - error indicator -

! Local scalars
!
  TYPE(date) :: date1 ! - date 1 -
  TYPE(date) :: date2 ! - date 2 -

!
! Executable Statements
!
  date1%iyr=date1_iyr
  date1%imn=date1_imn
  date1%idy=date1_idy
  date2%iyr=date2_iyr
  date2%imn=date2_imn
  date2%idy=date2_idy
  
  CALL check_date(date1,date2,seq,ifail)

  RETURN
 END SUBROUTINE check_date_c_wrapper
!
!
 SUBROUTINE get_sequence_dates_c_wrapper( date1_iyr,date1_imn,date1_idy, &
                                          date2_iyr,date2_imn,date2_idy, &
                                          seq,ifail &
                                        ) BIND(C)
!
! Checks that second date is immediately after first,
! seq=1 if years are consecutive.
! seq=2 if months are consecutive
! seq=3 if days are consecutive.
!
! On exit:
!    ifail =  0 Successful
!    ifail = -1 Dates are not consecutive
!    ifail =  1 Years are not consecutive
!    ifail =  2 Months are not consecutive
!    ifail =  3 Years and months are not consecutive
!    ifail =  4 Days are not consecutive
!    ifail =  5 Years and days are not consecutive
!    ifail =  6 Months and days are not consecutive
!    ifail =  7 Years, months, and days are not consecutive
!
! Modules
  USE, INTRINSIC:: ISO_C_BINDING
  USE time, ONLY: date, get_sequence_dates
!
  IMPLICIT NONE
!
! Arguments
!
! Input scalars
  INTEGER(KIND=C_INT), INTENT(IN) :: date1_iyr
  INTEGER(KIND=C_INT), INTENT(IN) :: date1_imn
  INTEGER(KIND=C_INT), INTENT(IN) :: date1_idy
  INTEGER(KIND=C_INT), INTENT(IN) :: date2_iyr
  INTEGER(KIND=C_INT), INTENT(IN) :: date2_imn
  INTEGER(KIND=C_INT), INTENT(IN) :: date2_idy
!
! Output scalars
  INTEGER(KIND=C_INT), INTENT(OUT) :: ifail ! - error indicator -
  INTEGER(KIND=C_INT), INTENT(OUT) :: seq   ! - sequence indicator -

! Local scalars
!
  TYPE(date) :: date1 ! - date 1 -
  TYPE(date) :: date2 ! - date 2 -

!
! Executable Statements
!
  date1%iyr=date1_iyr
  date1%imn=date1_imn
  date1%idy=date1_idy
  date2%iyr=date2_iyr
  date2%imn=date2_imn
  date2%idy=date2_idy
  
  CALL get_sequence_dates(date1,date2,seq,ifail)

  RETURN
 END SUBROUTINE get_sequence_dates_c_wrapper
!
!
 SUBROUTINE get_sequence_periods_c_wrapper( period0_date1_iyr, period0_date1_imn, period0_date1_idy, &
                                            period0_date2_iyr, period0_date2_imn, period0_date2_idy, &
                                            period1_date1_iyr, period1_date1_imn, period1_date1_idy, &
                                            period1_date2_iyr, period1_date2_imn, period1_date2_idy, &
                                            seq,ifail &
                                          ) BIND(C)
!
! Checks that second date is immediately after first,
! seq=1 if years are consecutive.
! seq=2 if months are consecutive
! seq=3 if days are consecutive.
!
! On exit:
!    ifail =  0 Successful
!    ifail = -1 Dates are consecutive
!    ifail =  1 Years are not consecutive
!    ifail =  2 Months are not consecutive
!    ifail =  3 Years and months are not consecutive
!    ifail =  4 Days are not consecutive
!    ifail =  5 Years and days are not consecutive
!    ifail =  6 Months and days are not consecutive
!    ifail =  7 Years, months, and days are not consecutive
!
! Modules
  USE, INTRINSIC:: ISO_C_BINDING
  USE time, ONLY: period, get_sequence_periods
!
  IMPLICIT NONE
!
! Arguments
!
! Input scalars
  INTEGER(KIND=C_INT), INTENT(IN) :: period0_date1_iyr
  INTEGER(KIND=C_INT), INTENT(IN) :: period0_date1_imn
  INTEGER(KIND=C_INT), INTENT(IN) :: period0_date1_idy
  INTEGER(KIND=C_INT), INTENT(IN) :: period0_date2_iyr
  INTEGER(KIND=C_INT), INTENT(IN) :: period0_date2_imn
  INTEGER(KIND=C_INT), INTENT(IN) :: period0_date2_idy
  INTEGER(KIND=C_INT), INTENT(IN) :: period1_date1_iyr
  INTEGER(KIND=C_INT), INTENT(IN) :: period1_date1_imn
  INTEGER(KIND=C_INT), INTENT(IN) :: period1_date1_idy
  INTEGER(KIND=C_INT), INTENT(IN) :: period1_date2_iyr
  INTEGER(KIND=C_INT), INTENT(IN) :: period1_date2_imn
  INTEGER(KIND=C_INT), INTENT(IN) :: period1_date2_idy
!
! Output scalars
  INTEGER(KIND=C_INT), INTENT(OUT) :: ifail ! - error indicator -
  INTEGER(KIND=C_INT), INTENT(OUT) :: seq   ! - sequence indicator -

! Local scalars
!
  TYPE(period) :: period0 ! - period 1 -
  TYPE(period) :: period1 ! - period 2 -

!
! Executable Statements
!
  period0%sdate%iyr=period0_date1_iyr
  period0%sdate%imn=period0_date1_imn
  period0%sdate%idy=period0_date1_idy
  period0%edate%iyr=period0_date2_iyr
  period0%edate%imn=period0_date2_imn
  period0%edate%idy=period0_date2_idy

  period1%sdate%iyr=period1_date1_iyr
  period1%sdate%imn=period1_date1_imn
  period1%sdate%idy=period1_date1_idy
  period1%edate%iyr=period1_date2_iyr
  period1%edate%imn=period1_date2_imn
  period1%edate%idy=period1_date2_idy
  
  CALL get_sequence_periods(period0,period1,seq,ifail)

  RETURN
 END SUBROUTINE get_sequence_periods_c_wrapper

!
 SUBROUTINE get_date_for_c (cdate,ctag,sdate_iyr,sdate_imn,sdate_idy,&
                            ifail,edate_iyr,edate_imn,edate_idy) BIND(C)
! This subroutine can be called from C function.

! It is wrapper for get_date. It accepts a fixed length
! ctag_in instead of dynamic length characters which C can not pass its string to
! it. Date is structure is not supported in C so all varaibales are assigned by
! its integer members.
!
!
! Identifies start and end dates and determines sequence
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Requested CPT date tag not present
!    ifail =  2 Problem with date format
!    ifail =  3 Invalid month
!    ifail =  4 Invalid day of month
!    ifail =  5 Invalid month and day of month
!    IF edate_iyr, edate_imn, edate_idy are assigned to -99, that means edate
!    'PRESENTED'. C can not have optional arguments. It is a workaround.
!
! Modules
  USE, INTRINSIC:: ISO_C_BINDING
  USE numbers,       ONLY: digits
  USE time,          ONLY: date, get_date
  USE IO_constants,  ONLY: lprd
!
  IMPLICIT NONE
!
! Arguments
!
! Input scalars
  CHARACTER(KIND=C_CHAR), INTENT(IN) :: cdate(1) ! - date type -
  CHARACTER(KIND=C_CHAR), INTENT(IN) :: ctag(lprd)  ! - tags - usually it won't exceed 128 bytes
!
! Output scalars
  INTEGER(KIND=C_INT), INTENT(OUT) :: ifail ! - error indicator -
!
! TYPE(date), INTENT(OUT) :: sdate ! - start date -
  INTEGER(KIND=C_INT), INTENT(OUT) :: sdate_iyr
  INTEGER(KIND=C_INT), INTENT(OUT) :: sdate_imn
  INTEGER(KIND=C_INT), INTENT(OUT) :: sdate_idy

! - optional output scalars -
  INTEGER(KIND=C_INT), INTENT(INOUT) :: edate_iyr
  INTEGER(KIND=C_INT), INTENT(INOUT) :: edate_imn
  INTEGER(KIND=C_INT), INTENT(INOUT) :: edate_idy

!
! Locals
!
! Local scalars
  INTEGER :: i  ! - index -
  TYPE(date) :: sdate ! - start date -
  TYPE(date) :: edate ! - end date -
  CHARACTER(LEN=lprd) :: ctag_in  ! - tags - usually it won't exceed 128 bytes
  CHARACTER(LEN=1) :: mycdate ! - date type -
!
!
! Executable Statements
!

  DO i=1,lprd
    ctag_in(i:i)=ctag(i)
  END DO
  mycdate(1:1)=cdate(1)

  IF(edate_iyr==-99.AND.edate_imn==-99.AND.edate_idy==-99) THEN
      ! edate presented
      CALL get_date(mycdate, ctag_in, sdate, ifail, edate)
      edate_iyr = edate%iyr
      edate_imn = edate%imn
      edate_idy = edate%idy
  ELSE
         CALL get_date(mycdate, ctag_in, sdate, ifail)
  END IF

  sdate_iyr = sdate%iyr
  sdate_imn = sdate%imn
  sdate_idy = sdate%idy

  RETURN
 END SUBROUTINE get_date_for_c
!
!
 FUNCTION date_diff_c_wrapper(d1_iyr,d1_imn,d1_idy,d2_iyr,d2_imn,d2_idy,iseq) BIND(C)
!
! Modules
  USE, INTRINSIC:: ISO_C_BINDING
  USE time, ONLY: date, date_diff
!
  IMPLICIT NONE
!
! Function type
  INTEGER(KIND=C_INT) :: date_diff_c_wrapper
!
! Arguments
!
! Input scalars
  INTEGER(KIND=C_INT), INTENT(IN) :: iseq ! - date sequence -
  INTEGER(KIND=C_INT), INTENT(IN) :: d1_iyr ! - date 1 year -
  INTEGER(KIND=C_INT), INTENT(IN) :: d1_imn ! - date 1 month -
  INTEGER(KIND=C_INT), INTENT(IN) :: d1_idy ! - date 1 dy -
  INTEGER(KIND=C_INT), INTENT(IN) :: d2_iyr ! - date 2 year -
  INTEGER(KIND=C_INT), INTENT(IN) :: d2_imn ! - date 2 month -
  INTEGER(KIND=C_INT), INTENT(IN) :: d2_idy ! - date 2 day -
!
!
! Locals
!
! Local scalars
  TYPE(date) :: d1 ! - first date -
  TYPE(date) :: d2 ! - second date -
!
! Executable Statements
!
! Compare dates
  d1%iyr = d1_iyr
  d1%imn = d1_imn
  d1%idy = d1_idy
  d2%iyr = d2_iyr
  d2%imn = d2_imn
  d2%idy = d2_idy

  date_diff_c_wrapper =  date_diff (d1,d2,iseq)
!
  RETURN
 END FUNCTION date_diff_c_wrapper

 FUNCTION check_sequence_for_c(ctag,iseq_c,period1_sdate_iyr, period1_sdate_imn, period1_sdate_idy,&
                                           period1_edate_iyr, period1_edate_imn, period1_edate_idy,&
                                           period0_sdate_iyr, period0_sdate_imn, period0_sdate_idy,&
                                           period0_edate_iyr, period0_edate_imn, period0_edate_idy&
                              ) BIND(C)
!
! Modules
  USE, INTRINSIC:: ISO_C_BINDING
  USE IO_constants, ONLY: lprd
  USE time
!
  IMPLICIT NONE

! On exit:
!    check_sequence =  0 Successful
!    check_sequence =  1 'cpt:T' tag not present
!    check_sequence =  2 Problem with date format
!    check_sequence =  3 Invalid month
!    check_sequence =  4 Invalid day of month
!    check_sequence =  5 Invalid month and day of month
!    check_sequence =  6 Years are not consecutive
!    check_sequence =  7 Months are not consecutive
!    check_sequence =  8 Years and months are not consecutive
!    check_sequence =  9 Days are not consecutive
!    check_sequence = 10 Years and days are not consecutive
!    check_sequence = 11 Months and days are not consecutive
!    check_sequence = 12 Years, months, and days are not consecutive
!    check_sequence = 13 The value of iseq is invalid
!    check_sequence = 14 Inconsistent lengths of seasons
!
! Function type
  INTEGER(KIND=C_INT) :: check_sequence_for_c
!
! Arguments
!
! Input scalars
  INTEGER(KIND=C_INT), INTENT(IN) :: iseq_c ! - sequence -
  INTEGER(KIND=C_INT), INTENT(IN) :: period0_sdate_iyr
  INTEGER(KIND=C_INT), INTENT(IN) :: period0_sdate_imn
  INTEGER(KIND=C_INT), INTENT(IN) :: period0_sdate_idy
  INTEGER(KIND=C_INT), INTENT(IN) :: period0_edate_iyr
  INTEGER(KIND=C_INT), INTENT(IN) :: period0_edate_imn
  INTEGER(KIND=C_INT), INTENT(IN) :: period0_edate_idy
!
  CHARACTER(KIND=C_CHAR), INTENT(IN) :: ctag(2048) ! - tag line -
!
!
! Output scalars
  INTEGER(KIND=C_INT), INTENT(OUT) :: period1_sdate_iyr  
  INTEGER(KIND=C_INT), INTENT(OUT) :: period1_sdate_imn  
  INTEGER(KIND=C_INT), INTENT(OUT) :: period1_sdate_idy
  INTEGER(KIND=C_INT), INTENT(OUT) :: period1_edate_iyr
  INTEGER(KIND=C_INT), INTENT(OUT) :: period1_edate_imn
  INTEGER(KIND=C_INT), INTENT(OUT) :: period1_edate_idy

!
! Local scalars
  TYPE(period) :: period1 ! - current period -
  TYPE(period) :: period0 ! - pervious period -
  CHARACTER(LEN=2048) :: myctag ! - tag line -
  INTEGER :: i  ! - index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC TRIM
!  ! Executable Statements
  DO i=1,2048
    myctag(i:i)=ctag(i)
  END DO
!
!  IF (PRESENT(period0)) THEN
    IF( (period0_sdate_iyr .NE. -99 ).AND. (period0_sdate_imn .NE. -99 ) &
     .AND.  (period0_sdate_idy .NE. -99) ) THEN
        period0%sdate%iyr = period0_sdate_iyr;
        period0%sdate%imn = period0_sdate_imn;
        period0%sdate%idy = period0_sdate_idy;
        period0%edate%iyr = period0_edate_iyr;
        period0%edate%imn = period0_edate_imn;
        period0%edate%idy = period0_edate_idy;
        check_sequence_for_c = check_sequence(myctag,iseq_c,period1,period0);
    ELSE
        ! no period0 presented
        check_sequence_for_c = check_sequence(myctag,iseq_c,period1);

    END IF

!
  period1_sdate_iyr = period1%sdate%iyr ; 
  period1_sdate_imn = period1%sdate%imn ; 
  period1_sdate_idy = period1%sdate%idy ; 
  period1_edate_iyr = period1%edate%iyr ; 
  period1_edate_imn = period1%edate%imn ; 
  period1_edate_idy = period1%edate%idy ; 
  RETURN
 END FUNCTION check_sequence_for_c

 SUBROUTINE get_cdate_c_wrapper(iyr1,imn1,idy1,iyr2,imn2,idy2,lsn,ifmt,datestr,strlen) BIND(C)
!
! Modules
  USE, INTRINSIC:: ISO_C_BINDING
  USE time, ONLY: get_cdate,period, date
  USE IO_constants, ONLY: lprd 
!
  IMPLICIT NONE
!
! Arguments
!
! Input scalars
  INTEGER(KIND=C_INT), INTENT(IN) :: iyr1
  INTEGER(KIND=C_INT), INTENT(IN) :: imn1
  INTEGER(KIND=C_INT), INTENT(IN) :: idy1
  INTEGER(KIND=C_INT), INTENT(IN) :: iyr2  ! -99 means not presented -
  INTEGER(KIND=C_INT), INTENT(IN) :: imn2  ! -99 means not presented -
  INTEGER(KIND=C_INT), INTENT(IN) :: idy2  ! -99 means not presented -
  INTEGER(KIND=C_INT), INTENT(IN) :: lsn   ! -99 means not presented -
  INTEGER(KIND=C_INT), INTENT(IN) :: ifmt  ! - format indicator - 
  CHARACTER(KIND=C_CHAR), INTENT(OUT) :: datestr(lprd)  ! - date string representing -
  INTEGER(KIND=C_INT), INTENT(OUT) :: strlen  ! - lenght of datestr - 
!
! Local scalars
  INTEGER:: i             ! - index -
  TYPE(period) :: period1 ! - pervious period -
  TYPE(date) :: date1     ! - pervious period -
  TYPE(date) :: date2     ! - pervious period -
  CHARACTER(LEN=lprd) :: datestr_local 

  date1%iyr = iyr1;
  date1%imn = imn1;
  date1%idy = idy1;
  date2%iyr = iyr2;
  date2%imn = imn2;
  date2%idy = idy2;

  IF( ( iyr2 .eq. -99 ) .and. (imn2 .eq. -99) .and. (idy2 .eq. -99) ) THEN

    IF( lsn .eq. -99) THEN
        datestr_local = get_cdate(date1,ifmt)
    ELSE
        datestr_local = get_cdate(date1,lsn,ifmt)
    END IF
  ELSE
      period1%sdate = date1;
      period1%edate = date2;
      datestr_local = get_cdate(period1,ifmt)
  END IF

  strlen = LEN_TRIM(datestr_local)
  DO i=1,strlen
     datestr(i) = datestr_local(i:i)
  END DO

 END SUBROUTINE get_cdate_c_wrapper

 SUBROUTINE get_period_c_wrapper(iyr1,imn1,idy1,iyr2,imn2,idy2,n,symbol) BIND(C)
!
! Modules
  USE, INTRINSIC:: ISO_C_BINDING
  USE time
!
  IMPLICIT NONE
!
! Arguments
!
! Input scalars
  INTEGER(KIND=C_INT), INTENT(INOUT) :: iyr1
  INTEGER(KIND=C_INT), INTENT(INOUT) :: imn1
  INTEGER(KIND=C_INT), INTENT(INOUT) :: idy1
  INTEGER(KIND=C_INT), INTENT(INOUT) :: iyr2
  INTEGER(KIND=C_INT), INTENT(INOUT) :: imn2
  INTEGER(KIND=C_INT), INTENT(INOUT) :: idy2
  INTEGER(KIND=C_INT), INTENT(IN)    :: n 
  CHARACTER(KIND=C_CHAR), INTENT(IN)  :: symbol

! Local scalars
  TYPE(period) :: p

  p%sdate%iyr = iyr1
  p%sdate%imn = imn1
  p%sdate%idy = idy1
  p%edate%iyr = iyr2
  p%edate%imn = imn2
  p%edate%idy = idy2

  IF( symbol == '+' ) THEN
     p = p+n
 ELSE IF ( symbol == '-' ) THEN
     p = p-n
 END IF 

 iyr1 = p%sdate%iyr
 imn1 = p%sdate%imn
 idy1 = p%sdate%idy
 iyr2 = p%edate%iyr
 imn2 = p%edate%imn
 idy2 = p%edate%idy

 END SUBROUTINE get_period_c_wrapper

 FUNCTION get_month_c_wrapper(cmn) BIND(C)
!
! Modules
  USE, INTRINSIC:: ISO_C_BINDING
  USE time, ONLY: get_month
!
! Identifies month
!
! Function type
  INTEGER(KIND=C_INT) :: get_month_c_wrapper
!
! Arguments
!
! Input scalars
  CHARACTER(KIND=C_CHAR), INTENT(IN) :: cmn(9) ! - month -
!
! Local scalar
  CHARACTER(LEN=9) :: mycmn ! - month -
 
  DO i=1,9
     mycmn(i:i)=cmn(i)
  END DO

  get_month_c_wrapper = get_month(mycmn)

 END FUNCTION get_month_c_wrapper
