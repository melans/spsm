MODULE time
!
! Modules
  USE time_constants
!
! Scalars
!
! Integer scalars
  INTEGER, PUBLIC :: iseq=0 ! - time sequence identifier -
!
! Derived type definitions
!
! - date -
  TYPE date
     INTEGER :: iyr ! - year -
     INTEGER :: imn ! - month -
     INTEGER :: idy ! - day -
  END TYPE date
!
! - period -
  TYPE period
     TYPE(date) :: sdate ! - start date -
     TYPE(date) :: edate ! - end date -
  END TYPE period
!
! Interfaces
!
! Interface assignments
  INTERFACE ASSIGNMENT(=)
     MODULE PROCEDURE assign_date
  END INTERFACE
!
  INTERFACE ASSIGNMENT(=)
     MODULE PROCEDURE assign_period
  END INTERFACE
!
! Interface operators
  INTERFACE OPERATOR(==)
     MODULE PROCEDURE same_period
  END INTERFACE
!
  INTERFACE OPERATOR(==)
     MODULE PROCEDURE same_date
     MODULE PROCEDURE equal_date
  END INTERFACE
!
  INTERFACE OPERATOR(<)
     MODULE PROCEDURE lt_date
  END INTERFACE
!
  INTERFACE OPERATOR(<=)
     MODULE PROCEDURE le_date
  END INTERFACE
!
  INTERFACE OPERATOR(>)
     MODULE PROCEDURE gt_date
  END INTERFACE
!
  INTERFACE OPERATOR(>=)
     MODULE PROCEDURE ge_date
  END INTERFACE
!
  INTERFACE OPERATOR(+)
     MODULE PROCEDURE add_date
  END INTERFACE
!
  INTERFACE OPERATOR(-)
     MODULE PROCEDURE minus_date
  END INTERFACE
!
  INTERFACE OPERATOR(+)
     MODULE PROCEDURE add_period
  END INTERFACE
!
  INTERFACE OPERATOR(-)
     MODULE PROCEDURE minus_period
  END INTERFACE
!
! Generic interfaces
  INTERFACE get_sequence
   MODULE PROCEDURE get_sequence_dates
   MODULE PROCEDURE get_sequence_periods
  END INTERFACE get_sequence
!
  INTERFACE get_cdate
   MODULE PROCEDURE get_cdate_date
   MODULE PROCEDURE get_cdate_period
   MODULE PROCEDURE get_cdate_lsn
  END INTERFACE get_cdate
!
CONTAINS
!
!
 SUBROUTINE assign_date (d1,i)
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: i ! - assignment value -
!
! Output scalars
  TYPE(date), INTENT(OUT) :: d1 ! - date -
!
! Executable Statements
!
! Assign dates
  d1%iyr=i
  d1%imn=i
  d1%idy=i
!
  RETURN
 END SUBROUTINE assign_date
!
!
!
 SUBROUTINE assign_period (p1,i)
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: i ! - assignment value -
!
! Output scalars
  TYPE(period), INTENT(OUT) :: p1 ! - period -
!
! Executable Statements
!
! Assign periods
  p1%sdate=i
  p1%edate=i
!
  RETURN
 END SUBROUTINE assign_period
!
!
!
 FUNCTION same_period(p1,p2)
!
! Function type
  LOGICAL :: same_period
!
! Arguments
!
! Input scalars
  TYPE(period), INTENT(IN) :: p1 ! - first period -
  TYPE(period), INTENT(IN) :: p2 ! - second period -
!
! Executable Statements
!
! Compare dates
  same_period=.false.
  IF (p1%sdate%iyr/=p2%sdate%iyr) RETURN
  IF (p1%sdate%imn/=p2%sdate%imn) RETURN
  IF (p1%sdate%idy/=p2%sdate%idy) RETURN
  IF (p1%edate%iyr/=p2%edate%iyr) RETURN
  IF (p1%edate%imn/=p2%edate%imn) RETURN
  IF (p1%edate%idy/=p2%edate%idy) RETURN
  same_period=.true.
!
  RETURN
 END FUNCTION same_period
!
!
!
 FUNCTION same_date(d1,d2)
!
! Function type
  LOGICAL :: same_date
!
! Arguments
!
! Input scalars
  TYPE(date), INTENT(IN) :: d1 ! - first date -
  TYPE(date), INTENT(IN) :: d2 ! - second date -
!
! Executable Statements
!
! Compare dates
  same_date=.false.
  IF (d1%iyr/=d2%iyr) RETURN
  IF (d1%imn/=d2%imn) RETURN
  IF (d1%idy/=d2%idy) RETURN
  same_date=.true.
!
  RETURN
 END FUNCTION same_date
!
!
!
 FUNCTION equal_date(d1,i1)
!
! Function type
  LOGICAL :: equal_date
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: i1 ! - constant -
!
  TYPE(date), INTENT(IN) :: d1 ! - first date -
!
! Executable Statements
!
! Compare dates
  equal_date=.false.
  IF (d1%iyr/=i1) RETURN
  IF (d1%imn/=i1) RETURN
  IF (d1%idy/=i1) RETURN
  equal_date=.true.
!
  RETURN
 END FUNCTION equal_date
!
!
!
 FUNCTION lt_date(d1,d2)
!
! Function type
  LOGICAL :: lt_date
!
! Arguments
!
! Input scalars
  TYPE(date), INTENT(IN) :: d1 ! - first date -
  TYPE(date), INTENT(IN) :: d2 ! - second date -
!
! Executable Statements
!
! Compare dates
  lt_date=.true.
  IF (d1%iyr<d2%iyr) THEN
     RETURN
  ELSE IF (d1%iyr==d2%iyr) THEN
     IF (d1%imn<d2%imn) THEN
        RETURN
     ELSE IF (d1%imn==d2%imn) THEN
        IF (d1%idy<d2%idy) RETURN
     END IF
  END IF
  lt_date=.false.
!
  RETURN
 END FUNCTION lt_date
!
!
!
 FUNCTION le_date(d1,d2)
!
! Function type
  LOGICAL :: le_date
!
! Arguments
!
! Input scalars
  TYPE(date), INTENT(IN) :: d1 ! - first date -
  TYPE(date), INTENT(IN) :: d2 ! - second date -
!
! Executable Statements
!
! Compare dates
  le_date=.true.
  IF ((d1==d2).OR.(d1<d2)) RETURN
  le_date=.false.
!
  RETURN
 END FUNCTION le_date
!
!
!
 FUNCTION gt_date(d1,d2)
!
! Function type
  LOGICAL :: gt_date
!
! Arguments
!
! Input scalars
  TYPE(date), INTENT(IN) :: d1 ! - first date -
  TYPE(date), INTENT(IN) :: d2 ! - second date -
!
! Executable Statements
!
! Compare dates
  gt_date=.true.
  IF (d1%iyr>d2%iyr) THEN
     RETURN
  ELSE IF (d1%iyr==d2%iyr) THEN
     IF (d1%imn>d2%imn) THEN
        RETURN
     ELSE IF (d1%imn==d2%imn) THEN
        IF (d1%idy>d2%idy) RETURN
     END IF
  END IF
  gt_date=.false.
!
  RETURN
 END FUNCTION gt_date
!
!
!
 FUNCTION ge_date(d1,d2)
!
! Function type
  LOGICAL :: ge_date
!
! Arguments
!
! Input scalars
  TYPE(date), INTENT(IN) :: d1 ! - first date -
  TYPE(date), INTENT(IN) :: d2 ! - second date -
!
! Executable Statements
!
! Compare dates
  ge_date=.true.
  IF ((d1==d2).OR.(d1>d2)) RETURN
  ge_date=.false.
!
  RETURN
 END FUNCTION ge_date
!
!
!
 FUNCTION add_date(d,i)
!
! Function type
  TYPE(date) :: add_date
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: i ! - increment -
!
  TYPE(date), INTENT(IN) :: d ! - date -
!
! Executable Statements
!
! Increment date
  add_date=d
  SELECT CASE (iseq)
   CASE (1) ! - year increment -
     add_date%iyr=d%iyr+i
   CASE (2) ! - month increment -
     add_date%imn=d%imn+i
     SELECT CASE (add_date%imn)
      CASE (1:nmn)
        CONTINUE
      CASE (:0)
        DO
           add_date%iyr=add_date%iyr-1
           add_date%imn=add_date%imn+nmn
           IF (add_date%imn>0) EXIT
        END DO
      CASE (nmn+1:)
        DO
           add_date%iyr=add_date%iyr+1
           add_date%imn=add_date%imn-nmn
           IF (add_date%imn>0) EXIT
        END DO
     END SELECT
   CASE (3) ! - day increment -
     add_date%idy=d%idy+i
     IF (add_date%idy<1) THEN
        DO
           add_date%idy=add_date%idy+ndays(add_date%iyr,add_date%imn)
           add_date%imn=add_date%imn-1
           IF (add_date%imn<1) THEN
              add_date%iyr=add_date%iyr-1
              add_date%imn=add_date%imn+nmn
           END IF
           IF (add_date%idy>=1) EXIT
        END DO
     ELSE IF (add_date%idy>ndays(add_date%iyr,add_date%imn)) THEN
        DO
           add_date%idy=add_date%idy-ndays(add_date%iyr,add_date%imn)
           add_date%imn=add_date%imn+1
           IF (add_date%imn>nmn) THEN
              add_date%iyr=add_date%iyr+1
              add_date%imn=add_date%imn-nmn
           END IF
           IF (add_date%idy<=ndays(add_date%iyr,add_date%imn)) EXIT
        END DO
     END IF
  END SELECT
!
  RETURN
 END FUNCTION add_date
!
!
!
 FUNCTION add_period(p,i)
!
! Function type
  TYPE(period) :: add_period
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: i ! - increment -
!
  TYPE(period), INTENT(IN) :: p ! - period -
!
! Executable Statements
!
! Increment period
  add_period%sdate=p%sdate+i
  add_period%edate=p%edate+i
!
  RETURN
 END FUNCTION add_period
!
!
!
 FUNCTION minus_period(p,i)
!
! Function type
  TYPE(period) :: minus_period
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: i ! - increment -
!
  TYPE(period), INTENT(IN) :: p ! - period -
!
! Executable Statements
!
! Increment period
  minus_period%sdate=p%sdate-i
  minus_period%edate=p%edate-i
!
  RETURN
 END FUNCTION minus_period
!
!
!
 FUNCTION minus_date(d,i)
!
! Function type
  TYPE(date) :: minus_date
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: i ! - decrease -
!
  TYPE(date), INTENT(IN) :: d ! - date -
!
! Executable Statements
!
! Decrease date
  minus_date=add_date(d,-i)
!
  RETURN
 END FUNCTION minus_date
!
!
!
 SUBROUTINE init_time ()
!
! Initialises time
!
! Modules
  USE version
!
! Locals
!
! Local scalars
  INTEGER :: iyr1 ! - index -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC INDEX
!
! Executable Statements
!
! Identify year of first version
  iyr1=INDEX(date_ver1,',')+2
  cyr1=date_ver1(iyr1:iyr1+3)
!
! Identify year of current version
  iyr1=INDEX(date_this,',')+2
  cyr2=date_this(iyr1:iyr1+3)
!
  RETURN
 END SUBROUTINE init_time
!
!
!
 FUNCTION get_month(cmn)
!
! Identifies month
!
! Modules
  USE CPT_text, ONLY: nlang
!
! Function type
  INTEGER :: get_month
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=*), INTENT(IN) :: cmn ! - month -
!
! Locals
!
! Local scalars
  INTEGER :: i ! - language index -
  INTEGER :: l ! - month index -
!
! Executable Statements
!
! Identify month
  DO l=1,nmn
     DO i=1,nlang
        IF ((cmn(1:lcmon)==cmon(l,i)).OR.(cmn(1:lcmon)==umon(l,i))) THEN
           get_month=l
           RETURN
        END IF
     END DO
  END DO
  get_month=0
!
  RETURN
 END FUNCTION get_month
!
!
!
 FUNCTION ndays(iyr,imn)
!
! Calculates number of days in the month
! NB - assumes the Gregorian calendard as implemented by Britain and the British Empire
!
! Function type
  INTEGER :: ndays
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iyr ! - year -
  INTEGER, INTENT(IN) :: imn ! - month -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MOD
!
! Executable Statements
!
! Define number of days
  SELECT CASE (imn)
   CASE (1)  ! January
     ndays=31
   CASE (3)  ! March
     ndays=31
   CASE (4)  ! April
     ndays=30
   CASE (5)  ! May
     ndays=31
   CASE (6)  ! June
     ndays=30
   CASE (7)  ! July
     ndays=31
   CASE (8)  ! August
     ndays=31
   CASE (9)  ! September
     IF (iyr/=1752) THEN
        ndays=30
     ELSE
        ndays=19
     END IF
   CASE (10) ! October
     ndays=31
   CASE (11) ! November
     ndays=30
   CASE (12) ! December
     ndays=31
!
! Check for leap years
   CASE (2)  ! February
     IF (MOD(iyr,4)==0) THEN
        IF (MOD(iyr,100)==0) THEN
           IF (MOD(iyr,400)==0) THEN
              ndays=29
           ELSE
              ndays=28
           END IF
        ELSE
           ndays=29
        END IF
     ELSE
        ndays=28
     END IF
  END SELECT
!
  RETURN
 END FUNCTION ndays
!
!
!
 SUBROUTINE get_date (cdate,ctag,sdate,ifail,edate)
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
!
! Modules
  USE numbers, ONLY: digits
!
! Arguments
!
! Input scalars
  CHARACTER(LEN=1), INTENT(IN) :: cdate ! - date type -
  CHARACTER(LEN=*), INTENT(IN) :: ctag  ! - tags -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
  TYPE(date), INTENT(OUT) :: sdate ! - start date -
! - optional output scalars -
  TYPE(date), INTENT(OUT), OPTIONAL :: edate ! - end date -
!
! Locals
!
! Local scalars
  INTEGER :: i1 ! - start of date tag -
  INTEGER :: i2 ! - locator -
  INTEGER :: i3 ! - end of date tag -
  INTEGER :: i4 ! - locator -
  INTEGER :: it ! - date -
!
  LOGICAL :: lday ! - day present flag -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC INDEX
  INTRINSIC LEN_TRIM
  INTRINSIC PRESENT
  INTRINSIC SCAN
  INTRINSIC VERIFY
!
! Executable Statements
!
! Initialise dates
  sdate=0
!
! Locate date tag
  SELECT CASE (cdate)
   CASE (' ')
     i1=1
     i3=VERIFY(ctag,digits//'/-T:')-1
     IF (i3==-1) i3=LEN_TRIM(ctag)
   CASE ('S','T')
     i1=INDEX(ctag(1:),'cpt:'//cdate//'=')
     IF (i1==0) THEN
        ifail=1
        RETURN
     END IF
     i1=i1+LEN('cpt:'//cdate//'=')
     i3=INDEX(ctag(i1:),',')
     IF (i3>0) THEN
        i3=i1+i3-2
     ELSE
        i3=LEN_TRIM(ctag)
     END IF
  END SELECT
  IF (i3==0) GOTO 3
!
! Identify start year
  i2=INDEX(ctag(i1:i3),'-')
  IF (i2==0) THEN
     READ (UNIT=ctag(i1:i3),FMT=*,ERR=3) sdate%iyr
     IF (PRESENT(edate)) edate=sdate
     GOTO 2
  END IF
  i2=i1+i2-2
  READ (UNIT=ctag(i1:i2),FMT=*,ERR=3) sdate%iyr
!
! Read month
  i1=i2+2
  i2=SCAN(ctag(i1:i3),'/-T:')
  IF (i2==0) THEN
     READ (UNIT=ctag(i1:i3),FMT=*,ERR=3) sdate%imn
     IF (PRESENT(edate)) edate=sdate
     GOTO 2
  END IF
  i2=i1+i2-2
  READ (UNIT=ctag(i1:i2),FMT=*,ERR=3) sdate%imn
!
! Identify whether day is present
  lday=.false.
  i1=i2+1
  IF (ctag(i1:i1)=='-') THEN ! - day -
     i1=i1+1
     i2=INDEX(ctag(i1:i3),'/')
     IF (i2==0) THEN
        i2=INDEX(ctag(i1:i3),'T')
        IF (i2==0) THEN
           i2=i3
        ELSE
           i2=i1+i2-2
        END IF
        READ (UNIT=ctag(i1:i2),FMT=*,ERR=3) sdate%idy
        IF (PRESENT(edate)) edate=sdate
        GOTO 2
     END IF
     i2=i1+i2-2
     i4=INDEX(ctag(i1:i3),'T')
     IF (i4==0) THEN
        i4=i2
     ELSE
        i4=i1+i4-2
     END IF
     READ (UNIT=ctag(i1:i4),FMT=*,ERR=3) sdate%idy
     lday=.true.
  END IF
!
! Identify end date
  IF (PRESENT(edate)) THEN
     edate=sdate
     i1=i2+1
     IF (ctag(i1:i1)=='/') THEN
        i1=i1+1
1       i2=INDEX(ctag(i1:i3),'-')
        IF (i2==0) THEN
           i2=INDEX(ctag(i1:i3),'T')
           IF (i2==0) THEN
              i2=i3
           ELSE
              i2=i1+i2-2
           END IF
           READ (UNIT=ctag(i1:i2),FMT=*,ERR=3) it
           IF (lday) THEN
              edate%idy=it
           ELSE
              edate%imn=it
           END IF
        ELSE
           i2=i1+i2-2
           READ (UNIT=ctag(i1:i2),FMT=*,ERR=3) it
           i1=i2+2
           i2=INDEX(ctag(i1:i3),'-')
           IF (i2==0) THEN
              IF (lday) THEN
                 edate%imn=it
              ELSE
                 edate%iyr=it
              END IF
           ELSE
              edate%iyr=it
              i1=i1+i2+1
           END IF
           GOTO 1
        END IF
     ELSE
        GOTO 3
     END IF
  END IF
!
! Check validity of dates
2 ifail=valid_date(sdate)
  IF (ifail/=0) THEN
     ifail=ifail+2
     RETURN
  END IF
  IF (PRESENT(edate)) THEN
     ifail=valid_date(edate)
     IF (ifail/=0) THEN
        ifail=ifail+2
        RETURN
     END IF
  END IF
  ifail=0
  RETURN
!
! Errors
3 ifail=2
  RETURN
 END SUBROUTINE get_date
!
!
!
 FUNCTION valid_date(adate)
!
! Checks that date is valid
!
! On exit:
!    valid_date =  0 Valid
!    valid_date =  1 Invalid month
!    valid_date =  2 Invalid day of month
!    valid_date =  3 Invalid month and day of month
!
! Function type
  INTEGER :: valid_date
!
! Arguments
!
! Input scalars
  TYPE(date), INTENT(IN) :: adate ! - date -
!
! Executable Statements
!
! Check validity of date
  valid_date=0
  IF ((adate%imn>=1).AND.(adate%imn<=nmn)) THEN
     IF ((adate%idy<0).OR.(adate%idy>ndays(adate%iyr,adate%imn))) valid_date=valid_date+2
  ELSE IF (adate%imn/=0) THEN
     valid_date=valid_date+1
     IF ((adate%idy<0).OR.(adate%idy>mdm)) valid_date=valid_date+2
  ELSE
     IF (adate%idy/=0) THEN
        valid_date=valid_date+1
        IF ((adate%idy<1).OR.(adate%idy>ndays(adate%iyr,adate%imn))) valid_date=valid_date+2
     END IF
  END IF
!
  RETURN
 END FUNCTION valid_date
!
!
!
 SUBROUTINE check_date (date1,date2,iseq,ifail)
!
! Checks that second date is immediately after first,
! If iseq=1 checks that years are consecutive.
! If iseq=2 checks that months are consecutive
! If iseq=3 checks that days are consecutive.
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
!    ifail =  8 The value of iseq is invalid
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iseq ! - sequence indicator -
!
  TYPE(date), INTENT(IN) :: date1 ! - date 1 -
  TYPE(date), INTENT(IN) :: date2 ! - date 2 -
!
! Output scalars
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Executable Statements
!
! Check for identical dates
  IF (date2==date1) THEN
     ifail=-1
     RETURN
  END IF
!
! Check for yearly data
  ifail=0
  SELECT CASE (iseq)
   CASE (1)
     IF (date2%iyr/=date1%iyr+1) ifail=ifail+1
     IF (date2%imn/=date1%imn) ifail=ifail+2
     IF (date2%idy/=date1%idy) ifail=ifail+4
!
! Check for monthly data
   CASE (2)
     IF (date2%imn==date1%imn+1) THEN
        IF (date2%iyr/=date1%iyr) ifail=ifail+1
     ELSE IF ((date1%imn==nmn).AND.(date2%imn==1)) THEN ! - next year -
        IF (date2%iyr/=date1%iyr+1) ifail=ifail+1
     ELSE
        ifail=ifail+2
        IF (date1%imn<nmn) THEN
           IF (date2%iyr/=date1%iyr) ifail=ifail+1
        ELSE
           IF (date2%iyr/=date1%iyr+1) ifail=ifail+1
        END IF
     END IF
     IF (date2%iyr/=date1%idy) ifail=ifail+4
!
! Check for daily data
   CASE (3)
     IF (date2%idy==date1%idy+1) THEN
        IF (date2%imn/=date1%imn) ifail=ifail+2
        IF (date2%iyr/=date1%iyr) ifail=ifail+1
     ELSE IF ((date1%idy==ndays(date1%iyr,date1%imn)).AND.(date2%idy==1)) THEN ! - next month -
        IF (date2%imn==date1%imn+1) THEN
           IF (date2%iyr/=date1%iyr) ifail=ifail+1
        ELSE IF ((date1%imn==nmn).AND.(date2%imn==1)) THEN ! - next year -
           IF (date2%iyr/=date1%iyr+1) ifail=ifail+1
        ELSE
           ifail=ifail+2
           IF (date1%imn<nmn) THEN
              IF (date2%iyr/=date1%iyr) ifail=ifail+1
           ELSE
              IF (date2%iyr/=date1%iyr+1) ifail=ifail+1
           END IF
        END IF
     ELSE
        ifail=4
        IF (date1%idy<ndays(date1%iyr,date1%imn)) THEN
           IF (date2%imn/=date1%imn) ifail=ifail+2
           IF (date2%iyr/=date1%iyr) ifail=ifail+1
        ELSE
           IF (date1%imn<nmn) THEN
              IF (date2%imn/=date1%imn+1) ifail=ifail+2
              IF (date2%iyr/=date1%iyr) ifail=ifail+1
           ELSE
              IF (date2%imn/=1) ifail=ifail+2
              IF (date2%iyr/=date1%iyr+1) ifail=ifail+1
           END IF
        END IF
     END IF
   CASE DEFAULT
     ifail=8
  END SELECT
!
  RETURN
 END SUBROUTINE check_date
!
!
!
 FUNCTION date_diff(d1,d2,iseq)
!
! Function type
  INTEGER :: date_diff
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iseq ! - date sequence -
!
  TYPE(date), INTENT(IN) :: d1 ! - first date -
  TYPE(date), INTENT(IN) :: d2 ! - second date -
!
! Locals
!
! Local scalars
  INTEGER :: iya   ! - current year -
  INTEGER :: ima   ! - current month -
  INTEGER :: id    ! - direction -
  INTEGER :: iendy ! - end year -
  INTEGER :: iendm ! - end month -
!
! Executable Statements
!
! Compare dates
  SELECT CASE (iseq)
   CASE (1) ! - yearly -
     date_diff=d2%iyr-d1%iyr
     IF ((d1%imn>0).AND.(d2%imn>0)) THEN
        IF (d2%imn-d1%imn<0) THEN
           date_diff=date_diff-1
        ELSE IF ((d1%idy>0).AND.(d2%idy>0)) THEN
           IF (d2%imn-d1%imn<0) THEN
              IF (d2%idy-d1%idy<0) date_diff=date_diff-1
           END IF
        END IF
     END IF
   CASE (2) ! - monthly -
     date_diff=(d2%iyr-d1%iyr)*nmn+d2%imn-d1%imn
   CASE (3) ! - daily -
     date_diff=d2%idy-d1%idy
     IF (d2>d1) THEN
        id=1
     ELSE IF (d2<d1) THEN
        id=-1
     ELSE
        RETURN
     END IF
     IF ((d2%iyr==d1%iyr).AND.(d2%imn==d1%imn)) RETURN
     SELECT CASE (id)
      CASE (1)
        iya=d1%iyr
        ima=d1%imn
        iendy=d2%iyr
        iendm=d2%imn
      CASE (-1)
        iya=d2%iyr
        ima=d2%imn
        iendy=d1%iyr
        iendm=d1%imn
        date_diff=-date_diff
     END SELECT
     DO
        date_diff=date_diff+ndays(iya,ima)
        ima=ima+1
        IF (ima>nmn) THEN
           iya=iya+1
           ima=ima-nmn
        END IF
        IF ((iya==iendy).AND.(ima==iendm)) EXIT
     END DO
     date_diff=date_diff*id
   CASE DEFAULT
     date_diff=0
  END SELECT
!
  RETURN
 END FUNCTION date_diff
!
!
!
 SUBROUTINE get_sequence_dates (date1,date2,iseq,ifail)
!
! Identifies date sequencing.
! iseq=1 if years are consecutive
! iseq=2 if months are consecutive
! iseq=3 if days are consecutive
! iseq=4 if seasons are consecutive
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Dates are identical
!    ifail =  2 Years are not consecutive
!    ifail =  3 Months are not consecutive
!    ifail =  4 Years and months are not consecutive
!    ifail =  5 Days are not consecutive
!    ifail =  6 Years and days are not consecutive
!    ifail =  7 Months and days are not consecutive
!    ifail =  8 Years, months, and days are not consecutive
!
! Arguments
!
! Input scalars
  TYPE(date), INTENT(IN) :: date1 ! - date 1 -
  TYPE(date), INTENT(IN) :: date2 ! - date 2 -
!
! Output scalars
  INTEGER, INTENT(OUT) :: iseq  ! - date sequence indicator -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Executable Statements
!
! Check for date sequences
! - day is constant -
  iseq=0
  IF (date2%idy==date1%idy) THEN
     IF (date2%imn==date1%imn) THEN
        IF (date2%iyr==date1%iyr) THEN
           ifail=1
        ELSE IF (date2%iyr==date1%iyr+1) THEN
           iseq=1
           ifail=0
        ELSE
           ifail=2
        END IF
     ELSE IF (date2%imn==date1%imn+1) THEN
        IF (date2%iyr==date1%iyr) THEN
           iseq=2
           ifail=0
        ELSE
           ifail=2
        END IF
     ELSE IF ((date1%imn==nmn).AND.(date2%imn==1)) THEN
        IF (date2%iyr==date1%iyr+1) THEN
           iseq=2
           ifail=0
        ELSE
           ifail=2
        END IF
     ELSE
        IF (date_diff(date1,date2,2)<nmn) THEN
           iseq=4
           ifail=0
        ELSE
           ifail=3
           IF ((date2%iyr/=date1%iyr).AND.(date2%iyr/=date1%iyr+1)) ifail=ifail+1
        END IF
     END IF
! - days are consecutive -
  ELSE IF (date2%idy==date1%idy+1) THEN
     IF (date2%imn==date1%imn) THEN
        IF (date2%iyr==date1%iyr) THEN
           iseq=3
           ifail=0
        ELSE
           ifail=2
        END IF
     ELSE
        ifail=3
        IF (date2%iyr/=date1%iyr) ifail=ifail+1
     END IF
! - end of month -
  ELSE IF ((date1%idy==ndays(date1%iyr,date1%imn)).AND.(date2%idy==1)) THEN
     IF (date2%imn==date1%imn+1) THEN
        IF (date2%iyr==date1%iyr) THEN
           iseq=3
           ifail=0
        ELSE
           ifail=2
        END IF
! - end of year -
     ELSE IF ((date1%imn==nmn).AND.(date2%imn==1)) THEN
        IF (date2%iyr==date1%iyr+1) THEN
           iseq=3
           ifail=0
        ELSE
           ifail=2
        END IF
     ELSE
        ifail=3
        IF (date2%iyr/=date1%iyr) ifail=ifail+1
     END IF
! - days are not consecutive -
  ELSE
     ifail=5
     IF (date2%imn/=date1%imn) ifail=ifail+2
     IF (date2%iyr/=date1%iyr) ifail=ifail+1
  END IF
!
  RETURN
 END SUBROUTINE get_sequence_dates
!
!
!
 SUBROUTINE get_sequence_periods (period1,period2,iseq,ifail)
!
! Identifies period sequencing.
! iseq=1 if years are consecutive
! iseq=2 if months are consecutive
! iseq=3 if days are consecutive
! iseq=4 if seasons are consecutive
!
! On exit:
!    ifail =  0 Successful
!    ifail =  1 Dates are identical
!    ifail =  2 Years are not consecutive
!    ifail =  3 Months are not consecutive
!    ifail =  4 Years and months are not consecutive
!    ifail =  5 Days are not consecutive
!    ifail =  6 Years and days are not consecutive
!    ifail =  7 Months and days are not consecutive
!    ifail =  8 Years, months, and days are not consecutive
!    ifail =  9 Inconsistent length of season
!
! Arguments
!
! Input scalars
  TYPE(period), INTENT(IN) :: period1 ! - date 1 -
  TYPE(period), INTENT(IN) :: period2 ! - date 2 -
!
! Output scalars
  INTEGER, INTENT(OUT) :: iseq  ! - date sequence indicator -
  INTEGER, INTENT(OUT) :: ifail ! - error indicator -
!
! Executable Statements
!
! Check for start date sequences
  CALL get_sequence_dates (period1%sdate,period2%sdate,iseq,ifail)
  IF (ifail==0) THEN
     CALL get_sequence_dates (period1%edate,period2%edate,iseq,ifail)
     IF (ifail/=0) ifail=9
  END IF
!
  RETURN
 END SUBROUTINE get_sequence_periods
!
!
!
 FUNCTION check_sequence(ctag,iseq,period1,period0)
!
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
  INTEGER :: check_sequence
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iseq ! - sequence -
!
  CHARACTER(LEN=*), INTENT(IN) :: ctag ! - tag line -
!
  TYPE(period), INTENT(IN), OPTIONAL :: period0 ! - pervious period -
!
! Output scalars
  TYPE(period), INTENT(OUT) :: period1 ! - current period -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC TRIM
!
! Executable Statements
!
! Get current date
  CALL get_date ('T',TRIM(ctag),period1%sdate,check_sequence, &
       edate=period1%edate)
  IF (check_sequence/=0) RETURN
  IF (PRESENT(period0)) THEN
     CALL check_date (period0%sdate,period1%sdate,iseq,check_sequence)
     IF (check_sequence/=0) THEN
        check_sequence=check_sequence+3
        RETURN
     END IF
     IF (date_diff(period0%sdate,period0%edate,iseq)/= &
         date_diff(period1%sdate,period1%edate,iseq)) check_sequence=14
  END IF
!
  RETURN
 END FUNCTION check_sequence
!
!
!
 FUNCTION get_cdate_date(adate,ifmt) RESULT(get_cdate)
!
! Creates date as a character string
!
! If ifmt=1, ISO format
! If ifmt=2, common format
!
! Modules
  USE CPT_text,     ONLY: ilang
  USE IO_constants, ONLY: ldat
!
! Function type
  CHARACTER(LEN=ldat) :: get_cdate
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: ifmt ! - format indicator -
!
  TYPE(date), INTENT(IN) :: adate ! - date -
!
! Executable Statements
!
! Create date
  SELECT CASE (ifmt)
   CASE (1) ! - ISO format -
     IF (adate%imn>0) THEN
        IF (adate%idy>0) THEN
           WRITE (UNIT=get_cdate,FMT='(I4,A,I2.2,A,I2.2)') adate%iyr,'-',adate%imn,'-',adate%idy
        ELSE
           WRITE (UNIT=get_cdate,FMT='(I4,A,I2.2)') adate%iyr,'-',adate%imn
        END IF
     ELSE
        WRITE (UNIT=get_cdate,FMT='(I4)') adate%iyr
     END IF
   CASE (2) ! - common format -
     IF (adate%imn>0) THEN
        IF (adate%idy>0) THEN
           WRITE (UNIT=get_cdate,FMT='(I2.2,3A,I4)') adate%idy,' ',cmon(adate%imn,ilang),' ',adate%iyr
        ELSE
           WRITE (UNIT=get_cdate,FMT='(2A,I4)') cmon(adate%imn,ilang),' ',adate%iyr
        END IF
     ELSE
        WRITE (UNIT=get_cdate,FMT='(I4)') adate%iyr
     END IF
  END SELECT
!
  RETURN
 END FUNCTION get_cdate_date
!
!
!
 FUNCTION get_cdate_period(aperiod,ifmt) RESULT (get_cdate)
!
! Creates period as a character string
!
! If ifmt=1, ISO format
! If ifmt=2, common format
!
! Modules
  USE CPT_text,     ONLY: ilang
  USE IO_constants, ONLY: lprd
!
! Function type
  CHARACTER(LEN=lprd) :: get_cdate
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: ifmt ! - format indicator -
!
  TYPE(period), INTENT(IN) :: aperiod ! - date -
!
! Executable Statements
!
! Create date
  IF (aperiod%edate==aperiod%sdate) THEN
     get_cdate=get_cdate_date(aperiod%sdate,ifmt)
  ELSE
     SELECT CASE (ifmt)
      CASE (1) ! - ISO format -
        IF (aperiod%edate%iyr>aperiod%sdate%iyr) THEN
           get_cdate=TRIM(get_cdate_date(aperiod%sdate,ifmt))//'/'//get_cdate_date(aperiod%edate,ifmt)
        ELSE IF (aperiod%edate%imn/=aperiod%sdate%imn) THEN
           IF (aperiod%sdate%idy>0) THEN
              WRITE (UNIT=get_cdate,FMT='(I4,4(A,I2.2))') &
                 aperiod%sdate%iyr,'-',aperiod%sdate%imn,'-',aperiod%sdate%idy,'/',aperiod%edate%imn,'-',aperiod%edate%idy
           ELSE
              WRITE (UNIT=get_cdate,FMT='(I4,2(A,I2.2))') aperiod%sdate%iyr,'-',aperiod%sdate%imn,'/',aperiod%edate%imn
           END IF
        ELSE
           WRITE (UNIT=get_cdate,FMT='(I4,3(A,I2.2))') &
              aperiod%sdate%iyr,'-',aperiod%sdate%imn,'-',aperiod%sdate%idy,'/',aperiod%edate%idy
        END IF
      CASE (2) ! - common format -
        IF (aperiod%edate%iyr==aperiod%sdate%iyr) THEN
           IF (aperiod%sdate%idy==0) THEN
              IF (aperiod%sdate%imn>0) THEN
                 WRITE (UNIT=get_cdate,FMT='(2A,I4)') cma(ilang)(aperiod%sdate%imn:aperiod%edate%imn),' ',aperiod%sdate%iyr
              ELSE
                 WRITE (UNIT=get_cdate,FMT='(I4)') aperiod%sdate%iyr
              END IF
           ELSE
              IF (aperiod%edate%imn==aperiod%sdate%imn) THEN
                 WRITE (UNIT=get_cdate,FMT='(2(I2.2,A),2A,I4)') &
                    aperiod%sdate%idy,'/',aperiod%edate%idy,' ',cmon(aperiod%sdate%imn,ilang),' ',aperiod%sdate%iyr
              ELSE
                 WRITE (UNIT=get_cdate,FMT='(2(I2.2,3A),I4)') &
                    aperiod%sdate%idy,' ',cmon(aperiod%sdate%imn,ilang),' / ',&
                    aperiod%edate%idy,' ',cmon(aperiod%edate%imn,ilang),' ',aperiod%sdate%iyr
              END IF
           END IF
        ELSE
           IF (aperiod%sdate%idy==0) THEN
              WRITE (UNIT=get_cdate,FMT='(2A,I4,A,I2.2)') &
                 cma(ilang)(aperiod%sdate%imn:aperiod%edate%imn+nmn),' ',aperiod%sdate%iyr,'/',MOD(aperiod%edate%iyr,100)
           ELSE
              get_cdate=TRIM(get_cdate_date(aperiod%sdate,ifmt))//' / '//get_cdate_date(aperiod%edate,ifmt)
           END IF
        END IF
     END SELECT
  END IF
!
  RETURN
 END FUNCTION get_cdate_period
!
!
!
 FUNCTION get_cdate_lsn(sdate,lsn,ifmt) RESULT (get_cdate)
!
! Creates period as a character string
!
! If ifmt=1, ISO format
! If ifmt=2, common format
!
! Modules
  USE IO_constants, ONLY: lprd
!
! Function type
  CHARACTER(LEN=lprd) :: get_cdate
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: lsn  ! - length of season -
  INTEGER, INTENT(IN) :: ifmt ! - format indicator -
!
  TYPE(date), INTENT(IN) :: sdate ! - start date -
!
! Locals
!
! Local scalars
  TYPE(date) :: edate ! - end date -
!
  TYPE(period) :: aperiod ! - date -
!
! Executable Statements
!
! Calculate end date
  SELECT CASE (iseq)
   CASE (1)
     edate=sdate
     edate%imn=edate%imn+lsn-1
     DO
        IF (edate%imn>nmn) THEN
           edate%iyr=edate%iyr+1
           edate%imn=edate%imn-nmn
           CYCLE
        END IF
        EXIT
     END DO
   CASE DEFAULT
     edate=sdate+(lsn-1)
  END SELECT
  aperiod%sdate=sdate
  aperiod%edate=edate
!
! Construct date
  get_cdate=get_cdate_period(aperiod,ifmt)
!
  RETURN
 END FUNCTION get_cdate_lsn
!
!
!
 FUNCTION get_cssn(aperiod,iseq)
!
! Creates season as a character string
!
! Modules
  USE CPT_text, ONLY: ilang
!
! Function type
  CHARACTER(LEN=21) :: get_cssn
!
! Arguments
!
! Input scalars
  INTEGER, INTENT(IN) :: iseq ! - sequence -
!
  TYPE(period), INTENT(IN) :: aperiod ! - period -
!
! Executable Statements
!
! Create season
  IF (iseq==1.AND.aperiod%sdate%imn/=0.AND.aperiod%edate%imn/=0) THEN
     IF (aperiod%edate==aperiod%sdate) THEN
        get_cssn=cmon(aperiod%sdate%imn,ilang)
     ELSE IF (aperiod%edate%iyr==aperiod%sdate%iyr) THEN
        IF (aperiod%sdate%imn==aperiod%edate%imn) THEN
           get_cssn=cmon(aperiod%sdate%imn,ilang)
        ELSE
           IF (aperiod%edate%idy==aperiod%sdate%idy) THEN
              WRITE (UNIT=get_cssn,FMT='(A)') cma(ilang)(aperiod%sdate%imn:aperiod%edate%imn)
           ELSE
              WRITE (UNIT=get_cssn,FMT='(I2.2,3A,I2.2,2A)') &
                 aperiod%sdate%idy,' ',cmon(aperiod%sdate%imn,ilang),' / ',aperiod%edate%idy,' ',cmon(aperiod%edate%imn,ilang)
           END IF
        END IF
     ELSE
        IF (aperiod%sdate%idy==0) THEN
           WRITE (UNIT=get_cssn,FMT='(2A)')  &
              cma(ilang)(aperiod%sdate%imn:aperiod%edate%imn+nmn)
        ELSE
           WRITE (UNIT=get_cssn,FMT='(I2.2,3A,I2.2,2A)') &
              aperiod%sdate%idy,' ',cmon(aperiod%sdate%imn,ilang),' / ',aperiod%edate%idy,' ',cmon(aperiod%edate%imn,ilang)
        END IF
     END IF
  ELSE
     get_cssn=' '
  END IF
!
  RETURN
 END FUNCTION get_cssn
END MODULE time