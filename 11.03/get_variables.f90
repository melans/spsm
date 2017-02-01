! $Id: get_variables.f90 1215 2011-02-25 21:30:20Z simon $
!
! This files contains get_XXX function or subroutines which will return public
! variables from Fortran modules. It is bridge to C interoperabablilty. Those
! functions are subroutines are C binded according Fortran 2003 standard.
!
MODULE getvariables 
!
! Modules
  USE numbers, ONLY: rp
!
! Implicit decalarations
 IMPLICIT NONE

 CONTAINS

 SUBROUTINE get_fields_dsdy(fillme,fillmelen) BIND(C)
 !
 ! Module
   USE,INTRINSIC:: ISO_C_BINDING
   USE fields, ONLY: dsdy
 !
 ! Input
   INTEGER(KIND=C_INT), INTENT(OUT) :: fillmelen  ! fillme len
 !
 ! Output 
   CHARACTER(KIND=C_CHAR), INTENT(OUT) :: fillme(30) ! will be assigned to dsdy
 !
 ! Local scalars
   CHARACTER(LEN=30) :: locstr ! will be assigned to dsdy
 !
 ! Functions and Subroutines
 !
 ! Intrinsic functions
   INTRINSIC LEN_TRIM
   INTRINSIC TRIM

   locstr=TRIM(dsdy)
   fillme=TRIM(locstr)
   fillmelen = LEN_TRIM(locstr);
   RETURN
 END SUBROUTINE get_fields_dsdy

 SUBROUTINE get_climatologyperiod(fillme,fillmelen) BIND(C)
 !
 ! Module
   USE,INTRINSIC:: ISO_C_BINDING
   USE categories, ONLY: climate_per 
 !
 ! Input
   INTEGER(KIND=C_INT), INTENT(OUT) :: fillmelen  ! fillme len
 !
 ! Output 
   CHARACTER(KIND=C_CHAR), INTENT(OUT) :: fillme(30) ! will be assigned to dsdy
 !
 ! Local Scalar
   CHARACTER(LEN=30) :: locstr ! will be assigned to dsdy
 !
 ! Functions and Subroutines
 !
 ! Intrinsic functions
   INTRINSIC LEN_TRIM
   INTRINSIC TRIM

   locstr = TRIM(climate_per%clim1)//' to '//TRIM(climate_per%clim2);
   fillme = locstr;
   fillmelen=LEN_TRIM(locstr)
   RETURN
 END SUBROUTINE get_climatologyperiod

 SUBROUTINE get_thresholds(fillme,fillmelen) BIND(C)
 !
 ! Module
   USE,INTRINSIC:: ISO_C_BINDING
   USE categories,      ONLY: ithr,anlg,iay 
   USE settings,        ONLY: iprec,iva
   USE CPT_constants,   ONLY: nts,cthr,ng
   USE arrays,          ONLY: tobs,y
   USE maths,           ONLY: magnitude 
 !
 ! Output 
   INTEGER(KIND=C_INT), INTENT(OUT) :: fillmelen  ! fillme len
 !
   CHARACTER(KIND=C_CHAR), INTENT(OUT) :: fillme(60) ! will be assigned to thresholds values
 !
 ! Locals
 !
 ! Local arrays
   REAL(KIND=rp) :: codds(ng) ! - climatological odds -
   CHARACTER(LEN=60) :: locstr ! will be assigned to dsdy
 !
 ! Local scalars
   INTEGER :: j    ! - threshold/category index -
   INTEGER :: k    ! - forecast index -
   INTEGER :: jj   ! - threshold index -
   INTEGER :: jmin ! - minimum threshold index -
 !
   REAL(KIND=rp) :: terr ! - threshold difference -
   REAL(KIND=rp) :: tmin ! - minimum threshold difference -
 !
   CHARACTER(LEN=19) :: fmt ! - format statement -
   CHARACTER(LEN=30) :: tmp ! will be assigned to dsdy
   CHARACTER(LEN=60) :: fill ! will be assigned to dsdy
 !
 ! Functions and Subroutines
 !
 ! Intrinsic functions
   INTRINSIC LEN_TRIM
   INTRINSIC TRIM

 !
 ! Update thresholds
 fill=' '
 IF (ithr/=3) THEN
     WRITE (UNIT=fmt,FMT='(A,I1,A)') '(4X,A,F12.',iprec,')'
     DO j=nts,1,-1
        WRITE (UNIT=tmp,FMT=fmt) cthr(j),tobs(iva,j)
        IF(j==nts) THEN
          fill =TRIM(tmp)
        ELSE
          fill =TRIM(fill)//' '//TRIM(tmp)
        END IF
        fillme = fill
     END DO
 ELSE
     WRITE (UNIT=fmt,FMT='(A,2(I1,A))') '(4X,A,F12.',iprec,',A,I',magnitude(anlg(1)%iyr),',A)'
     DO j=nts,1,-1
        jmin=0
        tmin=HUGE(tmin)
        DO jj=1,nts
           terr=ABS(tobs(iva,j)-y(iva,iay(jj)))
           IF (terr<tmin) THEN
              jmin=jj
              tmin=terr
           END IF 
        END DO  
        WRITE (UNIT=tmp,FMT=fmt) cthr(j),tobs(iva,j),' (',anlg(jmin)%iyr,')'
        IF(j==nts) THEN
          fill =TRIM(tmp)
        ELSE
          fill =TRIM(fill)//' '//TRIM(tmp)
        END IF
        fillme=fill
     END DO
 END IF

 fillmelen=LEN_TRIM(fill)
 RETURN
 END SUBROUTINE get_thresholds

 SUBROUTINE get_fields_y_coors(fillme,fillmelen) BIND(C)
 !
 ! Module
   USE,INTRINSIC:: ISO_C_BINDING
 USE fields, ONLY: y_coors 
 !
 ! Input
 INTEGER(KIND=C_INT), INTENT(OUT) :: fillmelen  ! fillme len
 !
 ! Output 
 CHARACTER(KIND=C_CHAR), INTENT(OUT) :: fillme(30) ! will be assigned to dsdy
 !
 ! Local scalar
 CHARACTER(LEN=30) :: fill ! will be assigned to dsdy
 !
 ! Functions and Subroutines
 !
 ! Intrinsic functions
   INTRINSIC LEN_TRIM

  fill=TRIM(y_coors)
  fillme=fill
  fillmelen = LEN_TRIM(fill);
  RETURN
 END SUBROUTINE get_fields_y_coors

 SUBROUTINE get_fields_gridlinelabel(igrid,fillme,fillmelen) BIND(C)
 !
 ! Module
   USE,INTRINSIC:: ISO_C_BINDING
   USE fields, ONLY:ilfy,y_coors,dsdy,cstny,cstnc,y_coors,&
                   get_grid_coors,get_stn_coors
   USE settings, ONLY:ivf
 !
 ! IMPLICIT NONE
   IMPLICIT NONE
 !
 ! Arguments
 !
 ! Input scalars
   INTEGER(KIND=C_INT), INTENT(IN) :: igrid ! - field structure -
 !
 ! Output
   CHARACTER(KIND=C_CHAR), INTENT(OUT) :: fillme(30) ! will be assigned to dsdy
   INTEGER(KIND=C_INT), INTENT(OUT) :: fillmelen  ! fillme len
 !
 !
 !Local scalars
 !
   CHARACTER(LEN=30) :: fill ! will be assigned to dsdy
   INTEGER :: i ! - index -                

 ! Functions and Subroutines
 !
 ! Intrinsic functions
   INTRINSIC LEN_TRIM
   INTRINSIC TRIM

   ! Construct and add coordinate label for initial point
   SELECT CASE (igrid)
     CASE (1)
          y_coors=get_grid_coors()
          fill=TRIM(dsdy)//': '//TRIM(y_coors)
     CASE (2)
          y_coors=get_stn_coors()
          cstnc=cstny(ivf,ilfy)
          fill=TRIM(dsdy)//': '//TRIM(cstnc)//' '//TRIM(y_coors)
     CASE (3)
          cstnc=cstny(ivf,ilfy) 
          fill=TRIM(dsdy)//': '//TRIM(cstnc)
   END SELECT

   fillme=fill
   fillmelen=LEN_TRIM(fill)

   RETURN
 END SUBROUTINE get_fields_gridlinelabel

 SUBROUTINE get_codds_probs(per,codds) BIND(C)
 !
 ! Module
   USE,INTRINSIC:: ISO_C_BINDING
   USE CPT_constants,   ONLY: ng
   USE arrays,          ONLY: pobs
   USE settings,        ONLY: iva
   USE numbers,         ONLY: one,zero,oneh
 !
 ! Output 
   INTEGER(KIND=C_INT), INTENT(OUT) :: per(ng)  ! climatological probabilities -
   REAL(KIND=C_DOUBLE), INTENT(OUT) :: codds(ng) ! - climatological odds -
 !
 ! Functions and Subroutines
 !
 ! Intrinsic functions
 !
 ! Calculate climatological odds
   IF ((one-pobs(iva,1)>zero).AND.(one-(pobs(iva,1)-pobs(iva,1))>zero).AND.(pobs(iva,2)>zero)) THEN
     codds(1)=pobs(iva,1)/(one-pobs(iva,1))
     codds(2)=(pobs(iva,2)-pobs(iva,1))/(one-(pobs(iva,2)-pobs(iva,1)))
     codds(3)=(one-pobs(iva,2))/pobs(iva,2)
   ELSE
     codds(1)=-one
     codds(2)=-one
     codds(3)=-one
   END IF
 !
 ! Update climatological probabilities
   per(1)=NINT((one-pobs(iva,2))*oneh)
   per(2)=MAX(0,NINT((pobs(iva,2)-pobs(iva,1))*oneh))
   per(3)=NINT(pobs(iva,1)*oneh)

 RETURN
 END SUBROUTINE get_codds_probs

 SUBROUTINE  get_ytime_sequence_type(fillme, fillmelen) BIND(C)
 !
 ! Modules
   USE,INTRINSIC:: ISO_C_BINDING
   USE CPT_constants,   ONLY:csequ
   USE iofiles,         ONLY:yfile 
 !
 ! Output 
   INTEGER(KIND=C_INT), INTENT(OUT) :: fillmelen  ! fillme len
 !
   CHARACTER(KIND=C_CHAR), INTENT(OUT) :: fillme(8) ! will be assigned to thresholds values
 !
 ! Local scalar
   CHARACTER(LEN=8) :: fill ! will be assigned to thresholds values
 !
 ! Functions and Subroutines
 !
 ! Intrinsic functions
   INTRINSIC LEN_TRIM
   INTRINSIC TRIM

   fill=TRIM(csequ(yfile%iseq))
   fillme=fill
   fillmelen=LEN_TRIM(fill)

   RETURN
 END SUBROUTINE get_ytime_sequence_type

 !
 ! Get Forecasts probabilities and odds
 SUBROUTINE get_fprobs_odds(fyears,fprobs,fodds) BIND(C)
 !
 ! Module
   USE,INTRINSIC:: ISO_C_BINDING
   USE CPT_constants,   ONLY: ng
   USE arrays,          ONLY: odds,fps,oddr
   USE settings,        ONLY: iva,iodds,nf,lag
   USE iofiles,         ONLY: zfile
   USE numbers,         ONLY: one,zero,oneh
 !
 ! Output 
   INTEGER(KIND=C_INT), INTENT(OUT) :: fyears(nf)  ! climatological time - 
   INTEGER(KIND=C_INT), INTENT(OUT) :: fprobs(ng*nf)  ! climatological probabilities -
   REAL(KIND=C_DOUBLE), INTENT(OUT) :: fodds(ng*nf) ! - climatological odds -
 !
 ! Local Scalars
   INTEGER :: k,j;
 !
 ! Intrinsic functions
   INTRINSIC NINT
 !
 ! Update forecasts probabilities and odds
   SELECT CASE (iodds)
   CASE (0) ! - odds -
     DO k=1,nf
        fyears(k)= zfile%fdate%iyr+lag+k-1
        DO j=1,ng
           fprobs(j+(k-1)*ng) = NINT(fps(iva,k,j))
           fodds(j+(k-1)*ng)  = odds(iva,k,j)
        END DO
     END DO
   CASE (1) ! - odds relative to climatology -
     DO k=1,nf
        fyears(k)= zfile%fdate%iyr+lag+k-1
        DO j=1,ng
           fprobs(j+(k-1)*ng) = NINT(fps(iva,k,j))
           fodds(j+(k-1)*ng)  = oddr(iva,k,j)
        END DO
     END DO
   END SELECT

   RETURN
 END SUBROUTINE get_fprobs_odds
 !
 ! Get Forecasts ranges 
 SUBROUTINE get_franges(fyears,fcst,fupper,flower) BIND(C)
 !
 ! Module
   USE,INTRINSIC:: ISO_C_BINDING
   USE CPT_constants,   ONLY: ng
   USE arrays,          ONLY: fcast,fpls 
   USE settings,        ONLY: iva,nf,lag
   USE iofiles,         ONLY: zfile
 !
 ! Output 
   INTEGER(KIND=C_INT), INTENT(OUT) :: fyears(nf)  ! climatological time - 
   REAL(KIND=C_DOUBLE), INTENT(OUT) :: fcst(nf) ! - climatological odds -
   REAL(KIND=C_DOUBLE), INTENT(OUT) :: fupper(nf) ! - climatological odds -
   REAL(KIND=C_DOUBLE), INTENT(OUT) :: flower(nf) ! - climatological odds -
 !
 ! Local Scalars
   INTEGER :: k;
 !
 ! Intrinsic functions
   INTRINSIC NINT
 !
 ! Update forecasts
   fyears= zfile%fdate%iyr+lag+k-1
   DO k=1,nf
      fyears(k) = zfile%fdate%iyr+lag+k-1
      fcst(k)   = fcast(iva,k,0)
      flower(k) = fpls(iva,k,1)
      fupper(k) = fpls(iva,k,2)
   END DO

   RETURN
 END SUBROUTINE get_franges
 
 FUNCTION get_fields_ilfy() BIND(C)
 !
 ! Modules
  USE, INTRINSIC :: ISO_C_BINDING
  USE  fields, ONLY: ilfy
 !
  INTEGER(KIND=C_INT) :: get_fields_ilfy

  get_fields_ilfy = ilfy
  RETURN
 END FUNCTION get_fields_ilfy
!
!
!
 FUNCTION file_version_c_wrapper(filename,filenamelen) BIND(C)
 !
 ! Module
   USE,INTRINSIC:: ISO_C_BINDING
   USE iofiles, ONLY: ifile,file_version
 !
 ! Input
   INTEGER(KIND=C_INT), INTENT(IN) :: filenamelen  ! filename len
 !
 ! Output 
   CHARACTER(KIND=C_CHAR), INTENT(IN) :: filename(filenamelen) ! filename
 !
   INTEGER(KIND=C_INT) :: file_version_c_wrapper
 !
 ! Local scalars
   CHARACTER(LEN=filenamelen) :: myfile;
   INTEGER :: i
 !
 ! Functions and Subroutines
 !
 ! Intrinsic functions
   INTRINSIC LEN_TRIM
   INTRINSIC TRIM

   DO i=1,filenamelen
      myfile(i:i)=filename(i)
   END DO

   file_version_c_wrapper = file_version(myfile);

   RETURN
 END FUNCTION file_version_c_wrapper
!
END MODULE getvariables 
