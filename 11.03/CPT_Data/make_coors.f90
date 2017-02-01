! Moved from fields.f90 written by Simon Mason.
! $Id: make_coors.f90 1219 2011-03-02 21:29:12Z lsong $
!
 FUNCTION make_coors(rlat,rlng)
!
! Modules
  USE maths,   ONLY: iprec
  USE numbers, ONLY: zero,ten,oneh,r180,r360,rp
!
! Function type
  CHARACTER(LEN=15) :: make_coors
!
! Arguments
!
! Input scalars
  REAL(KIND=rp), INTENT(IN) :: rlat ! - latitude -
  REAL(KIND=rp), INTENT(IN) :: rlng ! - longitude -
!
! Locals
!
! Local parameters
  INTEGER, PARAMETER :: mprec=2 ! - maximum precision required -
!
! Local scalars
  INTEGER :: nprec ! - precision -
!
  REAL(KIND=rp) :: rw    ! - longitude in western hemisphere -
  REAL(KIND=rp) :: rrlng ! - realigned longitude -
!
  CHARACTER(LEN=6) :: clat ! - latitude -
  CHARACTER(LEN=7) :: clng ! - longitude -
!
! Functions and Subroutines
!
! Intrinsic functions
  INTRINSIC MAX
  INTRINSIC NINT
!
! Executable Statements
!
! Realign longitude if necessary
  rrlng=rlng
  DO
     IF (rrlng<-r180) THEN
        rrlng=rrlng+r360
     ELSE IF (rrlng>r360) THEN
        rrlng=rrlng-r360
     ELSE
        EXIT
     END IF
  END DO
!
! Identify precision
  nprec=MAX(iprec(rlat,mprec),iprec(rlng,mprec))
!
! Construct coordinate label
! - latitudes -
  IF (rlat>zero) THEN
     IF (rlat<ten) THEN
        SELECT CASE (nprec)
         CASE (0)
           WRITE (clat,FMT='(I1,A)') NINT(rlat),'N'
         CASE (1)
           WRITE (clat,FMT='(F3.1,A)') rlat,'N'
         CASE (2)
           WRITE (clat,FMT='(F4.2,A)') rlat,'N'
        END SELECT
     ELSE
        SELECT CASE (nprec)
         CASE (0)
           WRITE (clat,FMT='(I2,A)') NINT(rlat),'N'
         CASE (1)
           WRITE (clat,FMT='(F4.1,A)') rlat,'N'
         CASE (2)
           WRITE (clat,FMT='(F5.2,A)') rlat,'N'
        END SELECT
     END IF
  ELSE IF (rlat<zero) THEN
     IF (rlat>-ten) THEN
        SELECT CASE (nprec)
         CASE (0)
           WRITE (clat,FMT='(I1,A)') -NINT(rlat),'S'
         CASE (1)
           WRITE (clat,FMT='(F3.1,A)') -rlat,'S'
         CASE (2)
           WRITE (clat,FMT='(F4.2,A)') -rlat,'S'
        END SELECT
     ELSE
        SELECT CASE (nprec)
         CASE (0)
           WRITE (clat,FMT='(I2,A)') -NINT(rlat),'S'
         CASE (1)
           WRITE (clat,FMT='(F4.1,A)') -rlat,'S'
         CASE (2)
           WRITE (clat,FMT='(F5.2,A)') -rlat,'S'
        END SELECT
     END IF
  ELSE
     clat='0'
  END IF
! - longitudes -
  IF (rrlng>zero) THEN
     IF (rrlng<ten) THEN
        SELECT CASE (nprec)
         CASE (0)
           WRITE (clng,FMT='(I1,A)') NINT(rrlng),'E'
         CASE (1)
           WRITE (clng,FMT='(F3.1,A)') rrlng,'E'
         CASE (2)
           WRITE (clng,FMT='(F4.2,A)') rrlng,'E'
        END SELECT
     ELSE IF (rrlng<oneh) THEN
        SELECT CASE (nprec)
         CASE (0)
           WRITE (clng,FMT='(I2,A)') NINT(rrlng),'E'
         CASE (1)
           WRITE (clng,FMT='(F4.1,A)') rrlng,'E'
         CASE (2)
           WRITE (clng,FMT='(F5.2,A)') rrlng,'E'
        END SELECT
     ELSE IF (rrlng<r180) THEN
        SELECT CASE (nprec)
         CASE (0)
           WRITE (clng,FMT='(I3,A)') NINT(rrlng),'E'
         CASE (1)
           WRITE (clng,FMT='(F5.1,A)') rrlng,'E'
         CASE (2)
           WRITE (clng,FMT='(F6.2,A)') rrlng,'E'
        END SELECT
     ELSE IF (rrlng>r180) THEN
        rw=rrlng-r360
        IF (rw>-ten) THEN
           SELECT CASE (nprec)
            CASE (0)
              WRITE (clng,FMT='(I1,A)') -NINT(rw),'W'
            CASE (1)
              WRITE (clng,FMT='(F3.1,A)') -rw,'W'
            CASE (2)
              WRITE (clng,FMT='(F4.2,A)') -rw,'W'
           END SELECT
        ELSE IF (rw>-oneh) THEN
           SELECT CASE (nprec)
            CASE (0)
              WRITE (clng,FMT='(I2,A)') -NINT(rw),'W'
            CASE (1)
              WRITE (clng,FMT='(F4.1,A)') -rw,'W'
            CASE (2)
              WRITE (clng,FMT='(F5.2,A)') -rw,'W'
           END SELECT
        ELSE
           SELECT CASE (nprec)
            CASE (0)
              WRITE (clng,FMT='(I3,A)') -NINT(rw),'W'
            CASE (1)
              WRITE (clng,FMT='(F5.1,A)') -rw,'W'
            CASE (2)
              WRITE (clng,FMT='(F6.2,A)') -rw,'W'
           END SELECT
        END IF
     ELSE
        clng='180'
     END IF
  ELSE IF (rrlng<zero) THEN
     IF (rrlng>-ten) THEN
        SELECT CASE (nprec)
         CASE (0)
           WRITE (clng,FMT='(I1,A)') -NINT(rrlng),'W'
         CASE (1)
           WRITE (clng,FMT='(F3.1,A)') -rrlng,'W'
         CASE (2)
           WRITE (clng,FMT='(F4.2,A)') -rrlng,'W'
        END SELECT
     ELSE IF (rrlng>-oneh) THEN
        SELECT CASE (nprec)
         CASE (0)
           WRITE (clng,FMT='(I2,A)') -NINT(rrlng),'W'
         CASE (1)
           WRITE (clng,FMT='(F4.1,A)') -rrlng,'W'
         CASE (2)
           WRITE (clng,FMT='(F5.2,A)') -rrlng,'W'
        END SELECT
     ELSE
        SELECT CASE (nprec)
         CASE (0)
           WRITE (clng,FMT='(I3,A)') -NINT(rrlng),'W'
         CASE (1)
           WRITE (clng,FMT='(F5.1,A)') -rrlng,'W'
         CASE (2)
           WRITE (clng,FMT='(F6.2,A)') -rrlng,'W'
        END SELECT
     END IF
  ELSE
     clng='0'
  END IF
  make_coors=TRIM(clat)//', '//TRIM(clng)
!
  RETURN
 END FUNCTION make_coors
