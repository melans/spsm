! $Id: time_constants.f90 1079 2010-10-07 19:43:42Z lsong $
! Author: Simon Mason
MODULE time_constants
!
! Modules
  USE CPT_text, ONLY: nlang
!
! Parameters
!
! Integer parameters
  INTEGER, PARAMETER, PUBLIC :: nmn=12  ! - number of months -
  INTEGER, PARAMETER, PUBLIC :: mdm=31  ! - maximum number of days per month -
  INTEGER, PARAMETER, PUBLIC :: mdy=366 ! - maximum number of days per year -
  INTEGER, PARAMETER, PUBLIC :: lmon=9  ! - length of month string -
  INTEGER, PARAMETER, PUBLIC :: lcmon=3 ! - length of cmon -
!
! Character parameters
  CHARACTER(LEN=nmn*2-1), DIMENSION(nlang), PARAMETER, PUBLIC :: cma= & ! - month abbeviations - 
     (/'JFMAMJJASONDJFMAMJJASON', &
       'EFMAMJJASONDEFMAMJJASON', &
       'JFMAMJJASONDJFMAMJJASON', &
       'JFMAMJJASONDJFMAMJJASON'/)
!
  CHARACTER(LEN=lcmon), DIMENSION(nmn,nlang), PARAMETER, PUBLIC :: cmon= & ! - months - 
     RESHAPE(SOURCE= &
     (/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec',   &
       'Ene','Feb','Mar','Abr','May','Jun','Jul','Ago','Sep','Oct','Nov','Dic',   &
       'Jan','F�v','Mar','Avr','Mai','Jun','Jul','Ao�','Sep','Oct','Nov','D�c',   &
       'Jan','Fev','Mar','Abr','Mai','Jun','Jul','Ago','Set','Out','Nov','Dez'/), &
     SHAPE=(/nmn,nlang/))
  CHARACTER(LEN=lcmon), DIMENSION(nmn,nlang), PARAMETER, PUBLIC :: umon= & ! - months - 
     RESHAPE(SOURCE= &
     (/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP','OCT','NOV','DEC',   &
       'ENE','FEB','MAR','ABR','MAY','JUN','JUL','AGO','SEP','OCT','NOV','DIC',   &
       'JAN','F�V','MAR','AVR','MAI','JUN','JUL','AO�','SEP','OCT','NOV','D�C',   &
       'JAN','FEV','MAR','ABR','MAI','JUN','JUL','AGO','SET','OUT','NOV','DEZ'/), &
     SHAPE=(/nmn,nlang/))
!
END MODULE time_constants