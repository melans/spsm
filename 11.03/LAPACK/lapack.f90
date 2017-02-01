      MODULE lapack
!
      INTERFACE
         SUBROUTINE dgesdd ( JOBZ, M, N, A, LDA, S, U, LDU, VT, LDVT,   &
     &                      WORK, LWORK, IWORK, INFO )
            CHARACTER          JOBZ
            INTEGER            INFO, LDA, LDU, LDVT, LWORK, M, N
            INTEGER            IWORK( * )
            DOUBLE PRECISION   A( LDA, * ), S( * ), U( LDU, * ),        &
     &                         VT( LDVT, * ), WORK( * )
         END SUBROUTINE dgesdd
      END INTERFACE
!
      END MODULE lapack
