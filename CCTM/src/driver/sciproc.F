
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SCIPROC ( CGRID, JDATE, JTIME, TSTEP, ASTEP )

C-----------------------------------------------------------------------
C Function:
C    Controls all of the physical and chemical processes for a grid
C    Operator splitting symmetric around chemistry
 
C Preconditions:
C    Dates and times represented YYYYDDD:HHMMSS.
C    No "skipped" dates and times.  All boundary input variables (layered or
C    non-layered) have the same perimeter structure with a thickness of NTHIK
 
C Subroutines and functions called:
C    All physical and chemical subroutines, 
C    DECOUPLE, COUPLE
 
C Revision History:
C    Oct. 24, 1995 by M. Talat Odman and Clint L. Ingram at NCSC: created
C    13 Dec 97 - Jeff - uncouple diffusion processes
C    27 Jun 98 - Jeff - sync step = chem step
C     7 Jul 01 - Shawn - mv cloud processing before chem
C       Jan 02 - Jeff - dyn alloc; remove PCGRID argument to ping
C    23 Jun 03 J.Young: for layer dependent advection tstep
C    18 Aug 03 J. Pleim - move vdiff before advection
C    29 Aug 03 J.Young: eliminate symmetric processing option and unused
C                       SUBST_GRID_ID string
C    30 May 05 J.Young: mass-conserving advection (yamo)
C     7 Jul 07 J.Young: No cksummer call after HADV - delay possible truncation of
C                       HADV negative conc's until after ZADV
C    25 Jul 08 S.Roselle: removed PING from modeled processes
C    16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C    11 May 11 D.Wong: incorporated twoway model implementation
C    25 Mar 16 R.Mathur: incorporated pot. vorticity strat. O3 scaling
C    16 Sep 16 J.Young: update for inline procan (IPR)
C    10 July 19 F. Sidi: Renamed Couple_WRF to be couple
C-----------------------------------------------------------------------

C     USE UTILIO_DEFN, ONLY: DT2STR, WKDAY
C     USE RUNTIME_VARS, ONLY: LOGDEV, OUTDEV, MYPE, CKSUM, 
C    &                        PRINT_PROC_TIME, WEEKDAY, 
C    &                        LOG_MESSAGE, TIMING_SPLIT, STM,
C    &                        COLUMN_MODEL
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE)
#endif
C     USE PA_DEFN, Only: LIPR   ! Process Analysis control and data variables

#ifdef isam
      USE SA_DEFN, Only: ISAM
      USE GRID_CONF
      USE CGRID_SPCS, ONLY: NSPCSD
#endif

      USE HDMod
      
      USE STM_MODULE, ONLY: STM_WRAP_GAS, STM_WRAP_AE
      USE STD_CONC           ! Write conc in STD_CONC, edited by J.E.L.
      USE RUNTIME_VARS       ! CONC_BLEV, CONC_ELEV are here, J.E.L.
      USE UTILIO_DEFN        ! J.E.L.       IMPLICIT NONE  

      IMPLICIT NONE   

#ifdef parallel
      INCLUDE 'mpif.h'
#endif
C Arguments:
      TYPE(hyperdual), POINTER :: CGRID( :,:,:,: )

      INTEGER, INTENT(INOUT) :: JDATE        ! current model date, coded YYYYDDD
      INTEGER, INTENT(INOUT) :: JTIME        ! current model time, coded HHMMSS
      INTEGER, INTENT(IN)    :: TSTEP( 3 )   ! time step vector (HHMMSS)
                                             ! TSTEP(1) = local output step
                                             ! TSTEP(2) = sciproc sync. step (chem)
                                             ! TSTEP(3) = twoway model time step w.r.t. wrf time
                                             !            step and wrf/cmaq call frequency

      INTEGER, INTENT(IN)    :: ASTEP( : )   ! layer advection time step

C Local Variables:

      CHARACTER( 16 ) :: PNAME = 'SCIPROC'
      CHARACTER( 120 ) :: XMSG = ' '
      CHARACTER( 36 ) :: NMSG = 'After NEXTIME: returned JDATE, JTIME'
      CHARACTER( 36 ) :: CGRID_BF_MSG = ' '
      CHARACTER( 36 ) :: CGRID_AF_MSG = ' '
      
      INTEGER         ::     SDATE       ! current science process date, coded YYYYDDD
      INTEGER         ::     STIME       ! current science process time, coded HHMMSS
 	
      REAL            :: REAL_TIME

      REAL( 8 )       :: CPU_TIME_START  ! Timing individual
      REAL( 8 )       :: CPU_TIME_START_MASTER
      CHARACTER( 24 ) :: CDATE_TIME
      
      INTEGER         :: V, S
c      INTEGER         :: DX1_WRITE_TIME, DX2_WRITE_TIME, DX1X2_WRITE_TIME
      TYPE(hyperdual), parameter :: PERT_HYPERDUAL = hyperdual(0.0D0, 1.D-3, 1.D-3, 0.0D0)
      INTEGER         :: R_HD, C_HD, L_HD, T_HD

#ifdef isam
      REAL, ALLOCATABLE, SAVE :: CGRID0( :,:,:,: )
      LOGICAL, SAVE :: FIRSTIME = .TRUE.
#endif
      INTERFACE
         SUBROUTINE CKSUMMER ( PNAME, CGRID, JDATE, JTIME )
         	use HDMod
            CHARACTER( * ), INTENT( IN ) :: PNAME
            TYPE(hyperdual), POINTER     :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )        :: JDATE, JTIME
         END SUBROUTINE CKSUMMER
         SUBROUTINE PA_UPDATE ( PNAME, CGRID, JDATE, JTIME, TSTEP )
            CHARACTER( * ), INTENT( IN ) :: PNAME
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE PA_UPDATE
         SUBROUTINE HADV ( CGRID, JDATE, JTIME, TSTEP, ASTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
            INTEGER, INTENT( IN )     :: ASTEP( : )
         END SUBROUTINE HADV
         SUBROUTINE ZADV ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE ZADV
         SUBROUTINE COUPLE ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE COUPLE
         SUBROUTINE DECOUPLE ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE DECOUPLE
         SUBROUTINE HDIFF ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE HDIFF
         SUBROUTINE VDIFF ( CGRID, JDATE, JTIME, TSTEP )
         	USE HDMod
            TYPE(hyperdual), POINTER  :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE VDIFF
         SUBROUTINE PHOT ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( : )
         END SUBROUTINE PHOT
         SUBROUTINE CLDPROC ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE CLDPROC
         SUBROUTINE CHEM ( CGRID, JDATE, JTIME, TSTEP )
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE CHEM
#ifdef potvorto3
         SUBROUTINE PVO3 ( CGRID, JDATE, JTIME )
            IMPLICIT NONE
            REAL, POINTER             :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
         END SUBROUTINE PVO3
#endif
         SUBROUTINE AERO ( CGRID, JDATE, JTIME, TSTEP )
         	USE HDMod
            TYPE(hyperdual), POINTER  :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE, JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE AERO
#ifdef isam
         SUBROUTINE SA_WRAP_AE( CGRID0, CGRID, JDATE, JTIME, TSTEP )
         	USE HDMod
            IMPLICIT NONE
            REAL                      :: CGRID0( :,:,:,: )
            TYPE(hyperdual), POINTER  :: CGRID( :,:,:,: )
            INTEGER, INTENT( IN )     :: JDATE
            INTEGER, INTENT( IN )     :: JTIME
            INTEGER, INTENT( IN )     :: TSTEP( 3 )
         END SUBROUTINE SA_WRAP_AE
#endif
      END INTERFACE
  
C-----------------------------------------------------------------------
#ifdef isam
      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         ALLOCATE( CGRID0( NCOLS,NROWS,NLAYS,NSPCSD ) )
      END IF
#endif

! Write Time Step Information to Summary Logfile
      IF ( MYPE .EQ. 0 ) WRITE( OUTDEV, * ) 
      WRITE( LOGDEV, * )

#ifdef parallel
            CPU_TIME_START =  MPI_WTIME()
#else
            CALL CPU_TIME( REAL_TIME )
            CPU_TIME_START = REAL( REAL_TIME,8 )
#endif
      CPU_TIME_START_MASTER = CPU_TIME_START
      WRITE( XMSG, '(A,I7.7,A,I6.6)' ), 'Processing Day/Time [YYYYDDD:HHMMSS]: ',JDATE, 
     &       ':',JTIME
      IF ( MYPE .EQ. 0 ) CALL LOG_MESSAGE( OUTDEV, XMSG )
      CALL LOG_MESSAGE( LOGDEV, XMSG )

      CDATE_TIME = DT2STR( JDATE, JTIME )
      WRITE( XMSG, '(2x,6A)' ), 'Which is Equivalent to (UTC): ',
     &       CDATE_TIME(1:8),' ',TRIM( WEEKDAY( WKDAY( JDATE ) ) ),
     &       ',', CDATE_TIME(9:)
      IF ( MYPE .EQ. 0 ) CALL LOG_MESSAGE( OUTDEV, XMSG )
      CALL LOG_MESSAGE( LOGDEV, XMSG )

      WRITE( XMSG, '(2x,A,I6.6)' ), 'Time-Step Length (HHMMSS): ',TSTEP(2) 
      IF ( MYPE .EQ. 0 ) CALL LOG_MESSAGE( OUTDEV, XMSG )
      CALL LOG_MESSAGE( LOGDEV, XMSG )


 	  IF (JTIME .EQ. 0) THEN
c        Hyperdual perturbation at the very first
 	    CGRID(:, :, :, 133) = CGRID(:, :, :, 133) + PERT_HYPERDUAL
 	    
c       Hyperdual <-- CD
 	    DO V = 1, N_CSPCS
         	S = CONC_MAP( V )
         	SGRID( :,:,:,V ) = REAL ( CGRID( :,:, CONC_BLEV:CONC_ELEV,S )%x , 4 )
      	END DO
      	
      	CALL WR_CONC ( JDATE, JTIME, TSTEP( 1 ) )
 	    
 	  END IF
	  
	  
C Synchronization Step Physical Processes Sequence

      CALL VDIFF ( CGRID, JDATE, JTIME, TSTEP)
      IF ( CKSUM ) CALL CKSUMMER ( 'VDIFF', CGRID, JDATE, JTIME )
      ! IF ( LIPR ) CALL PA_UPDATE ( 'VDIF', CGRID, JDATE, JTIME, TSTEP )
      IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'VDIFF' )


C     IF ( .NOT. COLUMN_MODEL ) THEN
C couple CGRID for advection and horizontal diffusion
C The COUPLE routine converts aerosol mass from ug/m3 to kg/m3. The 
C DECOUPLE routine converts it back to ug/m3.

C        CALL COUPLE ( CGRID, JDATE, JTIME, TSTEP )
C        IF ( CKSUM ) CALL CKSUMMER ( 'COUPLE', CGRID, JDATE, JTIME )
C        IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'COUPLE' )
C        
C        CALL SUBST_BARRIER()
C        CALL HADV ( CGRID, JDATE, JTIME, TSTEP, ASTEP )
C        IF ( CKSUM ) CALL CKSUMMER ( 'HADV', CGRID, JDATE, JTIME )
C     ! IF ( LIPR ) CALL PA_UPDATE ( 'HADV', CGRID, JDATE, JTIME, TSTEP )
C        IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'HADV' )
C        
C        CALL ZADV ( CGRID, JDATE, JTIME, TSTEP )
C        IF ( CKSUM ) CALL CKSUMMER ( 'ADV', CGRID, JDATE, JTIME )
C        ! IF ( LIPR ) CALL PA_UPDATE ( 'ZADV', CGRID, JDATE, JTIME, TSTEP )
C        IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'ZADV' )
         
C        CALL HDIFF ( CGRID, JDATE, JTIME, TSTEP )
C        IF ( CKSUM ) CALL CKSUMMER ( 'HDIFF', CGRID, JDATE, JTIME )
C        ! IF ( LIPR ) CALL PA_UPDATE ( 'HDIF', CGRID, JDATE, JTIME, TSTEP )
C        IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'HDIFF' )

C decouple CGRID for cloud and chemistry

C        SDATE = JDATE
C        STIME = JTIME
C        CALL NEXTIME ( SDATE, STIME, TSTEP( 2 ) )
C        
C        CALL DECOUPLE ( CGRID, SDATE, STIME, TSTEP )
C        IF ( CKSUM ) CALL CKSUMMER ( 'DECOUPLE', CGRID, JDATE, JTIME )
C        IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'DECOUPLE' )
C
C     END IF
C     
C     CALL PHOT ( CGRID, JDATE, JTIME, TSTEP )
C     IF ( CKSUM ) CALL CKSUMMER ( 'PHOT', CGRID, JDATE, JTIME )
C     IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'PHOT' )
C
C     CALL CLDPROC ( CGRID, JDATE, JTIME, TSTEP )
C     IF ( CKSUM ) CALL CKSUMMER ( 'CLDPROC', CGRID, JDATE, JTIME )
C     ! IF ( LIPR ) CALL PA_UPDATE ( 'CLDS', CGRID, JDATE, JTIME, TSTEP )
C     IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'CLDPROC' )
C
C     IF ( STM ) CALL STM_WRAP_GAS ( CGRID_HD, JDATE, JTIME, .TRUE. )
C     CALL CHEM ( CGRID, JDATE, JTIME, TSTEP )
C     IF ( CKSUM ) CALL CKSUMMER ( 'CHEM', CGRID, JDATE, JTIME )
C     ! IF ( LIPR ) CALL PA_UPDATE ( 'CHEM', CGRID, JDATE, JTIME, TSTEP ) 
C     IF ( STM ) CALL STM_WRAP_GAS ( CGRID_HD, JDATE, JTIME, .FALSE. )
C     IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'CHEM' )
C
C#ifdef potvorto3
C     CALL PVO3 ( CGRID, JDATE, JTIME )
C     IF ( CKSUM ) CALL CKSUMMER ( 'PVO3', CGRID, JDATE, JTIME )
C     ! IF ( LIPR ) CALL PA_UPDATE ( 'PVO3', CGRID, JDATE, JTIME, TSTEP )
C     IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'PVO3' )
C#endif
C
C#ifdef isam
C     CGRID0  = CGRID
C#endif
	  
		  	    
   	  CALL AERO ( CGRID, JDATE, JTIME, TSTEP )
      IF ( CKSUM ) CALL CKSUMMER ( 'AERO', CGRID, JDATE, JTIME )
      ! IF ( LIPR ) CALL PA_UPDATE_AERO ( CGRID, JDATE, JTIME, TSTEP )
      IF ( PRINT_PROC_TIME ) CALL TIMING_SPLIT ( CPU_TIME_START, 1, 'AERO' )
      IF ( STM ) CALL STM_WRAP_AE ( CGRID, JDATE, JTIME )
	  

C     Write the results 

C  	  
C  DO V = 1, N_CSPCS
C           S = CONC_MAP( V )
C           SGRID( :,:,:,V ) = REAL ( CGRID( :,:,CONC_BLEV:CONC_ELEV,S )%x , 4 )
C           IF (S .EQ. 156) THEN
C           	WRITE(*,*) 'SGRID(SP)'
C           	WRITE(*,*) SGRID(15, 15, 1, V)
C           END IF
C           	
C     END DO 
      
      
C      write(*,*) CGRID(15,15,1,156)%x
C      write(*,*) SGRID(15,15,1,10)

C     DO V = 1, N_CSPCS
C        S = CONC_MAP( V )
C        SGRID( :,:,:,V ) = REAL ( CGRID( :,:,CONC_BLEV:CONC_ELEV,S )%x , 4 )
C     END DO
C  
C  IF ( MOD( TIME2SEC( JTIME ), TIME2SEC( TSTEP( 1 ) ) ) .EQ. 0 ) THEN
C        CALL WR_CONC( JDATE, JTIME, TSTEP( 1 ) )
C     END IF
C         
C     DO V = 1, N_CSPCS
C           S = CONC_MAP( V )
C           SGRID( :,:,:,V ) = REAL ( CGRID( :,:,CONC_BLEV:CONC_ELEV,S )%dx1 , 4 )
C     END DO
C     
C     IF ( MOD( TIME2SEC( JTIME ), TIME2SEC( TSTEP( 1 ) ) ) .EQ. 0 ) THEN
C     	 DX1_WRITE_TIME = JTIME + 10000
C        CALL WR_CONC( JDATE, DX1_WRITE_TIME, TSTEP( 1 ) )
C     END IF
C     
C     
C      DO V = 1, N_CSPCS
C           S = CONC_MAP( V )
C           SGRID( :,:,:,V ) = REAL ( CGRID( :,:,CONC_BLEV:CONC_ELEV,S )%dx2 , 4 ) 
C     END DO
C     
C     IF ( MOD( TIME2SEC( JTIME ), TIME2SEC( TSTEP( 1 ) ) ) .EQ. 0 ) THEN
C     	 DX2_WRITE_TIME = JTIME + 20000
C        CALL WR_CONC( JDATE, DX2_WRITE_TIME, TSTEP( 1 ) )
C     END IF
C     
C     
C      DO V = 1, N_CSPCS
C           S = CONC_MAP( V )
C           SGRID( :,:,:,V ) = REAL ( CGRID( :,:,CONC_BLEV:CONC_ELEV,S )%dx1x2 , 4 ) 
C     END DO
C     
C     IF ( MOD( TIME2SEC( JTIME ), TIME2SEC( TSTEP( 1 ) ) ) .EQ. 0 ) THEN
C     	 DX1X2_WRITE_TIME = JTIME + 30000
C        CALL WR_CONC( JDATE, DX1X2_WRITE_TIME, TSTEP( 1 ) )
C     END IF


#ifdef isam
      CALL SA_WRAP_AE( CGRID0, CGRID, JDATE, JTIME, TSTEP )
#endif

      CALL NEXTIME ( JDATE, JTIME, TSTEP( 2 ) )
      WRITE( LOGDEV,'(/ 5X, A, I8, I7.6)' ) NMSG, JDATE, JTIME

! Print Summary Timing for Master time step      
      CALL TIMING_SPLIT ( CPU_TIME_START_MASTER, 2 )

1002  FORMAT ( 2x, A15, ' completed... ', F6.1, ' seconds' )

      RETURN
      END
