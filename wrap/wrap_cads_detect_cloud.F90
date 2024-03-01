SUBROUTINE wrap_cads_detect_cloud( &
&    K__NChans,               &
&    K__Nprof,               &
&    K__ChanID,               &
&    K__Minlev,               &
&    K__Maxlev,               &
&    P__ObsBTs,               &
&    P__ModelBTs,             &
&    P__Chan_Level,           &
&    K__Sensor,               &
&    K__Cloud_Flag)  


IMPLICIT NONE

!* 0.1 Global arrays
INTEGER(KIND=4), INTENT(IN)  :: K__NChans              ! No. of channels
INTEGER(KIND=4), INTENT(IN)  :: K__Nprof               ! No. of profiles
INTEGER(KIND=4), INTENT(IN)  :: K__ChanID(K__NChans)   ! Channel IDs
INTEGER(KIND=4), INTENT(IN)  :: K__Minlev(K__Nprof)    ! Highest starting point
INTEGER(KIND=4), INTENT(IN)  :: K__Maxlev(K__Nprof)    ! Lowest starting point
INTEGER(KIND=4), allocatable :: K__Chan_ID_Imager(:)   ! Imager channel IDs
INTEGER(KIND=4), INTENT(OUT) :: K__Cloud_Flag(K__Nprof,K__Nchans)       ! Output cloud flags
REAL(KIND=8),    INTENT(IN)  :: P__ObsBTs(K__Nprof,K__Nchans)           ! Observed BTs
REAL(KIND=8),    INTENT(IN)  :: P__ModelBTs(K__Nprof,K__Nchans)         ! Model clear BTs
REAL(KIND=8),    INTENT(IN)  :: P__Chan_Level(K__Nprof,K__Nchans)       ! Channel height
INTEGER(KIND=4), INTENT(IN)  :: K__Sensor              ! Sensor
!BMK hard shutoff of this in Setup with use imager
INTEGER(KIND=4)  :: K__Num_Imager_Clusters ! No. of imager clusters
INTEGER(KIND=4)  :: K__Num_Imager_Chans 
REAL(KIND=8), allocatable   :: P__Cluster_Fraction(:) ! Cluster coverages
REAL(KIND=8),allocatable   :: P__BT_in_Cluster(:,:)  ! Mean BT in cluster / 
                                                       ! channel
REAL(KIND=8),allocatable   :: P__BT_Overall_Sdev(:)  ! St.Dev of imager BT
                                                       ! in FOV
REAL(KIND=8),allocatable    :: P__BT_Model_Imager(:)  ! Model-based estimate
INTEGER(KIND=4) :: Jprof
INTERFACE
SUBROUTINE CADS_Detect_Cloud( &
 & K__Sensor,                 &
 & K__NChans,                 &
 & K__ChanID,                 &
 & K__Minlev,                 &
 & K__Maxlev,                 &
 & K__Num_Imager_Chans,       &
 & K__Chan_ID_Imager,         &
 & K__Num_Imager_Clusters,    &
 & K__Cloud_Flag,             &
 & P__ObsBTs,                 &
 & P__ModelBTs,               &
 & P__Chan_Level,             &
 & P__Cluster_Fraction,       &
 & P__BT_in_Cluster,          &
 & P__BT_Overall_Sdev,        &
 & P__BT_Model_Imager )
INTEGER(KIND=4), INTENT(IN)  :: K__Sensor
INTEGER(KIND=4), INTENT(IN)  :: K__NChans
INTEGER(KIND=4), INTENT(IN)  :: K__ChanID(:)
INTEGER(KIND=4), INTENT(IN)  :: K__Minlev
INTEGER(KIND=4), INTENT(IN)  :: K__Maxlev
INTEGER(KIND=4), INTENT(IN)  :: K__Num_Imager_Chans
INTEGER(KIND=4), INTENT(IN)  :: K__Chan_ID_Imager(:)
INTEGER(KIND=4), INTENT(IN)  :: K__Num_Imager_Clusters
INTEGER(KIND=4), INTENT(OUT) :: K__Cloud_Flag(:)
REAL(KIND=8),    INTENT(IN)  :: P__ObsBTS(:)
REAL(KIND=8),    INTENT(IN)  :: P__ModelBTS(:)
REAL(KIND=8),    INTENT(IN)  :: P__Chan_Level(:)
REAL(KIND=8),    INTENT(IN)  :: P__Cluster_Fraction(:)
REAL(KIND=8),    INTENT(IN)  :: P__BT_in_Cluster(:,:)
REAL(KIND=8),    INTENT(IN)  :: P__BT_Overall_SDev(:)
REAL(KIND=8),    INTENT(IN)  :: P__BT_Model_Imager(:)
END SUBROUTINE CADS_Detect_Cloud
END INTERFACE

INTERFACE
SUBROUTINE CADS_Setup_Cloud
END SUBROUTINE CADS_Setup_Cloud
END INTERFACE
                                                      ! of imager BT



Call CADS_Setup_Cloud
! Hard code clusters off.
K__Num_Imager_Clusters = 0
K__Num_Imager_Chans = 0

allocate(P__Cluster_Fraction(K__Num_Imager_Clusters)) ! Cluster coverages
allocate(P__BT_in_Cluster(K__Num_Imager_Chans,K__Num_Imager_Clusters))  ! Mean BT in cluster / 
                                                       ! channel
allocate(P__BT_Overall_Sdev(K__Num_Imager_Chans))  ! St.Dev of imager BT
                                                       ! in FOV
allocate(P__BT_Model_Imager(K__Num_Imager_Chans))  ! Model-based estimate
! End Hard code clusters.
DO Jprof = 1,K__Nprof
    Call CADS_Detect_Cloud( &
    &    K__Sensor,               &
    &    K__NChans,               &
    &    K__ChanID,               &
    &    K__Minlev(Jprof),        &
    &    K__Maxlev(Jprof),        &
    &    K__Num_Imager_Chans,     &
    &    K__Chan_ID_Imager,       &
    &    K__Num_Imager_Clusters,  &
    &    K__Cloud_Flag(Jprof,1:K__Nchans), &
    &    P__ObsBTs(Jprof,1:K__Nchans),     &
    &    P__ModelBTs(Jprof,1:K__Nchans),   &
    &    P__Chan_Level(Jprof,1:K__Nchans), &
    &    P__Cluster_Fraction,     &
    &    P__BT_in_Cluster,        &
    &    P__BT_Overall_SDev,      &
    &    P__BT_Model_Imager )
END DO
END SUBROUTINE

