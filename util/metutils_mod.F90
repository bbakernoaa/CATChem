@ -0,0 +1,188 @@
MODULE MetUtils_Mod
   USE Precision_Mod
   USE Constants
   USE Error_Mod, ONLY : CC_SUCCESS

   IMPLICIT NONE

   PRIVATE

CONTAINS

   !>
   !! \brief Calculate the weibull distribution for 10m wind speed (u10, v10)
   !!
   !! The Weibull distribution correction ends up being a multiplicative constant
   !! (g) times our present source function (see Eq. 12 in Fan & Toon, 2011 and notes for
   !! (9/22/11). This constant is derived from the incomplete and complete forms of the gamma
   !! function, hence the utilities pasted below.  The Weibull function and shape
   !! parameters (k, c) assumed are from Justus 1978.
   !!
   !! \param[inout] gweibull
   !! \param[in]    weibullFlag
   !! \param[in]    u10
   !! \param[in]    v10
   !! \param[out]   RC
   !!!>
   subroutine weibullDistribution(gweibull, weibullFlag, u10, v10, RC)

      implicit none

      ! Input/Output
      !-------------
      real(fp), intent(inout) :: gweibull

      ! Input
      !------
      integer,  intent(in)    :: weibullFlag
      real(fp), intent(in)    :: u10
      real(fp), intent(in)    :: v10
      errMsg = ''
      thisLoc = ' -> at weibullDistribution (in util/metutils_mod.F90)'

      ! Output
      !-------
      integer,  intent(out)   :: RC

      ! Local Variables
      real(kind=f8) :: a, c, k, wt, x
      real(kind=f8) :: wm
      integer       :: i, j

      integer :: status

      ! Initialize
      RC = CC_SUCCESS
      gweibull = 1.0_fp

      wm = sqrt(u10 ** 2 + v10 ** 2) ! Mean wind speed
      wt = 4.d0

      if (weibullFlag) then
         gweibull = 0.0_fp

         if (wm > 0.01) then
            k = 0.94d0 * sqrt(wm)
            c = wm / gamma(1.d0 + 1.d0 / k)
            x = (wt / c) ** k
            a = 3.41d0 / k + 1.d0
            gweibull = (c / wm) ** 3.41d0 * igamma(a,x, RC)
         endif
      endif

      if (RC /= CC_SUCCESS) then
         errMsg = 'Error Calculating Weibull Distribution'
         call CC_Error(errMsg, RC, thisLoc)
         return
      endif

   end subroutine weibullDistribution

   !>
   !! \brief Calculate the incomplete Gamma function
   !!
   !! The incomplete Gamma function is defined as
   !! \int_x^\infty t^{A-1}\exp(-t) dt
   !!
   !! \param[in]    A
   !! \param[in]    X
   !! \param[out]   RC
   DOUBLE PRECISION function igamma(A, X, rc)
!-----------------------------------------------------------------------
! incomplete (upper) Gamma function
! \int_x^\infty t^{A-1}\exp(-t) dt
!-----------------------------------------------------------------------
      IMPLICIT NONE
      double precision, intent(in) ::        A
      DOUBLE PRECISION, INTENT(IN) ::      X

      integer, intent(out) :: rc
! LOCAL VARIABLE
      DOUBLE PRECISION :: XAM, GIN,  S, R, T0
      INTEGER K
      rc = __SUCCESS__

      XAM=-X+A*LOG(X)
      IF (XAM.GT.700.0.OR.A.GT.170.0) THEN
         WRITE(*,*)'IGAMMA: a and/or x too large, X = ', X
         WRITE(*,*) 'A = ', A
         rc = __FAIL__
         return
      ENDIF

      IF (X.EQ.0.0) THEN
         IGAMMA=GAMMA(A)

      ELSE IF (X.LE.1.0+A) THEN
         S=1.0/A
         R=S
         DO  K=1,60
            R=R*X/(A+K)
            S=S+R
            IF (ABS(R/S).LT.1.0e-15) EXIT
         END DO
         GIN=EXP(XAM)*S
         IGAMMA=GAMMA(A)-GIN
      ELSE IF (X.GT.1.0+A) THEN
         T0=0.0
         DO K=60,1,-1
            T0=(K-A)/(1.0+K/(X+T0))
         end do

         IGAMMA=EXP(XAM)/(X+T0)

      ENDIF

   end function igamma

   !>
   !! \brief Calculate the friction velocity
   !!
   !! \param[in]    U10
   !! \param[in]    V10
   !! \param[in]    z
   !! \param[in]    z0
   !! \param[out]   USTAR
   !! \param[out]   RC
   !!!>
   SUBROUTINE Calc_USTAR(U10, V10, z, z0, USTAR, RC)

      IMPLICIT NONE

      ! Input
      !------
      real(fp), intent(in)  :: U10   ! Wind Speed (m/s)
      real(fp), intent(in)  :: V10   ! Wind Speed (m/s)
      real(fp), intent(in)  :: z     ! Reference Height for wind speed [m] (typically 10m)
      real(fp), intent(in)  :: z0    ! Surface roughness [m]

      ! Output
      !-------
      real(fp), intent(out) :: USTAR
      integer, intent(out) :: RC

      ! Local Variables
      !----------------
      CHARACTER(LEN=512) :: errMsg, thisLoc
      real(fp) :: wind_speed ! wind speed
      errMsg = ''
      thisLoc = ' -> at Calc_USTAR (in util/metutils_mod.F90)'

      ! Initialize
      RC = CC_SUCCESS
      USTAR = 0.0_fp

      ! Calculate Mean wind speed from u10 and v10
      !-------------------------------------------
      wind_speed = sqrt(U10**2 + V10**2)

      ! Calculate USTAR
      !----------------
      USTAR = VON_KARMAN / log (z / z0) * wind_speed

      RETURN

   END SUBROUTINE Calc_USTAR
