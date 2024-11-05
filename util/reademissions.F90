MODULE ReadEmissions

   implicit none

   private

   public :: ReadASCIIPointEmissions

contains

! This is taken from GOCART2G_Process
subroutine ReadASCIIPointEmissions(nymd, filename, nPts, vLat, vLon, vBase, vTop, vEmis, vStart, vEnd, unusable, label, rc )

      integer, intent(in)            :: nymd
      character(*), intent(in) :: filename
      integer, intent(out)           :: nPts
      real, allocatable, dimension(:), intent(out)    :: vLat, vLon, vTop, vBase, vEmis
      integer, allocatable, dimension(:), intent(out) :: vStart, vEnd

      type(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: label
      integer, optional, intent(out) :: rc

      ! Local arguments
      type(EmissionReader) :: reader
      character(:), allocatable :: label_
      real, allocatable :: table(:,:)
      integer :: nCols
      integer :: status, status1, status2, status3

      if (present(label)) then
         label_ = trim(label)
      else
         label_ = 'source'
      end if

      reader = EmissionReader()
      !$omp critical (process1)
      call reader%open(filename, rc=status1)              ! There's an open function in GOCART2G_Process
      table = reader%read_table(label=label_, rc=status2) ! there's a read table function in GOCART2G_Process
      call reader%close(rc=status3)                       ! There's a close function in GOCART2G_Process
      !$omp end critical (process1)
      __VERIFY__(status1)
      __VERIFY__(status2)
      __VERIFY__(status3)

      nCols = size(table,1)
      nPts = size(table,2)
      vStart = spread(-1, 1, nPts)
      vEnd = spread(-1, 1, nPts)

      vLat  = table(1,:)
      vLon  = table(2,:)
      vEmis = table(3,:)
      vBase = table(4,:)
      vTop  = table(5,:)
      if (nCols >= 6) vStart = table(6,:)
      if (nCols >= 7) vEnd = table(7,:)

      where(vStart < 0) vStart = 000000
      where(vEnd < 0)   vEnd   = 240000
      !call reader%close()

      __RETURN__(__SUCCESS__)

end subroutine ReadASCIIPointEmissions

end module ReadEmissions