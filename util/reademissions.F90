MODULE ReadEmissions

   implicit none

   private

   public :: ReadASCIIPointEmissions


  ! Define a derived type to hold the data for each emission entry
  ! This may need to go into the Emissions State type???
  type :: VolcanicEmissionData
    real :: vlat
    real :: vlon
    real :: vbase
    real :: vtop
    real :: VEmis
    integer :: nPts
    character(len=20) :: vstart
    character(len=20) :: vend
  end type VolcanicEmissionData

contains


subroutine ReadASCIIPointEmissions(nymd, filename, VolcanicEmissions, rc )

   implicit none

      integer, intent(in)            :: nymd  !! Need to do something with this???
      character(*), intent(in) :: filename, label

      integer, intent(out) :: rc

      type(VolcanicEmissionData), allocatable, intent(out) :: VolcanicEmissions(:)
      integer, intent(out) :: num_emiss_sources
      integer :: i
      character(*) :: line
      type(VolcanicEmissionData), allocatable :: temp_emissions(:)

    ! Open the file
    open(unit=10, file=filename, status='old', action='read', iostat=rc)
    if (rc /= 0) then
       print *, "Error opening file:", filename
       num_emissions = 0
       return
    end if

    ! Count the number of lines in the file
    num_emissions = 0
    do
       read(10, '(A)', iostat=rc) line
       ! if (rc /= 0) exit
       num_emiss_sources = num_emiss_sources + 1
    end do

    ! Rewind the file to start reading data
    rewind(10)

    ! Allocate the array to hold all entries
    allocate(temp_emissions(num_emiss_sources))

    ! Read each line and store data in the array
    ! Need a way to generalize this, first several lines are commented out
    do i = 1, num_emiss_sources
       read(10, *, iostat=rc) &
            temp_emissions(i)%vlat, &
            temp_emissions(i)%vlon, &
            temp_emissions(i)%vbase, &
            temp_emissions(i)%vtop, &
            temp_emissions(i)%vemis, &
            temp_emissions(i)%vstart, &
            temp_emissions(i)%vend
       if (rc /= 0) then
          print *, "Error reading line", i
          num_emiss_sources = i - 1
          stop 1
       end if
    end do

    ! Close the file and transfer data to output array
    close(10)
    VolcanicEmissions = temp_emissions

      where(vStart < 0) vStart = 000000
      where(vEnd < 0)   vEnd   = 240000


end subroutine ReadASCIIPointEmissions

end module ReadEmissions