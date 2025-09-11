!> \file DustProcessCreator_Mod.F90
!! \brief Factory for creating dust process instances
!!
!! This module provides the factory functions for creating dust
!! process instances following the CATChem Process Factory pattern.
!!
!! Generated on: 2025-09-09T14:29:24.652244
!! Author: Barry Baker
!! Version: 1.0.0

module DustProcessCreator_Mod

   use precision_mod, only: fp
   use error_mod, only: CC_SUCCESS, CC_FAILURE, CC_Error, CC_Warning, ErrorManagerType
   use ProcessInterface_Mod
   use ProcessDustInterface_Mod

   implicit none
   private

   public :: create_dust_process
   public :: register_dust_process
   public :: get_dust_default_config

contains

   !> Create a new dust process instance
   !!
   !! This factory function creates and returns a new instance of the
   !! dust process. The process is not initialized - the caller
   !! must call the init() method with appropriate configuration.
   !!
   !! @param[out] process     Allocated process instance
   !! @param[out] rc          Return code
   subroutine create_dust_process(process, rc)
      class(ProcessInterface), allocatable, intent(out) :: process
      integer, intent(out) :: rc

      type(ProcessDustInterface), allocatable :: dust_process
      integer :: alloc_stat

      rc = CC_SUCCESS

      ! Allocate the process instance
      allocate(dust_process, stat=alloc_stat)
      if (alloc_stat /= 0) then
         rc = CC_FAILURE
         return
      end if

      ! Move to polymorphic variable
      call move_alloc(dust_process, process)

   end subroutine create_dust_process

   !> Register the dust process with the global registry
   !!
   !! This subroutine should be called during module initialization to
   !! register the dust process with the global process registry.
   !!
   !! @param[out] rc Return code
   subroutine register_dust_process(rc)
      use ProcessRegistry_Mod, only: get_global_registry, ProcessRegistryType

      integer, intent(out) :: rc
      type(ProcessRegistryType), pointer :: registry

      rc = CC_SUCCESS
      registry => get_global_registry()

      call registry%register_process( &
         name='dust', &
         category='emission', &
         description='Process for computing windblown dust emissions', &
         creator=create_dust_process, &
         rc=rc &
      )

   end subroutine register_dust_process

   !> Get default configuration for dust process
   !!
   !! This function returns a default configuration string that can be
   !! used to initialize the dust process with reasonable defaults.
   !!
   !! @param[out] config_data Default configuration string
   subroutine get_dust_default_config(config_data)
      character(len=*), intent(out) :: config_data

      ! Return default YAML configuration
      config_data = &
         '# Default dust process configuration' // new_line('A') // &
         'process:' // new_line('A') // &
         '  name: "dust"' // new_line('A') // &
         '  version: "1.0.0"' // new_line('A') // &
         '  active_scheme: ""' // new_line('A') // &
         '  is_active: true' // new_line('A') // &
         '' // new_line('A') // &
         '# Scheme configuration' // new_line('A') // &
         'schemes:' // new_line('A') // &
         '  fengsha:' // new_line('A') // &
         '    description: "Fengsha Dust emission scheme developed at NOAA ARL for use at NOAA NWS"' // new_line('A') // &
         '    algorithm_type: "explicit"' // new_line('A') // &
         '    parameters:' // new_line('A') // &
         '      scale_factor: 1.0' // new_line('A') // &
         '' // new_line('A') // &
         '  ginoux:' // new_line('A') // &
         '    description: "Ginoux dust emission scheme"' // new_line('A') // &
         '    algorithm_type: "explicit"' // new_line('A') // &
         '    parameters:' // new_line('A') // &
         '      scale_factor: 1.0' // new_line('A') // &
         '' // new_line('A') // &
         '# Diagnostic configuration' // new_line('A') // &
         'diagnostics:' // new_line('A') // &
         '  output_frequency: 3600.0  # seconds' // new_line('A') // &
         '  output_diagnostics: true'

   end subroutine get_dust_default_config

end module DustProcessCreator_Mod