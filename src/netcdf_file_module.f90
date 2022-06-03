module netcdf_file_module

  use netcdf
  implicit none

  public :: netcdf_file

  private

  !> Function name with an underline at the end
  !> is a direct wrapper to the original NetCDF API.

  !> NetCDF Fortran file type
  type, abstract :: netcdf_file

    character(:), allocatable :: path
    integer :: status = 0
    integer :: mode = 0
    integer :: ncid = 0

    integer :: ndim = 0
    integer :: nvar = 0
    integer :: natt = 0
    integer :: unlimited_dimid = 0

  contains

    procedure :: strerror_
    procedure :: create_
    procedure :: open_
    procedure :: redef_
    procedure :: enddef_
    procedure :: close_
    procedure :: inquire_
    procedure :: check

  end type netcdf_file

  interface

    module function strerror_(self) result(ret)
      class(netcdf_file), intent(in) :: self
      character(len=80) :: ret
    end function strerror_

    module function create_(self) result(ret)
      class(netcdf_file), intent(inout) :: self
      integer :: ret
    end function create_

    module function open_(self) result(ret)
      class(netcdf_file), intent(inout) :: self
      integer :: ret
    end function open_

    module function redef_(self) result(ret)
      class(netcdf_file), intent(inout) :: self
      integer :: ret
    end function redef_

    module function enddef_(self) result(ret)
      class(netcdf_file), intent(inout) :: self
      integer :: ret
    end function enddef_

    module function close_(self) result(ret)
      class(netcdf_file), intent(in) :: self
      integer :: ret
    end function close_

    module function inquire_(self) result(ret)
      class(netcdf_file), intent(inout) :: self
      integer :: ret
    end function inquire_

    module subroutine check(self)
      class(netcdf_file), intent(inout) :: self
    end subroutine check

  end interface

end module netcdf_file_module
