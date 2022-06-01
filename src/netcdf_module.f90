module netcdf_module

  use netcdf
  implicit none

  public :: netcdf_file

  private

  !> Function name with an underline at the end
  !> is a direct wrapper to the original NetCDF API.

  !> NetCDF Fortran file type
  type :: netcdf_file

    private
    character(:), allocatable :: filename
    integer :: status = 0
    integer :: mode = 0
    integer :: ncid = 0

  contains

    private
    procedure :: strerror_
    procedure :: check
    procedure :: open_
    procedure :: create_
    procedure :: close_
    procedure :: read_formatted
    generic :: read (formatted) => read_formatted

  end type netcdf_file

  !> Multidimensional unlimited 
  !> polymorphic data container.
  type, abstract :: netcdf_data
  end type netcdf_data

  type, extends(netcdf_data) :: netcdf_data_1d
    class(*), allocatable :: values(:)
  end type netcdf_data_1d

  type, extends(netcdf_data) :: netcdf_data_2d
    class(*), allocatable :: values(:, :)
  end type netcdf_data_2d

  type, extends(netcdf_data) :: netcdf_data_3d
    class(*), allocatable :: values(:, :, :)
  end type netcdf_data_3d

  type, extends(netcdf_data) :: netcdf_data_4d
    class(*), allocatable :: values(:, :, :, :)
  end type netcdf_data_4d

  !> NetCDF Fortran group type
  type, extends(netcdf_file) :: netcdf_group
    character(:), allocatable :: name
    type(netcdf_attributes), allocatable :: attributes(:)
    type(netcdf_dimensions), allocatable :: dimensions(:)
    type(netcdf_variables), allocatable :: variables(:)
    type(netcdf_group), allocatable :: group
  end type netcdf_group

  !> NetCDF Fortran attribute
  type :: netcdf_attribute
    character(:), allocatable :: name
    class(*), allocatable :: values(:)
  end type netcdf_attribute

  type :: netcdf_attributes
    type(netcdf_attribute), pointer :: value => null()
  end type netcdf_attributes

  !> NetCDF Fortran dimension
  type :: netcdf_dimension
    character(:), allocatable :: name
    class(*), allocatable :: values(:)
    integer :: length
    logical :: is_unlimited
  end type netcdf_dimension

  type :: netcdf_dimensions
    type(netcdf_dimension), pointer :: value => null()
  end type netcdf_dimensions

  !> NetCDF variable type
  type :: netcdf_variable
    character(:), allocatable :: name
    type(netcdf_dimensions), allocatable :: dimensions(:)
    type(netcdf_attributes), allocatable :: attributes(:)
    class(netcdf_data), allocatable :: value
  end type netcdf_variable

  type :: netcdf_variables
    type(netcdf_variable), pointer :: value => null()
  end type netcdf_variables

  interface

    module function strerror_(self) result(ret)
      class(netcdf_file), intent(in) :: self
      character(len=80) :: ret
    end function strerror_

    module subroutine check(self)
      class(netcdf_file), intent(inout) :: self
    end subroutine check

    module function open_(self) result(ret)
      class(netcdf_file), intent(inout) :: self
      integer :: ret
    end function open_

    module function create_(self) result(ret)
      class(netcdf_file), intent(inout) :: self
      integer :: ret
    end function create_

    module function close_(self) result(ret)
      class(netcdf_file), intent(in) :: self
      integer :: ret
    end function close_

    module subroutine read_formatted(self, unit, iotype, v_list, iostat, iomsg)
      class(netcdf_file), intent(inout) :: self
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg
    end subroutine read_formatted

  end interface

  integer, parameter :: filename_max_len = 1000

end module netcdf_module
