module netcdf_group_module

  use netcdf
  use netcdf_file_module
  implicit none

  public :: netcdf_dataset

  private

  !> Root group is automatically initialized
  !> after the netcdf_file is initialized.
  type, extends(netcdf_file) :: netcdf_dataset

    integer :: numgrps = 0
    integer, allocatable :: ncids(:)
    type(netcdf_groups), allocatable :: grps(:)

  contains

    procedure :: inq_grps_ => inq_grps_root
    procedure :: open => open_dataset

  end type netcdf_dataset

  !> In order to let each group has its own
  !> subgroups we construct a node-like type.
  type :: netcdf_groups
    type(netcdf_group) :: grp
    type(netcdf_groups), allocatable :: grps(:)
  end type netcdf_groups

  !> NetCDF group type
  type :: netcdf_group

    character(:), allocatable :: name
    integer :: ncid = 0
    integer :: nvar = 0
    integer :: ndim = 0
    integer :: parents = 0
    integer, allocatable :: dimids(:)

  contains

    procedure :: inq_dimids_

  end type netcdf_group

  interface

    module function inq_grps_root(self) result(ret)
      class(netcdf_dataset), intent(inout) :: self
      integer :: ret
    end function inq_grps_root

    module function inq_dimids_(self) result(ret)
      class(netcdf_group), intent(inout) :: self
      integer :: ret
    end function inq_dimids_

    module subroutine open_dataset(self, filename, mode)
      class(netcdf_dataset), intent(inout) :: self
      character(*), intent(in) :: filename
      character(*), intent(in) :: mode
    end subroutine open_dataset

  end interface

end module netcdf_group_module
