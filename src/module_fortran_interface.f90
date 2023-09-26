module module_fortran_interface

use :: iso_fortran_env
use :: iso_c_binding
use :: module_c_interface
implicit none

!> The data model follows the netCDF data model introduced
!> https://docs.unidata.ucar.edu/netcdf-c/current/netcdf_data_model.html

public :: nc_dim, nc_att, nc_var, nc_grp
public :: operator(.dim.), operator(.att.)
public :: write(formatted)
public :: shape, atts, dims
public :: to_netcdf, data_array
private

type, abstract :: netcdf_type
  integer(c_int) :: ID
  character(len=:), allocatable :: name
end type netcdf_type

type, extends(netcdf_type) :: nc_dim
  integer(c_size_t) :: len = 0
  logical :: is_unlim = .false.
end type nc_dim

type, extends(netcdf_type) :: nc_att
  integer(c_size_t) :: len = 0
  integer(c_int) :: type = 0
  class(*), allocatable :: vals(:)
end type nc_att

type, extends(netcdf_type) :: nc_var
  type(nc_dim), allocatable :: dims(:)
  type(nc_att), allocatable :: atts(:)
  integer(c_int), pointer :: grpID => null()
  integer(c_int) :: type = 0
  class(*), allocatable :: vals(:)
end type nc_var

type, extends(netcdf_type) :: nc_grp
  type(nc_dim), allocatable :: dims(:)
  type(nc_att), allocatable :: atts(:)
  type(nc_var), allocatable :: vars(:)
  type(nc_grp), allocatable :: grps(:)
  integer(c_int) :: mode = 0
  integer(c_int) :: fmt = 0
end type nc_grp

type, extends(nc_grp) :: nc_file
  character(len=:), allocatable :: filename
end type nc_file

interface operator(.dim.)
  module procedure :: new_dim
end interface operator(.dim.)

interface dims
  module procedure :: new_dims
  module procedure :: new_dims_unlim
end interface dims

interface operator(.att.)
  module procedure :: new_att_scal
  module procedure :: new_att_vec
end interface operator(.att.)

interface atts
  module procedure :: new_atts
end interface atts

interface to_netcdf
  module procedure :: to_netcdf_var
end interface to_netcdf

interface shape
  module procedure :: shape_dims
  module procedure :: shape_var
end interface shape

interface write (formatted)
  module procedure :: write_formatted_att
  module procedure :: write_formatted_dim
  module procedure :: write_formatted_var
end interface write (formatted)

interface
  module pure function cstr(string) result(cstring)
    character(len=*), intent(in) :: string
    character(kind=c_char, len=:), allocatable :: cstring
  end function cstr

  module pure function cstrip(cstring) result(string)
    character(len=*), intent(in) :: cstring
    character(:), allocatable :: string
  end function cstrip

  module subroutine handle_error(stat, err_msg)
    integer(c_int), intent(in) :: stat
    character(*), intent(in), optional :: err_msg
  end subroutine handle_error

  module pure function new_dim(name, len) result(dim)
    character(len=*), intent(in) :: name
    integer, intent(in) :: len
    type(nc_dim) :: dim
  end function new_dim

  module pure function new_dims(dims) result(ret)
    type(nc_dim), intent(in) :: dims(:)
    type(nc_dim), allocatable :: ret(:)
  end function new_dims

  module pure function new_dims_unlim(dims, unlim_dim) result(ret)
    type(nc_dim), intent(in) :: dims(:)
    integer, intent(in) :: unlim_dim
    type(nc_dim), allocatable :: ret(:)
  end function new_dims_unlim

  module pure function shape_dims(dims) result(ret)
    type(nc_dim), intent(in) :: dims(:)
    integer, allocatable :: ret(:)
  end function shape_dims

  module subroutine def_grp_dim(grp)
    class(nc_grp), intent(in) :: grp
  end subroutine def_grp_dim

  module subroutine def_var_dim(var)
    type(nc_var), intent(in) :: var
  end subroutine def_var_dim

  module pure function new_att_vec(name, vals) result(ret)
    character(len=*), intent(in) :: name
    class(*), intent(in) :: vals(:)
    type(nc_att) :: ret
  end function new_att_vec

  module pure function new_att_scal(name, val) result(ret)
    character(len=*), intent(in) :: name
    class(*), intent(in) :: val
    type(nc_att) :: ret
  end function new_att_scal

  module pure function new_atts(atts) result(ret)
    type(nc_att), intent(in) :: atts(:)
    type(nc_att), allocatable :: ret(:)
  end function new_atts

  module subroutine put_grp_atts(grp)
    class(nc_grp), intent(in) :: grp
  end subroutine put_grp_atts

  module subroutine put_var_atts(var)
    type(nc_var), intent(in) :: var
  end subroutine put_var_atts

  module function data_array(data, name, dims, atts) result(var)
    class(*), intent(in) :: data(*)
    character(len=*), intent(in) :: name
    type(nc_dim), intent(in) :: dims(:)
    type(nc_att), intent(in), optional :: atts(:)
    type(nc_var) :: var
  end function data_array

  module pure function shape_var(var) result(ret)
    type(nc_var), intent(in) :: var
    integer, allocatable :: ret(:)
  end function shape_var

  module subroutine to_netcdf_var(var, filename, mode)
    type(nc_var), intent(inout) :: var
    character(len=*), intent(in) :: filename
    integer(c_int), intent(in), optional :: mode
  end subroutine to_netcdf_var

  module subroutine write_formatted_dim( &
    & dim, unit, iotype, v_list, iostat, iomsg)
    class(nc_dim), intent(in) :: dim
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
  end subroutine write_formatted_dim

  module subroutine write_formatted_att( &
    & att, unit, iotype, v_list, iostat, iomsg)
    class(nc_att), intent(in) :: att
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
  end subroutine write_formatted_att

  module subroutine write_formatted_var( &
    & var, unit, iotype, v_list, iostat, iomsg)
    class(nc_var), intent(in) :: var
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
  end subroutine write_formatted_var
end interface

end module module_fortran_interface
