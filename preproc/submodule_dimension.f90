submodule(module_interface) submodule_dimension
implicit none
contains

!> Dimension constructor
module pure function new_dim(name, len) result(dim)
  character(len=*), intent(in) :: name
  integer, intent(in) :: len
  type(dimension_type) :: dim

  dim%name = name
  dim%len = len
end function new_dim

!> Dimensions constructor
module function new_dims(dims) result(ret)
  type(dimension_type), intent(in) :: dims(:)
  type(dimension_type), allocatable :: ret(:)
  integer :: i

  ret = dims
  do i = 1, size(ret)
    ret(i)%ID = i - 1
  end do
end function new_dims

!> Dimensions contructor (with one-based unlimited dimension specified)
module function new_dims_unlim(dims, unlim_dim) result(ret)
  type(dimension_type), intent(in) :: dims(:)
  integer, intent(in) :: unlim_dim
  type(dimension_type), allocatable :: ret(:)
  integer :: i

  ret = dims
  do i = 1, size(ret)
    ret(i)%ID = i - 1
    ret(i)%is_unlim = ret(i)%ID == unlim_dim - 1
  end do
end function new_dims_unlim

!> Shape of dimensions
module pure function shape_dims(dims) result(ret)
  type(dimension_type), intent(in) :: dims(:)
  integer, allocatable :: ret(:)
  integer :: i

  ret = [(dims(i)%len, i=1, size(dims))]
end function shape_dims

module subroutine def_grp_dim(grp)
  class(group_type), intent(in) :: grp
  integer(c_int) :: stat, i

  do i = 1, size(grp%dims)
    stat = nc_def_dim(grp%ID, cstr(grp%dims(i)%name), &
      & int(grp%dims(i)%len, c_size_t), grp%dims(i)%ID)
    call handle_error(stat, "nc_def_dim")
  end do
end subroutine def_grp_dim

module subroutine def_var_dim(var)
  type(variable_type), intent(in) :: var
  integer(c_int) :: stat, i

  associate (var_ => var%var)
    do i = 1, size(var_%dims)
      associate (dim_ => var_%dims(i))
        stat = nc_def_dim(var_%grpID, &
          & cstr(dim_%name), dim_%len, dim_%ID)
        call handle_error(stat, "nc_def_dim")
      end associate
    end do
  end associate
end subroutine def_var_dim

end submodule submodule_dimension
