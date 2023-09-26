submodule(module_fortran_interface) submodule_dimension
implicit none
contains

module pure function new_dim(name, len) result(dim)
  character(len=*), intent(in) :: name
  integer, intent(in) :: len
  type(nc_dim) :: dim

  dim%name = name
  dim%len = len
end function new_dim

module pure function new_dims(dims) result(ret)
  type(nc_dim), intent(in) :: dims(:)
  type(nc_dim), allocatable :: ret(:)
  integer :: i

  ret = dims
  do i = 1, size(ret)
    ret(i)%ID = i - 1
  end do
end function new_dims

module pure function new_dims_unlim(dims, unlim_dim) result(ret)
  type(nc_dim), intent(in) :: dims(:)
  integer, intent(in) :: unlim_dim
  type(nc_dim), allocatable :: ret(:)
  integer :: i

  ret = dims
  do i = 1, size(ret)
    ret(i)%ID = i - 1
  end do
  ret(unlim_dim - 1)%is_unlim = .true.
end function new_dims_unlim

module pure function shape_dims(dims) result(ret)
  type(nc_dim), intent(in) :: dims(:)
  integer, allocatable :: ret(:)
  integer :: i

  ret = [(dims(i)%len, i=1, size(dims))]
end function shape_dims

module subroutine def_grp_dim(grp)
  class(nc_grp), intent(in) :: grp
  integer(c_int) :: stat, i

  if (allocated(grp%dims)) then
    do i = 1, size(grp%dims)
      stat = nc_def_dim(grp%ID, cstr(grp%dims(i)%name), &
        & int(grp%dims(i)%len, c_size_t), grp%dims(i)%ID)
      call handle_error(stat, "nc_def_dim")
    end do
  else
    error stop "Group dimensions not allocated."
  end if
end subroutine def_grp_dim

module subroutine def_var_dim(var)
  type(nc_var), intent(in) :: var
  integer(c_int) :: stat, i

  if (associated(var%grpID)) then
    do i = 1, size(var%dims)
      stat = nc_def_dim(var%grpID, cstr(var%dims(i)%name), &
        & int(var%dims(i)%len, c_size_t), var%dims(i)%ID)
      call handle_error(stat, "nc_def_dim")
    end do
  else
    error stop "Variable dimensions not allocated."
  end if
end subroutine def_var_dim

end submodule submodule_dimension
