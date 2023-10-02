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
    error stop "[def_var_dim] Group dimensions not allocated."
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
    error stop "[def_var_dim] Variable dimensions not allocated."
  end if
end subroutine def_var_dim

function inq_dims_(ncid, dimids) result(dims)
  integer(c_int), intent(in) :: ncid
  integer(c_int), intent(in) :: dimids(:)
  type(nc_dim), allocatable :: dims(:)
  integer(c_int) :: stat, ndims, i, unlim_dim
  character(kind=c_char, len=nc_max_name) :: tmp

  ndims = size(dimids)
  if (allocated(dims)) deallocate (dims)
  allocate (dims(ndims))

  stat = nc_inq_unlimdim(ncid, unlim_dim)
  call handle_error(stat, "nc_inq_unlimdim")

  do i = 1, ndims
    associate (dim => dims(i))
      dim%ID = dimids(i)
      stat = nc_inq_dimlen(ncid, dim%ID, dim%len)
      call handle_error(stat, "nc_inq_dimlen")

      stat = nc_inq_dimname(ncid, dim%ID, tmp)
      call handle_error(stat, "nc_inq_dimname")
      dim%name = cstrip(tmp)
      dim%is_unlim = dim%ID == unlim_dim
    end associate
  end do
end function inq_dims_

module subroutine inq_grp_dims(grp)
  class(nc_grp), intent(inout) :: grp
  integer(c_int) :: stat, ndims, include_parents
  integer(c_int), allocatable :: dimids(:)

  stat = nc_inq_ndims(grp%ID, ndims)
  call handle_error(stat, "nc_inq_ndims")

  allocate (dimids(ndims))
  stat = nc_inq_dimids(grp%ID, ndims, dimids, include_parents)
  call handle_error(stat, "nc_inq_dimids")

  grp%dims = inq_dims_(grp%ID, dimids)
end subroutine inq_grp_dims

module subroutine inq_var_dims(var)
  type(nc_var), intent(inout) :: var
  integer(c_int) :: stat, ndims
  integer(c_int), allocatable :: dimids(:)

  if (.not. associated(var%grpID)) &
    & error stop "[inq_var_dims] Group ID not associated."

  stat = nc_inq_varndims(var%grpID, var%ID, ndims)
  call handle_error(stat, "nc_inq_varndims")

  allocate (dimids(ndims))
  stat = nc_inq_vardimid(var%grpID, var%ID, dimids)
  call handle_error(stat, "nc_inq_vardimid")

  var%dims = inq_dims_(var%grpID, dimids)
end subroutine inq_var_dims

end submodule submodule_dimension
