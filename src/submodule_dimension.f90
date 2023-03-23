submodule(module_netcdf) submodule_dimension

implicit none
contains

!> New dimensions based on nc_id.
module subroutine inquire_group_dimensions(group)
  type(group_type), intent(inout) :: group
  type(dimension_type), allocatable :: dimensions(:)
  integer(c_int) :: status, unlimited_dim
  integer(c_int), allocatable :: dim_ids(:)
  integer(c_size_t), allocatable :: dim_lens(:)
  character(kind=c_char, len=:), allocatable :: dim_name
  integer :: num_dims, i
  integer(c_int), parameter :: include_parents = 0

  !> Inquire number of dimensions
  status = nc_inq_ndims(group%id, num_dims)
  call handle_error(status, "nc_inq_ndims")

  !> Inquire dimension IDs
  allocate (dim_ids(num_dims), dim_lens(num_dims))
  status = nc_inq_dimids(group%id, num_dims, dim_ids, include_parents)
  call handle_error(status, "nc_inq_dimids")

  !> Inquire dimension lengths and names
  allocate (dimensions(num_dims))
  allocate (character(kind=c_char, len=num_chars) :: dim_name)

  status = nc_inq_unlimdim(group%id, unlimited_dim)
  call handle_error(status, "nc_inq_unlimdim")

  do i = 1, num_dims
    status = nc_inq_dimlen(group%id, dim_ids(i), dim_lens(i))
    call handle_error(status, "nc_inq_dimlen")
    dimensions(i)%id = dim_ids(i)
    dimensions(i)%length = dim_lens(i)

    status = nc_inq_dimname(group%id, dimensions(i)%id, dim_name)
    call handle_error(status, "nc_inq_dimname")
    dimensions(i)%name = strip(dim_name, num_chars)
    dimensions(i)%is_unlimited = dim_ids(i) == unlimited_dim
  end do

  group%dimensions = dimensions
end subroutine inquire_group_dimensions

!> New dimensions based on nc_id.
module subroutine inquire_variable_dimensions(group, variable)
  type(group_type), intent(inout) :: group
  type(variable_type), intent(inout) :: variable
  integer(c_int), allocatable :: dim_ids(:), grp_ids(:)
  integer(c_int) :: status, unlimited_dim
  integer :: num_dims, i
  integer, allocatable :: dim_idx(:)
  integer(c_int), parameter :: include_parents = 0

  if (.not. allocated(group%dimensions)) &
    & error stop "You have to allocate group first."

  !> Inquire number of dimensions
  status = nc_inq_varndims(group%id, variable%id, num_dims)
  call handle_error(status, "nc_inq_varndims")

  !> Inquire dimension IDs
  allocate (dim_ids(num_dims), dim_idx(num_dims))
  status = nc_inq_vardimid(group%id, variable%id, dim_ids)
  call handle_error(status, "nc_inq_dimids")

  grp_ids = [(group%dimensions(i)%id, i=1, size(group%dimensions))]
  dim_ids = dim_ids(num_dims:1:-1)
  dim_idx = [(findloc(grp_ids, dim_ids(i), dim=1), i=1, num_dims)]

  if (associated(variable%dimensions)) deallocate (variable%dimensions)
  allocate (variable%dimensions, source=group%dimensions(dim_idx))
end subroutine inquire_variable_dimensions

!> Shape of dimensions
module function shape_dimensions(dimensions) result(shapes)
  type(dimension_type), intent(in) :: dimensions(:)
  integer(int64), allocatable :: shapes(:)
  integer :: i, start, end

  if (allocated(shapes)) deallocate (shapes)
  start = lbound(dimensions, dim=1)
  end = ubound(dimensions, dim=1)
  shapes = [(dimensions(i)%length, i=start, end)]
end function shape_dimensions

end submodule submodule_dimension
