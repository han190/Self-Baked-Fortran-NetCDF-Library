submodule(module_netcdf) submodule_dimension

implicit none
contains

!> New dimensions based on nc_id.
module function inquire_dimensions(nc_id) result(dimensions)
  integer(c_int), intent(in) :: nc_id
  type(dimension_type), allocatable :: dimensions(:)
  integer(c_int), allocatable :: dim_ids(:)
  integer(int64), allocatable :: dim_lens(:)
  character(kind=c_char, len=:), allocatable :: dim_name
  integer(c_int) :: status, unlimited_dim
  integer :: num_dims, i
  integer(c_int), parameter :: include_parents = 0

  !> Inquire number of dimensions
  status = nc_inq_ndims(nc_id, num_dims)
  call handle_error(status, "nc_inq_ndims")

  !> Inquire dimension IDs
  allocate (dim_ids(num_dims), dim_lens(num_dims))
  status = nc_inq_dimids(nc_id, num_dims, dim_ids, include_parents)
  call handle_error(status, "nc_inq_dimids")

  !> Inquire dimension lengths and names
  if (allocated(dimensions)) deallocate (dimensions)
  allocate (dimensions(num_dims))
  allocate (character(kind=c_char, len=num_chars) :: dim_name)

  status = nc_inq_unlimdim(nc_id, unlimited_dim)
  call handle_error(status, "nc_inq_unlimdim")

  do i = 1, num_dims
    status = nc_inq_dimlen(nc_id, dim_ids(i), dim_lens(i))
    call handle_error(status, "nc_inq_dimlen")
    dimensions(i)%id = dim_ids(i)
    dimensions(i)%length = dim_lens(i)

    status = nc_inq_dimname(nc_id, dimensions(i)%id, dim_name)
    call handle_error(status, "nc_inq_dimname")
    dimensions(i)%name = strip(dim_name, num_chars)
    dimensions(i)%is_unlimited = dim_ids(i) == unlimited_dim
  end do
end function inquire_dimensions

!> New dimensions based on nc_id.
module function inquire_variable_dimensions(nc_id, var_id) result(dimensions)
  integer(c_int), intent(in) :: nc_id, var_id
  type(dimension_type), allocatable :: dimensions(:)
  integer(c_int), allocatable :: dim_ids(:)
  integer(int64), allocatable :: dim_lens(:)
  character(kind=c_char, len=:), allocatable :: dim_name
  integer(c_int) :: status, unlimited_dim
  integer :: num_dims, i
  integer(c_int), parameter :: include_parents = 0

  !> Inquire number of dimensions
  status = nc_inq_varndims(nc_id, var_id, num_dims)
  call handle_error(status, "nc_inq_varndims")

  !> Inquire dimension IDs
  allocate (dim_ids(num_dims), dim_lens(num_dims))
  status = nc_inq_vardimid(nc_id, var_id, dim_ids)
  call handle_error(status, "nc_inq_dimids")

  !> Inquire dimension lengths and names
  if (allocated(dimensions)) deallocate (dimensions)
  allocate (dimensions(num_dims))
  allocate (character(kind=c_char, len=num_chars) :: dim_name)

  status = nc_inq_unlimdim(nc_id, unlimited_dim)
  call handle_error(status, "nc_inq_unlimdim")

  do i = 1, num_dims
    status = nc_inq_dimlen(nc_id, dim_ids(i), dim_lens(i))
    call handle_error(status, "nc_inq_dimlen")
    dimensions(i)%id = dim_ids(i)
    dimensions(i)%length = dim_lens(i)

    status = nc_inq_dimname(nc_id, dimensions(i)%id, dim_name)
    call handle_error(status, "nc_inq_dimname")
    dimensions(i)%name = strip(dim_name, num_chars)
    dimensions(i)%is_unlimited = dim_ids(i) == unlimited_dim
  end do
end function inquire_variable_dimensions

!> Shape of dimensions
module function shape_dimensions(dimensions) result(shapes)
  type(dimension_type), intent(in) :: dimensions(:)
  integer(int64), allocatable :: shapes(:)
  integer :: i

  if (allocated(shapes)) deallocate (shapes)
  shapes = [(dimensions(i)%length, i=1, size(dimensions))]
end function shape_dimensions

end submodule submodule_dimension
