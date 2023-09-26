submodule(module_fortran_interface) submodule_variable
implicit none
contains

module function data_array(data, name, dims, atts) result(var)
  class(*), intent(in) :: data(*)
  character(len=*), intent(in) :: name
  type(nc_dim), intent(in) :: dims(:)
  type(nc_att), intent(in), optional :: atts(:)
  type(nc_var) :: var

  var%name = name
  var%dims = dims
  if (present(atts)) var%atts = atts
  if (allocated(var%vals)) deallocate (var%vals)
  allocate (var%vals, source=data)

  select type (data)
  type is (integer(int8))
    var%type = nc_byte
  type is (integer(int16))
    var%type = nc_short
  type is (integer(int32))
    var%type = nc_int
  type is (integer(int64))
    var%type = nc_int64
  type is (real(real32))
    var%type = nc_float
  type is (real(real64))
    var%type = nc_double
  type is (character(*))
    var%type = nc_char
  end select
end function data_array

module pure function shape_var(var) result(ret)
  type(nc_var), intent(in) :: var
  integer, allocatable :: ret(:)

  ret = shape_dims(var%dims)
end function shape_var

subroutine def_var(var)
  type(nc_var), intent(inout) :: var
  integer(c_int) :: stat, ndims, i
  integer(c_int), allocatable :: dimids(:)

  ndims = size(var%dims, kind=c_int)
  dimids = [(var%dims(i)%ID, i=1, ndims)]
  if (associated(var%grpID)) then
    stat = nc_def_var(var%grpID, cstr(var%name), &
      & var%type, ndims, dimids, var%ID)
    call handle_error(stat, "nc_def_var")
  else
    error stop "Group ID not associated."
  end if
end subroutine def_var

subroutine put_var(var)
  type(nc_var), intent(in) :: var
  integer(c_int) :: stat

  select type (vals_ => var%vals)
  type is (integer(int8))
    stat = nc_put_var_ubyte(var%grpID, var%ID, vals_)
    call handle_error(stat, "nc_put_var_ubyte")
  type is (integer(int16))
    stat = nc_put_var_short(var%grpID, var%ID, vals_)
    call handle_error(stat, "nc_put_var_short")
  type is (integer(int32))
    stat = nc_put_var_int(var%grpID, var%ID, vals_)
    call handle_error(stat, "nc_put_var_int")
  type is (integer(int64))
    stat = nc_put_var_longlong(var%grpID, var%ID, vals_)
    call handle_error(stat, "nc_put_var_longlong")
  type is (real(real32))
    stat = nc_put_var_float(var%grpID, var%ID, vals_)
    call handle_error(stat, "nc_put_var_float")
  type is (real(real64))
    stat = nc_put_var_double(var%grpID, var%ID, vals_)
    call handle_error(stat, "nc_put_var_double")
  end select
end subroutine put_var

module subroutine to_netcdf_var(var, filename, mode)
  type(nc_var), intent(inout) :: var
  character(len=*), intent(in) :: filename
  integer(c_int), intent(in), optional :: mode
  type(nc_file), target :: file
  integer(c_int) :: stat

  !> Copy metadata
  file%filename = filename
  file%name = cstr("/")

  if (present(mode)) then
    file%mode = mode
  else
    file%mode = ior(nc_netcdf4, nc_clobber)
  end if

  select case (file%mode)
  case (ior(nc_netcdf4, nc_clobber))
    stat = nc_create(cstr(filename), file%mode, file%ID)
    call handle_error(stat, "nc_create")
    var%grpID => file%ID

    call def_var_dim(var)
    if (allocated(file%atts)) call put_grp_atts(file)
    call def_var(var)
    if (allocated(var%atts)) call put_var_atts(var)
    call put_var(var)
    nullify (var%grpID)
  case default
    error stop "Mode not supported yet."
  end select

  stat = nc_close(file%ID)
  call handle_error(stat, "nc_close")
end subroutine to_netcdf_var

end submodule submodule_variable
