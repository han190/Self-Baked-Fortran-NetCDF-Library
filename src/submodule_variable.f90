submodule(module_fortran_interface) submodule_variable
implicit none
contains

module function new_var(data, name, dims, atts) result(var)
  class(*), intent(in) :: data(:)
  character(len=*), intent(in) :: name
  type(nc_dim), intent(in) :: dims(:)
  type(nc_att), intent(in), optional :: atts(:)
  type(nc_var) :: var

  var%name = name
  var%dims = dims
  if (present(atts)) var%atts = atts
  ! if (allocated(var%vals)) deallocate (var%vals)
  ! allocate (var%vals, source=data)
  var%vals = data

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
end function new_var

module pure function shape_var(var) result(ret)
  type(nc_var), intent(in) :: var
  integer, allocatable :: ret(:)

  ret = shape_dims(var%dims)
end function shape_var

module subroutine def_var(var)
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

module subroutine put_var(var)
  type(nc_var), intent(in) :: var
  integer(c_int) :: stat

  if (.not. allocated(var%vals)) &
    & error stop "[put_var] Varible values not allocated."

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

module function inq_var(grp, name) result(var)
  class(nc_grp), target, intent(inout) :: grp
  character(len=*), intent(in) :: name
  type(nc_var) :: var
  integer(c_int) :: stat

  var%grpID => grp%ID
  var%name = name
  stat = nc_inq_varid(var%grpID, cstr(name), var%ID)
  call handle_error(stat, "nc_inq_varid")

  stat = nc_inq_vartype(var%grpID, var%ID, var%type)
  call handle_error(stat, "nc_inq_vartype")

  call inq_var_dims(var)
  call inq_var_atts(var)
  call get_var_atts(var)
end function inq_var

subroutine get_var(var)
  type(nc_var), intent(inout) :: var
  integer(c_int) :: stat, total_len

  if (.not. associated(var%grpID)) &
    & error stop "[get_var] Group ID not associated."

  if (allocated(var%vals)) deallocate (var%vals)
  total_len = product(shape(var))

  select case (var%type)
  case (nc_byte)
    allocate (integer(int8) :: var%vals(total_len))
  case (nc_short)
    allocate (integer(int16) :: var%vals(total_len))
  case (nc_int)
    allocate (integer(int32) :: var%vals(total_len))
  case (nc_int64)
    allocate (integer(int64) :: var%vals(total_len))
  case (nc_float)
    allocate (real(real32) :: var%vals(total_len))
  case (nc_double)
    allocate (real(real64) :: var%vals(total_len))
  end select

  select type (vals_ => var%vals)
  type is (integer(int8))
    stat = nc_get_var_ubyte(var%grpID, var%ID, vals_)
    call handle_error(stat, "nc_get_var_ubyte")
  type is (integer(int16))
    stat = nc_get_var_short(var%grpID, var%ID, vals_)
    call handle_error(stat, "nc_get_var_short")
  type is (integer(int32))
    stat = nc_get_var_int(var%grpID, var%ID, vals_)
    call handle_error(stat, "nc_get_var_int")
  type is (integer(int64))
    stat = nc_get_var_longlong(var%grpID, var%ID, vals_)
    call handle_error(stat, "nc_get_var_longlong")
  type is (real(real32))
    stat = nc_get_var_float(var%grpID, var%ID, vals_)
    call handle_error(stat, "nc_get_var_float")
  type is (real(real64))
    stat = nc_get_var_double(var%grpID, var%ID, vals_)
    call handle_error(stat, "nc_get_var_double")
  end select
end subroutine get_var

module subroutine to_netcdf_var(var, filename, mode)
  type(nc_var), intent(in) :: var
  character(len=*), intent(in) :: filename
  integer(c_int), intent(in), optional :: mode
  type(nc_file), target :: file

  file = data_set([var])
  if (present(mode)) then
    file%mode = mode
  else
    file%mode = ior(nc_netcdf4, nc_clobber)
  end if
  file%filename = filename
  call to_netcdf_grp(file)
end subroutine to_netcdf_var

module subroutine to_netcdf_vars(vars, filename, mode)
  type(nc_var), intent(in) :: vars(:)
  character(len=*), intent(in) :: filename
  integer(c_int), intent(in), optional :: mode
  type(nc_file), target :: file

  file = data_set(vars)
  if (present(mode)) then
    file%mode = mode
  else
    file%mode = ior(nc_netcdf4, nc_clobber)
  end if
  file%filename = filename
  call to_netcdf_grp(file)
end subroutine to_netcdf_vars

module function from_netcdf_var(filename, name) result(var)
  character(len=*), intent(in) :: filename, name
  type(nc_var) :: var
  type(nc_file) :: file
  integer(c_int) :: stat
  character(kind=c_char, len=nc_max_name) :: tmp

  file%filename = filename
  file%mode = nc_nowrite

  stat = nc_open(cstr(file%filename), file%mode, file%ID)
  call handle_error(stat, "nc_open")
  var = inq_var(file, name)
  call get_var(var)
end function from_netcdf_var

end submodule submodule_variable
