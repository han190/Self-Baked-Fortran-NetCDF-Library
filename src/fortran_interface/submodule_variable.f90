submodule(module_fortran_interface) submodule_variable
implicit none
contains

module function new_var(data, name, dims, unlim_dim, atts) result(var)
  class(*), target, intent(in) :: data(..)
  character(len=*), intent(in) :: name
  type(netcdf_dimension), intent(in) :: dims(:)
  integer, intent(in), optional :: unlim_dim
  type(netcdf_attribute), intent(in), optional :: atts(:)
  type(netcdf_variable) :: var

  var%name = name
  if (present(unlim_dim)) then
    var%dims = new_dims_unlim(dims, unlim_dim)
  else
    var%dims = new_dims(dims)
  end if
  if (present(atts)) then
    var%atts = new_atts(atts)
  end if

  select rank (data_ => data)
  rank (1)
    if (.not. all(shape(var%dims) == shape(data_))) &
      & error stop "[new_var] Shape of dims /= shape of data."
    var%vals(1:size(data_)) => data_
    select type (data_)
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
  rank (2)
    if (.not. all(shape(var%dims) == shape(data_))) &
      & error stop "[new_var] Shape of dims /= shape of data."
    var%vals(1:size(data_)) => data_
    select type (data_)
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
  rank (3)
    if (.not. all(shape(var%dims) == shape(data_))) &
      & error stop "[new_var] Shape of dims /= shape of data."
    var%vals(1:size(data_)) => data_
    select type (data_)
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
  rank (4)
    if (.not. all(shape(var%dims) == shape(data_))) &
      & error stop "[new_var] Shape of dims /= shape of data."
    var%vals(1:size(data_)) => data_
    select type (data_)
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
  rank (5)
    if (.not. all(shape(var%dims) == shape(data_))) &
      & error stop "[new_var] Shape of dims /= shape of data."
    var%vals(1:size(data_)) => data_
    select type (data_)
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
  rank default
    error stop "[new_var] Invalid rank."
  end select
end function new_var

module pure function shape_var(var) result(ret)
  type(netcdf_variable), intent(in) :: var
  integer, allocatable :: ret(:)

  ret = shape_dims(var%dims)
end function shape_var

module subroutine def_var(var)
  type(netcdf_variable), intent(inout) :: var
  integer(c_int) :: stat, ndims, i
  integer(c_int), allocatable :: dimids(:)

  ndims = size(var%dims, kind=c_int)
  dimids = [(var%dims(i)%ID, i=1, ndims)]
  if (associated(var%grpID)) then
    stat = nc_def_var(var%grpID, cstr(var%name), &
      & var%type, ndims, dimids, var%ID)
    call handle_error(stat, "nc_def_var")
  else
    error stop "[def_var] Group ID not associated."
  end if
end subroutine def_var

module subroutine put_var(var)
  type(netcdf_variable), intent(in) :: var
  integer(c_int) :: stat

  if (.not. associated(var%vals)) &
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
  class(netcdf_group), target, intent(inout) :: grp
  character(len=*), intent(in) :: name
  type(netcdf_variable) :: var
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
  type(netcdf_variable), intent(inout) :: var
  integer(c_int) :: stat, total_len

  if (.not. associated(var%grpID)) &
    & error stop "[get_var] Group ID not associated."

  if (associated(var%vals)) deallocate (var%vals)
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
  type(netcdf_variable), intent(in) :: var
  character(len=*), intent(in) :: filename
  integer(c_int), intent(in), optional :: mode
  type(netcdf_file), target :: file

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
  type(netcdf_variable), intent(in) :: vars(:)
  character(len=*), intent(in) :: filename
  integer(c_int), intent(in), optional :: mode
  type(netcdf_file), target :: file

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
  type(netcdf_variable) :: var
  type(netcdf_file) :: file
  integer(c_int) :: stat
  character(kind=c_char, len=nc_max_name) :: tmp

  file%filename = filename
  file%mode = nc_nowrite

  stat = nc_open(cstr(file%filename), file%mode, file%ID)
  call handle_error(stat, "nc_open")
  var = inq_var(file, name)
  call get_var(var)
end function from_netcdf_var

module subroutine extract_var_int8_1d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  integer(int8), pointer, intent(out) :: raw(:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (integer(int8))
      raw(1:s(1)) => vals
    class default
      error stop "[extract_var_int8_1d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_int8_1d
module subroutine extract_var_int16_1d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  integer(int16), pointer, intent(out) :: raw(:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (integer(int16))
      raw(1:s(1)) => vals
    class default
      error stop "[extract_var_int16_1d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_int16_1d
module subroutine extract_var_int32_1d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  integer(int32), pointer, intent(out) :: raw(:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (integer(int32))
      raw(1:s(1)) => vals
    class default
      error stop "[extract_var_int32_1d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_int32_1d
module subroutine extract_var_int64_1d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  integer(int64), pointer, intent(out) :: raw(:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (integer(int64))
      raw(1:s(1)) => vals
    class default
      error stop "[extract_var_int64_1d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_int64_1d
module subroutine extract_var_real32_1d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  real(real32), pointer, intent(out) :: raw(:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (real(real32))
      raw(1:s(1)) => vals
    class default
      error stop "[extract_var_real32_1d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_real32_1d
module subroutine extract_var_real64_1d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  real(real64), pointer, intent(out) :: raw(:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (real(real64))
      raw(1:s(1)) => vals
    class default
      error stop "[extract_var_real64_1d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_real64_1d
module subroutine extract_var_int8_2d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  integer(int8), pointer, intent(out) :: raw(:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (integer(int8))
      raw(1:s(1),1:s(2)) => vals
    class default
      error stop "[extract_var_int8_2d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_int8_2d
module subroutine extract_var_int16_2d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  integer(int16), pointer, intent(out) :: raw(:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (integer(int16))
      raw(1:s(1),1:s(2)) => vals
    class default
      error stop "[extract_var_int16_2d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_int16_2d
module subroutine extract_var_int32_2d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  integer(int32), pointer, intent(out) :: raw(:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (integer(int32))
      raw(1:s(1),1:s(2)) => vals
    class default
      error stop "[extract_var_int32_2d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_int32_2d
module subroutine extract_var_int64_2d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  integer(int64), pointer, intent(out) :: raw(:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (integer(int64))
      raw(1:s(1),1:s(2)) => vals
    class default
      error stop "[extract_var_int64_2d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_int64_2d
module subroutine extract_var_real32_2d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  real(real32), pointer, intent(out) :: raw(:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (real(real32))
      raw(1:s(1),1:s(2)) => vals
    class default
      error stop "[extract_var_real32_2d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_real32_2d
module subroutine extract_var_real64_2d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  real(real64), pointer, intent(out) :: raw(:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (real(real64))
      raw(1:s(1),1:s(2)) => vals
    class default
      error stop "[extract_var_real64_2d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_real64_2d
module subroutine extract_var_int8_3d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  integer(int8), pointer, intent(out) :: raw(:,:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (integer(int8))
      raw(1:s(1),1:s(2),1:s(3)) => vals
    class default
      error stop "[extract_var_int8_3d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_int8_3d
module subroutine extract_var_int16_3d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  integer(int16), pointer, intent(out) :: raw(:,:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (integer(int16))
      raw(1:s(1),1:s(2),1:s(3)) => vals
    class default
      error stop "[extract_var_int16_3d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_int16_3d
module subroutine extract_var_int32_3d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  integer(int32), pointer, intent(out) :: raw(:,:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (integer(int32))
      raw(1:s(1),1:s(2),1:s(3)) => vals
    class default
      error stop "[extract_var_int32_3d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_int32_3d
module subroutine extract_var_int64_3d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  integer(int64), pointer, intent(out) :: raw(:,:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (integer(int64))
      raw(1:s(1),1:s(2),1:s(3)) => vals
    class default
      error stop "[extract_var_int64_3d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_int64_3d
module subroutine extract_var_real32_3d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  real(real32), pointer, intent(out) :: raw(:,:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (real(real32))
      raw(1:s(1),1:s(2),1:s(3)) => vals
    class default
      error stop "[extract_var_real32_3d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_real32_3d
module subroutine extract_var_real64_3d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  real(real64), pointer, intent(out) :: raw(:,:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (real(real64))
      raw(1:s(1),1:s(2),1:s(3)) => vals
    class default
      error stop "[extract_var_real64_3d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_real64_3d
module subroutine extract_var_int8_4d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  integer(int8), pointer, intent(out) :: raw(:,:,:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (integer(int8))
      raw(1:s(1),1:s(2),1:s(3),1:s(4)) => vals
    class default
      error stop "[extract_var_int8_4d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_int8_4d
module subroutine extract_var_int16_4d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  integer(int16), pointer, intent(out) :: raw(:,:,:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (integer(int16))
      raw(1:s(1),1:s(2),1:s(3),1:s(4)) => vals
    class default
      error stop "[extract_var_int16_4d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_int16_4d
module subroutine extract_var_int32_4d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  integer(int32), pointer, intent(out) :: raw(:,:,:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (integer(int32))
      raw(1:s(1),1:s(2),1:s(3),1:s(4)) => vals
    class default
      error stop "[extract_var_int32_4d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_int32_4d
module subroutine extract_var_int64_4d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  integer(int64), pointer, intent(out) :: raw(:,:,:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (integer(int64))
      raw(1:s(1),1:s(2),1:s(3),1:s(4)) => vals
    class default
      error stop "[extract_var_int64_4d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_int64_4d
module subroutine extract_var_real32_4d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  real(real32), pointer, intent(out) :: raw(:,:,:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (real(real32))
      raw(1:s(1),1:s(2),1:s(3),1:s(4)) => vals
    class default
      error stop "[extract_var_real32_4d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_real32_4d
module subroutine extract_var_real64_4d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  real(real64), pointer, intent(out) :: raw(:,:,:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (real(real64))
      raw(1:s(1),1:s(2),1:s(3),1:s(4)) => vals
    class default
      error stop "[extract_var_real64_4d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_real64_4d
module subroutine extract_var_int8_5d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  integer(int8), pointer, intent(out) :: raw(:,:,:,:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (integer(int8))
      raw(1:s(1),1:s(2),1:s(3),1:s(4),1:s(5)) => vals
    class default
      error stop "[extract_var_int8_5d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_int8_5d
module subroutine extract_var_int16_5d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  integer(int16), pointer, intent(out) :: raw(:,:,:,:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (integer(int16))
      raw(1:s(1),1:s(2),1:s(3),1:s(4),1:s(5)) => vals
    class default
      error stop "[extract_var_int16_5d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_int16_5d
module subroutine extract_var_int32_5d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  integer(int32), pointer, intent(out) :: raw(:,:,:,:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (integer(int32))
      raw(1:s(1),1:s(2),1:s(3),1:s(4),1:s(5)) => vals
    class default
      error stop "[extract_var_int32_5d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_int32_5d
module subroutine extract_var_int64_5d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  integer(int64), pointer, intent(out) :: raw(:,:,:,:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (integer(int64))
      raw(1:s(1),1:s(2),1:s(3),1:s(4),1:s(5)) => vals
    class default
      error stop "[extract_var_int64_5d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_int64_5d
module subroutine extract_var_real32_5d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  real(real32), pointer, intent(out) :: raw(:,:,:,:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (real(real32))
      raw(1:s(1),1:s(2),1:s(3),1:s(4),1:s(5)) => vals
    class default
      error stop "[extract_var_real32_5d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_real32_5d
module subroutine extract_var_real64_5d(var, raw)
  type(netcdf_variable), target, intent(in) :: var
  real(real64), pointer, intent(out) :: raw(:,:,:,:,:)

  associate (s => shape(var))
    select type (vals => var%vals)
    type is (real(real64))
      raw(1:s(1),1:s(2),1:s(3),1:s(4),1:s(5)) => vals
    class default
      error stop "[extract_var_real64_5d] Invalid pointer type."
    end select
  end associate
end subroutine extract_var_real64_5d

end submodule submodule_variable
