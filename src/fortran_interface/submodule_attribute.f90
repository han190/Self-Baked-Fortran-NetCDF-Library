submodule(module_fortran_interface) submodule_attribute
implicit none
contains

module pure function new_att_int8_0d(name, vals) result(ret)
  character(len=*), intent(in) :: name
  integer(int8), intent(in) :: vals
  type(netcdf_attribute) :: ret

  ret%name = name
  ret%type = nc_byte
  if (associated(ret%vals)) deallocate (ret%vals)
  allocate (ret%vals, source=[vals])
  ret%len = 1
end function new_att_int8_0d

module pure function new_att_int16_0d(name, vals) result(ret)
  character(len=*), intent(in) :: name
  integer(int16), intent(in) :: vals
  type(netcdf_attribute) :: ret

  ret%name = name
  ret%type = nc_short
  if (associated(ret%vals)) deallocate (ret%vals)
  allocate (ret%vals, source=[vals])
  ret%len = 1
end function new_att_int16_0d

module pure function new_att_int32_0d(name, vals) result(ret)
  character(len=*), intent(in) :: name
  integer(int32), intent(in) :: vals
  type(netcdf_attribute) :: ret

  ret%name = name
  ret%type = nc_int
  if (associated(ret%vals)) deallocate (ret%vals)
  allocate (ret%vals, source=[vals])
  ret%len = 1
end function new_att_int32_0d

module pure function new_att_int64_0d(name, vals) result(ret)
  character(len=*), intent(in) :: name
  integer(int64), intent(in) :: vals
  type(netcdf_attribute) :: ret

  ret%name = name
  ret%type = nc_int64
  if (associated(ret%vals)) deallocate (ret%vals)
  allocate (ret%vals, source=[vals])
  ret%len = 1
end function new_att_int64_0d

module pure function new_att_real32_0d(name, vals) result(ret)
  character(len=*), intent(in) :: name
  real(real32), intent(in) :: vals
  type(netcdf_attribute) :: ret

  ret%name = name
  ret%type = nc_float
  if (associated(ret%vals)) deallocate (ret%vals)
  allocate (ret%vals, source=[vals])
  ret%len = 1
end function new_att_real32_0d

module pure function new_att_real64_0d(name, vals) result(ret)
  character(len=*), intent(in) :: name
  real(real64), intent(in) :: vals
  type(netcdf_attribute) :: ret

  ret%name = name
  ret%type = nc_double
  if (associated(ret%vals)) deallocate (ret%vals)
  allocate (ret%vals, source=[vals])
  ret%len = 1
end function new_att_real64_0d

module pure function new_att_int8_1d(name, vals) result(ret)
  character(len=*), intent(in) :: name
  integer(int8), intent(in) :: vals(:)
  type(netcdf_attribute) :: ret

  ret%name = name
  ret%type = nc_byte
  if (associated(ret%vals)) deallocate (ret%vals)
  allocate (ret%vals, source=vals)
  ret%len = size(vals)
end function new_att_int8_1d

module pure function new_att_int16_1d(name, vals) result(ret)
  character(len=*), intent(in) :: name
  integer(int16), intent(in) :: vals(:)
  type(netcdf_attribute) :: ret

  ret%name = name
  ret%type = nc_short
  if (associated(ret%vals)) deallocate (ret%vals)
  allocate (ret%vals, source=vals)
  ret%len = size(vals)
end function new_att_int16_1d

module pure function new_att_int32_1d(name, vals) result(ret)
  character(len=*), intent(in) :: name
  integer(int32), intent(in) :: vals(:)
  type(netcdf_attribute) :: ret

  ret%name = name
  ret%type = nc_int
  if (associated(ret%vals)) deallocate (ret%vals)
  allocate (ret%vals, source=vals)
  ret%len = size(vals)
end function new_att_int32_1d

module pure function new_att_int64_1d(name, vals) result(ret)
  character(len=*), intent(in) :: name
  integer(int64), intent(in) :: vals(:)
  type(netcdf_attribute) :: ret

  ret%name = name
  ret%type = nc_int64
  if (associated(ret%vals)) deallocate (ret%vals)
  allocate (ret%vals, source=vals)
  ret%len = size(vals)
end function new_att_int64_1d

module pure function new_att_real32_1d(name, vals) result(ret)
  character(len=*), intent(in) :: name
  real(real32), intent(in) :: vals(:)
  type(netcdf_attribute) :: ret

  ret%name = name
  ret%type = nc_float
  if (associated(ret%vals)) deallocate (ret%vals)
  allocate (ret%vals, source=vals)
  ret%len = size(vals)
end function new_att_real32_1d

module pure function new_att_real64_1d(name, vals) result(ret)
  character(len=*), intent(in) :: name
  real(real64), intent(in) :: vals(:)
  type(netcdf_attribute) :: ret

  ret%name = name
  ret%type = nc_double
  if (associated(ret%vals)) deallocate (ret%vals)
  allocate (ret%vals, source=vals)
  ret%len = size(vals)
end function new_att_real64_1d


module pure function new_att_char_0d(name, vals) result(ret)
  character(len=*), intent(in) :: name
  character(len=*), intent(in) :: vals
  type(netcdf_attribute) :: ret

  ret%name = name
  ret%type = nc_char
  if (associated(ret%vals)) deallocate (ret%vals)
  allocate (ret%vals, source=[vals])
  ret%len = 1
end function new_att_char_0d

module pure function new_att_char_1d(name, vals) result(ret)
  character(len=*), intent(in) :: name
  character(len=*), intent(in) :: vals(:)
  type(netcdf_attribute) :: ret

  ret%name = name
  ret%type = nc_char
  if (associated(ret%vals)) deallocate (ret%vals)
  allocate (ret%vals, source=vals)
  ret%len = size(vals)
end function new_att_char_1d


module function new_atts(atts) result(ret)
  type(netcdf_attribute), intent(in) :: atts(:)
  type(netcdf_attribute), allocatable :: ret(:)
  integer :: i

  ret = atts
  do i = 1, size(ret)
    ret(i)%ID = i - 1
  end do
end function new_atts

subroutine put_att_(ncid, varid, att)
  integer(c_int), intent(in) :: ncid, varid
  type(netcdf_attribute) :: att
  integer(c_int) :: stat

  select type (vals_ => att%vals)
  type is (integer (int8))
    stat = nc_put_att_ubyte ( &
      & ncid, varid, cstr(att%name), &
      & att%type, att%len, vals_)
    call handle_error(stat, "nc_put_att_ubyte")
  type is (integer (int16))
    stat = nc_put_att_short ( &
      & ncid, varid, cstr(att%name), &
      & att%type, att%len, vals_)
    call handle_error(stat, "nc_put_att_short")
  type is (integer (int32))
    stat = nc_put_att_int ( &
      & ncid, varid, cstr(att%name), &
      & att%type, att%len, vals_)
    call handle_error(stat, "nc_put_att_int")
  type is (integer (int64))
    stat = nc_put_att_longlong ( &
      & ncid, varid, cstr(att%name), &
      & att%type, att%len, vals_)
    call handle_error(stat, "nc_put_att_longlong")
  type is (real (real32))
    stat = nc_put_att_float ( &
      & ncid, varid, cstr(att%name), &
      & att%type, att%len, vals_)
    call handle_error(stat, "nc_put_att_float")
  type is (real (real64))
    stat = nc_put_att_double ( &
      & ncid, varid, cstr(att%name), &
      & att%type, att%len, vals_)
    call handle_error(stat, "nc_put_att_double")
  type is (character(*))
    stat = nc_put_att_text( &
      & ncid, varid, cstr(att%name), &
      & len(vals_, c_size_t), vals_)
    call handle_error(stat, "nc_put_att_text")
  end select
end subroutine put_att_

subroutine put_atts_(ncid, varid, atts)
  integer(c_int), intent(in) :: ncid, varid
  type(netcdf_attribute), intent(in) :: atts(:)
  integer :: i

  do i = 1, size(atts)
    call put_att_(ncid, varid, atts(i))
  end do
end subroutine put_atts_

module subroutine put_grp_atts(grp)
  class(netcdf_group), intent(in) :: grp

  if (allocated(grp%atts)) &
    & call put_atts_(grp%ID, nc_global, grp%atts)
end subroutine put_grp_atts

module subroutine put_var_atts(var)
  type(netcdf_variable), intent(in) :: var

  if (allocated(var%atts)) &
    & call put_atts_(var%grpID, var%ID, var%atts)
end subroutine put_var_atts

function inq_atts_(ncid, varid, natts) result(atts)
  integer(c_int), intent(in) :: ncid, varid, natts
  type(netcdf_attribute), allocatable :: atts(:)
  integer(c_int) :: i, stat
  character(kind=c_char, len=nc_max_name) :: tmp

  if (allocated(atts)) deallocate (atts)
  allocate (atts(natts))

  do i = 1, natts
    associate (att => atts(i))
      att%ID = i - 1
      stat = nc_inq_attname(ncid, varid, att%ID, tmp)
      call handle_error(stat, "nc_inq_attname")
      att%name = cstrip(tmp)

      stat = nc_inq_att(ncid, varid, tmp, att%type, att%len)
      call handle_error(stat, "nc_inq_att")
    end associate
  end do
end function inq_atts_

module subroutine inq_grp_atts(grp)
  class(netcdf_group), intent(inout) :: grp
  integer(c_int) :: stat, natts

  stat = nc_inq_natts(grp%ID, natts)
  call handle_error(stat, "nc_inq_natts")
  grp%atts = inq_atts_(grp%ID, nc_global, natts)
end subroutine inq_grp_atts

module subroutine inq_var_atts(var)
  type(netcdf_variable), intent(inout) :: var
  integer(c_int) :: stat, natts

  if (.not. associated(var%grpID)) &
    & error stop "[inq_var_atts] Group ID not associated."

  stat = nc_inq_varnatts(var%grpID, var%ID, natts)
  call handle_error(stat, "nc_inq_varnatts")
  var%atts = inq_atts_(var%grpID, var%ID, natts)
end subroutine inq_var_atts

subroutine get_atts_(ncid, varid, atts)
  integer(c_int), intent(in) :: ncid, varid
  type(netcdf_attribute), intent(inout) :: atts(:)
  integer(c_int) :: stat, i

  do i = 1, size(atts)
    associate (att => atts(i))
      if (associated(att%vals)) deallocate (att%vals)
      select case (att%type)
      case (nc_byte)
        allocate (integer(int8) :: att%vals(att%len))
        select type (vals_ => att%vals)
        type is (integer(int8))
          stat = nc_get_att_ubyte(ncid, varid, cstr(att%name), vals_)
          call handle_error(stat, "nc_get_att_ubyte")
        end select
      case (nc_short)
        allocate (integer(int16) :: att%vals(att%len))
        select type (vals_ => att%vals)
        type is (integer(int16))
          stat = nc_get_att_short(ncid, varid, cstr(att%name), vals_)
          call handle_error(stat, "nc_get_att_short")
        end select
      case (nc_int)
        allocate (integer(int32) :: att%vals(att%len))
        select type (vals_ => att%vals)
        type is (integer(int32))
          stat = nc_get_att_int(ncid, varid, cstr(att%name), vals_)
          call handle_error(stat, "nc_get_att_int")
        end select
      case (nc_int64)
        allocate (integer(int64) :: att%vals(att%len))
        select type (vals_ => att%vals)
        type is (integer(int64))
          stat = nc_get_att_longlong(ncid, varid, cstr(att%name), vals_)
          call handle_error(stat, "nc_get_att_longlong")
        end select
      case (nc_float)
        allocate (real(real32) :: att%vals(att%len))
        select type (vals_ => att%vals)
        type is (real(real32))
          stat = nc_get_att_float(ncid, varid, cstr(att%name), vals_)
          call handle_error(stat, "nc_get_att_float")
        end select
      case (nc_double)
        allocate (real(real64) :: att%vals(att%len))
        select type (vals_ => att%vals)
        type is (real(real64))
          stat = nc_get_att_double(ncid, varid, cstr(att%name), vals_)
          call handle_error(stat, "nc_get_att_double")
        end select
      case (nc_char)
        if (att%len > nc_max_char) then
          allocate (character(kind=c_char, len=nc_max_char) :: &
            & att%vals(att%len/nc_max_char + 1))
        else
          allocate (character(kind=c_char, len=att%len) :: att%vals(1))
        end if
        select type (vals_ => att%vals)
        type is (character(kind=c_char, len=*))
          stat = nc_get_att_text(ncid, varid, cstr(att%name), vals_)
          call handle_error(stat, "nc_get_att_text")
        end select
      end select
    end associate
  end do
end subroutine get_atts_

module subroutine get_var_atts(var)
  type(netcdf_variable), intent(inout) :: var

  if (.not. associated(var%grpID)) &
    & error stop "[inq_var_atts] Group ID not associated."
  call get_atts_(var%grpID, var%ID, var%atts)
end subroutine get_var_atts

module subroutine extract_att_int8(var, name, raw)
  type(netcdf_variable), target, intent(in) :: var
  character(len=*), intent(in) :: name
  integer(int8), pointer, intent(out) :: raw(:)
  integer :: i

  do i = 1, size(var%atts)
    if (name == var%atts(i)%name) then
      select type (vals => var%atts(i)%vals)
      type is (integer(int8))
        raw(1:size(vals)) => vals
        return
      class default
        error stop "[extract_att_int8] Invalid attribute pointer."
      end select
    end if
  end do
end subroutine extract_att_int8

module subroutine extract_att_int16(var, name, raw)
  type(netcdf_variable), target, intent(in) :: var
  character(len=*), intent(in) :: name
  integer(int16), pointer, intent(out) :: raw(:)
  integer :: i

  do i = 1, size(var%atts)
    if (name == var%atts(i)%name) then
      select type (vals => var%atts(i)%vals)
      type is (integer(int16))
        raw(1:size(vals)) => vals
        return
      class default
        error stop "[extract_att_int16] Invalid attribute pointer."
      end select
    end if
  end do
end subroutine extract_att_int16

module subroutine extract_att_int32(var, name, raw)
  type(netcdf_variable), target, intent(in) :: var
  character(len=*), intent(in) :: name
  integer(int32), pointer, intent(out) :: raw(:)
  integer :: i

  do i = 1, size(var%atts)
    if (name == var%atts(i)%name) then
      select type (vals => var%atts(i)%vals)
      type is (integer(int32))
        raw(1:size(vals)) => vals
        return
      class default
        error stop "[extract_att_int32] Invalid attribute pointer."
      end select
    end if
  end do
end subroutine extract_att_int32

module subroutine extract_att_int64(var, name, raw)
  type(netcdf_variable), target, intent(in) :: var
  character(len=*), intent(in) :: name
  integer(int64), pointer, intent(out) :: raw(:)
  integer :: i

  do i = 1, size(var%atts)
    if (name == var%atts(i)%name) then
      select type (vals => var%atts(i)%vals)
      type is (integer(int64))
        raw(1:size(vals)) => vals
        return
      class default
        error stop "[extract_att_int64] Invalid attribute pointer."
      end select
    end if
  end do
end subroutine extract_att_int64

module subroutine extract_att_real32(var, name, raw)
  type(netcdf_variable), target, intent(in) :: var
  character(len=*), intent(in) :: name
  real(real32), pointer, intent(out) :: raw(:)
  integer :: i

  do i = 1, size(var%atts)
    if (name == var%atts(i)%name) then
      select type (vals => var%atts(i)%vals)
      type is (real(real32))
        raw(1:size(vals)) => vals
        return
      class default
        error stop "[extract_att_real32] Invalid attribute pointer."
      end select
    end if
  end do
end subroutine extract_att_real32

module subroutine extract_att_real64(var, name, raw)
  type(netcdf_variable), target, intent(in) :: var
  character(len=*), intent(in) :: name
  real(real64), pointer, intent(out) :: raw(:)
  integer :: i

  do i = 1, size(var%atts)
    if (name == var%atts(i)%name) then
      select type (vals => var%atts(i)%vals)
      type is (real(real64))
        raw(1:size(vals)) => vals
        return
      class default
        error stop "[extract_att_real64] Invalid attribute pointer."
      end select
    end if
  end do
end subroutine extract_att_real64


end submodule submodule_attribute
