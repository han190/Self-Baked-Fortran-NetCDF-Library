submodule(module_fortran_interface) submodule_attribute
implicit none
contains

module pure function new_att_vec(name, vals) result(ret)
  character(len=*), intent(in) :: name
  class(*), intent(in) :: vals(:)
  type(nc_att) :: ret

  ret%name = name
  select type (vals_ => vals)
  type is (integer(int8))
    ret%type = nc_byte
    if (allocated(ret%vals)) deallocate (ret%vals)
    allocate (ret%vals, source=vals_)
    ret%len = size(vals_)
  type is (integer(int16))
    ret%type = nc_short
    if (allocated(ret%vals)) deallocate (ret%vals)
    allocate (ret%vals, source=vals_)
    ret%len = size(vals_)
  type is (integer(int32))
    ret%type = nc_int
    if (allocated(ret%vals)) deallocate (ret%vals)
    allocate (ret%vals, source=vals_)
    ret%len = size(vals_)
  type is (integer(int64))
    ret%type = nc_int64
    if (allocated(ret%vals)) deallocate (ret%vals)
    allocate (ret%vals, source=vals_)
    ret%len = size(vals_)
  type is (real(real32))
    ret%type = nc_float
    if (allocated(ret%vals)) deallocate (ret%vals)
    allocate (ret%vals, source=vals_)
    ret%len = size(vals_)
  type is (real(real64))
    ret%type = nc_double
    if (allocated(ret%vals)) deallocate (ret%vals)
    allocate (ret%vals, source=vals_)
    ret%len = size(vals_)
  type is (character(*))
    ret%type = nc_char
    if (allocated(ret%vals)) deallocate (ret%vals)
    allocate (ret%vals, source=vals_)
    ret%len = size(vals_)
  end select
end function new_att_vec

module pure function new_att_scal(name, val) result(ret)
  character(len=*), intent(in) :: name
  class(*), intent(in) :: val
  type(nc_att) :: ret

  ret%name = name
  select type (val_ => val)
  type is (integer(int8))
    ret%type = nc_byte
    if (allocated(ret%vals)) deallocate (ret%vals)
    allocate (ret%vals, source=[val_])
    ret%len = 1
  type is (integer(int16))
    ret%type = nc_short
    if (allocated(ret%vals)) deallocate (ret%vals)
    allocate (ret%vals, source=[val_])
    ret%len = 1
  type is (integer(int32))
    ret%type = nc_int
    if (allocated(ret%vals)) deallocate (ret%vals)
    allocate (ret%vals, source=[val_])
    ret%len = 1
  type is (integer(int64))
    ret%type = nc_int64
    if (allocated(ret%vals)) deallocate (ret%vals)
    allocate (ret%vals, source=[val_])
    ret%len = 1
  type is (real(real32))
    ret%type = nc_float
    if (allocated(ret%vals)) deallocate (ret%vals)
    allocate (ret%vals, source=[val_])
    ret%len = 1
  type is (real(real64))
    ret%type = nc_double
    if (allocated(ret%vals)) deallocate (ret%vals)
    allocate (ret%vals, source=[val_])
    ret%len = 1
  type is (character(*))
    ret%type = nc_char
    if (allocated(ret%vals)) deallocate (ret%vals)
    allocate (ret%vals, source=[val_])
    ret%len = 1
  end select
end function new_att_scal

module pure function new_atts(atts) result(ret)
  type(nc_att), intent(in) :: atts(:)
  type(nc_att), allocatable :: ret(:)
  integer :: i

  ret = atts
  do i = 1, size(ret)
    ret(i)%ID = i - 1
  end do
end function new_atts

subroutine put_att_(ncid, varid, att)
  integer(c_int), intent(in) :: ncid, varid
  type(nc_att) :: att
  integer(c_int) :: stat

  select type (vals_ => att%vals)
  type is (integer(int8))
    stat = nc_put_att_ubyte( &
      & ncid, varid, cstr(att%name), &
      & att%type, att%len, vals_)
    call handle_error(stat, "nc_put_att_ubyte")
  type is (integer(int16))
    stat = nc_put_att_short( &
      & ncid, varid, cstr(att%name), &
      & att%type, att%len, vals_)
    call handle_error(stat, "nc_put_att_short")
  type is (integer(int32))
    stat = nc_put_att_int( &
      & ncid, varid, cstr(att%name), &
      & att%type, att%len, vals_)
    call handle_error(stat, "nc_put_att_int")
  type is (integer(int64))
    stat = nc_put_att_longlong( &
      & ncid, varid, cstr(att%name), &
      & att%type, att%len, vals_)
    call handle_error(stat, "nc_put_att_longlong")
  type is (real(real32))
    stat = nc_put_att_float( &
      & ncid, varid, cstr(att%name), &
      & att%type, att%len, vals_)
    call handle_error(stat, "nc_put_att_float")
  type is (real(real64))
    stat = nc_put_att_double( &
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
  type(nc_att), intent(in) :: atts(:)
  integer :: i

  do i = 1, size(atts)
    call put_att_(ncid, varid, atts(i))
  end do
end subroutine put_atts_

module subroutine put_grp_atts(grp)
  class(nc_grp), intent(in) :: grp

  if (allocated(grp%atts)) &
    & call put_atts_(grp%ID, nc_global, grp%atts)
end subroutine put_grp_atts

module subroutine put_var_atts(var)
  type(nc_var), intent(in) :: var

  if (allocated(var%atts)) &
    & call put_atts_(var%grpID, var%ID, var%atts)
end subroutine put_var_atts

end submodule submodule_attribute