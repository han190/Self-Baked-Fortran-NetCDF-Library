submodule(module_interface) submodule_attribute
implicit none
contains

!> integer(int8) attribute constructor
module pure function new_att_vec_int8(name, vals) result(att)
  character(*), intent(in) :: name
  integer(int8), intent(in) :: vals(:)
  type(attribute_int8_type) :: tmp
  type(attribute_type) :: att

  tmp%len = size(vals)
  tmp%name = name
  tmp%type = nc_byte
  tmp%vals = vals
  att%att = tmp
end function new_att_vec_int8

!> integer(int16) attribute constructor
module pure function new_att_vec_int16(name, vals) result(att)
  character(*), intent(in) :: name
  integer(int16), intent(in) :: vals(:)
  type(attribute_int16_type) :: tmp
  type(attribute_type) :: att

  tmp%len = size(vals)
  tmp%name = name
  tmp%type = nc_short
  tmp%vals = vals
  att%att = tmp
end function new_att_vec_int16

!> integer(int32) attribute constructor
module pure function new_att_vec_int32(name, vals) result(att)
  character(*), intent(in) :: name
  integer(int32), intent(in) :: vals(:)
  type(attribute_int32_type) :: tmp
  type(attribute_type) :: att

  tmp%len = size(vals)
  tmp%name = name
  tmp%type = nc_int
  tmp%vals = vals
  att%att = tmp
end function new_att_vec_int32

!> integer(int64) attribute constructor
module pure function new_att_vec_int64(name, vals) result(att)
  character(*), intent(in) :: name
  integer(int64), intent(in) :: vals(:)
  type(attribute_int64_type) :: tmp
  type(attribute_type) :: att

  tmp%len = size(vals)
  tmp%name = name
  tmp%type = nc_int64
  tmp%vals = vals
  att%att = tmp
end function new_att_vec_int64

!> real(real32) attribute constructor
module pure function new_att_vec_real32(name, vals) result(att)
  character(*), intent(in) :: name
  real(real32), intent(in) :: vals(:)
  type(attribute_real32_type) :: tmp
  type(attribute_type) :: att

  tmp%len = size(vals)
  tmp%name = name
  tmp%type = nc_float
  tmp%vals = vals
  att%att = tmp
end function new_att_vec_real32

!> real(real64) attribute constructor
module pure function new_att_vec_real64(name, vals) result(att)
  character(*), intent(in) :: name
  real(real64), intent(in) :: vals(:)
  type(attribute_real64_type) :: tmp
  type(attribute_type) :: att

  tmp%len = size(vals)
  tmp%name = name
  tmp%type = nc_double
  tmp%vals = vals
  att%att = tmp
end function new_att_vec_real64

!> integer(int8) attribute constructor
module pure function new_att_scal_int8(name, val) result(att)
  character(*), intent(in) :: name
  integer(int8), intent(in) :: val
  type(attribute_int8_type) :: tmp
  type(attribute_type) :: att

  tmp%len = 1
  tmp%name = name
  tmp%type = nc_byte
  tmp%vals = [val]
  att%att = tmp
end function new_att_scal_int8

!> integer(int16) attribute constructor
module pure function new_att_scal_int16(name, val) result(att)
  character(*), intent(in) :: name
  integer(int16), intent(in) :: val
  type(attribute_int16_type) :: tmp
  type(attribute_type) :: att

  tmp%len = 1
  tmp%name = name
  tmp%type = nc_short
  tmp%vals = [val]
  att%att = tmp
end function new_att_scal_int16

!> integer(int32) attribute constructor
module pure function new_att_scal_int32(name, val) result(att)
  character(*), intent(in) :: name
  integer(int32), intent(in) :: val
  type(attribute_int32_type) :: tmp
  type(attribute_type) :: att

  tmp%len = 1
  tmp%name = name
  tmp%type = nc_int
  tmp%vals = [val]
  att%att = tmp
end function new_att_scal_int32

!> integer(int64) attribute constructor
module pure function new_att_scal_int64(name, val) result(att)
  character(*), intent(in) :: name
  integer(int64), intent(in) :: val
  type(attribute_int64_type) :: tmp
  type(attribute_type) :: att

  tmp%len = 1
  tmp%name = name
  tmp%type = nc_int64
  tmp%vals = [val]
  att%att = tmp
end function new_att_scal_int64

!> real(real32) attribute constructor
module pure function new_att_scal_real32(name, val) result(att)
  character(*), intent(in) :: name
  real(real32), intent(in) :: val
  type(attribute_real32_type) :: tmp
  type(attribute_type) :: att

  tmp%len = 1
  tmp%name = name
  tmp%type = nc_float
  tmp%vals = [val]
  att%att = tmp
end function new_att_scal_real32

!> real(real64) attribute constructor
module pure function new_att_scal_real64(name, val) result(att)
  character(*), intent(in) :: name
  real(real64), intent(in) :: val
  type(attribute_real64_type) :: tmp
  type(attribute_type) :: att

  tmp%len = 1
  tmp%name = name
  tmp%type = nc_double
  tmp%vals = [val]
  att%att = tmp
end function new_att_scal_real64

module pure function new_att_scal_char(name, val) result(att)
  character(len=*), intent(in) :: name, val
  type(attribute_char_type) :: tmp
  type(attribute_type) :: att

  tmp%len = len(val)
  tmp%name = name
  tmp%type = nc_char
  tmp%vals = [val]
  att%att = tmp
end function new_att_scal_char

!> Attributes contructor
module function new_atts(atts) result(ret)
  type(attribute_type), intent(in) :: atts(:)
  type(attribute_type), allocatable :: ret(:)
  integer :: i

  ret = atts
  do i = 1, size(ret)
    ret(i)%att%ID = i - 1
  end do
end function new_atts

!> Put attribute (internal)
subroutine put_att_(ncid, varid, att)
  integer(c_int), intent(in) :: ncid, varid
  type(attribute_type) :: att
  integer(c_int) :: stat

  select type (att_ => att%att)
  type is (attribute_int16_type)

    stat = nc_put_att_short( &
      & ncid, varid, att_%name, &
      & att_%type, att_%len, att_%vals)
    call handle_error(stat, "nc_put_att_short")

  type is (attribute_int32_type)

    stat = nc_put_att_int( &
      & ncid, varid, att_%name, &
      & att_%type, att_%len, att_%vals)
    call handle_error(stat, "nc_put_att_int")

  type is (attribute_int64_type)

    stat = nc_put_att_longlong( &
      & ncid, varid, att_%name, &
      & att_%type, att_%len, att_%vals)
    call handle_error(stat, "nc_put_att_longlong")

  type is (attribute_real32_type)

    stat = nc_put_att_float( &
      & ncid, varid, att_%name, &
      & att_%type, att_%len, att_%vals)
    call handle_error(stat, "nc_put_att_float")

  type is (attribute_real64_type)

    stat = nc_put_att_double( &
      & ncid, varid, att_%name, &
      & att_%type, att_%len, att_%vals)
    call handle_error(stat, "nc_put_att_double")

  type is (attribute_char_type)

    stat = nc_put_att_text( &
      & ncid, varid, att_%name, &
      & len(att_%vals(1), c_size_t), att_%vals(1))
    call handle_error(stat, "nc_put_att_text")

  end select
end subroutine put_att_

!> Put attributes (internal)
subroutine put_atts_(ncid, varid, atts)
  integer(c_int), intent(in) :: ncid, varid
  type(attribute_type), intent(in) :: atts(:)
  integer :: i

  do i = 1, size(atts)
    call put_att_(ncid, varid, atts(i))
  end do
end subroutine put_atts_

!> Put group attributes
module subroutine put_grp_atts(grp)
  class(group_type), intent(in) :: grp

  if (allocated(grp%atts)) &
    & call put_atts_(grp%ID, nc_global, grp%atts)
end subroutine put_grp_atts

!> Put variable attributes
module subroutine put_var_atts(var)
  type(variable_type), intent(in) :: var

  associate (var_ => var%var)
    if (allocated(var_%atts)) &
      & call put_atts_(var_%grpID, var_%ID, var_%atts)
  end associate
end subroutine put_var_atts

end submodule submodule_attribute
