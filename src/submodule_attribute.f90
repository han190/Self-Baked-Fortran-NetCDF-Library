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

module function new_atts(atts) result(ret)
  type(attribute_type), intent(in) :: atts(:)
  type(attribute_type), allocatable :: ret(:)
  integer :: i

  ret = atts
  do i = 1, size(ret)
    ret(i)%att%ID = i - 1
  end do
end function new_atts

end submodule submodule_attribute
