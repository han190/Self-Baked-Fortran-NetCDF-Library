submodule(module_interface) submodule_variable
implicit none
contains

!> variable constructor for integer(int8), rank(0)
module function new_variable_int8_1d( &
  & data, name, dims, atts) result(var)
  integer(int8), target, contiguous, intent(in) :: data(:)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int8_type) :: tmp
  integer(int8), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_byte
  var%var = tmp
  nullify (ptr)
end function new_variable_int8_1d

!> variable constructor for integer(int8), rank(0)
module function new_variable_noatt_int8_1d( &
  & data, name, dims) result(var)
  integer(int8), target, contiguous, intent(in) :: data(:)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int8_type) :: tmp
  integer(int8), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_byte
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int8_1d

!> variable constructor for integer(int16), rank(0)
module function new_variable_int16_1d( &
  & data, name, dims, atts) result(var)
  integer(int16), target, contiguous, intent(in) :: data(:)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int16_type) :: tmp
  integer(int16), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_short
  var%var = tmp
  nullify (ptr)
end function new_variable_int16_1d

!> variable constructor for integer(int16), rank(0)
module function new_variable_noatt_int16_1d( &
  & data, name, dims) result(var)
  integer(int16), target, contiguous, intent(in) :: data(:)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int16_type) :: tmp
  integer(int16), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_short
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int16_1d

!> variable constructor for integer(int32), rank(0)
module function new_variable_int32_1d( &
  & data, name, dims, atts) result(var)
  integer(int32), target, contiguous, intent(in) :: data(:)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int32_type) :: tmp
  integer(int32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_int
  var%var = tmp
  nullify (ptr)
end function new_variable_int32_1d

!> variable constructor for integer(int32), rank(0)
module function new_variable_noatt_int32_1d( &
  & data, name, dims) result(var)
  integer(int32), target, contiguous, intent(in) :: data(:)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int32_type) :: tmp
  integer(int32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_int
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int32_1d

!> variable constructor for integer(int64), rank(0)
module function new_variable_int64_1d( &
  & data, name, dims, atts) result(var)
  integer(int64), target, contiguous, intent(in) :: data(:)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int64_type) :: tmp
  integer(int64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_int64
  var%var = tmp
  nullify (ptr)
end function new_variable_int64_1d

!> variable constructor for integer(int64), rank(0)
module function new_variable_noatt_int64_1d( &
  & data, name, dims) result(var)
  integer(int64), target, contiguous, intent(in) :: data(:)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int64_type) :: tmp
  integer(int64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_int64
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int64_1d

!> variable constructor for real(real32), rank(0)
module function new_variable_real32_1d( &
  & data, name, dims, atts) result(var)
  real(real32), target, contiguous, intent(in) :: data(:)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_real32_type) :: tmp
  real(real32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_float
  var%var = tmp
  nullify (ptr)
end function new_variable_real32_1d

!> variable constructor for real(real32), rank(0)
module function new_variable_noatt_real32_1d( &
  & data, name, dims) result(var)
  real(real32), target, contiguous, intent(in) :: data(:)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_real32_type) :: tmp
  real(real32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_float
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_real32_1d

!> variable constructor for real(real64), rank(0)
module function new_variable_real64_1d( &
  & data, name, dims, atts) result(var)
  real(real64), target, contiguous, intent(in) :: data(:)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_real64_type) :: tmp
  real(real64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_double
  var%var = tmp
  nullify (ptr)
end function new_variable_real64_1d

!> variable constructor for real(real64), rank(0)
module function new_variable_noatt_real64_1d( &
  & data, name, dims) result(var)
  real(real64), target, contiguous, intent(in) :: data(:)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_real64_type) :: tmp
  real(real64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_double
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_real64_1d

!> variable constructor for integer(int8), rank(1)
module function new_variable_int8_2d( &
  & data, name, dims, atts) result(var)
  integer(int8), target, contiguous, intent(in) :: data(:, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int8_type) :: tmp
  integer(int8), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_byte
  var%var = tmp
  nullify (ptr)
end function new_variable_int8_2d

!> variable constructor for integer(int8), rank(1)
module function new_variable_noatt_int8_2d( &
  & data, name, dims) result(var)
  integer(int8), target, contiguous, intent(in) :: data(:, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int8_type) :: tmp
  integer(int8), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_byte
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int8_2d

!> variable constructor for integer(int16), rank(1)
module function new_variable_int16_2d( &
  & data, name, dims, atts) result(var)
  integer(int16), target, contiguous, intent(in) :: data(:, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int16_type) :: tmp
  integer(int16), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_short
  var%var = tmp
  nullify (ptr)
end function new_variable_int16_2d

!> variable constructor for integer(int16), rank(1)
module function new_variable_noatt_int16_2d( &
  & data, name, dims) result(var)
  integer(int16), target, contiguous, intent(in) :: data(:, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int16_type) :: tmp
  integer(int16), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_short
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int16_2d

!> variable constructor for integer(int32), rank(1)
module function new_variable_int32_2d( &
  & data, name, dims, atts) result(var)
  integer(int32), target, contiguous, intent(in) :: data(:, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int32_type) :: tmp
  integer(int32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_int
  var%var = tmp
  nullify (ptr)
end function new_variable_int32_2d

!> variable constructor for integer(int32), rank(1)
module function new_variable_noatt_int32_2d( &
  & data, name, dims) result(var)
  integer(int32), target, contiguous, intent(in) :: data(:, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int32_type) :: tmp
  integer(int32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_int
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int32_2d

!> variable constructor for integer(int64), rank(1)
module function new_variable_int64_2d( &
  & data, name, dims, atts) result(var)
  integer(int64), target, contiguous, intent(in) :: data(:, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int64_type) :: tmp
  integer(int64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_int64
  var%var = tmp
  nullify (ptr)
end function new_variable_int64_2d

!> variable constructor for integer(int64), rank(1)
module function new_variable_noatt_int64_2d( &
  & data, name, dims) result(var)
  integer(int64), target, contiguous, intent(in) :: data(:, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int64_type) :: tmp
  integer(int64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_int64
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int64_2d

!> variable constructor for real(real32), rank(1)
module function new_variable_real32_2d( &
  & data, name, dims, atts) result(var)
  real(real32), target, contiguous, intent(in) :: data(:, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_real32_type) :: tmp
  real(real32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_float
  var%var = tmp
  nullify (ptr)
end function new_variable_real32_2d

!> variable constructor for real(real32), rank(1)
module function new_variable_noatt_real32_2d( &
  & data, name, dims) result(var)
  real(real32), target, contiguous, intent(in) :: data(:, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_real32_type) :: tmp
  real(real32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_float
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_real32_2d

!> variable constructor for real(real64), rank(1)
module function new_variable_real64_2d( &
  & data, name, dims, atts) result(var)
  real(real64), target, contiguous, intent(in) :: data(:, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_real64_type) :: tmp
  real(real64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_double
  var%var = tmp
  nullify (ptr)
end function new_variable_real64_2d

!> variable constructor for real(real64), rank(1)
module function new_variable_noatt_real64_2d( &
  & data, name, dims) result(var)
  real(real64), target, contiguous, intent(in) :: data(:, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_real64_type) :: tmp
  real(real64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_double
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_real64_2d

!> variable constructor for integer(int8), rank(2)
module function new_variable_int8_3d( &
  & data, name, dims, atts) result(var)
  integer(int8), target, contiguous, intent(in) :: data(:, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int8_type) :: tmp
  integer(int8), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_byte
  var%var = tmp
  nullify (ptr)
end function new_variable_int8_3d

!> variable constructor for integer(int8), rank(2)
module function new_variable_noatt_int8_3d( &
  & data, name, dims) result(var)
  integer(int8), target, contiguous, intent(in) :: data(:, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int8_type) :: tmp
  integer(int8), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_byte
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int8_3d

!> variable constructor for integer(int16), rank(2)
module function new_variable_int16_3d( &
  & data, name, dims, atts) result(var)
  integer(int16), target, contiguous, intent(in) :: data(:, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int16_type) :: tmp
  integer(int16), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_short
  var%var = tmp
  nullify (ptr)
end function new_variable_int16_3d

!> variable constructor for integer(int16), rank(2)
module function new_variable_noatt_int16_3d( &
  & data, name, dims) result(var)
  integer(int16), target, contiguous, intent(in) :: data(:, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int16_type) :: tmp
  integer(int16), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_short
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int16_3d

!> variable constructor for integer(int32), rank(2)
module function new_variable_int32_3d( &
  & data, name, dims, atts) result(var)
  integer(int32), target, contiguous, intent(in) :: data(:, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int32_type) :: tmp
  integer(int32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_int
  var%var = tmp
  nullify (ptr)
end function new_variable_int32_3d

!> variable constructor for integer(int32), rank(2)
module function new_variable_noatt_int32_3d( &
  & data, name, dims) result(var)
  integer(int32), target, contiguous, intent(in) :: data(:, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int32_type) :: tmp
  integer(int32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_int
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int32_3d

!> variable constructor for integer(int64), rank(2)
module function new_variable_int64_3d( &
  & data, name, dims, atts) result(var)
  integer(int64), target, contiguous, intent(in) :: data(:, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int64_type) :: tmp
  integer(int64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_int64
  var%var = tmp
  nullify (ptr)
end function new_variable_int64_3d

!> variable constructor for integer(int64), rank(2)
module function new_variable_noatt_int64_3d( &
  & data, name, dims) result(var)
  integer(int64), target, contiguous, intent(in) :: data(:, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int64_type) :: tmp
  integer(int64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_int64
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int64_3d

!> variable constructor for real(real32), rank(2)
module function new_variable_real32_3d( &
  & data, name, dims, atts) result(var)
  real(real32), target, contiguous, intent(in) :: data(:, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_real32_type) :: tmp
  real(real32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_float
  var%var = tmp
  nullify (ptr)
end function new_variable_real32_3d

!> variable constructor for real(real32), rank(2)
module function new_variable_noatt_real32_3d( &
  & data, name, dims) result(var)
  real(real32), target, contiguous, intent(in) :: data(:, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_real32_type) :: tmp
  real(real32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_float
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_real32_3d

!> variable constructor for real(real64), rank(2)
module function new_variable_real64_3d( &
  & data, name, dims, atts) result(var)
  real(real64), target, contiguous, intent(in) :: data(:, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_real64_type) :: tmp
  real(real64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_double
  var%var = tmp
  nullify (ptr)
end function new_variable_real64_3d

!> variable constructor for real(real64), rank(2)
module function new_variable_noatt_real64_3d( &
  & data, name, dims) result(var)
  real(real64), target, contiguous, intent(in) :: data(:, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_real64_type) :: tmp
  real(real64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_double
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_real64_3d

!> variable constructor for integer(int8), rank(3)
module function new_variable_int8_4d( &
  & data, name, dims, atts) result(var)
  integer(int8), target, contiguous, intent(in) :: data(:, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int8_type) :: tmp
  integer(int8), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_byte
  var%var = tmp
  nullify (ptr)
end function new_variable_int8_4d

!> variable constructor for integer(int8), rank(3)
module function new_variable_noatt_int8_4d( &
  & data, name, dims) result(var)
  integer(int8), target, contiguous, intent(in) :: data(:, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int8_type) :: tmp
  integer(int8), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_byte
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int8_4d

!> variable constructor for integer(int16), rank(3)
module function new_variable_int16_4d( &
  & data, name, dims, atts) result(var)
  integer(int16), target, contiguous, intent(in) :: data(:, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int16_type) :: tmp
  integer(int16), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_short
  var%var = tmp
  nullify (ptr)
end function new_variable_int16_4d

!> variable constructor for integer(int16), rank(3)
module function new_variable_noatt_int16_4d( &
  & data, name, dims) result(var)
  integer(int16), target, contiguous, intent(in) :: data(:, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int16_type) :: tmp
  integer(int16), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_short
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int16_4d

!> variable constructor for integer(int32), rank(3)
module function new_variable_int32_4d( &
  & data, name, dims, atts) result(var)
  integer(int32), target, contiguous, intent(in) :: data(:, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int32_type) :: tmp
  integer(int32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_int
  var%var = tmp
  nullify (ptr)
end function new_variable_int32_4d

!> variable constructor for integer(int32), rank(3)
module function new_variable_noatt_int32_4d( &
  & data, name, dims) result(var)
  integer(int32), target, contiguous, intent(in) :: data(:, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int32_type) :: tmp
  integer(int32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_int
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int32_4d

!> variable constructor for integer(int64), rank(3)
module function new_variable_int64_4d( &
  & data, name, dims, atts) result(var)
  integer(int64), target, contiguous, intent(in) :: data(:, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int64_type) :: tmp
  integer(int64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_int64
  var%var = tmp
  nullify (ptr)
end function new_variable_int64_4d

!> variable constructor for integer(int64), rank(3)
module function new_variable_noatt_int64_4d( &
  & data, name, dims) result(var)
  integer(int64), target, contiguous, intent(in) :: data(:, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int64_type) :: tmp
  integer(int64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_int64
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int64_4d

!> variable constructor for real(real32), rank(3)
module function new_variable_real32_4d( &
  & data, name, dims, atts) result(var)
  real(real32), target, contiguous, intent(in) :: data(:, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_real32_type) :: tmp
  real(real32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_float
  var%var = tmp
  nullify (ptr)
end function new_variable_real32_4d

!> variable constructor for real(real32), rank(3)
module function new_variable_noatt_real32_4d( &
  & data, name, dims) result(var)
  real(real32), target, contiguous, intent(in) :: data(:, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_real32_type) :: tmp
  real(real32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_float
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_real32_4d

!> variable constructor for real(real64), rank(3)
module function new_variable_real64_4d( &
  & data, name, dims, atts) result(var)
  real(real64), target, contiguous, intent(in) :: data(:, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_real64_type) :: tmp
  real(real64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_double
  var%var = tmp
  nullify (ptr)
end function new_variable_real64_4d

!> variable constructor for real(real64), rank(3)
module function new_variable_noatt_real64_4d( &
  & data, name, dims) result(var)
  real(real64), target, contiguous, intent(in) :: data(:, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_real64_type) :: tmp
  real(real64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_double
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_real64_4d

!> variable constructor for integer(int8), rank(4)
module function new_variable_int8_5d( &
  & data, name, dims, atts) result(var)
  integer(int8), target, contiguous, intent(in) :: data(:, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int8_type) :: tmp
  integer(int8), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_byte
  var%var = tmp
  nullify (ptr)
end function new_variable_int8_5d

!> variable constructor for integer(int8), rank(4)
module function new_variable_noatt_int8_5d( &
  & data, name, dims) result(var)
  integer(int8), target, contiguous, intent(in) :: data(:, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int8_type) :: tmp
  integer(int8), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_byte
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int8_5d

!> variable constructor for integer(int16), rank(4)
module function new_variable_int16_5d( &
  & data, name, dims, atts) result(var)
  integer(int16), target, contiguous, intent(in) :: data(:, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int16_type) :: tmp
  integer(int16), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_short
  var%var = tmp
  nullify (ptr)
end function new_variable_int16_5d

!> variable constructor for integer(int16), rank(4)
module function new_variable_noatt_int16_5d( &
  & data, name, dims) result(var)
  integer(int16), target, contiguous, intent(in) :: data(:, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int16_type) :: tmp
  integer(int16), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_short
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int16_5d

!> variable constructor for integer(int32), rank(4)
module function new_variable_int32_5d( &
  & data, name, dims, atts) result(var)
  integer(int32), target, contiguous, intent(in) :: data(:, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int32_type) :: tmp
  integer(int32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_int
  var%var = tmp
  nullify (ptr)
end function new_variable_int32_5d

!> variable constructor for integer(int32), rank(4)
module function new_variable_noatt_int32_5d( &
  & data, name, dims) result(var)
  integer(int32), target, contiguous, intent(in) :: data(:, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int32_type) :: tmp
  integer(int32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_int
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int32_5d

!> variable constructor for integer(int64), rank(4)
module function new_variable_int64_5d( &
  & data, name, dims, atts) result(var)
  integer(int64), target, contiguous, intent(in) :: data(:, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int64_type) :: tmp
  integer(int64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_int64
  var%var = tmp
  nullify (ptr)
end function new_variable_int64_5d

!> variable constructor for integer(int64), rank(4)
module function new_variable_noatt_int64_5d( &
  & data, name, dims) result(var)
  integer(int64), target, contiguous, intent(in) :: data(:, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int64_type) :: tmp
  integer(int64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_int64
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int64_5d

!> variable constructor for real(real32), rank(4)
module function new_variable_real32_5d( &
  & data, name, dims, atts) result(var)
  real(real32), target, contiguous, intent(in) :: data(:, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_real32_type) :: tmp
  real(real32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_float
  var%var = tmp
  nullify (ptr)
end function new_variable_real32_5d

!> variable constructor for real(real32), rank(4)
module function new_variable_noatt_real32_5d( &
  & data, name, dims) result(var)
  real(real32), target, contiguous, intent(in) :: data(:, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_real32_type) :: tmp
  real(real32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_float
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_real32_5d

!> variable constructor for real(real64), rank(4)
module function new_variable_real64_5d( &
  & data, name, dims, atts) result(var)
  real(real64), target, contiguous, intent(in) :: data(:, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_real64_type) :: tmp
  real(real64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_double
  var%var = tmp
  nullify (ptr)
end function new_variable_real64_5d

!> variable constructor for real(real64), rank(4)
module function new_variable_noatt_real64_5d( &
  & data, name, dims) result(var)
  real(real64), target, contiguous, intent(in) :: data(:, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_real64_type) :: tmp
  real(real64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_double
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_real64_5d

!> variable constructor for integer(int8), rank(5)
module function new_variable_int8_6d( &
  & data, name, dims, atts) result(var)
  integer(int8), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int8_type) :: tmp
  integer(int8), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_byte
  var%var = tmp
  nullify (ptr)
end function new_variable_int8_6d

!> variable constructor for integer(int8), rank(5)
module function new_variable_noatt_int8_6d( &
  & data, name, dims) result(var)
  integer(int8), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int8_type) :: tmp
  integer(int8), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_byte
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int8_6d

!> variable constructor for integer(int16), rank(5)
module function new_variable_int16_6d( &
  & data, name, dims, atts) result(var)
  integer(int16), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int16_type) :: tmp
  integer(int16), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_short
  var%var = tmp
  nullify (ptr)
end function new_variable_int16_6d

!> variable constructor for integer(int16), rank(5)
module function new_variable_noatt_int16_6d( &
  & data, name, dims) result(var)
  integer(int16), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int16_type) :: tmp
  integer(int16), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_short
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int16_6d

!> variable constructor for integer(int32), rank(5)
module function new_variable_int32_6d( &
  & data, name, dims, atts) result(var)
  integer(int32), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int32_type) :: tmp
  integer(int32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_int
  var%var = tmp
  nullify (ptr)
end function new_variable_int32_6d

!> variable constructor for integer(int32), rank(5)
module function new_variable_noatt_int32_6d( &
  & data, name, dims) result(var)
  integer(int32), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int32_type) :: tmp
  integer(int32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_int
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int32_6d

!> variable constructor for integer(int64), rank(5)
module function new_variable_int64_6d( &
  & data, name, dims, atts) result(var)
  integer(int64), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_int64_type) :: tmp
  integer(int64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_int64
  var%var = tmp
  nullify (ptr)
end function new_variable_int64_6d

!> variable constructor for integer(int64), rank(5)
module function new_variable_noatt_int64_6d( &
  & data, name, dims) result(var)
  integer(int64), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_int64_type) :: tmp
  integer(int64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_int64
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_int64_6d

!> variable constructor for real(real32), rank(5)
module function new_variable_real32_6d( &
  & data, name, dims, atts) result(var)
  real(real32), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_real32_type) :: tmp
  real(real32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_float
  var%var = tmp
  nullify (ptr)
end function new_variable_real32_6d

!> variable constructor for real(real32), rank(5)
module function new_variable_noatt_real32_6d( &
  & data, name, dims) result(var)
  real(real32), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_real32_type) :: tmp
  real(real32), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_float
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_real32_6d

!> variable constructor for real(real64), rank(5)
module function new_variable_real64_6d( &
  & data, name, dims, atts) result(var)
  real(real64), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(attribute_type), intent(in) :: atts(:)
  type(variable_type) :: var
  type(variable_real64_type) :: tmp
  real(real64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%atts = atts
  tmp%type = nc_double
  var%var = tmp
  nullify (ptr)
end function new_variable_real64_6d

!> variable constructor for real(real64), rank(5)
module function new_variable_noatt_real64_6d( &
  & data, name, dims) result(var)
  real(real64), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
  character(len=*), intent(in) :: name
  type(dimension_type), intent(in) :: dims(:)
  type(variable_type) :: var
  type(variable_real64_type) :: tmp
  real(real64), pointer :: ptr(:) => null()

  ptr(1:size(data)) => data
  allocate (tmp%vals, source=ptr)
  tmp%name = name
  tmp%dims = dims
  tmp%type = nc_double
  var%var = tmp
  nullify (ptr)
end function new_variable_noatt_real64_6d

!> Shape of variable
module pure function shape_var(var) result(ret)
  type(variable_type), intent(in) :: var
  integer, allocatable :: ret(:)

  ret = shape_dims(var%var%dims)
end function shape_var

!> Define variable
subroutine def_var(var)
  type(variable_type), intent(inout) :: var
  integer(c_int) :: stat, ndims, i
  integer(c_int), allocatable :: dimids(:)

  select type (var_ => var%var)
  type is (variable_int16_type)

    ndims = size(var_%dims, kind=c_int)
    dimids = [(var_%dims(i)%ID, i=1, ndims)]
    stat = nc_def_var(var_%grpID, cstr(var_%name), &
      & var_%type, ndims, dimids, var_%ID)
    call handle_error(stat, "nc_def_var")

  type is (variable_int32_type)

    ndims = size(var_%dims, kind=c_int)
    dimids = [(var_%dims(i)%ID, i=1, ndims)]
    stat = nc_def_var(var_%grpID, cstr(var_%name), &
      & var_%type, ndims, dimids, var_%ID)
    call handle_error(stat, "nc_def_var")

  type is (variable_int64_type)

    ndims = size(var_%dims, kind=c_int)
    dimids = [(var_%dims(i)%ID, i=1, ndims)]
    stat = nc_def_var(var_%grpID, cstr(var_%name), &
      & var_%type, ndims, dimids, var_%ID)
    call handle_error(stat, "nc_def_var")

  type is (variable_real32_type)

    ndims = size(var_%dims, kind=c_int)
    dimids = [(var_%dims(i)%ID, i=1, ndims)]
    stat = nc_def_var(var_%grpID, cstr(var_%name), &
      & var_%type, ndims, dimids, var_%ID)
    call handle_error(stat, "nc_def_var")

  type is (variable_real64_type)

    ndims = size(var_%dims, kind=c_int)
    dimids = [(var_%dims(i)%ID, i=1, ndims)]
    stat = nc_def_var(var_%grpID, cstr(var_%name), &
      & var_%type, ndims, dimids, var_%ID)
    call handle_error(stat, "nc_def_var")

  end select
end subroutine def_var

!> Put variable
subroutine put_var(var)
  type(variable_type), intent(in) :: var
  integer(c_int) :: stat

  select type (var_ => var%var)
  type is (variable_int16_type)

    stat = nc_put_var_short( &
      & var_%grpID, var_%ID, var_%vals)
    call handle_error(stat, "nc_put_var_short")

  type is (variable_int32_type)

    stat = nc_put_var_int( &
      & var_%grpID, var_%ID, var_%vals)
    call handle_error(stat, "nc_put_var_int")

  type is (variable_int64_type)

    stat = nc_put_var_longlong( &
      & var_%grpID, var_%ID, var_%vals)
    call handle_error(stat, "nc_put_var_longlong")

  type is (variable_real32_type)

    stat = nc_put_var_float( &
      & var_%grpID, var_%ID, var_%vals)
    call handle_error(stat, "nc_put_var_float")

  type is (variable_real64_type)

    stat = nc_put_var_double( &
      & var_%grpID, var_%ID, var_%vals)
    call handle_error(stat, "nc_put_var_double")

  end select
end subroutine put_var

!> Write variable to a netcdf file
module subroutine to_netcdf_var(var, filename)
  type(variable_type), intent(inout) :: var
  character(len=*), intent(in) :: filename
  type(file_type), target :: file
  integer(c_int) :: stat

  !> copy metadata
  file%filename = filename
  file%mode = ior(nc_netcdf4, nc_clobber)

  !> create
  stat = nc_create(cstr(filename), file%mode, file%ID)
  call handle_error(stat, "nc_create")
  var%var%grpID => file%ID

  !> rootgroup name
  file%name = cstr("/")

  !> Define dimension
  call def_var_dim(var)

  !> Put group attributes
  if (allocated(file%atts)) then
    call put_grp_atts(file)
  end if

  !> Put variable attributes
  call def_var(var)
  if (allocated(var%var%atts)) then
    call put_var_atts(var)
  end if
  call put_var(var)
  nullify (var%var%grpID)

  stat = nc_close(file%ID)
  call handle_error(stat, "nc_close")
end subroutine to_netcdf_var

end submodule submodule_variable
