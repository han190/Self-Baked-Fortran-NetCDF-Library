submodule(module_netcdf) submodule_attribute

implicit none
contains

!> Inquire attributes based on nc_id
module subroutine inquire_variable_attributes(group, variable)
  type(group_type), intent(inout) :: group
  type(variable_type), intent(inout) :: variable
  integer(c_int) :: nc_id, var_id
  ! type(attribute_type), allocatable :: attributes(:)
  integer(c_int) :: status
  integer :: num_atts, i
  character(kind=c_char, len=num_chars) :: att_name
  integer(c_size_t) :: att_len
  integer(c_int) :: att_type
  class(*), allocatable :: values(:)

  !> Inquire number of attributes
  nc_id = group%id
  var_id = variable%id
  status = nc_inq_varnatts(nc_id, var_id, num_atts)
  call handle_error(status, "nc_inq_natts")

  !> Reallocate attributes
  if (allocated(variable%attributes)) deallocate (variable%attributes)
  allocate (variable%attributes(num_atts))

  !> Iteratively copy metadata to attributes.
  do i = 1, num_atts
    status = nc_inq_attname(nc_id, var_id, i - 1, att_name)
    call handle_error(status, "nc_inq_attname")
    variable%attributes(i)%name = strip(att_name, num_chars)

    status = nc_inq_att(nc_id, var_id, att_name, att_type, att_len)
    call handle_error(status, "nc_inq_att")
    variable%attributes(i)%type = att_type
    variable%attributes(i)%length = int(att_len, kind=int64)
  end do

  call get_var_att_(group, variable)
end subroutine inquire_variable_attributes

!> Get variable attribute
subroutine get_var_att_(group, variable)
  type(group_type), intent(in) :: group
  type(variable_type), intent(inout) :: variable
  integer(c_int) :: status
  integer :: i

  do i = 1, size(variable%attributes)
    associate (attr => variable%attributes(i))
      select case (attr%type)
      case (nc_char)
        allocate (character(kind=c_char, len=attr%length) :: attr%values(1))
      case (nc_short)
        allocate (integer(int16) :: attr%values(attr%length))
      case (nc_int)
        allocate (integer(int32) :: attr%values(attr%length))
      case (nc_int64)
        allocate (integer(int64) :: attr%values(attr%length))
      case (nc_float)
        allocate (real(real32) :: attr%values(attr%length))
      case (nc_double)
        allocate (real(real64) :: attr%values(attr%length))
      end select

      select type (values_ => attr%values)
      type is (character(kind=c_char, len=*))
        status = nc_get_att_text(group%id, variable%id, attr%name, values_(1))
        call handle_error(status, "nc_get_att_text")
      type is (integer(int16))
        status = nc_get_att_short(group%id, variable%id, attr%name, values_)
        call handle_error(status, "nc_get_att_short")
      type is (integer(int32))
        status = nc_get_att_int(group%id, variable%id, attr%name, values_)
        call handle_error(status, "nc_get_att_int")
      type is (integer(int64))
        status = nc_get_att_longlong(group%id, variable%id, attr%name, values_)
        call handle_error(status, "nc_get_att_longlong")
      type is (real(real32))
        status = nc_get_att_float(group%id, variable%id, attr%name, values_)
        call handle_error(status, "nc_get_att_float")
      type is (real(real64))
        status = nc_get_att_double(group%id, variable%id, attr%name, values_)
        call handle_error(status, "nc_get_att_double")
      end select
    end associate
  end do
end subroutine get_var_att_

end submodule submodule_attribute
