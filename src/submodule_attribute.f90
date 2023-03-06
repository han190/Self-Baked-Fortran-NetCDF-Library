submodule(module_netcdf) submodule_attribute

implicit none
contains

!> Inquire attributes based on nc_id
module function inquire_variable_attributes(nc_id, var_id) result(attributes)
  integer(c_int), intent(in) :: nc_id, var_id
  type(attribute_type), allocatable :: attributes(:)
  integer(c_int) :: status
  integer :: num_atts, i
  character(kind=c_char, len=num_chars) :: att_name
  integer(c_size_t) :: att_len
  integer(c_int) :: att_type
  class(*), allocatable :: values(:)

  !> Inquire number of attributes
  status = nc_inq_varnatts(nc_id, var_id, num_atts)
  call handle_error(status, "nc_inq_natts")

  if (allocated(attributes)) deallocate (attributes)
  allocate (attributes(num_atts))

  do i = 1, num_atts
    status = nc_inq_attname(nc_id, var_id, i - 1, att_name)
    call handle_error(status, "nc_inq_attname")
    attributes(i)%name = strip(att_name, num_chars)

    status = nc_inq_att(nc_id, var_id, att_name, att_type, att_len)
    call handle_error(status, "nc_inq_att")
    attributes(i)%type = att_type
    attributes(i)%length = int(att_len, kind=int64)

    select case (att_type)
    case (nc_char)
      allocate (character(kind=c_char, len=att_len) :: values(1))
    case (nc_short)
      allocate (integer(kind=int16) :: values(att_len))
    case (nc_int)
      allocate (integer(kind=int32) :: values(att_len))
    case (nc_int64)
      allocate (integer(kind=int64) :: values(att_len))
    case (nc_float)
      allocate (real(kind=real32) :: values(att_len))
    case (nc_double)
      allocate (real(kind=real64) :: values(att_len))
    end select

    select type (values_ => values)
    type is (character(kind=c_char, len=*))
      status = nc_get_att_text(nc_id, var_id, att_name, values_(1))
    type is (integer(kind=int16))
      status = nc_get_att_short(nc_id, var_id, att_name, values_)
    type is (integer(kind=int32))
      status = nc_get_att_int(nc_id, var_id, att_name, values_)
    type is (integer(kind=int64))
      status = nc_get_att_long(nc_id, var_id, att_name, values_)
    type is (real(kind=real32))
      status = nc_get_att_float(nc_id, var_id, att_name, values_)
    type is (real(kind=real64))
      status = nc_get_att_double(nc_id, var_id, att_name, values_)
    end select

    select case (att_type)
    case (nc_char)
      select type (values_ => values)
      type is (character(*))
        allocate (attributes(i)%values, source=[strip(values_(1), num_chars)])
      end select
    case default
      allocate (attributes(i)%values, source=values)
    end select
    if (allocated(values)) deallocate (values)
  end do
end function inquire_variable_attributes

end submodule submodule_attribute
