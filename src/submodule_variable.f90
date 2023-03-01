submodule(module_netcdf) submodule_variable

implicit none
contains

!> Get meta data for a variable
module function get_var_(group, name) result(variable)
  type(group_type), intent(in) :: group
  character(*), intent(in) :: name
  type(variable_type) :: variable
  integer :: status
  integer :: num_dims, num_atts, i
  integer(c_int), allocatable :: dim_ids(:)
  integer(int64) :: att_len
  integer(int64), allocatable :: dim_lens(:)
  integer, parameter :: nlen = 500
  character(kind=c_char, len=nlen) :: temp

  !> Inquire variable ID
  variable%name = name
  status = nc_inq_varid(group%id, name//c_null_char, variable%id)
  call check(status, name)

  !> Inquire number of dimensions
  status = nc_inq_varndims(group%id, variable%id, num_dims)
  call check(status)

  !> Inquire dimension IDs
  allocate(dim_ids(num_dims), dim_lens(num_dims))
  status = nc_inq_vardimid(group%id, variable%id, dim_ids)
  call check(status)

  !> Inquire dimension lengths
  do i = 1, num_dims
    status = nc_inq_dimlen(group%id, dim_ids(i), dim_lens(i))
    call check(status)
  end do

  !> Copy dimension info to variable type
  allocate(variable%dimensions(num_dims))
  associate (dims => variable%dimensions)
    do i = 1, num_dims
      dims(i)%id = dim_ids(i)
      dims(i)%length = dim_lens(i)        
      status = nc_inq_dimname(group%id, dims(i)%id, temp)
      call check(status)
      dims(i)%name = strip(temp, nlen)
    end do
  end associate

  !> Inquire the number of attributes
  status = nc_inq_varnatts(group%id, variable%id, num_atts)
  call check(status)

  !> Copy attribute info to variable type
  allocate (variable%attributes(num_atts))
    associate (atts => variable%attributes)
    do i = 1, num_atts
      status = nc_inq_attname(group%id, variable%id, i - 1, temp)
      call check(status)
      atts(i)%name = strip(temp, nlen)
      status = nc_inq_att(group%id, variable%id, &
        & temp, atts(i)%type, atts(i)%length)
      call check(status)
    end do
  end associate

  !> Inquire variable type
  status = nc_inq_vartype(group%id, variable%id, variable%type)
  call check(status)
end function get_var_

end submodule submodule_variable