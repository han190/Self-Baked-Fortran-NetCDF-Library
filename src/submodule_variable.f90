submodule(module_netcdf) submodule_variable

implicit none
contains

!> Get meta data for a variable
module function get_var(group, name) result(variable)
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
  call handle_error(status, "nc_inq_varid")

  !> Inquire dimensions and attributes
  variable%dimensions = inquire_variable_dimensions(group%id, variable%id)
  variable%attributes = inquire_variable_attributes(group%id, variable%id)

  !> Inquire variable type
  status = nc_inq_vartype(group%id, variable%id, variable%type)
  call handle_error(status, "nc_inq_vartype")

  select case (size(variable%dimensions))
  case (1)
    allocate (container_1d :: variable%container)
  case (2)
    allocate (container_2d :: variable%container)
  case (3)
    allocate (container_3d :: variable%container)
  case (4)
    allocate (container_4d :: variable%container)
  case default
    error stop "Invalid data rank."
  end select

  select case (variable%type)
  case (nc_short)

    select type (container_ => variable%container)
    type is (container_1d)
      allocate (integer(kind=int16) :: container_%data( &
        & variable%dimensions(1)%length))
    type is (container_2d)
      allocate (integer(kind=int16) :: container_%data( &
        & variable%dimensions(2)%length, &
        & variable%dimensions(1)%length))
    type is (container_3d)
      allocate (integer(kind=int16) :: container_%data( &
        & variable%dimensions(3)%length, &
        & variable%dimensions(2)%length, &
        & variable%dimensions(1)%length))
    type is (container_4d)
      allocate (integer(kind=int16) :: container_%data( &
        & variable%dimensions(4)%length, &
        & variable%dimensions(3)%length, &
        & variable%dimensions(2)%length, &
        & variable%dimensions(1)%length))
    end select

  end select
end function get_var

end submodule submodule_variable
