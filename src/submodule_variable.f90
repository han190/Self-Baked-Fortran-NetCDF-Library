submodule(module_netcdf) submodule_variable

implicit none
contains

!> Get meta data for a variable
module function get_var(group, name) result(variable)
  type(group_type), intent(inout) :: group
  character(*), intent(in) :: name
  type(variable_type) :: variable
  integer :: status

  !> Inquire variable ID
  variable%name = name
  status = nc_inq_varid(group%id, name//c_null_char, variable%id)
  call handle_error(status, "nc_inq_varid")

  !> Inquire dimensions and attributes
  call inquire_dimensions(group, variable)
  call inquire_attributes(group, variable)

  !> Inquire variable type
  status = nc_inq_vartype(group%id, variable%id, variable%type)
  call handle_error(status, "nc_inq_vartype")

  ! !> Get variable
  ! call get_var_(group, variable)
end function get_var

! !> Based on the rank and type of the data, get the variable.
! subroutine get_var_(group, variable)
!   type(group_type), intent(inout) :: group
!   type(variable_type), intent(inout) :: variable
!   integer(c_int) :: status
!   integer(int64) :: s

!   if (.not. associated(variable%dimensions)) &
!     error stop "Dimensions not associated."
!   s = product(shape(variable))

!   !> Determine type
!   select case (variable%type)
!   case (nc_short)
!     allocate (integer(int16) :: variable%values(s))
!   case (nc_int)
!     allocate (integer(int32) :: variable%values(s))
!   case (nc_int64)
!     allocate (integer(int64) :: variable%values(s))
!   case (nc_float)
!     allocate (integer(real32) :: variable%values(s))
!   case (nc_double)
!     allocate (integer(real64) :: variable%values(s))
!   end select

!   !> Select type
!   select type (values_ => variable%values)
!   type is (integer(int16))
!     status = nc_get_var_short(group%id, variable%id, values_)
!     call handle_error(status, "nc_get_var_short")
!   type is (integer(int32))
!     status = nc_get_var_int(group%id, variable%id, values_)
!     call handle_error(status, "nc_get_var_int")
!   type is (integer(int64))
!     status = nc_get_var_longlong(group%id, variable%id, values_)
!     call handle_error(status, "nc_get_var_longlong")
!   type is (real(real32))
!     status = nc_get_var_float(group%id, variable%id, values_)
!     call handle_error(status, "nc_get_var_float")
!   type is (real(real64))
!   end select
! end subroutine get_var_

! module subroutine extract_variable_float_1d(variable, data)
!   type(variable_type), intent(inout) :: variable
!   real, allocatable, intent(out) :: data(:)
!   integer(int64), allocatable :: s(:)

!   s = shape(variable)
!   if (allocated(data)) deallocate (data)
!   allocate (data(s(1)))

!   select type (values_ => variable%values)
!   type is (real(real32))
!     data = reshape(values_, shape(data))
!   end select
! end subroutine extract_variable_float_1d

! module subroutine extract_variable_short_3d(variable, data)
!   type(variable_type), intent(inout) :: variable
!   integer(int16), allocatable, intent(out) :: data(:, :, :)
!   integer(int64), allocatable :: s(:)

!   s = shape(variable)
!   if (allocated(data)) deallocate (data)
!   allocate (data(s(1), s(2), s(3)))

!   select type (values_ => variable%values)
!   type is (integer(int16))
!     data = reshape(values_, shape(data))
!   end select
! end subroutine extract_variable_short_3d

!> Shape of a variable
module function shape_variable(variable) result(shapes)
  type(variable_type), intent(in) :: variable
  integer(int64), allocatable :: shapes(:)

  if (allocated(shapes)) deallocate (shapes)
  if (.not. associated(variable%dimensions)) &
    error stop "Variable dimensions not associated."
  shapes = shape_dimensions(variable%dimensions)
end function shape_variable

end submodule submodule_variable
