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

  !> Get variable
  call get_var_(group, variable)
end function get_var

!> Based on the rank and type of the data, get the variable.
subroutine get_var_(group, variable)
  type(group_type), intent(inout) :: group
  type(variable_type), intent(inout) :: variable
  integer(c_int) :: status

  if (.not. associated(variable%dimensions)) &
    error stop "Dimensions not associated."

  !> Select rank.
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

  !> Select type
  select case (variable%type)
  case (nc_short)

    select type (container_ => variable%container)
    type is (container_1d)

      associate (s => shape(variable%dimensions))
        allocate (integer(int16) :: container_%data(s(1)))
      end associate
      select type (data_ => container_%data)
      type is (integer(int16))
        status = nc_get_var_short (group%id, variable%id, data_)
        call handle_error(status, "nc_get_var_short")
      end select

    type is (container_2d)

      associate (s => shape(variable%dimensions))
        allocate (integer(int16) :: container_%data(s(1), s(2)))
      end associate
      select type (data_ => container_%data)
      type is (integer(int16))
        status = nc_get_var_short (group%id, variable%id, data_)
        call handle_error(status, "nc_get_var_short")
      end select

    type is (container_3d)

      associate (s => shape(variable%dimensions))
        allocate (integer(int16) :: container_%data(s(1), s(2), s(3)))
      end associate
      select type (data_ => container_%data)
      type is (integer(int16))
        status = nc_get_var_short (group%id, variable%id, data_)
        call handle_error(status, "nc_get_var_short")
      end select

    type is (container_4d)

      associate (s => shape(variable%dimensions))
        allocate (integer(int16) :: container_%data(s(1), s(2), s(3), s(4)))
      end associate
      select type (data_ => container_%data)
      type is (integer(int16))
        status = nc_get_var_short (group%id, variable%id, data_)
        call handle_error(status, "nc_get_var_short")
      end select

    end select

  case (nc_int)

    select type (container_ => variable%container)
    type is (container_1d)

      associate (s => shape(variable%dimensions))
        allocate (integer(int32) :: container_%data(s(1)))
      end associate
      select type (data_ => container_%data)
      type is (integer(int32))
        status = nc_get_var_int (group%id, variable%id, data_)
        call handle_error(status, "nc_get_var_int")
      end select

    type is (container_2d)

      associate (s => shape(variable%dimensions))
        allocate (integer(int32) :: container_%data(s(1), s(2)))
      end associate
      select type (data_ => container_%data)
      type is (integer(int32))
        status = nc_get_var_int (group%id, variable%id, data_)
        call handle_error(status, "nc_get_var_int")
      end select

    type is (container_3d)

      associate (s => shape(variable%dimensions))
        allocate (integer(int32) :: container_%data(s(1), s(2), s(3)))
      end associate
      select type (data_ => container_%data)
      type is (integer(int32))
        status = nc_get_var_int (group%id, variable%id, data_)
        call handle_error(status, "nc_get_var_int")
      end select

    type is (container_4d)

      associate (s => shape(variable%dimensions))
        allocate (integer(int32) :: container_%data(s(1), s(2), s(3), s(4)))
      end associate
      select type (data_ => container_%data)
      type is (integer(int32))
        status = nc_get_var_int (group%id, variable%id, data_)
        call handle_error(status, "nc_get_var_int")
      end select

    end select

  case (nc_int64)

    select type (container_ => variable%container)
    type is (container_1d)

      associate (s => shape(variable%dimensions))
        allocate (integer(int64) :: container_%data(s(1)))
      end associate
      select type (data_ => container_%data)
      type is (integer(int64))
        status = nc_get_var_longlong (group%id, variable%id, data_)
        call handle_error(status, "nc_get_var_longlong")
      end select

    type is (container_2d)

      associate (s => shape(variable%dimensions))
        allocate (integer(int64) :: container_%data(s(1), s(2)))
      end associate
      select type (data_ => container_%data)
      type is (integer(int64))
        status = nc_get_var_longlong (group%id, variable%id, data_)
        call handle_error(status, "nc_get_var_longlong")
      end select

    type is (container_3d)

      associate (s => shape(variable%dimensions))
        allocate (integer(int64) :: container_%data(s(1), s(2), s(3)))
      end associate
      select type (data_ => container_%data)
      type is (integer(int64))
        status = nc_get_var_longlong (group%id, variable%id, data_)
        call handle_error(status, "nc_get_var_longlong")
      end select

    type is (container_4d)

      associate (s => shape(variable%dimensions))
        allocate (integer(int64) :: container_%data(s(1), s(2), s(3), s(4)))
      end associate
      select type (data_ => container_%data)
      type is (integer(int64))
        status = nc_get_var_longlong (group%id, variable%id, data_)
        call handle_error(status, "nc_get_var_longlong")
      end select

    end select

  case (nc_float)

    select type (container_ => variable%container)
    type is (container_1d)

      associate (s => shape(variable%dimensions))
        allocate (real(real32) :: container_%data(s(1)))
      end associate
      select type (data_ => container_%data)
      type is (real(real32))
        status = nc_get_var_float (group%id, variable%id, data_)
        call handle_error(status, "nc_get_var_float")
      end select

    type is (container_2d)

      associate (s => shape(variable%dimensions))
        allocate (real(real32) :: container_%data(s(1), s(2)))
      end associate
      select type (data_ => container_%data)
      type is (real(real32))
        status = nc_get_var_float (group%id, variable%id, data_)
        call handle_error(status, "nc_get_var_float")
      end select

    type is (container_3d)

      associate (s => shape(variable%dimensions))
        allocate (real(real32) :: container_%data(s(1), s(2), s(3)))
      end associate
      select type (data_ => container_%data)
      type is (real(real32))
        status = nc_get_var_float (group%id, variable%id, data_)
        call handle_error(status, "nc_get_var_float")
      end select

    type is (container_4d)

      associate (s => shape(variable%dimensions))
        allocate (real(real32) :: container_%data(s(1), s(2), s(3), s(4)))
      end associate
      select type (data_ => container_%data)
      type is (real(real32))
        status = nc_get_var_float (group%id, variable%id, data_)
        call handle_error(status, "nc_get_var_float")
      end select

    end select

  case (nc_double)

    select type (container_ => variable%container)
    type is (container_1d)

      associate (s => shape(variable%dimensions))
        allocate (real(real64) :: container_%data(s(1)))
      end associate
      select type (data_ => container_%data)
      type is (real(real64))
        status = nc_get_var_double (group%id, variable%id, data_)
        call handle_error(status, "nc_get_var_double")
      end select

    type is (container_2d)

      associate (s => shape(variable%dimensions))
        allocate (real(real64) :: container_%data(s(1), s(2)))
      end associate
      select type (data_ => container_%data)
      type is (real(real64))
        status = nc_get_var_double (group%id, variable%id, data_)
        call handle_error(status, "nc_get_var_double")
      end select

    type is (container_3d)

      associate (s => shape(variable%dimensions))
        allocate (real(real64) :: container_%data(s(1), s(2), s(3)))
      end associate
      select type (data_ => container_%data)
      type is (real(real64))
        status = nc_get_var_double (group%id, variable%id, data_)
        call handle_error(status, "nc_get_var_double")
      end select

    type is (container_4d)

      associate (s => shape(variable%dimensions))
        allocate (real(real64) :: container_%data(s(1), s(2), s(3), s(4)))
      end associate
      select type (data_ => container_%data)
      type is (real(real64))
        status = nc_get_var_double (group%id, variable%id, data_)
        call handle_error(status, "nc_get_var_double")
      end select

    end select

  end select
end subroutine get_var_

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
