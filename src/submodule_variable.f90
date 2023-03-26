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

  !> Select type
  select case (variable%type)
  case (nc_short)

    associate (s => product(shape(variable%dimensions)))
      allocate (integer(int16) :: variable%values(s))
    end associate
    select type (values_ => variable%values)
    type is (integer(int16))
      status = nc_get_var_short(group%id, variable%id, values_)
      call handle_error(status, "nc_get_var_short")
    end select

  case (nc_int)

    associate (s => product(shape(variable%dimensions)))
      allocate (integer(int32) :: variable%values(s))
    end associate
    select type (values_ => variable%values)
    type is (integer(int32))
      status = nc_get_var_int(group%id, variable%id, values_)
      call handle_error(status, "nc_get_var_int")
    end select

  case (nc_int64)

    associate (s => product(shape(variable%dimensions)))
      allocate (integer(int64) :: variable%values(s))
    end associate
    select type (values_ => variable%values)
    type is (integer(int64))
      status = nc_get_var_longlong(group%id, variable%id, values_)
      call handle_error(status, "nc_get_var_longlong")
    end select

  case (nc_float)

    associate (s => product(shape(variable%dimensions)))
      allocate (real(real32) :: variable%values(s))
    end associate
    select type (values_ => variable%values)
    type is (real(real32))
      status = nc_get_var_float(group%id, variable%id, values_)
      call handle_error(status, "nc_get_var_float")
    end select

  case (nc_double)

    associate (s => product(shape(variable%dimensions)))
      allocate (real(real64) :: variable%values(s))
    end associate
    select type (values_ => variable%values)
    type is (real(real64))
      status = nc_get_var_double(group%id, variable%id, values_)
      call handle_error(status, "nc_get_var_double")
    end select

  end select
end subroutine get_var_

!> Extract data
module subroutine extract_variable(variable, data)
  type(variable_type), intent(inout) :: variable
  class(*), allocatable, intent(out) :: data(..)
  integer(int64), allocatable :: s(:)

  ! if (.not. allocated(variable%values)) &
  !   & call get_var_(group, variable)

  s = shape(variable%dimensions)
  select rank (data)
  rank (1)

    if (size(variable%dimensions) /= rank(data)) &
      & error stop "Invalid rank of data."

    select type (values_ => variable%values)
    type is (integer(int16))

      allocate (integer(int16) :: data(s(1)))
      select type (data_ => data)
      type is (integer(int16))
        data_ = reshape(values_, shape(data_))
      end select

    type is (integer(int32))

      allocate (integer(int32) :: data(s(1)))
      select type (data_ => data)
      type is (integer(int32))
        data_ = reshape(values_, shape(data_))
      end select

    type is (integer(int64))

      allocate (integer(int64) :: data(s(1)))
      select type (data_ => data)
      type is (integer(int64))
        data_ = reshape(values_, shape(data_))
      end select

    type is (real(real32))

      allocate (real(real32) :: data(s(1)))
      select type (data_ => data)
      type is (real(real32))
        data_ = reshape(values_, shape(data_))
      end select

    type is (real(real64))

      allocate (real(real64) :: data(s(1)))
      select type (data_ => data)
      type is (real(real64))
        data_ = reshape(values_, shape(data_))
      end select

    end select

  rank (2)

    if (size(variable%dimensions) /= rank(data)) &
      & error stop "Invalid rank of data."

    select type (values_ => variable%values)
    type is (integer(int16))

      allocate (integer(int16) :: data(s(1), s(2)))
      select type (data_ => data)
      type is (integer(int16))
        data_ = reshape(values_, shape(data_))
      end select

    type is (integer(int32))

      allocate (integer(int32) :: data(s(1), s(2)))
      select type (data_ => data)
      type is (integer(int32))
        data_ = reshape(values_, shape(data_))
      end select

    type is (integer(int64))

      allocate (integer(int64) :: data(s(1), s(2)))
      select type (data_ => data)
      type is (integer(int64))
        data_ = reshape(values_, shape(data_))
      end select

    type is (real(real32))

      allocate (real(real32) :: data(s(1), s(2)))
      select type (data_ => data)
      type is (real(real32))
        data_ = reshape(values_, shape(data_))
      end select

    type is (real(real64))

      allocate (real(real64) :: data(s(1), s(2)))
      select type (data_ => data)
      type is (real(real64))
        data_ = reshape(values_, shape(data_))
      end select

    end select

  rank (3)

    if (size(variable%dimensions) /= rank(data)) &
      & error stop "Invalid rank of data."

    select type (values_ => variable%values)
    type is (integer(int16))

      allocate (integer(int16) :: data(s(1), s(2), s(3)))
      select type (data_ => data)
      type is (integer(int16))
        data_ = reshape(values_, shape(data_))
      end select

    type is (integer(int32))

      allocate (integer(int32) :: data(s(1), s(2), s(3)))
      select type (data_ => data)
      type is (integer(int32))
        data_ = reshape(values_, shape(data_))
      end select

    type is (integer(int64))

      allocate (integer(int64) :: data(s(1), s(2), s(3)))
      select type (data_ => data)
      type is (integer(int64))
        data_ = reshape(values_, shape(data_))
      end select

    type is (real(real32))

      allocate (real(real32) :: data(s(1), s(2), s(3)))
      select type (data_ => data)
      type is (real(real32))
        data_ = reshape(values_, shape(data_))
      end select

    type is (real(real64))

      allocate (real(real64) :: data(s(1), s(2), s(3)))
      select type (data_ => data)
      type is (real(real64))
        data_ = reshape(values_, shape(data_))
      end select

    end select

  rank (4)

    if (size(variable%dimensions) /= rank(data)) &
      & error stop "Invalid rank of data."

    select type (values_ => variable%values)
    type is (integer(int16))

      allocate (integer(int16) :: data(s(1), s(2), s(3), s(4)))
      select type (data_ => data)
      type is (integer(int16))
        data_ = reshape(values_, shape(data_))
      end select

    type is (integer(int32))

      allocate (integer(int32) :: data(s(1), s(2), s(3), s(4)))
      select type (data_ => data)
      type is (integer(int32))
        data_ = reshape(values_, shape(data_))
      end select

    type is (integer(int64))

      allocate (integer(int64) :: data(s(1), s(2), s(3), s(4)))
      select type (data_ => data)
      type is (integer(int64))
        data_ = reshape(values_, shape(data_))
      end select

    type is (real(real32))

      allocate (real(real32) :: data(s(1), s(2), s(3), s(4)))
      select type (data_ => data)
      type is (real(real32))
        data_ = reshape(values_, shape(data_))
      end select

    type is (real(real64))

      allocate (real(real64) :: data(s(1), s(2), s(3), s(4)))
      select type (data_ => data)
      type is (real(real64))
        data_ = reshape(values_, shape(data_))
      end select

    end select

  end select
end subroutine extract_variable

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
