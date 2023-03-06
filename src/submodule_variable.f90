submodule(module_netcdf) submodule_variable

implicit none
contains

!> Get meta data for a variable
module function get_var(group, name) result(variable)
  type(group_type), intent(in) :: group
  character(*), intent(in) :: name
  type(variable_type) :: variable
  integer :: status

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

      select type (data_ => container_%data)
      type is (integer(kind=int16))
        status = nc_get_var_short(group%id, variable%id, data_)
      end select

    type is (container_2d)

      allocate (integer(kind=int16) :: container_%data( &
        & variable%dimensions(2)%length, &
        & variable%dimensions(1)%length))

      select type (data_ => container_%data)
      type is (integer(kind=int16))
        status = nc_get_var_short(group%id, variable%id, data_)
      end select

    type is (container_3d)

      allocate (integer(kind=int16) :: container_%data( &
        & variable%dimensions(3)%length, &
        & variable%dimensions(2)%length, &
        & variable%dimensions(1)%length))

      select type (data_ => container_%data)
      type is (integer(kind=int16))
        status = nc_get_var_short(group%id, variable%id, data_)
      end select

    type is (container_4d)

      allocate (integer(kind=int16) :: container_%data( &
        & variable%dimensions(4)%length, &
        & variable%dimensions(3)%length, &
        & variable%dimensions(2)%length, &
        & variable%dimensions(1)%length))

      select type (data_ => container_%data)
      type is (integer(kind=int16))
        status = nc_get_var_short(group%id, variable%id, data_)
      end select

    end select
    call handle_error(status, "nc_get_var_short")

  case (nc_int)

    select type (container_ => variable%container)
    type is (container_1d)

      allocate (integer(kind=int32) :: container_%data( &
        & variable%dimensions(1)%length))

      select type (data_ => container_%data)
      type is (integer(kind=int32))
        status = nc_get_var_int(group%id, variable%id, data_)
      end select

    type is (container_2d)

      allocate (integer(kind=int32) :: container_%data( &
        & variable%dimensions(2)%length, &
        & variable%dimensions(1)%length))

      select type (data_ => container_%data)
      type is (integer(kind=int32))
        status = nc_get_var_int(group%id, variable%id, data_)
      end select

    type is (container_3d)

      allocate (integer(kind=int32) :: container_%data( &
        & variable%dimensions(3)%length, &
        & variable%dimensions(2)%length, &
        & variable%dimensions(1)%length))

      select type (data_ => container_%data)
      type is (integer(kind=int32))
        status = nc_get_var_int(group%id, variable%id, data_)
      end select

    type is (container_4d)

      allocate (integer(kind=int32) :: container_%data( &
        & variable%dimensions(4)%length, &
        & variable%dimensions(3)%length, &
        & variable%dimensions(2)%length, &
        & variable%dimensions(1)%length))

      select type (data_ => container_%data)
      type is (integer(kind=int32))
        status = nc_get_var_int(group%id, variable%id, data_)
      end select

    end select
    call handle_error(status, "nc_get_var_int")

  case (nc_int64)

    select type (container_ => variable%container)
    type is (container_1d)

      allocate (integer(kind=int64) :: container_%data( &
        & variable%dimensions(1)%length))

      select type (data_ => container_%data)
      type is (integer(kind=int64))
        status = nc_get_var_longlong(group%id, variable%id, data_)
      end select

    type is (container_2d)

      allocate (integer(kind=int64) :: container_%data( &
        & variable%dimensions(2)%length, &
        & variable%dimensions(1)%length))

      select type (data_ => container_%data)
      type is (integer(kind=int64))
        status = nc_get_var_longlong(group%id, variable%id, data_)
      end select

    type is (container_3d)

      allocate (integer(kind=int64) :: container_%data( &
        & variable%dimensions(3)%length, &
        & variable%dimensions(2)%length, &
        & variable%dimensions(1)%length))

      select type (data_ => container_%data)
      type is (integer(kind=int64))
        status = nc_get_var_longlong(group%id, variable%id, data_)
      end select

    type is (container_4d)

      allocate (integer(kind=int64) :: container_%data( &
        & variable%dimensions(4)%length, &
        & variable%dimensions(3)%length, &
        & variable%dimensions(2)%length, &
        & variable%dimensions(1)%length))

      select type (data_ => container_%data)
      type is (integer(kind=int64))
        status = nc_get_var_longlong(group%id, variable%id, data_)
      end select

    end select
    call handle_error(status, "nc_get_var_longlong")

  case (nc_float)

    select type (container_ => variable%container)
    type is (container_1d)

      allocate (real(kind=real32) :: container_%data( &
        & variable%dimensions(1)%length))

      select type (data_ => container_%data)
      type is (real(kind=real32))
        status = nc_get_var_float(group%id, variable%id, data_)
      end select

    type is (container_2d)

      allocate (real(kind=real32) :: container_%data( &
        & variable%dimensions(2)%length, &
        & variable%dimensions(1)%length))

      select type (data_ => container_%data)
      type is (real(kind=real32))
        status = nc_get_var_float(group%id, variable%id, data_)
      end select

    type is (container_3d)

      allocate (real(kind=real32) :: container_%data( &
        & variable%dimensions(3)%length, &
        & variable%dimensions(2)%length, &
        & variable%dimensions(1)%length))

      select type (data_ => container_%data)
      type is (real(kind=real32))
        status = nc_get_var_float(group%id, variable%id, data_)
      end select

    type is (container_4d)

      allocate (real(kind=real32) :: container_%data( &
        & variable%dimensions(4)%length, &
        & variable%dimensions(3)%length, &
        & variable%dimensions(2)%length, &
        & variable%dimensions(1)%length))

      select type (data_ => container_%data)
      type is (real(kind=real32))
        status = nc_get_var_float(group%id, variable%id, data_)
      end select

    end select
    call handle_error(status, "nc_get_var_float")

  case (nc_double)

    select type (container_ => variable%container)
    type is (container_1d)

      allocate (real(kind=real64) :: container_%data( &
        & variable%dimensions(1)%length))

      select type (data_ => container_%data)
      type is (real(kind=real64))
        status = nc_get_var_double(group%id, variable%id, data_)
      end select

    type is (container_2d)

      allocate (real(kind=real64) :: container_%data( &
        & variable%dimensions(2)%length, &
        & variable%dimensions(1)%length))

      select type (data_ => container_%data)
      type is (real(kind=real64))
        status = nc_get_var_double(group%id, variable%id, data_)
      end select

    type is (container_3d)

      allocate (real(kind=real64) :: container_%data( &
        & variable%dimensions(3)%length, &
        & variable%dimensions(2)%length, &
        & variable%dimensions(1)%length))

      select type (data_ => container_%data)
      type is (real(kind=real64))
        status = nc_get_var_double(group%id, variable%id, data_)
      end select

    type is (container_4d)

      allocate (real(kind=real64) :: container_%data( &
        & variable%dimensions(4)%length, &
        & variable%dimensions(3)%length, &
        & variable%dimensions(2)%length, &
        & variable%dimensions(1)%length))

      select type (data_ => container_%data)
      type is (real(kind=real64))
        status = nc_get_var_double(group%id, variable%id, data_)
      end select

    end select
    call handle_error(status, "nc_get_var_double")

  end select
end function get_var

end submodule submodule_variable
