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

    if (associated(variable%nc_id)) deallocate (variable%nc_id)
    allocate (variable%nc_id, source=group%id)

    !> Inquire dimensions and attributes
    call inquire_dimensions(group, variable)
    call inquire_attributes(group, variable)

    !> Inquire variable type
    status = nc_inq_vartype(group%id, variable%id, variable%type)
    call handle_error(status, "nc_inq_vartype")
  end function get_var

  !> Shape of a variable
  module function shape_variable(variable) result(shapes)
    type(variable_type), intent(in) :: variable
    integer(int64), allocatable :: shapes(:)

    if (allocated(shapes)) deallocate (shapes)
    if (.not. associated(variable%dimensions)) &
      error stop "Variable dimensions not allocated."
    shapes = shape_dimensions(variable%dimensions)
  end function shape_variable

  !> Size of a variable
  module function size_variable(variable) result(ret)
    type(variable_type), intent(in) :: variable
    integer(int64) :: ret

    ret = product(shape_dimensions(variable%dimensions))
  end function size_variable

  !> Extract variable
  module subroutine extract_variable_int16(variable, values)
    type(variable_type), intent(in) :: variable
    integer(int16), allocatable, intent(out) :: values(:)
    integer :: status

    if (allocated(values)) deallocate (values)
    allocate (values(size(variable)))
    status = nc_get_var_short(variable%nc_id, variable%id, values)
    call handle_error(status, "nc_get_var_short")
  end subroutine extract_variable_int16

  module subroutine extract_variable_int32(variable, values)
    type(variable_type), intent(in) :: variable
    integer(int32), allocatable, intent(out) :: values(:)
    integer :: status

    if (allocated(values)) deallocate (values)
    allocate (values(size(variable)))
    status = nc_get_var_int(variable%nc_id, variable%id, values)
    call handle_error(status, "nc_get_var_int")
  end subroutine extract_variable_int32

  module subroutine extract_variable_int64(variable, values)
    type(variable_type), intent(in) :: variable
    integer(int64), allocatable, intent(out) :: values(:)
    integer :: status

    if (allocated(values)) deallocate (values)
    allocate (values(size(variable)))
    status = nc_get_var_longlong(variable%nc_id, variable%id, values)
    call handle_error(status, "nc_get_var_longlong")
  end subroutine extract_variable_int64

  module subroutine extract_variable_real32(variable, values)
    type(variable_type), intent(in) :: variable
    real(real32), allocatable, intent(out) :: values(:)
    integer :: status

    if (allocated(values)) deallocate (values)
    allocate (values(size(variable)))
    status = nc_get_var_float(variable%nc_id, variable%id, values)
    call handle_error(status, "nc_get_var_float")
  end subroutine extract_variable_real32

  module subroutine extract_variable_real64(variable, values)
    type(variable_type), intent(in) :: variable
    real(real64), allocatable, intent(out) :: values(:)
    integer :: status

    if (allocated(values)) deallocate (values)
    allocate (values(size(variable)))
    status = nc_get_var_double(variable%nc_id, variable%id, values)
    call handle_error(status, "nc_get_var_double")
  end subroutine extract_variable_real64

end submodule submodule_variable
