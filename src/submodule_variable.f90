submodule(module_netcdf) submodule_variable
  implicit none
contains

  !> Inquire variables from a group
  module subroutine inquire_group_variables(group)
    type(group_type), intent(inout) :: group
    integer(c_int) :: status
    integer :: num_vars, i
    integer(c_int), allocatable :: var_ids(:)
    character(kind=c_char, len=nc_max_name) :: var_name

    !> Inquire number of variables
    status = nc_inq_nvars(group%id, num_vars)
    call handle_error(status, "nc_inq_nvars")

    !> Inquire variable IDs
    if (allocated(group%variables)) deallocate (group%variables)
    allocate (group%variables(num_vars), var_ids(num_vars))
    status = nc_inq_varids(group%id, num_vars, var_ids)
    call handle_error(status, "nc_inq_varids")

    !> Get variable metadata one by one
    do i = 1, size(group%variables)
      associate (var => group%variables(i))
        !> Link group id
        if (associated(var%nc_id)) deallocate (var%nc_id)
        allocate (var%nc_id, source=group%id)

        !> Copy variable id
        var%id = var_ids(i)

        !> Inquire dimensions and attributes
        call inquire_dimensions(group, var)
        call inquire_attributes(group, var)

        !> Inquire variable type
        status = nc_inq_vartype(group%id, var%id, var%type)
        call handle_error(status, "nc_inq_vartype")

        !> Inquire variable name
        status = nc_inq_varname(group%id, var%id, var_name)
        call handle_error(status, "nc_inq_varname")
        var%name = strip(var_name)
      end associate
    end do
  end subroutine inquire_group_variables

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
