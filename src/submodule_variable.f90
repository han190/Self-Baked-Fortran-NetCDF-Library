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

end submodule submodule_variable
