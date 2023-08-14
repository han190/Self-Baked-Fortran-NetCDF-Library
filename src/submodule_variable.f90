submodule(module_netcdf) submodule_variable
  implicit none
contains

  !> Inquire group variables
  module subroutine inq_grp_vars(grp)
    type(group_type), target, intent(inout) :: grp
    integer(c_int) :: stat, nvars
    integer(c_int), allocatable :: varids(:)
    character(kind=c_char, len=nc_max_name) :: tmp
    integer :: i

    !> inquire number of variables
    stat = nc_inq_nvars(grp%id, nvars)
    call handle_error(stat, "nc_inq_nvars")

    !> inquire variable ids
    if (allocated(grp%vars)) deallocate (grp%vars)
    allocate (grp%vars(nvars), varids(nvars))
    stat = nc_inq_varids(grp%id, nvars, varids)
    call handle_error(stat, "nc_inq_varids")

    do i = 1, nvars
      associate (var => grp%vars(i))

        if (associated(var%grp_id)) deallocate (var%grp_id)
        var%grp_id => grp%id

        var%id = varids(i)
        !> inquire variable type
        stat = nc_inq_vartype(grp%id, var%id, var%type)
        call handle_error(stat, "nc_inq_vartype")

        !> inquire variable name
        stat = nc_inq_varname(grp%id, var%id, tmp)
        call handle_error(stat, "nc_inq_varname")
        var%name = strip(tmp)
      end associate

      call inq_var_atts(grp, grp%vars(i))
      call inq_var_dims(grp, grp%vars(i))
    end do
  end subroutine inq_grp_vars

  !> Inquire variable
  module function inq_var(grp, name) result(var)
    type(group_type), target, intent(inout) :: grp
    character(len=*), intent(in) :: name
    type(variable_type) :: var
    integer :: stat

    !> inquire variable id
    var%name = name
    stat = nc_inq_varid(grp%id, name//c_null_char, var%id)
    call handle_error(stat, "nc_inq_varid")

    if (associated(var%grp_id)) deallocate (var%grp_id)
    var%grp_id => grp%id

    stat = nc_inq_vartype(grp%id, var%id, var%type)
    call handle_error(stat, "nc_inq_vartype")

    call inq_var_dims(grp, var)
    call inq_var_atts(grp, var)
  end function inq_var

  !> Shape of a variable
  module function shape_var(var) result(ret)
    type(variable_type), intent(in) :: var
    integer(int64), allocatable :: ret(:)

    if (allocated(ret)) deallocate (ret)
    ret = shape_dims(var%dims)
  end function shape_var

  !> Size of a variable
  module function size_var(var) result(ret)
    type(variable_type), intent(in) :: var
    integer(int64) :: ret

    ret = size_dims(var%dims)
  end function size_var

  !> Rank of a variable
  module function rank_var(var) result(ret)
    type(variable_type), intent(in) :: var
    integer(int64) :: ret

    ret = rank_dims(var%dims)
  end function rank_var

  !> Extract variable
  !> ----------------
  
  !> Extract variable int16
  module subroutine extract_var_int16(var, vals)
    type(variable_type), intent(in) :: var
    integer(int16), intent(out) :: vals(*)
    integer :: status

    status = nc_get_var_short(var%grp_id, var%id, vals)
    call handle_error(status, "nc_get_var_short")
  end subroutine extract_var_int16

  !> Extract variable int32
  module subroutine extract_var_int32(var, vals)
    type(variable_type), intent(in) :: var
    integer(int32), intent(out) :: vals(*)
    integer :: status

    status = nc_get_var_int(var%grp_id, var%id, vals)
    call handle_error(status, "nc_get_var_int")
  end subroutine extract_var_int32

  !> Extract variable int64
  module subroutine extract_var_int64(var, vals)
    type(variable_type), intent(in) :: var
    integer(int64), intent(out) :: vals(*)
    integer :: status

    status = nc_get_var_longlong(var%grp_id, var%id, vals)
    call handle_error(status, "nc_get_var_longlong")
  end subroutine extract_var_int64

  !> Extract variable real32
  module subroutine extract_var_real32(var, vals)
    type(variable_type), intent(in) :: var
    real(real32), intent(out) :: vals(*)
    integer :: status

    status = nc_get_var_float(var%grp_id, var%id, vals)
    call handle_error(status, "nc_get_var_float")
  end subroutine extract_var_real32

  !> Extract variable real64
  module subroutine extract_var_real64(var, vals)
    type(variable_type), intent(in) :: var
    real(real64), intent(out) :: vals(*)
    integer :: status

    status = nc_get_var_double(var%grp_id, var%id, vals)
    call handle_error(status, "nc_get_var_double")
  end subroutine extract_var_real64

end submodule submodule_variable