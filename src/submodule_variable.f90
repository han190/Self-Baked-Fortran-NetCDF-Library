submodule(module_interface) submodule_variable
  implicit none
contains

  !> Define variable
  module function def_grp_var(grp, name, type, dim_names) result(var)
    type(group_type), target, intent(in) :: grp
    character(len=*), intent(in) :: name, dim_names(:)
    integer(nc_type), intent(in) :: type
    type(variable_type) :: var
    integer(c_int), allocatable :: dimids(:)
    integer(c_int) :: ndims, i, j, stat

    ndims = size(dim_names)
    if (allocated(var%dims%ptrs)) deallocate (var%dims%ptrs)
    allocate (var%dims%ptrs(ndims), dimids(ndims))

    var%type = type
    var%name = name
    var%grp_id => grp%id
    do i = 1, ndims
      inner: do j = 1, size(grp%dims)
        if (trim(dim_names(i)) == trim(grp%dims(j)%name)) then
          var%dims%ptrs(i)%ptr => grp%dims(j)
          dimids(i) = grp%dims(i)%id
          exit inner
        end if
      end do inner
    end do

    stat = nc_def_var(grp%id, to_cstr(name), &
      & type, ndims, dimids, var%id)
    call handle_error(stat, "nc_def_var")
  end function def_grp_var

  !> Put variable
  module subroutine put_var_int(var, vals)
    type(variable_type), intent(in) :: var
    integer, intent(in) :: vals(*)
    integer(c_int) :: stat

    stat = nc_put_var_int(var%grp_id, var%id, vals)
  end subroutine put_var_int

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
    type(group_type), target, intent(in) :: grp
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

  module subroutine get_var_name_int16(grp, name, vals)
    type(group_type), intent(in) :: grp
    character(len=*), intent(in) :: name
    integer(int16), allocatable, intent(out) :: vals(:)
    integer :: stat
    type(variable_type) :: var

    var = inq_var(grp, name)
    if (allocated(vals)) deallocate (vals)
    allocate (vals(size(var)))
    stat = nc_get_var_short(grp%id, var%id, vals)
    call handle_error(stat, "nc_get_var_short")
  end subroutine get_var_name_int16

  module subroutine get_var_int16(var, vals)
    type(variable_type), intent(in) :: var
    integer(int16), allocatable, intent(out) :: vals(:)
    integer :: stat

    if (allocated(vals)) deallocate (vals)
    allocate (vals(size(var)))
    stat = nc_get_var_short(var%grp_id, var%id, vals)
    call handle_error(stat, "nc_get_var_short")
  end subroutine get_var_int16

  module subroutine get_var_name_int32(grp, name, vals)
    type(group_type), intent(in) :: grp
    character(len=*), intent(in) :: name
    integer(int32), allocatable, intent(out) :: vals(:)
    integer :: stat
    type(variable_type) :: var

    var = inq_var(grp, name)
    if (allocated(vals)) deallocate (vals)
    allocate (vals(size(var)))
    stat = nc_get_var_int(grp%id, var%id, vals)
    call handle_error(stat, "nc_get_var_int")
  end subroutine get_var_name_int32

  module subroutine get_var_int32(var, vals)
    type(variable_type), intent(in) :: var
    integer(int32), allocatable, intent(out) :: vals(:)
    integer :: stat

    if (allocated(vals)) deallocate (vals)
    allocate (vals(size(var)))
    stat = nc_get_var_int(var%grp_id, var%id, vals)
    call handle_error(stat, "nc_get_var_int")
  end subroutine get_var_int32

  module subroutine get_var_name_int64(grp, name, vals)
    type(group_type), intent(in) :: grp
    character(len=*), intent(in) :: name
    integer(int64), allocatable, intent(out) :: vals(:)
    integer :: stat
    type(variable_type) :: var

    var = inq_var(grp, name)
    if (allocated(vals)) deallocate (vals)
    allocate (vals(size(var)))
    stat = nc_get_var_longlong(grp%id, var%id, vals)
    call handle_error(stat, "nc_get_var_longlong")
  end subroutine get_var_name_int64

  module subroutine get_var_int64(var, vals)
    type(variable_type), intent(in) :: var
    integer(int64), allocatable, intent(out) :: vals(:)
    integer :: stat

    if (allocated(vals)) deallocate (vals)
    allocate (vals(size(var)))
    stat = nc_get_var_longlong(var%grp_id, var%id, vals)
    call handle_error(stat, "nc_get_var_longlong")
  end subroutine get_var_int64

  module subroutine get_var_name_real32(grp, name, vals)
    type(group_type), intent(in) :: grp
    character(len=*), intent(in) :: name
    real(real32), allocatable, intent(out) :: vals(:)
    integer :: stat
    type(variable_type) :: var

    var = inq_var(grp, name)
    if (allocated(vals)) deallocate (vals)
    allocate (vals(size(var)))
    stat = nc_get_var_float(grp%id, var%id, vals)
    call handle_error(stat, "nc_get_var_float")
  end subroutine get_var_name_real32

  module subroutine get_var_real32(var, vals)
    type(variable_type), intent(in) :: var
    real(real32), allocatable, intent(out) :: vals(:)
    integer :: stat

    if (allocated(vals)) deallocate (vals)
    allocate (vals(size(var)))
    stat = nc_get_var_float(var%grp_id, var%id, vals)
    call handle_error(stat, "nc_get_var_float")
  end subroutine get_var_real32

  module subroutine get_var_name_real64(grp, name, vals)
    type(group_type), intent(in) :: grp
    character(len=*), intent(in) :: name
    real(real64), allocatable, intent(out) :: vals(:)
    integer :: stat
    type(variable_type) :: var

    var = inq_var(grp, name)
    if (allocated(vals)) deallocate (vals)
    allocate (vals(size(var)))
    stat = nc_get_var_double(grp%id, var%id, vals)
    call handle_error(stat, "nc_get_var_double")
  end subroutine get_var_name_real64

  module subroutine get_var_real64(var, vals)
    type(variable_type), intent(in) :: var
    real(real64), allocatable, intent(out) :: vals(:)
    integer :: stat

    if (allocated(vals)) deallocate (vals)
    allocate (vals(size(var)))
    stat = nc_get_var_double(var%grp_id, var%id, vals)
    call handle_error(stat, "nc_get_var_double")
  end subroutine get_var_real64

end submodule submodule_variable
