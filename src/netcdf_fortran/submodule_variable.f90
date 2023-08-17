submodule(module_interface) submodule_variable
  implicit none
contains

  !> Define variable
  module function def_grp_var(grp, name, type, dim_names) result(var)
    class(group_type), target, intent(in) :: grp
    character(len=*), intent(in) :: name, dim_names(:)
    integer(nc_type), intent(in) :: type
    type(variable_type) :: var
    integer(c_int), allocatable :: dimids(:)
    integer(c_int) :: ndims, i, j, stat
    type(pair_type) :: pair

    ndims = size(dim_names)
    allocate (dimids(ndims))

    var%type = type
    var%name = name
    var%grp_id => grp%id

    do i = 1, ndims
      pair = scan(grp%dims, trim(dim_names(i)))
      call append(var%dims, pair)

      select type (dim => pair%val)
      type is (dimension_type)
        dimids(i) = dim%id
      end select
    end do

    stat = nc_def_var(grp%id, to_cstr(name), &
      & type, ndims, dimids, var%id)
    call handle_error(stat, "nc_def_var")
  end function def_grp_var

  !> Put variable
  subroutine put_var_(var, vals)
    type(variable_type), intent(in) :: var
    class(*), intent(in) :: vals(:)
    integer(c_int) :: stat

    select type (v => vals)
    type is (integer(int16))
      stat = nc_put_var_short(var%grp_id, var%id, v)
      call handle_error(stat, "nc_put_var_short")
    type is (integer(int32))
      stat = nc_put_var_int(var%grp_id, var%id, v)
      call handle_error(stat, "nc_put_var_int")
    type is (integer(int64))
      stat = nc_put_var_longlong(var%grp_id, var%id, v)
      call handle_error(stat, "nc_put_var_longlong")
    type is (real(real32))
      stat = nc_put_var_float(var%grp_id, var%id, v)
      call handle_error(stat, "nc_put_var_float")
    type is (real(real64))
      stat = nc_put_var_double(var%grp_id, var%id, v)
      call handle_error(stat, "nc_put_var_double")
    type is (character(*))
      stat = nc_put_var_text(var%grp_id, var%id, v)
      call handle_error(stat, "nc_put_var_text")
    class default
      error stop "Invalid variable type."
    end select
  end subroutine put_var_

  module subroutine put_var(var, vals)
    type(variable_type), intent(in) :: var
    class(*), intent(in) :: vals(..)
    class(*), pointer :: ptr(:) => null()

    select rank (vals)
    rank (1)
      ptr(1:size(vals)) => vals
    rank (2)
      ptr(1:size(vals)) => vals
    rank (3)
      ptr(1:size(vals)) => vals
    rank (4)
      ptr(1:size(vals)) => vals
    rank (5)
      ptr(1:size(vals)) => vals
    end select

    call put_var_(var, ptr)
    nullify (ptr)
  end subroutine put_var

  !> Inquire group variables
  module subroutine inq_grp_vars(grp)
    class(group_type), target, intent(inout) :: grp
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
    class(group_type), target, intent(inout) :: grp
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
    ret = shape_dict(var%dims)
  end function shape_var

  !> Size of a variable
  module function size_var(var) result(ret)
    type(variable_type), intent(in) :: var
    integer(int64) :: ret

    ret = product(shape(var%dims))
  end function size_var

  !> Rank of a variable
  module function rank_var(var) result(ret)
    type(variable_type), intent(in) :: var
    integer(int64) :: ret

    ret = var%dims%length
  end function rank_var

  !> Extract variable
  !> ----------------

  module subroutine get_var_name_int16(grp, name, vals)
    class(group_type), intent(inout) :: grp
    character(len=*), intent(in) :: name
    integer(int16), allocatable, intent(out) :: vals(:)
    integer :: stat
    type(variable_type) :: var

    var = inq_var(grp, name)
    if (allocated(vals)) deallocate (vals)
    allocate (vals(size_var(var)))
    stat = nc_get_var_short(grp%id, var%id, vals)
    call handle_error(stat, "nc_get_var_short")
  end subroutine get_var_name_int16

  module subroutine get_var_int16(var, vals)
    type(variable_type), intent(in) :: var
    integer(int16), allocatable, intent(out) :: vals(:)
    integer :: stat

    if (allocated(vals)) deallocate (vals)
    allocate (vals(size_var(var)))
    stat = nc_get_var_short(var%grp_id, var%id, vals)
    call handle_error(stat, "nc_get_var_short")
  end subroutine get_var_int16

  module subroutine get_var_name_int32(grp, name, vals)
    class(group_type), intent(inout) :: grp
    character(len=*), intent(in) :: name
    integer(int32), allocatable, intent(out) :: vals(:)
    integer :: stat
    type(variable_type) :: var

    var = inq_var(grp, name)
    if (allocated(vals)) deallocate (vals)
    allocate (vals(size_var(var)))
    stat = nc_get_var_int(grp%id, var%id, vals)
    call handle_error(stat, "nc_get_var_int")
  end subroutine get_var_name_int32

  module subroutine get_var_int32(var, vals)
    type(variable_type), intent(in) :: var
    integer(int32), allocatable, intent(out) :: vals(:)
    integer :: stat

    if (allocated(vals)) deallocate (vals)
    allocate (vals(size_var(var)))
    stat = nc_get_var_int(var%grp_id, var%id, vals)
    call handle_error(stat, "nc_get_var_int")
  end subroutine get_var_int32

  module subroutine get_var_name_int64(grp, name, vals)
    class(group_type), intent(inout) :: grp
    character(len=*), intent(in) :: name
    integer(int64), allocatable, intent(out) :: vals(:)
    integer :: stat
    type(variable_type) :: var

    var = inq_var(grp, name)
    if (allocated(vals)) deallocate (vals)
    allocate (vals(size_var(var)))
    stat = nc_get_var_longlong(grp%id, var%id, vals)
    call handle_error(stat, "nc_get_var_longlong")
  end subroutine get_var_name_int64

  module subroutine get_var_int64(var, vals)
    type(variable_type), intent(in) :: var
    integer(int64), allocatable, intent(out) :: vals(:)
    integer :: stat

    if (allocated(vals)) deallocate (vals)
    allocate (vals(size_var(var)))
    stat = nc_get_var_longlong(var%grp_id, var%id, vals)
    call handle_error(stat, "nc_get_var_longlong")
  end subroutine get_var_int64

  module subroutine get_var_name_real32(grp, name, vals)
    class(group_type), intent(inout) :: grp
    character(len=*), intent(in) :: name
    real(real32), allocatable, intent(out) :: vals(:)
    integer :: stat
    type(variable_type) :: var

    var = inq_var(grp, name)
    if (allocated(vals)) deallocate (vals)
    allocate (vals(size_var(var)))
    stat = nc_get_var_float(grp%id, var%id, vals)
    call handle_error(stat, "nc_get_var_float")
  end subroutine get_var_name_real32

  module subroutine get_var_real32(var, vals)
    type(variable_type), intent(in) :: var
    real(real32), allocatable, intent(out) :: vals(:)
    integer :: stat

    if (allocated(vals)) deallocate (vals)
    allocate (vals(size_var(var)))
    stat = nc_get_var_float(var%grp_id, var%id, vals)
    call handle_error(stat, "nc_get_var_float")
  end subroutine get_var_real32

  module subroutine get_var_name_real64(grp, name, vals)
    class(group_type), intent(inout) :: grp
    character(len=*), intent(in) :: name
    real(real64), allocatable, intent(out) :: vals(:)
    integer :: stat
    type(variable_type) :: var

    var = inq_var(grp, name)
    if (allocated(vals)) deallocate (vals)
    allocate (vals(size_var(var)))
    stat = nc_get_var_double(grp%id, var%id, vals)
    call handle_error(stat, "nc_get_var_double")
  end subroutine get_var_name_real64

  module subroutine get_var_real64(var, vals)
    type(variable_type), intent(in) :: var
    real(real64), allocatable, intent(out) :: vals(:)
    integer :: stat

    if (allocated(vals)) deallocate (vals)
    allocate (vals(size_var(var)))
    stat = nc_get_var_double(var%grp_id, var%id, vals)
    call handle_error(stat, "nc_get_var_double")
  end subroutine get_var_real64

  module subroutine destroy_variable(var)
    type(variable_type), intent(inout) :: var

    if (associated(var%grp_id)) deallocate (var%grp_id)
    call destroy(var%atts)
    call destroy(var%dims)
  end subroutine destroy_variable

end submodule submodule_variable
