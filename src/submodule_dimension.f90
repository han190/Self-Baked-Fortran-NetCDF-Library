submodule(module_netcdf) submodule_dimension
  implicit none
contains

  !> Inquire group dimension
  module subroutine inq_grp_dims(grp)
    type(group_type), intent(inout) :: grp
    integer(c_int) :: stat, ndims, include_parents
    integer(c_int), allocatable :: dimids(:)
    integer(c_size_t), allocatable :: dimlens(:)
    character(kind=c_char, len=nc_max_name) :: tmp
    integer :: i

    !> inquire number of dimensions
    stat = nc_inq_ndims(grp%id, ndims)
    call handle_error(stat, "nc_inq_ndims")

    !> inquire dimension ids
    allocate (dimids(ndims), dimlens(ndims))
    stat = nc_inq_dimids(grp%id, ndims, dimids, include_parents)
    call handle_error(stat, "nc_inq_dimids")

    !> inquire dimension lengths and names
    if (allocated(grp%dims)) deallocate (grp%dims)
    allocate (grp%dims(ndims))

    !> Iteratively copy info to dim
    do i = 1, ndims
      associate (dim => grp%dims(i))
        dim%id = dimids(i)

        stat = nc_inq_dimlen(grp%id, dim%id, dim%length)
        call handle_error(stat, "nc_inq_dimlen")

        stat = nc_inq_dimname(grp%id, dim%id, tmp)
        call handle_error(stat, "nc_inq_dimname")
        dim%name = strip(tmp)
      end associate
    end do
  end subroutine inq_grp_dims

  !> Inquire variable dimensions
  module subroutine inq_var_dims(grp, var)
    type(group_type), target, intent(inout) :: grp
    type(variable_type), intent(inout) :: var
    integer(c_int) :: stat, ndims
    integer(c_int), allocatable :: dimids(:), grp_dimids(:)
    integer :: i, loc

    !> inquire number of dimensions
    stat = nc_inq_varndims(grp%id, var%id, ndims)
    call handle_error(stat, "nc_inq_varndims")
    var%dims%length = ndims

    !> inquire variable dimension ids
    if (allocated(var%dims%ptrs)) deallocate (var%dims%ptrs)
    allocate (var%dims%ptrs(ndims), dimids(ndims))
    stat = nc_inq_vardimid(grp%id, var%id, dimids)
    call handle_error(stat, "nc_inq_vardimid")
    
    grp_dimids = [(grp%dims(i)%id, i=1, size(grp%dims))]
    do i = 1, ndims
      loc = findloc(grp_dimids, dimids(i), dim=1)
      if (loc >= 1) then
        var%dims%ptrs(i)%ptr => grp%dims(loc)
      else
        error stop "Invalid location."
      end if
    end do
  end subroutine inq_var_dims

  !> shape of dimensions
  module function shape_dims(dims) result(ret)
    type(dimensions_type), intent(in) :: dims
    integer(int64), allocatable :: ret(:)
    integer :: i, lbnd, ubnd

    if (allocated(ret)) deallocate(ret)
    lbnd = lbound(dims%ptrs, dim=1)
    ubnd = ubound(dims%ptrs, dim=1)
    ret = [(dims%ptrs(i)%ptr%length, i=lbnd, ubnd)]
  end function shape_dims

  !> size of dimensions
  module function size_dims(dims) result(ret)
    type(dimensions_type), intent(in) :: dims
    integer(int64) :: ret
    integer :: i

    ret = 1
    do i = 1, dims%length
      if (associated(dims%ptrs(i)%ptr)) then
        ret = ret*dims%ptrs(i)%ptr%length
      else
        error stop "Invalid dimension pointer."
      end if
    end do
  end function size_dims

  !> rank of dimensions
  module function rank_dims(dims) result(ret)
    type(dimensions_type), intent(in) :: dims
    integer(int64) :: ret

    ret = dims%length
  end function rank_dims

end submodule submodule_dimension