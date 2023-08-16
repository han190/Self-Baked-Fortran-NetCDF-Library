submodule(module_interface) submodule_dimension
  implicit none
contains

  !> Define group dimension
  function def_grp_dim(grp, name, ndim) result(dim)
    type(group_type), intent(in) :: grp
    character(len=*), intent(in) :: name
    integer, intent(in) :: ndim
    type(dimension_type) :: dim
    integer(c_int) :: stat

    dim%name = trim(name)
    dim%length = ndim
    stat = nc_def_dim(grp%id, to_cstr(dim%name), &
      & int(ndim, c_size_t), dim%id)
    call handle_error(stat, "nc_def_dim")
  end function def_grp_dim

  !> Define group dimensions
  module subroutine def_grp_dims(grp, names, ndims)
    type(group_type), intent(inout) :: grp
    character(len=*), intent(in) :: names(:)
    integer, intent(in) :: ndims(:)
    integer(c_int) :: stat, i
    type(dimension_type), target :: dim
    class(*), pointer :: item

    if (size(names) /= size(ndims)) &
      & error stop "size(names) /= size(ndims)"

    do i = 1, size(names)
      dim = def_grp_dim(grp, names(i), ndims(i))
      item => dim
      call append(grp%dims, dim%name.pair.item)
    end do
    nullify (item)
  end subroutine def_grp_dims

  !> Inquire group dimension
  module subroutine inq_grp_dims(grp)
    type(group_type), intent(inout) :: grp
    integer(c_int) :: stat, ndims, include_parents, unlim_dim, i
    integer(c_int), allocatable :: dimids(:)
    integer(c_size_t), allocatable :: dimlens(:)
    character(kind=c_char, len=nc_max_name) :: tmp
    type(dimension_type), target :: dim
    class(*), pointer :: item

    !> inquire number of dimensions
    stat = nc_inq_ndims(grp%id, ndims)
    call handle_error(stat, "nc_inq_ndims")

    !> inquire dimension ids
    allocate (dimids(ndims), dimlens(ndims))
    stat = nc_inq_dimids(grp%id, ndims, dimids, include_parents)
    call handle_error(stat, "nc_inq_dimids")

    !> inquire unlimited dimension
    stat = nc_inq_unlimdim(grp%id, unlim_dim)
    call handle_error(stat, "nc_inq_unlimdim")

    !> Iteratively copy info to dim
    do i = 1, ndims
      dim%id = dimids(i)
      stat = nc_inq_dimlen(grp%id, dim%id, dim%length)
      call handle_error(stat, "nc_inq_dimlen")

      stat = nc_inq_dimname(grp%id, dim%id, tmp)
      call handle_error(stat, "nc_inq_dimname")
      dim%name = strip(tmp)
      dim%is_unlimited = dim%id == unlim_dim

      item => dim
      call append(grp%dims, dim%name.pair.item)
    end do
    nullify (item)
  end subroutine inq_grp_dims

  !> Inquire variable dimensions
  module subroutine inq_var_dims(grp, var)
    type(group_type), target, intent(inout) :: grp
    type(variable_type), intent(inout) :: var
    integer(c_int) :: stat, ndims, i, unlim_dim
    integer(c_int), allocatable :: dimids(:)
    character(kind=c_char, len=nc_max_name) :: tmp
    type(dimension_type), target :: dim
    class(*), pointer :: item

    !> inquire number of dimensions
    stat = nc_inq_varndims(grp%id, var%id, ndims)
    call handle_error(stat, "nc_inq_varndims")

    !> inquire variable dimension ids
    allocate (dimids(ndims))
    stat = nc_inq_vardimid(grp%id, var%id, dimids)
    call handle_error(stat, "nc_inq_vardimid")

     !> inquire unlimited dimension
    stat = nc_inq_unlimdim(grp%id, unlim_dim)
    call handle_error(stat, "nc_inq_unlimdim")

    do i = 1, ndims
      dim%id = dimids(i)
      stat = nc_inq_dimlen(grp%id, dim%id, dim%length)
      call handle_error(stat, "nc_inq_dimlen")

      stat = nc_inq_dimname(grp%id, dim%id, tmp)
      call handle_error(stat, "nc_inq_dimname")
      dim%name = strip(tmp)
      dim%is_unlimited = dim%id == unlim_dim

      item => dim
      call append(var%dims, dim%name.pair.item)
    end do
    nullify (item)
  end subroutine inq_var_dims

  !> shape of dimensions
  module function shape_dict(dict) result(ret)
    type(dictionary_type), intent(in) :: dict
    integer(int64), allocatable :: ret(:)
    type(node_type), pointer :: current
    integer :: i, j

    if (allocated(ret)) then
      deallocate (ret)
    end if

    if (dict%length == 0) then
      ret = [0]
      return 
    end if

    allocate (ret(dict%length))
    j = 1
    do i = 1, dict%len
      current => dict%buckets(i)%head
      do while (associated(current))
        select type (x => current%pair%val)
        type is (dimension_type)
          ret(j) = x%length
          j = j + 1
          if (j > dict%length) return
        type is (attribute_type)
          ret(j) = x%length
          j = j + 1
          if (j > dict%length) return
        end select
        current => current%next
      end do
    end do
    nullify (current)
  end function shape_dict

  !> size of dimensions
  ! module function size_dims(dims) result(ret)
  !   type(dictionary_type), intent(in) :: dims
  !   integer(int64) :: ret

  !   ret = product(shape(dims))
  ! end function size_dims

  ! !> rank of dimensions
  ! module function rank_dims(dims) result(ret)
  !   type(dictionary_type), intent(in) :: dims
  !   integer(int64) :: ret

  !   ret = dims%length
  ! end function rank_dims

end submodule submodule_dimension
