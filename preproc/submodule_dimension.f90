submodule(module_interface) submodule_dimension
implicit none
contains

!> Dimension constructor
module pure function new_dim(name, len) result(dim)
  character(len=*), intent(in) :: name
  integer, intent(in) :: len
  type(dimension_type) :: dim

  dim%name = name
  dim%len = len
end function new_dim

!> Dimensions constructor
module function new_dims(dims) result(ret)
  type(dimension_type), intent(in) :: dims(:)
  type(dimension_type), allocatable :: ret(:)
  integer :: i

  ret = dims
  do i = 1, size(ret)
    ret(i)%ID = i - 1
  end do
end function new_dims

!> Dimensions contructor (with one-based unlimited dimension specified)
module function new_dims_unlim(dims, unlim_dim) result(ret)
  type(dimension_type), intent(in) :: dims(:)
  integer, intent(in) :: unlim_dim
  type(dimension_type), allocatable :: ret(:)
  integer :: i

  ret = dims
  do i = 1, size(ret)
    ret(i)%ID = i - 1
    ret(i)%is_unlim = ret(i)%ID == unlim_dim - 1
  end do
end function new_dims_unlim

end submodule submodule_dimension