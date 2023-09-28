module module_data_structure
implicit none

public :: list_type, dict_type
public :: size, append, scan, destroy
public :: operator(.pair.), new_dict
public :: invalid
private

integer, parameter :: invalid = -2147483647

type :: pair_type
  character(:), allocatable :: key
  integer :: val
end type pair_type

type :: node_type
  type(pair_type) :: pair
  type(node_type), pointer :: next => null()
  type(node_type), pointer :: prev => null()
end type node_type

type :: list_type
  integer :: len = 0
  type(node_type), pointer :: head => null()
  type(node_type), pointer :: tail => null()
end type list_type

type :: dict_type
  integer :: len = 0
  integer :: num_buckets = 50
  type(list_type), allocatable :: buckets(:)
end type dict_type

interface append
  module procedure :: append_list
  module procedure :: append_dict
end interface append

interface size
  module procedure :: size_list
  module procedure :: size_dict
end interface size

interface scan
  module procedure :: scan_list
  module procedure :: scan_dict
end interface scan

interface destroy
  module procedure :: destroy_node
  module procedure :: destroy_list
  module procedure :: destroy_dict
end interface destroy

interface operator(.pair.)
  module procedure :: new_pair
end interface operator(.pair.)

interface
  pure module function new_pair(key, val) result(pair)
    character(len=*), intent(in) :: key
    integer, intent(in) :: val
    type(pair_type) :: pair
  end function new_pair

  pure module function new_node(pair) result(node)
    type(pair_type), intent(in) :: pair
    type(node_type) :: node
  end function new_node

  pure module subroutine destroy_node(node)
    type(node_type), intent(inout) :: node
  end subroutine destroy_node

  module subroutine append_list(list, pair)
    type(list_type), intent(inout) :: list
    type(pair_type), intent(in) :: pair
  end subroutine append_list

  elemental module function size_list(list) result(ret)
    type(list_type), intent(in) :: list
    integer :: ret
  end function size_list

  module function scan_list(list, key) result(val)
    type(list_type), intent(in), target :: list
    character(len=*), intent(in) :: key
    integer :: val
  end function scan_list

  module subroutine destroy_list(list)
    type(list_type), intent(inout) :: list
  end subroutine destroy_list

  module function new_dict(num_buckets) result(dict)
    integer, intent(in), optional :: num_buckets
    type(dict_type) :: dict
  end function new_dict

  module subroutine append_dict(dict, pair)
    type(dict_type), intent(inout) :: dict
    type(pair_type), intent(in) :: pair
  end subroutine append_dict

  elemental module function size_dict(dict) result(ret)
    type(dict_type), intent(in) :: dict
    integer :: ret
  end function size_dict

  module function scan_dict(dict, key) result(val)
    type(dict_type), intent(in) :: dict
    character(len=*), intent(in) :: key
    integer :: val
  end function scan_dict

  module subroutine destroy_dict(dict)
    type(dict_type), intent(inout) :: dict
  end subroutine destroy_dict
end interface

end module module_data_structure
