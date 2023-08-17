module module_data_structure

  implicit none

  public :: pair_type, node_type, list_type, dictionary_type
  public :: size, append, scan, destroy
  public :: operator(.pair.)
  private

  integer, parameter :: NUM_BUCKETS = 50

  !> Key-value pair
  type :: pair_type
    character(:), allocatable :: key
    class(*), allocatable :: val
  end type pair_type

  !> Node type
  type :: node_type
    type(pair_type) :: pair
    type(node_type), pointer :: next => null()
    type(node_type), pointer :: prev => null()
  end type node_type

  !> List type
  type :: list_type
    integer :: length = 0
    type(node_type), pointer :: head => null()
    type(node_type), pointer :: tail => null()
  end type list_type

  !> Dictionary type
  type :: dictionary_type
    integer :: length = 0
    integer :: len = NUM_BUCKETS ! size of buckets
    type(list_type) :: buckets(NUM_BUCKETS)
  end type dictionary_type

  interface size
    module procedure :: size_list
    module procedure :: size_dictionary
  end interface size

  interface append
    module procedure :: append_list
    module procedure :: append_dictionary
  end interface append

  interface scan
    module procedure :: scan_list
    module procedure :: scan_dictionary
  end interface scan

  interface destroy
    module procedure :: destroy_pair
    module procedure :: destroy_list
    module procedure :: destroy_node
    module procedure :: destroy_dictionary
  end interface destroy

  interface operator(.pair.)
    module procedure :: new_pair
  end interface operator(.pair.)

  !> Interfaces to submodule
  interface
    pure module function new_pair(key, val) result(pair)
      character(*), intent(in) :: key
      class(*), intent(in) :: val
      type(pair_type) :: pair
    end function new_pair

    pure module subroutine destroy_pair(pair)
      type(pair_type), intent(inout) :: pair
    end subroutine destroy_pair

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

    module function scan_list(list, key) result(pair)
      type(list_type), intent(in), target :: list
      character(*), intent(in) :: key
      type(pair_type) :: pair
    end function scan_list

    module subroutine append_dictionary(dictionary, pair)
      type(dictionary_type), intent(inout) :: dictionary
      type(pair_type), intent(in) :: pair
    end subroutine append_dictionary

    elemental module function size_dictionary(dictionary) result(ret)
      type(dictionary_type), intent(in) :: dictionary
      integer :: ret
    end function size_dictionary

    module function scan_dictionary(dictionary, key) result(pair)
      type(dictionary_type), intent(in) :: dictionary
      character(*), intent(in) :: key
      type(pair_type) :: pair
    end function scan_dictionary

    module subroutine destroy_list(list)
      type(list_type), intent(inout) :: list
    end subroutine destroy_list

    module subroutine destroy_dictionary(dictionary)
      type(dictionary_type), intent(inout) :: dictionary
    end subroutine destroy_dictionary
  end interface
  
end module module_data_structure