module module_data_structure

  use :: module_types
  implicit none

  public :: pair_type, list_type, dictionary_type
  public :: operator(.pair.), operator(.node.)
  public :: size, scan, append
  private

  !> Pair constructor
  interface operator(.pair.)
    module procedure :: new_pair
  end interface operator(.pair.)

  !> Node constructor
  interface operator(.node.)
    module procedure :: new_node
  end interface operator(.node.)

  !> Size of a list
  interface size
    module procedure :: size_list
    module procedure :: size_dictionary
  end interface size

  !> Scan a list
  interface scan
    module procedure :: scan_list
    module procedure :: scan_dictionary
  end interface scan

  interface append
    module procedure :: append_list
    module procedure :: append_dictionary
  end interface append

contains

  !> Pair constructor
  pure function new_pair(key, value) result(pair)
    character(len=*), intent(in) :: key
    class(netcdf_type), allocatable, intent(in) :: value
    type(pair_type) :: pair

    pair%key = key
    if (allocated(pair%value)) deallocate (pair%value)
    allocate (pair%value, source=value)
  end function new_pair

  !> Node constructor
  pure function new_node(pair) result(node)
    type(pair_type), intent(in) :: pair
    type(node_type) :: node
    
    node%pair = new_pair(pair%key, pair%value)
  end function new_node

  !> Append list
  subroutine append_list(list, pair)
    type(list_type), intent(inout) :: list
    type(pair_type), intent(in) :: pair

    if (associated(list%tail)) then
      allocate (list%tail%next, source=.node.pair)
      list%tail => list%tail%next
    else
      allocate(list%head, source=.node.pair)
      list%tail => list%head
    end if

    list%length = list%length + 1
  end subroutine append_list

  !> Size of list
  elemental function size_list(list) result(ret)
    type(list_type), intent(in) :: list
    integer :: ret

    ret = list%length
  end function size_list

  !> Find node by the character key of the pair
  function scan_list(list, key) result(pair)
    type(list_type), intent(in), target :: list
    character(len=*), intent(in) :: key
    type(pair_type) :: pair
    type(node_type), pointer :: current_node

    current_node => list%head
    do while (associated(current_node))
      if (key == current_node%pair%key) then
        pair = current_node%pair
        return
      end if
      current_node => current_node%next
    end do

    !> If not found
    pair%key = "NaN"

    !> Pointer does not auto deallocate
    nullify (current_node)
  end function scan_list

  !> Hash function (djb2)
  pure function hash(key, num_items) result(hash_value)
    character(len=*), intent(in) :: key
    integer, intent(in) :: num_items
    integer :: hash_value
    integer :: i, shft_value

    !> Start from an arbitrary large prime
    hash_value = 5381
    shft_value = 5

    do i = 1, len(key)
      associate (shifted => ishft(hash_value, shft_value))
        hash_value = hash_value + &
          & shifted + iachar(key(i:i))
      end associate
    end do

    !> Put hash value into one of the buckets.
    if (hash_value < 0) hash_value = -hash_value 
    hash_value = mod(hash_value, num_items) + 1

    if (hash_value < 1 .or. hash_value > num_items) &
      & error stop "Invalid hash value"
  end function hash

  !> Append to dictionary
  subroutine append_dictionary(dictionary, pair)
    type(dictionary_type(len=*)), intent(inout) :: dictionary
    type(pair_type), intent(in) :: pair
    integer :: idx

    idx = hash(pair%key, dictionary%len)
    call append_list(dictionary%items(idx), pair)
    dictionary%length = dictionary%length + 1
  end subroutine append_dictionary

  !> Size of a dictionary
  pure function size_dictionary(dictionary) result(ret)
    type(dictionary_type(len=*)), intent(in) :: dictionary
    integer :: ret

    ret = dictionary%length
  end function size_dictionary

  !> Scan dictionary
  function scan_dictionary(dictionary, key) result(pair)
    type(dictionary_type(len=*)), intent(in) :: dictionary
    character(len=*), intent(in) :: key
    type(pair_type) :: pair
    integer :: idx

    idx = hash(key, dictionary%len)
    pair = scan_list(dictionary%items(idx), key)
  end function scan_dictionary

end module module_data_structure