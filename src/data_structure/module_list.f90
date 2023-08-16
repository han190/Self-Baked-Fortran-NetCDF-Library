submodule(module_data_structure) submodule_list
  implicit none
contains

  !> Pair contructor
  pure module function new_pair(key, val) result(pair)
    character(*), intent(in) :: key
    class(*), intent(in) :: val
    type(pair_type) :: pair

    pair%key = key
    if (allocated(pair%val)) deallocate (pair%val)
    allocate (pair%val, source=val)
  end function new_pair

  !> Node constructor
  pure module function new_node(pair) result(node)
    type(pair_type), intent(in) :: pair
    type(node_type) :: node

    node%pair = new_pair(pair%key, pair%val)
  end function new_node

  !> Append list
  module subroutine append_list(list, pair)
    type(list_type), intent(inout) :: list
    type(pair_type), intent(in) :: pair

    if (associated(list%tail)) then
      allocate (list%tail%next, source=new_node(pair))
      list%tail => list%tail%next
    else
      allocate (list%head, source=new_node(pair))
      list%tail => list%head
    end if

    list%length = list%length + 1
  end subroutine append_list

  !> Size of list
  elemental module function size_list(list) result(ret)
    type(list_type), intent(in) :: list
    integer :: ret

    ret = list%length
  end function size_list

  !> Scan list for a specific key
  module function scan_list(list, key) result(pair)
    type(list_type), intent(in), target :: list
    character(*), intent(in) :: key
    type(pair_type) :: pair
    type(node_type), pointer :: current

    current => list%head
    do while (associated(current))
      if (key == current%pair%key) then
        pair = current%pair
        return
      end if
      current => current%next
    end do

    !> If not found
    pair%key = "NaN"

    !> Pointer does not auto deallocate
    nullify (current)
  end function scan_list

end submodule submodule_list