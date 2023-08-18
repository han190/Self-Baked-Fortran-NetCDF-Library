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

  !> Pair destructor
  pure module subroutine destroy_pair(pair)
    type(pair_type), intent(inout) :: pair

    if (allocated(pair%key)) deallocate (pair%key)
    if (allocated(pair%val)) deallocate (pair%val)
  end subroutine destroy_pair

  !> Node constructor
  pure module function new_node(pair) result(node)
    type(pair_type), intent(in) :: pair
    type(node_type) :: node

    node%pair = new_pair(pair%key, pair%val)
  end function new_node

  !> Node destructor
  pure module subroutine destroy_node(node)
    type(node_type), intent(inout) :: node

    call destroy_pair(node%pair)
    nullify (node%prev, node%next)
  end subroutine destroy_node

  !> Append list
  module subroutine append_list(list, pair)
    type(list_type), intent(inout) :: list
    type(pair_type), intent(in) :: pair

    if (associated(list%tail)) then
      allocate (list%tail%next, source=new_node(pair))
      list%tail%next%prev => list%tail
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
        nullify (current)
        return
      end if
      current => current%next
    end do

    !> Pointer does not auto deallocate
    nullify (current)
    error stop "key: "//key//" not found."
  end function scan_list

  !> List destructor
  module subroutine destroy_list(list)
    type(list_type), intent(inout) :: list
    type(node_type), pointer :: current

    do while (list%length > 0)
      current => list%head
      if (associated(current%next)) then
        nullify (current%next%prev)
        list%head => current%next
      end if

      call destroy_node(current)
      deallocate (current)
      list%length = list%length - 1
    end do
  end subroutine destroy_list

end submodule submodule_list