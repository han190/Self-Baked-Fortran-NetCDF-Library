submodule(module_data_structure) submodule_list
implicit none
contains

pure module function new_pair(key, val) result(pair)
  character(len=*), intent(in) :: key
  integer, intent(in) :: val
  type(pair_type) :: pair

  pair%key = key
  pair%val = val
end function new_pair

pure module function new_node(pair) result(node)
  type(pair_type), intent(in) :: pair
  type(node_type) :: node

  node%pair = pair
end function new_node

pure module subroutine destroy_node(node)
  type(node_type), intent(inout) :: node

  nullify (node%prev, node%next)
end subroutine destroy_node

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

  list%len = list%len + 1
end subroutine append_list

elemental module function size_list(list) result(ret)
  type(list_type), intent(in) :: list
  integer :: ret

  ret = list%len
end function size_list

module function scan_list(list, key) result(val)
  type(list_type), intent(in), target :: list
  character(len=*), intent(in) :: key
  integer :: val
  type(node_type), pointer :: tmp

  tmp => list%head
  do while (associated(tmp))
    if (key == tmp%pair%key) then
      val = tmp%pair%val
      nullify (tmp)
      return
    end if
    tmp => tmp%next
  end do

  nullify(tmp)
  val = invalid
end function scan_list

module subroutine destroy_list(list)
  type(list_type), intent(inout) :: list
  type(node_type), pointer :: tmp

  do while (list%len > 0)
    tmp => list%head
    if (associated(tmp%next)) then
      nullify (tmp%next%prev)
      list%head  => tmp%next
    end if

    call destroy_node(tmp)
    deallocate (tmp)
    list%len = list%len - 1
  end do
end subroutine destroy_list

end submodule submodule_list