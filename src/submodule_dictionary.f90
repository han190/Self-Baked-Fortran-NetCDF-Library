submodule(module_data_structure) submodule_dictionary
implicit none
contains

pure function hash(key, num_items) result(hash_val)
  character(len=*), intent(in) :: key
  integer, intent(in) :: num_items
  integer :: hash_val
  integer :: shift_val, shifted, i

  !> Start from an arbitrary prime
  hash_val = 33
  shift_val = 5

  do i = 1, len(key)
    shifted = ishft(hash_val, shift_val)
    hash_val = hash_val + shifted + iachar(key(i:i))
  end do

  !> Put hash value into one of the buckets.
  if (hash_val < 0) hash_val = -hash_val
  hash_val = mod(hash_val, num_items) + 1

  if (hash_val < 1 .or. hash_val > num_items) &
    & error stop "[hash] Invalid hash value"
end function hash

module function new_dict(num_buckets) result(dict)
  integer, intent(in), optional :: num_buckets
  type(dict_type) :: dict

  if (present(num_buckets)) then
    dict%num_buckets = num_buckets
  else
    dict%num_buckets = 50
  end if
  if (allocated(dict%buckets)) deallocate (dict%buckets)
  allocate (dict%buckets(dict%num_buckets))
end function new_dict

module subroutine append_dict(dict, pair)
  type(dict_type), intent(inout) :: dict
  type(pair_type), intent(in) :: pair
  integer :: loc

  loc = hash(pair%key, dict%num_buckets)
  call append_list(dict%buckets(loc), pair)
  dict%len = dict%len + 1
end subroutine append_dict

elemental module function size_dict(dict) result(ret)
  type(dict_type), intent(in) :: dict
  integer :: ret

  ret = dict%len
end function size_dict

module function scan_dict(dict, key) result(val)
  type(dict_type), intent(in) :: dict
  character(len=*), intent(in) :: key
  integer :: val
  integer :: loc

  loc = hash(key, dict%num_buckets)
  val = scan_list(dict%buckets(loc), key)
end function scan_dict

module subroutine destroy_dict(dict)
  type(dict_type), intent(inout) :: dict
  integer :: i

  do i = 1, dict%len
    call destroy_list(dict%buckets(i))
  end do
  deallocate (dict%buckets)
end subroutine destroy_dict

end submodule submodule_dictionary