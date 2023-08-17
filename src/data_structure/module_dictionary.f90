submodule(module_data_structure) submodule_dictionary
  implicit none
contains

  !> Hash function (djb2)
  pure function hash(key, num_items) result(hash_val)
    character(*), intent(in) :: key
    integer, intent(in) :: num_items
    integer :: hash_val, shift_val, shifted, i

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
      & error stop "Invalid hash value"
  end function hash

  !> Append to dictionary
  module subroutine append_dictionary(dictionary, pair)
    type(dictionary_type), intent(inout) :: dictionary
    type(pair_type), intent(in) :: pair
    integer :: idx

    idx = hash(pair%key, dictionary%len)
    call append_list(dictionary%buckets(idx), pair)
    dictionary%length = dictionary%length + 1
  end subroutine append_dictionary

  !> Size of a dictionary
  elemental module function size_dictionary(dictionary) result(ret)
    type(dictionary_type), intent(in) :: dictionary
    integer :: ret

    ret = dictionary%length
  end function size_dictionary

  !> Scan dictionary
  module function scan_dictionary(dictionary, key) result(pair)
    type(dictionary_type), intent(in) :: dictionary
    character(*), intent(in) :: key
    type(pair_type) :: pair
    integer :: idx

    idx = hash(key, dictionary%len)
    pair = scan_list(dictionary%buckets(idx), key)
  end function scan_dictionary

  !> Dictionary destructor
  module subroutine destroy_dictionary(dictionary)
    type(dictionary_type), intent(inout) :: dictionary
    integer :: i

    do i = 1, dictionary%len
      call destroy_list(dictionary%buckets(i))
    end do
  end subroutine destroy_dictionary

end submodule submodule_dictionary