submodule(module_netcdf) submodule_io
  implicit none
contains

  pure function id_name(type_id) result(type_name)
    integer(c_int), intent(in) :: type_id
    character(:), allocatable :: type_name

    select case (type_id)
    case (nc_nat)
      type_name = "not a type."
    case (nc_byte)
      type_name = "signed 1 byte integer"
    case (nc_char)
      type_name = "iso/ascii character"
    case (nc_short)
      type_name = "16-bit integer"
    case (nc_int)
      type_name = "32-bit integer"
    ! case (nc_long)
    !   type_name = "32-bit integer (deprecated)"
    case (nc_float)
      type_name = "32-bit float"
    case (nc_double)
      type_name = "64-bit float"
    case (nc_int64)
      type_name = "64-bit integer"
    case default
      error stop "Not supported type id."
    end select
  end function id_name

  elemental function num_digits(value) result(ret)
    integer(int64), intent(in) :: value
    integer(int64) :: ret

    ret = merge(1, floor(log10(real(value))) + 1, &
      & abs(value - 0.) < tiny(0.))
  end function num_digits

  pure function int2char(value) result(ret)
    integer(int64), intent(in) :: value
    character(:), allocatable :: ret

    if (allocated(ret)) deallocate(ret)
    allocate (character(len=num_digits(value)) :: ret)
    write (ret, "(i0)") value
  end function int2char

  module subroutine write_formatted_variable( &
    & variable, unit, iotype, v_list, iostat, iomsg)
    class(variable_type), intent(in) :: variable
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list (:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    integer :: i
    character(len=:), allocatable :: fmt, tmp

    if (iotype == "DT" .or. iotype == "LISTDIRECTED") then

      !> Construct a character for dimensions
      !> Yes, C starts from zero.
      tmp = ""
      do i = 0, size(variable%dimensions) - 1
        associate (dim => variable%dimensions(i))
          tmp = tmp//dim%name//":"//int2char(dim%length)//", "
        end associate
      end do
      tmp = "("//tmp(1:len(tmp) - 2)//")"

      fmt = "(a, 1x, '<', a, '>', 1x, a, /)"
      write (unit, fmt) variable%name, id_name(variable%type), tmp

      do i = 1, size(variable%attributes)
        associate (att => variable%attributes(i))
          write (unit, "(4x, 'Attribute:', 1x, a, /)") att%name
        end associate
      end do
    end if
  end subroutine write_formatted_variable

end submodule submodule_io