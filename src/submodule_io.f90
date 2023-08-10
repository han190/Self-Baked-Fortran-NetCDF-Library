submodule(module_netcdf) submodule_io
  implicit none
contains

  pure function get_type_name_(type_id) result(type_name)
    integer(c_int), intent(in) :: type_id
    character(:), allocatable :: type_name

    select case (type_id)
    case (nc_nat)
      type_name = "not a type."
    case (nc_byte)
      type_name = "byte"
    case (nc_char)
      type_name = "char"
    case (nc_short)
      type_name = "short"
    case (nc_int)
      type_name = "int"
    ! case (nc_long)
    !   type_name = "32-bit integer (deprecated)"
    case (nc_float)
      type_name = "float"
    case (nc_double)
      type_name = "double"
    case (nc_int64)
      type_name = "int64"
    case default
      error stop "Not supported type id."
    end select

    type_name = "<"//type_name//">"
  end function get_type_name_

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
    character(len=:), allocatable :: fmt, dim_str, tmp, type_name
    integer, parameter :: line_width = 50

    if (iotype == "DT" .or. iotype == "LISTDIRECTED") then

      !> Construct a character for dimensions
      !> Yes, C starts from zero.
      dim_str = ""
      do i = 0, size(variable%dimensions) - 1
        associate (dim => variable%dimensions(i))
          dim_str = dim_str//dim%name//":"//int2char(dim%length)//", "
        end associate
      end do
      dim_str = "("//dim_str(1:len(dim_str) - 2)//")"

      fmt = "(a, 1x, a, 1x, a, /)"
      write (unit, fmt) variable%name, get_type_name_(variable%type), dim_str

      do i = 1, size(variable%attributes)
        associate (att => variable%attributes(i))
          type_name = get_type_name_(att%type)
          select type (val_ => att%values)
          type is (character(*))

            tmp = strip(val_(1), num_chars)
            fmt = "(4x,a,1x,a,':',1x,a,/)"
            if (len(tmp) + len(att%name) + 4 >= line_width) then
              tmp = tmp(1:line_width - len(att%name) - 7)
              write (unit, fmt) att%name, type_name, '"'//tmp//'..."'
            else
              write (unit, fmt) att%name, type_name, '"'//tmp//'"'
            end if

          type is (integer(int16))

            fmt = "(4x,a,1x,a,':',1x,i0, "
            if (size(val_) > 1) then
              fmt = fmt//"'...', /)"
            else
              fmt = fmt//"/)"
            end if
            write (unit, fmt) att%name, type_name, val_(1)

          type is (integer(int32))

            fmt = "(4x,a,1x,a,':',1x,i0, "
            if (size(val_) > 1) then
              fmt = fmt//"'...', /)"
            else
              fmt = fmt//"/)"
            end if
            write (unit, fmt) att%name, type_name, val_(1)

          type is (integer(int64))

            fmt = "(4x,a,1x,a,':',1x,i0, "
            if (size(val_) > 1) then
              fmt = fmt//"'...', /)"
            else
              fmt = fmt//"/)"
            end if
            write (unit, fmt) att%name, type_name, val_(1)

          type is (real(real32))

            fmt = "(4x,a,1x,a,':',1x,e10.3, "
            if (size(val_) > 1) then
              fmt = fmt//"'...', /)"
            else
              fmt = fmt//"/)"
            end if
            write (unit, fmt) att%name, type_name, val_(1)

          type is (real(real64))

            fmt = "(4x,a,1x,a,':',1x,e10.3, "
            if (size(val_) > 1) then
              fmt = fmt//"'...', /)"
            else
              fmt = fmt//"/)"
            end if
            write (unit, fmt) att%name, type_name, val_(1)
            
          end select
        end associate
      end do

    end if
  end subroutine write_formatted_variable

  module subroutine write_formatted_group( &
    & group, unit, iotype, v_list, iostat, iomsg)
    class(group_type), intent(in) :: group
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list (:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    integer :: i

    if (iotype == "DT" .or. iotype == "LISTDIRECTED") then
      write (unit, "(a,/)") "VARIABLES:"
      do i = 1, size(group%variables)
        associate (var => group%variables(i))
          call write_formatted_variable( &
            & var, unit, iotype, v_list, iostat, iomsg)
        end associate
      end do
    end if
  end subroutine write_formatted_group

end submodule submodule_io