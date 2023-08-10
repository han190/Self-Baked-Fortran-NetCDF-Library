submodule(module_netcdf) submodule_io
  implicit none
  integer, parameter :: line_width = 50
  integer, parameter :: num_indents = 3
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

  module subroutine write_formatted_attribute( &
    & attribute, unit, iotype, v_list, iostat, iomsg)
    class(attribute_type), intent(in) :: attribute
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list (:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    character(len=:), allocatable :: fmt, tmp, type_name, indent

    !> Indent spaces
    if (size(v_list) == 0) then
      indent = ""
    else
      indent = repeat(" ", v_list(1))
    end if

    if (iotype == "DT" .or. iotype == "LISTDIRECTED") then
      type_name = get_type_name_(attribute%type)
      select type (val_ => attribute%values)
      type is (character(*))

        tmp = strip(val_(1))
        fmt = "(a,1x,a,':',1x,a,/)"
        if (len(tmp) + len(attribute%name) + 4 >= line_width) then
          tmp = tmp(1:line_width - len(attribute%name) - 7)
          write (unit, fmt) indent//attribute%name, type_name, '"'//tmp//'..."'
        else
          write (unit, fmt) indent//attribute%name, type_name, '"'//tmp//'"'
        end if

      type is (integer(int16))

        fmt = "(a,1x,a,':',1x,i0, "
        if (size(val_) > 1) then
          fmt = fmt//"'...', /)"
        else
          fmt = fmt//"/)"
        end if
        write (unit, fmt) indent//attribute%name, type_name, val_(1)

      type is (integer(int32))

        fmt = "(a,1x,a,':',1x,i0, "
        if (size(val_) > 1) then
          fmt = fmt//"'...', /)"
        else
          fmt = fmt//"/)"
        end if
        write (unit, fmt) indent//attribute%name, type_name, val_(1)

      type is (integer(int64))

        fmt = "(a,1x,a,':',1x,i0, "
        if (size(val_) > 1) then
          fmt = fmt//"'...', /)"
        else
          fmt = fmt//"/)"
        end if
        write (unit, fmt) indent//attribute%name, type_name, val_(1)

      type is (real(real32))

        fmt = "(a,1x,a,':',1x,e10.3, "
        if (size(val_) > 1) then
          fmt = fmt//"'...', /)"
        else
          fmt = fmt//"/)"
        end if
        write (unit, fmt) indent//attribute%name, type_name, val_(1)

      type is (real(real64))

        fmt = "(a,1x,a,':',1x,e10.3, "
        if (size(val_) > 1) then
          fmt = fmt//"'...', /)"
        else
          fmt = fmt//"/)"
        end if
        write (unit, fmt) indent//attribute%name, type_name, val_(1)

      end select
    end if
  end subroutine write_formatted_attribute

  module subroutine write_formatted_dimension( &
    & dimension_, unit, iotype, v_list, iostat, iomsg)
    class(dimension_type), intent(in) :: dimension_
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list (:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    character(len=:), allocatable :: indent

    !> Indent spaces
    if (size(v_list) == 0) then
      indent = ""
    else
      indent = repeat(" ", v_list(1))
    end if

    if (iotype == "DT" .or. iotype == "LISTDIRECTED") then
      if (dimension_%is_unlimited) then
        write (unit, "(a, ':', 1x, i0, ' (unlimited)', /)") &
          & indent//dimension_%name, dimension_%length
      else
        write (unit, "(a, ':', 1x, i0, /)") &
          & indent//dimension_%name, dimension_%length
      end if
    end if
  end subroutine write_formatted_dimension

  module subroutine write_formatted_variable( &
    & variable, unit, iotype, v_list, iostat, iomsg)
    class(variable_type), intent(in) :: variable
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list (:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    integer :: i
    character(len=:), allocatable :: fmt, dim_str
    integer, allocatable :: v_list_(:)

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

      if (size(v_list) == 0) then
        v_list_ = [0]
      else
        v_list_ = v_list
      end if

      fmt = "(a, 1x, a, 1x, a, /)"
      write (unit, fmt) repeat(" ", v_list_(1))//variable%name, &
        & get_type_name_(variable%type), dim_str

      v_list_ = v_list_ + num_indents
      do i = 1, size(variable%attributes)
        associate (att => variable%attributes(i))
          call write_formatted_attribute( &
            & att, unit, iotype, v_list_, iostat, iomsg)
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
    integer, allocatable :: v_list_(:)

    if (size(v_list) == 0) then
      v_list_ = [num_indents]
    else
      v_list_ = v_list
    end if

    if (iotype == "DT" .or. iotype == "LISTDIRECTED") then
      write (unit, "(a,1x,'(',a,')',':')") "GROUP", group%name
      write (unit, "(/,a,/)") "DIMENSIONS:"
      do i = 1, size(group%dimensions)
        associate (dim => group%dimensions(i))
          call write_formatted_dimension( &
            & dim, unit, iotype, v_list_, iostat, iomsg)
        end associate
      end do

      if (allocated(group%variables)) then
        write (unit, "(/,a,/)") "VARIABLES:"
        do i = 1, size(group%variables)
          associate (var => group%variables(i))
            call write_formatted_variable( &
              & var, unit, iotype, v_list_, iostat, iomsg)
          end associate
        end do
      end if

      if (allocated(group%attributes)) then
        write (unit, "(/,a,/)") "ATTRIBUTES:"
        do i = 1, size(group%attributes)
          associate (att => group%attributes(i))
            call write_formatted_attribute( &
              & att, unit, iotype, v_list_, iostat, iomsg)
          end associate
        end do
      end if
    end if
  end subroutine write_formatted_group

end submodule submodule_io
