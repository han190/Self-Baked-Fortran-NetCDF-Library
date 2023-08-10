submodule(module_netcdf) submodule_io

  implicit none
  integer, parameter :: line_width = 50
  integer, parameter :: num_indents = 2
  character(len=*), parameter :: space = " "

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

  pure function split_string(string, width) result(ret)
    character(len=*), intent(in) :: string
    integer, intent(in) :: width
    character(len=:), allocatable :: ret(:)
    integer :: num_lines, i, n

    num_lines = len(string)/width + 1
    if (allocated(ret)) deallocate (ret)
    allocate (character(len=nc_max_char) :: ret(num_lines))

    n = 0
    do i = 1, num_lines, width
      if (i + width - 1 > num_lines) then
        ret(i) = adjustl(adjustr(string(i:)))
        n = n + 1
        exit
      else
        ret(i) = adjustl(adjustr(string(i:i + width - 1)))
        n = n + 1
      end if
    end do
    ! ret = ret(1:n)
  end function split_string

  pure function indent_level(string, level) result(ret)
    character(len=*), intent(in) :: string
    integer, intent(in) :: level
    character(:), allocatable :: ret

    ret = repeat(space, num_indents*level)//string
  end function indent_level

  module subroutine write_formatted_attribute( &
    & attribute, unit, iotype, v_list, iostat, iomsg)
    class(attribute_type), intent(in) :: attribute
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list (:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    character(len=:), allocatable :: fmt, tmp, att_type, att_name
    integer :: i, j, level

    !> Indent spaces
    if (size(v_list) == 0) then
      level = 0
    else
      level = v_list(1)
    end if

    if (iotype == "DT" .or. iotype == "LISTDIRECTED") then
      att_type = get_type_name_(attribute%type)
      att_name = indent_level(attribute%name, level)

      select type (val_ => attribute%values)
      type is (character(*))

        if (size(val_) > 1) then
          !!TODO
          ! write (unit, "(a,/)") val_
        else
          tmp = att_name//" "//att_type//' "'//strip(val_(1))//'"'
          if (len(tmp) > line_width) then
            !!TODO
            ! write (unit, "(a,/)") tmp
          else
            write (unit, "(a,/)") tmp
          end if
        end if

      type is (integer(int16))

        fmt = "(a,1x,a,1x,i0, "
        if (size(val_) > 1) then
          fmt = fmt//"'...', /)"
        else
          fmt = fmt//"/)"
        end if
        write (unit, fmt) att_name, att_type, val_(1)

      type is (integer(int32))

        fmt = "(a,1x,a,1x,i0, "
        if (size(val_) > 1) then
          fmt = fmt//"'...', /)"
        else
          fmt = fmt//"/)"
        end if
        write (unit, fmt) att_name, att_type, val_(1)

      type is (integer(int64))

        fmt = "(a,1x,a,1x,i0, "
        if (size(val_) > 1) then
          fmt = fmt//"'...', /)"
        else
          fmt = fmt//"/)"
        end if
        write (unit, fmt) att_name, att_type, val_(1)

      type is (real(real32))

        fmt = "(a,1x,a,1x,e10.3, "
        if (size(val_) > 1) then
          fmt = fmt//"'...', /)"
        else
          fmt = fmt//"/)"
        end if
        write (unit, fmt) att_name, att_type, val_(1)

      type is (real(real64))

        fmt = "(a,1x,a,1x,e10.3, "
        if (size(val_) > 1) then
          fmt = fmt//"'...', /)"
        else
          fmt = fmt//"/)"
        end if
        write (unit, fmt) att_name, att_type, val_(1)

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
    integer :: level
    character(len=:), allocatable :: fmt, dim_name

    !> Indent level
    level = merge(0, v_list(1), size(v_list) == 0)

    if (iotype == "DT" .or. iotype == "LISTDIRECTED") then
      associate (dim => dimension_)
        dim_name = indent_level(dim%name, level)
        if (dim%is_unlimited) then
          fmt = "(a, ':', 1x, i0, ' (unlimited)', /)"
        else
          fmt = "(a, ':', 1x, i0, /)"
        end if
        write (unit, fmt) dim_name, dim%length
      end associate
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
    integer :: level

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
        v_list_ = [1]
      else
        v_list_ = v_list
      end if
      level = v_list_(1)

      fmt = "(a, 1x, a, 1x, a, /)"
      write (unit, fmt) indent_level(variable%name, level), &
        & get_type_name_(variable%type), dim_str

      v_list_ = v_list_ + 1
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
      v_list_ = [1]
    else
      v_list_ = v_list
    end if

    if (iotype == "DT" .or. iotype == "LISTDIRECTED") then
      write (unit, "(a,1x,'(',a,')',':',/)") "GROUP", group%name
      write (unit, "(a,/)") "DIMENSIONS:"
      do i = 1, size(group%dimensions)
        associate (dim => group%dimensions(i))
          call write_formatted_dimension( &
            & dim, unit, iotype, v_list_, iostat, iomsg)
        end associate
      end do

      if (allocated(group%variables)) then
        write (unit, "(a,/)") "VARIABLES:"
        do i = 1, size(group%variables)
          associate (var => group%variables(i))
            call write_formatted_variable( &
              & var, unit, iotype, v_list_, iostat, iomsg)
          end associate
        end do
      end if

      if (allocated(group%attributes)) then
        write (unit, "(a,/)") "ATTRIBUTES:"
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
