submodule(module_interface) submodule_io

  implicit none
  integer, parameter :: line_width = 50
  integer, parameter :: num_indents = 4
  character(len=*), parameter :: space = " "

contains

  pure function type_name(id) result(ret)
    integer(c_int), intent(in) :: id
    character(:), allocatable :: ret

    select case (id)
    case (nc_nat)
      ret = "not a type."
    case (nc_byte)
      ret = "byte"
    case (nc_char)
      ret = "char"
    case (nc_short)
      ret = "short"
    case (nc_int)
      ret = "int"
      ! case (nc_long)
      !   ret = "32-bit integer (deprecated)"
    case (nc_float)
      ret = "float"
    case (nc_double)
      ret = "double"
    case (nc_int64)
      ret = "int64"
    case default
      error stop "Not supported type id."
    end select

    ret = "<"//ret//">"
  end function type_name

  elemental function num_digits(val) result(ret)
    integer(int64), intent(in) :: val
    integer(int64) :: ret

    ret = merge(1, floor(log10(real(val))) + 1, &
      & abs(val - 0.) < tiny(0.))
  end function num_digits

  pure function int2char(val) result(ret)
    integer(int64), intent(in) :: val
    character(:), allocatable :: ret

    if (allocated(ret)) deallocate (ret)
    allocate (character(len=num_digits(val)) :: ret)
    write (ret, "(i0)") val
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

  module subroutine write_formatted_att( &
    & att, unit, iotype, v_list, iostat, iomsg)
    class(attribute_type), intent(in) :: att
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    character(len=:), allocatable :: fmt, tmp, att_type, att_name
    integer :: i, j, level

    !> Indent spaces
    if (size(v_list) == 0) then
      level = 0
    else
      level = v_list(1) + 1
    end if

    if (iotype == "DT" .or. iotype == "LISTDIRECTED") then
      att_type = type_name(att%type)
      att_name = indent_level(att%name, level)

      select type (val_ => att%values)
      type is (character(*))

        !!TODO Find a way to remove all blackslash
        !! and split long lines into shorter lines with
        !! correct indentation.
        tmp = att_name//" "//att_type//' "'//strip(val_(1))//'"'
        if (len(tmp) > line_width) then
          write (unit, "(a,/)") tmp(1:line_width - 4)//'..."'
        else
          write (unit, "(a,/)") tmp
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
  end subroutine write_formatted_att

  module subroutine write_formatted_dim( &
    & dim, unit, iotype, v_list, iostat, iomsg)
    class(dimension_type), intent(in) :: dim
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    integer :: level
    character(len=:), allocatable :: fmt, dim_name

    !> Indent level
    level = merge(0, v_list(1) + 1, size(v_list) == 0)

    if (iotype == "DT" .or. iotype == "LISTDIRECTED") then
      associate (dim => dim)
        dim_name = indent_level(dim%name, level)
        if (dim%is_unlimited) then
          fmt = "(a, ':', 1x, i0, ' (unlimited)', /)"
        else
          fmt = "(a, ':', 1x, i0, /)"
        end if
        write (unit, fmt) dim_name, dim%length
      end associate
    end if
  end subroutine write_formatted_dim

  module subroutine write_formatted_var( &
    & var, unit, iotype, v_list, iostat, iomsg)
    class(variable_type), intent(in) :: var
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    integer :: i
    character(len=:), allocatable :: fmt, dim_str
    integer, allocatable :: v_list_(:)
    integer :: level

    if (iotype == "DT" .or. iotype == "LISTDIRECTED") then

      !> Construct a character for dimensions
      !> Yes, C starts from zero. Also, keep in mind Fortran
      !> is column major so loop backwards.
      dim_str = ""
      do i = rank(var), 1, -1
        associate (dim => var%dims(i)%ptr)
          dim_str = dim_str//dim%name//":"// &
            & int2char(dim%length)//", "
        end associate
      end do
      dim_str = "("//dim_str(1:len(dim_str) - 2)//")"

      if (size(v_list) == 0) then
        v_list_ = [0]
      else
        v_list_ = v_list + 1
      end if
      level = v_list_(1)

      fmt = "(a, 1x, a, 1x, a, /)"
      write (unit, fmt) indent_level(var%name, level), &
        & type_name(var%type), dim_str

      if (allocated(var%atts)) then
        do i = 1, size(var%atts)
          associate (att => var%atts(i))
            call write_formatted_att( &
              & att, unit, iotype, v_list_, iostat, iomsg)
          end associate
        end do
      end if

    end if
  end subroutine write_formatted_var

  module subroutine write_formatted_grp( &
    & grp, unit, iotype, v_list, iostat, iomsg)
    class(group_type), intent(in) :: grp
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
    integer :: i
    integer, allocatable :: v_list_(:)

    if (size(v_list) == 0) then
      v_list_ = [0]
    else
      v_list_ = v_list
    end if

    if (iotype == "DT" .or. iotype == "LISTDIRECTED") then
      write (unit, "(a,1x,'(',a,')',':',/)") "GROUP", grp%name
      write (unit, "(a,/)") "DIMENSIONS:"
      do i = 1, size(grp%dims)
        associate (dim => grp%dims(i))
          call write_formatted_dim( &
            & dim, unit, iotype, v_list_, iostat, iomsg)
        end associate
      end do

      if (allocated(grp%vars)) then
        write (unit, "(a,/)") "VARIABLES:"
        do i = 1, size(grp%vars)
          associate (var => grp%vars(i))
            call write_formatted_var( &
              & var, unit, iotype, v_list_, iostat, iomsg)
          end associate
        end do
      end if

      if (allocated(grp%atts)) then
        write (unit, "(a,/)") "ATTRIBUTES:"
        do i = 1, size(grp%atts)
          associate (att => grp%atts(i))
            call write_formatted_att( &
              & att, unit, iotype, v_list_, iostat, iomsg)
          end associate
        end do
      end if

    end if
  end subroutine write_formatted_grp

end submodule submodule_io
