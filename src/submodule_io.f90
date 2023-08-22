submodule(module_interface) submodule_io
implicit none
integer, parameter :: line_width = 50
integer, parameter :: num_indents = 4
character(len=*), parameter :: space = " "
contains

!> Indent level
pure function indent_level(string, level) result(ret)
  character(len=*), intent(in) :: string
  integer, intent(in) :: level
  character(:), allocatable :: ret

  ret = repeat(space, num_indents*level)//string
end function indent_level

!> write(formatted) for dimension_type
module subroutine write_formatted_dim( &
  & dim, unit, iotype, v_list, iostat, iomsg)
  class(dimension_type), intent(in) :: dim
  integer, intent(in) :: unit
  character(*), intent(in) :: iotype
  integer, intent(in) :: v_list(:)
  integer, intent(out) :: iostat
  character(*), intent(inout) :: iomsg
  character(len=:), allocatable :: fmt
  integer :: lvl

  if (size(v_list) <= 0) then
    lvl = 0
  else
    lvl = v_list(1)
  end if

  if (.not. allocated(dim%name)) &
    & error stop "Dimension name not allocated."

  if (iotype == "DT" .or. iotype == "LISTDIRECTED") then
    if (dim%is_unlim) then
      fmt = "(a,'(unlimited):',i0,1x"
    else
      fmt = "(a,':',i0,1x"
    end if

    if (iomsg == "inline") then
      fmt = fmt//")"
    else
      fmt = fmt//",/)"
    end if

    write (unit, fmt) &
      & indent_level(dim%name, lvl), dim%len
  end if
end subroutine write_formatted_dim

!> type name of ID
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

!> write(formatted) for attribute_type
module subroutine write_formatted_att( &
  & att, unit, iotype, v_list, iostat, iomsg)
  class(attribute_type), intent(in) :: att
  integer, intent(in) :: unit
  character(*), intent(in) :: iotype
  integer, intent(in) :: v_list(:)
  integer, intent(out) :: iostat
  character(*), intent(inout) :: iomsg
  character(len=:), allocatable :: fmt, tmp
  character(len=:), allocatable :: att_type, att_name
  integer :: lvl

  if (size(v_list) <= 0) then
    lvl = 0
  else
    lvl = v_list(1)
  end if

  if (.not. allocated(att%att%name)) &
    & error stop "Attribute name not allocated."

  if (iotype == "DT" .or. iotype == "LISTDIRECTED") then
    att_type = type_name(att%att%type)
    att_name = indent_level(att%att%name, lvl)

    select type (att_ => att%att)
    type is (attribute_char_type)

      tmp = att_name//" "//att_type//' "'//cstrip(att_%vals(1))//'"'
      if (len(tmp) > line_width) then
        write (unit, "(a,/)") tmp(1:line_width - 4)//'..."'
      else
        write (unit, "(a,/)") tmp
      end if

    type is (attribute_int8_type)

      fmt = "(a,1x,a,1x,i0,"
      if (size(att_%vals) > 1) then
        fmt = fmt//"'...',/)"
      else
        fmt = fmt//"/)"
      end if
      write (unit, fmt) att_name, att_type, att_%vals(1)

    type is (attribute_int16_type)

      fmt = "(a,1x,a,1x,i0,"
      if (size(att_%vals) > 1) then
        fmt = fmt//"'...',/)"
      else
        fmt = fmt//"/)"
      end if
      write (unit, fmt) att_name, att_type, att_%vals(1)

    type is (attribute_int32_type)

      fmt = "(a,1x,a,1x,i0,"
      if (size(att_%vals) > 1) then
        fmt = fmt//"'...',/)"
      else
        fmt = fmt//"/)"
      end if
      write (unit, fmt) att_name, att_type, att_%vals(1)

    type is (attribute_int64_type)

      fmt = "(a,1x,a,1x,i0,"
      if (size(att_%vals) > 1) then
        fmt = fmt//"'...',/)"
      else
        fmt = fmt//"/)"
      end if
      write (unit, fmt) att_name, att_type, att_%vals(1)

    type is (attribute_real32_type)

      fmt = "(a,1x,a,1x,e10.3,"
      if (size(att_%vals) > 1) then
        fmt = fmt//"'...',/)"
      else
        fmt = fmt//"/)"
      end if
      write (unit, fmt) att_name, att_type, att_%vals(1)

    type is (attribute_real64_type)

      fmt = "(a,1x,a,1x,e10.3,"
      if (size(att_%vals) > 1) then
        fmt = fmt//"'...',/)"
      else
        fmt = fmt//"/)"
      end if
      write (unit, fmt) att_name, att_type, att_%vals(1)

    end select
  end if
end subroutine write_formatted_att

end submodule submodule_io
