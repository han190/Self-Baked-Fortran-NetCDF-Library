submodule(module_fortran_interface) submodule_io
implicit none

integer, parameter :: line_width = 80
integer, parameter :: num_indents = 4
character(len=*), parameter :: space = achar(32)

contains

elemental function num_digits(val) result(ret)
  integer, intent(in) :: val
  integer :: ret

  ret = merge(1, floor(log10(real(val))) + 1, &
    & abs(val - 0.) < tiny(0.))
end function num_digits

pure function int2char(val) result(ret)
  integer, intent(in) :: val
  character(len=:), allocatable :: ret

  if (allocated(ret)) deallocate (ret)
  allocate (character(len=num_digits(val)) :: ret)
  write (ret, "(i0)") val
end function int2char

pure function indent(string, level) result(ret)
  character(len=*), intent(in) :: string
  integer, intent(in) :: level
  character(len=:), allocatable :: ret

  ret = repeat(space, num_indents*level)//string
end function indent

module subroutine write_formatted_dim( &
  & dim, unit, iotype, v_list, iostat, iomsg)
  class(nc_dim), intent(in) :: dim
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
    & error stop "[write_formatted_dim] Dimension name not allocated."

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

    write (unit, fmt) indent(dim%name, lvl), dim%len
  end if
end subroutine write_formatted_dim

pure function type_name(ID) result(ret)
  integer(c_int), intent(in) :: ID
  character(:), allocatable :: ret

  select case (ID)
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
    error stop "[type_name] Not supported type id."
  end select

  ret = "{"//ret//"}"
end function type_name

module subroutine write_formatted_att( &
  & att, unit, iotype, v_list, iostat, iomsg)
  class(nc_att), intent(in) :: att
  integer, intent(in) :: unit
  character(*), intent(in) :: iotype
  integer, intent(in) :: v_list(:)
  integer, intent(out) :: iostat
  character(*), intent(inout) :: iomsg
  character(len=:), allocatable :: fmt, tmp, val_str
  character(len=:), allocatable :: att_type, att_name
  integer :: lvl

  if (size(v_list) <= 0) then
    lvl = 0
  else
    lvl = v_list(1)
  end if

  if (.not. allocated(att%name)) &
    & error stop "[write_formatted_att] Attribute name not allocated."

  if (iotype == "DT" .or. iotype == "LISTDIRECTED") then
    att_type = type_name(att%type)
    att_name = indent(att%name, lvl)
    allocate (character(len=100) :: val_str)

    select type (vals_ => att%vals)
    type is (character(*))
      tmp = att_name//" "//att_type//' "'//cstrip(vals_(1))//'"'
      if (len(tmp) > line_width) then
        write (unit, "(a,/)") tmp(1:line_width - 4)//'..."'
      else
        write (unit, "(a,/)") tmp
      end if
    type is (integer(int8))
      write (val_str, *) vals_(1)
      fmt = "(a,1x,a,1x,a,"
      if (size(att%vals) > 1) then
        fmt = fmt//"'...',/)"
      else
        fmt = fmt//"/)"
      end if
      write (unit, fmt) att_name, att_type, adjustl(trim(val_str))
    type is (integer(int16))
      write (val_str, *) vals_(1)
      fmt = "(a,1x,a,1x,a,"
      if (size(att%vals) > 1) then
        fmt = fmt//"'...',/)"
      else
        fmt = fmt//"/)"
      end if
      write (unit, fmt) att_name, att_type, adjustl(trim(val_str))
    type is (integer(int32))
      write (val_str, *) vals_(1)
      fmt = "(a,1x,a,1x,a,"
      if (size(att%vals) > 1) then
        fmt = fmt//"'...',/)"
      else
        fmt = fmt//"/)"
      end if
      write (unit, fmt) att_name, att_type, adjustl(trim(val_str))
    type is (integer(int64))
      write (val_str, *) vals_(1)
      fmt = "(a,1x,a,1x,a,"
      if (size(att%vals) > 1) then
        fmt = fmt//"'...',/)"
      else
        fmt = fmt//"/)"
      end if
      write (unit, fmt) att_name, att_type, adjustl(trim(val_str))
    type is (real(real32))
      write (val_str, *) vals_(1)
      fmt = "(a,1x,a,1x,a,"
      if (size(att%vals) > 1) then
        fmt = fmt//"'...',/)"
      else
        fmt = fmt//"/)"
      end if
      write (unit, fmt) att_name, att_type, adjustl(trim(val_str))
    type is (real(real64))
      write (val_str, *) vals_(1)
      fmt = "(a,1x,a,1x,a,"
      if (size(att%vals) > 1) then
        fmt = fmt//"'...',/)"
      else
        fmt = fmt//"/)"
      end if
      write (unit, fmt) att_name, att_type, adjustl(trim(val_str))
    end select
  end if
end subroutine write_formatted_att

module subroutine write_formatted_var( &
  & var, unit, iotype, v_list, iostat, iomsg)
  class(nc_var), intent(in) :: var
  integer, intent(in) :: unit
  character(*), intent(in) :: iotype
  integer, intent(in) :: v_list(:)
  integer, intent(out) :: iostat
  character(*), intent(inout) :: iomsg
  character(:), allocatable :: fmt, msg
  integer, allocatable :: v_list_(:)
  character(len=nc_max_char) :: tmp

  if (size(v_list) == 0) then
    v_list_ = [0]
  else
    v_list_ = v_list + 1
  end if

  if (iotype == "DT" .or. iotype == "LISTDIRECTED") then
    msg = "inline"
    write (tmp, "(*(dt))", iomsg=msg) var%dims

    fmt = "(a,1x,a,1x,a,/)"
    write (unit, fmt) indent(var%name, v_list_(1)), &
      & type_name(var%type), "("//trim(tmp)//")"

    if (allocated(var%atts)) then
      fmt = "(*(dt("//int2char(v_list_(1) + 1)//")))"
      write (unit, fmt) var%atts
    end if
  end if
end subroutine write_formatted_var

end submodule submodule_io
