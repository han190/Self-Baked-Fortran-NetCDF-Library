submodule(module_interface) submodule_attribute
  implicit none
contains

  !> Put attributes (internal)
  subroutine put_att_(atts, grpid, varid, name, val)
    type(dictionary_type), intent(inout) :: atts
    integer(c_int), intent(in) :: grpid, varid
    character(len=*), intent(in) :: name
    class(*), intent(in) :: val(:)
    type(attribute_type) :: att
    class(*), allocatable :: item
    integer(c_int) :: stat

    att%name = name
    att%length = 1
    att%id = atts%length + 1
    att%values = val

    select type (v => val)
    type is (integer(int16))
      att%type = nc_short
      stat = nc_put_att_short(grpid, varid, &
        & to_cstr(name), nc_short, 1_c_size_t, v)
      call handle_error(stat, "nc_put_att_short")
    type is (integer(int32))
      att%type = nc_int
      stat = nc_put_att_int(grpid, varid, &
        & to_cstr(name), nc_int, 1_c_size_t, v)
      call handle_error(stat, "nc_put_att_int")
    type is (integer(int64))
      att%type = nc_int64
      stat = nc_put_att_longlong(grpid, varid, &
        & to_cstr(name), nc_int64, 1_c_size_t, v)
      call handle_error(stat, "nc_put_att_longlong")
    type is (real(real32))
      att%type = nc_float
      stat = nc_put_att_float(grpid, varid, &
        & to_cstr(name), nc_float, 1_c_size_t, v)
      call handle_error(stat, "nc_put_att_float")
    type is (real(real64))
      att%type = nc_double
      stat = nc_put_att_double(grpid, varid, &
        & to_cstr(name), nc_double, 1_c_size_t, v)
      call handle_error(stat, "nc_put_att_double")
    type is (character(*))
      att%type = nc_char
      stat = nc_put_att_text(grpid, varid, &
        & to_cstr(name), len(v(1), kind=c_size_t), v)
      call handle_error(stat, "nc_put_att_text")
    class default
      error stop "Invalid attribute type."
    end select

    item = att
    call append(atts, att%name.pair.item)
  end subroutine put_att_

  !> Put group attribute
  module subroutine put_grp_att(grp, name, val)
    class(group_type), intent(inout) :: grp
    character(len=*), intent(in) :: name
    class(*), intent(in) :: val
    class(*), allocatable :: val_(:)

    allocate (val_(1), source=val)
    call put_att_(grp%atts, grp%id, nc_global, name, val_)
  end subroutine put_grp_att

  !> Put variable attribute
  module subroutine put_var_att(var, name, val)
    type(variable_type), intent(inout) :: var
    character(len=*), intent(in) :: name
    class(*), intent(in) :: val
    class(*), allocatable :: val_(:)

    allocate (val_(1), source=val)
    call put_att_(var%atts, var%grp_id, var%id, name, val_)
  end subroutine put_var_att

  !> Get attributes (internal)
  subroutine get_atts_(grpid, varid, natts, att)
    integer(c_int), intent(in) :: grpid, varid, natts
    type(attribute_type), intent(inout) :: att
    integer(c_int) :: stat
    integer :: i
    character(kind=c_char, len=:), allocatable :: tmp

    if (allocated(att%values)) then
      deallocate (att%values)
    end if

    select case (att%type)
    case (nc_char)
      if (att%length > nc_max_char) then
        allocate (character(kind=c_char, len=nc_max_char) :: &
          & att%values(att%length/nc_max_char + 1))
      else
        allocate (character(kind=c_char, len=att%length) :: att%values(1))
      end if
    case (nc_short)
      allocate (integer(int16) :: att%values(att%length))
    case (nc_int)
      allocate (integer(int32) :: att%values(att%length))
    case (nc_int64)
      allocate (integer(int64) :: att%values(att%length))
    case (nc_float)
      allocate (real(real32) :: att%values(att%length))
    case (nc_double)
      allocate (real(real64) :: att%values(att%length))
    end select

    tmp = att%name//c_null_char
    select type (val_ => att%values)
    type is (character(kind=c_char, len=*))
      stat = nc_get_att_text(grpid, varid, tmp, val_)
      call handle_error(stat, "nc_get_att_text")
    type is (integer(int16))
      stat = nc_get_att_short(grpid, varid, tmp, val_)
      call handle_error(stat, "nc_get_att_short")
    type is (integer(int32))
      stat = nc_get_att_int(grpid, varid, tmp, val_)
      call handle_error(stat, "nc_get_att_int")
    type is (integer(int64))
      stat = nc_get_att_longlong(grpid, varid, tmp, val_)
      call handle_error(stat, "nc_get_att_longlong")
    type is (real(real32))
      stat = nc_get_att_float(grpid, varid, tmp, val_)
      call handle_error(stat, "nc_get_att_float")
    type is (real(real64))
      stat = nc_get_att_double(grpid, varid, tmp, val_)
      call handle_error(stat, "nc_get_att_double")
    end select
  end subroutine get_atts_

  !> Inquire attributes (internal)
  subroutine inq_atts_(grp, atts, varid, natts)
    class(group_type), intent(inout) :: grp
    type(dictionary_type), intent(inout) :: atts
    integer(c_int), intent(in) :: varid, natts
    integer(c_int) :: stat
    character(kind=c_char, len=nc_max_name) :: tmp
    integer :: i
    type(attribute_type) :: att
    class(*), allocatable :: item

    do i = 1, natts
      att%id = i - 1
      stat = nc_inq_attname(grp%id, varid, att%id, tmp)
      call handle_error(stat, "nc_inq_attname")
      att%name = strip(tmp)

      stat = nc_inq_att(grp%id, varid, tmp, att%type, att%length)
      call handle_error(stat, "nc_inq_att")
      call get_atts_(grp%id, varid, natts, att)

      if (allocated(item)) deallocate (item)
      item = att
      call append(atts, att%name.pair.item)
    end do
  end subroutine inq_atts_

  !> Inquire group attribute
  module subroutine inq_grp_atts(grp)
    class(group_type), intent(inout) :: grp
    integer(c_int) :: stat, natts

    !> inquire number of attributes
    stat = nc_inq_natts(grp%id, natts)
    call handle_error(stat, "nc_inq_natts")
    call inq_atts_(grp, grp%atts, nc_global, natts)
  end subroutine inq_grp_atts

  !> Inquire variable attributes
  module subroutine inq_var_atts(grp, var)
    class(group_type), intent(inout) :: grp
    type(variable_type), intent(inout) :: var
    integer(c_int) :: stat, natts

    !> inquire number of attributes
    stat = nc_inq_varnatts(grp%id, var%id, natts)
    call handle_error(stat, "nc_inq_varnatts")
    call inq_atts_(grp, var%atts, var%id, natts)
  end subroutine inq_var_atts

  !> Get attribute scalar
  module subroutine get_att_scalar(var, name, val)
    type(variable_type), intent(in) :: var
    character(len=*), intent(in) :: name
    class(*), intent(out) :: val
    integer(c_int) :: stat
    integer(c_size_t) :: attlen

    stat = nc_inq_attlen(var%grp_id, var%id, to_cstr(name), attlen)
    call handle_error(stat, "nc_inq_attlen")

    select type (val_ => val)
    type is (integer(int16))

      if (attlen /= 1_c_size_t) &
        & error stop "Attribute is not a scalar."
      block
        integer(int16) :: vals(1)

        stat = nc_get_att_short( &
          & var%grp_id, var%id, to_cstr(name), vals)
        call handle_error(stat, "nc_get_att_short")
        val_ = vals(1)
      end block

    type is (integer(int32))

      if (attlen /= 1_c_size_t) &
        & error stop "Attribute is not a scalar."
      block
        integer(int32) :: vals(1)

        stat = nc_get_att_int( &
          & var%grp_id, var%id, to_cstr(name), vals)
        call handle_error(stat, "nc_get_att_int")
        val_ = vals(1)
      end block

    type is (integer(int64))

      if (attlen /= 1_c_size_t) &
        & error stop "Attribute is not a scalar."
      block
        integer(int64) :: vals(1)

        stat = nc_get_att_longlong( &
          & var%grp_id, var%id, to_cstr(name), vals)
        call handle_error(stat, "nc_get_att_longlong")
        val_ = vals(1)
      end block

    type is (real(real32))

      if (attlen /= 1_c_size_t) &
        & error stop "Attribute is not a scalar."
      block
        real(real32) :: vals(1)

        stat = nc_get_att_float( &
          & var%grp_id, var%id, to_cstr(name), vals)
        call handle_error(stat, "nc_get_att_float")
        val_ = vals(1)
      end block

    type is (real(real64))

      if (attlen /= 1_c_size_t) &
        & error stop "Attribute is not a scalar."
      block
        real(real64) :: vals(1)

        stat = nc_get_att_double( &
          & var%grp_id, var%id, to_cstr(name), vals)
        call handle_error(stat, "nc_get_att_double")
        val_ = vals(1)
      end block

    type is (character(*))

      block
        character(len=attlen, kind=c_char) :: vals(1)

        stat = nc_get_att_text( &
          & var%grp_id, var%id, to_cstr(name), vals)
        call handle_error(stat, "nc_get_att_text")
        val_ = vals(1)
      end block

    end select
  end subroutine get_att_scalar

end submodule submodule_attribute
