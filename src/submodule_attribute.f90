submodule(module_netcdf) submodule_attribute
  implicit none
contains

  !> Inquire attributes (internal)
  subroutine inq_atts_(grpid, varid, natts, atts)
    integer(c_int), intent(in) :: grpid, varid, natts
    type(attribute_type), intent(inout) :: atts(:)
    integer(c_int) :: stat
    character(kind=c_char, len=nc_max_name) :: tmp
    integer :: i

    do i = 1, natts
      associate (att => atts(i))
        att%id = i - 1
        stat = nc_inq_attname(grpid, varid, att%id, tmp)
        call handle_error(stat, "nc_inq_attname")
        att%name = strip(tmp)

        stat = nc_inq_att(grpid, varid, tmp, att%type, att%length)
        call handle_error(stat, "nc_inq_att")
      end associate
    end do
  end subroutine inq_atts_

  !> Get attributes (internal)
  subroutine get_atts_(grpid, varid, natts, atts)
    integer(c_int), intent(in) :: grpid, varid, natts
    type(attribute_type), intent(inout) :: atts(:)
    integer(c_int) :: stat
    integer :: i
    character(kind=c_char, len=:), allocatable :: tmp

    do i = 1, natts
      associate (att => atts(i))
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
      end associate
    end do
  end subroutine get_atts_

  !> Inquire group attribute
  module subroutine inq_grp_atts(grp)
    type(group_type), intent(inout) :: grp
    integer(c_int) :: stat, natts

    !> inquire number of attributes
    stat = nc_inq_natts(grp%id, natts)
    call handle_error(stat, "nc_inq_natts")

    if (allocated(grp%atts)) deallocate (grp%atts)
    allocate (grp%atts(natts))
    call inq_atts_(grp%id, nc_global, natts, grp%atts)
    call get_atts_(grp%id, nc_global, natts, grp%atts)
  end subroutine inq_grp_atts

  !> Inquire variable attributes
  module subroutine inq_var_atts(grp, var)
    type(group_type), intent(in) :: grp
    type(variable_type), intent(inout) :: var
    integer(c_int) :: stat, natts

    !> inquire number of attributes
    stat = nc_inq_varnatts(grp%id, var%id, natts)
    call handle_error(stat, "nc_inq_varnatts")

    if (allocated(var%atts)) deallocate (var%atts)
    allocate (var%atts(natts))
    call inq_atts_(grp%id, var%id, natts, var%atts)
    call get_atts_(grp%id, var%id, natts, var%atts)
  end subroutine inq_var_atts

  !> Get attributes
  module subroutine get_att_name_scalar_int16(var, name, val)
    type(variable_type), intent(in) :: var
    character(len=*), intent(in) :: name
    integer(int16), intent(out) :: val
    integer(c_int) :: stat
    integer(c_size_t) :: attlen
    integer(int16), allocatable :: tmp(:)

    stat = nc_inq_attlen(var%grp_id, var%id, name//c_null_char, attlen)
    call handle_error(stat, "nc_inq_attlen")
    if (attlen /= 1_c_size_t) error stop "Attribute is not a scalar."
    allocate (tmp(attlen))

    stat = nc_get_att_short(var%grp_id, var%id, name//c_null_char, tmp(1))
    call handle_error(stat, "nc_get_att_short")
    val = tmp(1)
  end subroutine get_att_name_scalar_int16

  module subroutine get_att_name_scalar_int32(var, name, val)
    type(variable_type), intent(in) :: var
    character(len=*), intent(in) :: name
    integer(int32), intent(out) :: val
    integer(c_int) :: stat
    integer(c_size_t) :: attlen
    integer(int32), allocatable :: tmp(:)

    stat = nc_inq_attlen(var%grp_id, var%id, name//c_null_char, attlen)
    call handle_error(stat, "nc_inq_attlen")
    if (attlen /= 1_c_size_t) error stop "Attribute is not a scalar."
    allocate (tmp(attlen))

    stat = nc_get_att_int(var%grp_id, var%id, name//c_null_char, tmp(1))
    call handle_error(stat, "nc_get_att_int")
    val = tmp(1)
  end subroutine get_att_name_scalar_int32

  module subroutine get_att_name_scalar_int64(var, name, val)
    type(variable_type), intent(in) :: var
    character(len=*), intent(in) :: name
    integer(int64), intent(out) :: val
    integer(c_int) :: stat
    integer(c_size_t) :: attlen
    integer(int64), allocatable :: tmp(:)

    stat = nc_inq_attlen(var%grp_id, var%id, name//c_null_char, attlen)
    call handle_error(stat, "nc_inq_attlen")
    if (attlen /= 1_c_size_t) error stop "Attribute is not a scalar."
    allocate (tmp(attlen))

    stat = nc_get_att_longlong(var%grp_id, var%id, name//c_null_char, tmp(1))
    call handle_error(stat, "nc_get_att_longlong")
    val = tmp(1)
  end subroutine get_att_name_scalar_int64

  module subroutine get_att_name_scalar_real32(var, name, val)
    type(variable_type), intent(in) :: var
    character(len=*), intent(in) :: name
    real(real32), intent(out) :: val
    integer(c_int) :: stat
    integer(c_size_t) :: attlen
    real(real32), allocatable :: tmp(:)

    stat = nc_inq_attlen(var%grp_id, var%id, name//c_null_char, attlen)
    call handle_error(stat, "nc_inq_attlen")
    if (attlen /= 1_c_size_t) error stop "Attribute is not a scalar."
    allocate (tmp(attlen))

    stat = nc_get_att_float(var%grp_id, var%id, name//c_null_char, tmp(1))
    call handle_error(stat, "nc_get_att_float")
    val = tmp(1)
  end subroutine get_att_name_scalar_real32

  module subroutine get_att_name_scalar_real64(var, name, val)
    type(variable_type), intent(in) :: var
    character(len=*), intent(in) :: name
    real(real64), intent(out) :: val
    integer(c_int) :: stat
    integer(c_size_t) :: attlen
    real(real64), allocatable :: tmp(:)

    stat = nc_inq_attlen(var%grp_id, var%id, name//c_null_char, attlen)
    call handle_error(stat, "nc_inq_attlen")
    if (attlen /= 1_c_size_t) error stop "Attribute is not a scalar."
    allocate (tmp(attlen))

    stat = nc_get_att_double(var%grp_id, var%id, name//c_null_char, tmp(1))
    call handle_error(stat, "nc_get_att_double")
    val = tmp(1)
  end subroutine get_att_name_scalar_real64

end submodule submodule_attribute
