module module_cwrapper

use iso_c_binding
implicit none
public

interface

  !> Wrapper to nc_strerror
  pure function nc_strerror(ncerr1) result(ret) &
    & bind(c, name='nc_strerror')
    import c_int, c_ptr
    implicit none
    integer(c_int), intent(in) :: ncerr1
    type(c_ptr) :: ret
  end function nc_strerror

  !> Wrapper to nc_open
  function nc_open(path, mode, ncid) result(ret) &
    & bind(c, name='nc_open')
    import c_char, c_int
    implicit none
    character(kind=c_char), intent(in) :: path(*)
    integer(c_int), value :: mode
    integer(c_int), intent(out) :: ncid
    integer(c_int) :: ret
  end function nc_open

  !> Wrapper to nc_inq_varid
  function nc_inq_varid(ncid, name, varid) result(ret) &
    & bind(c, name='nc_inq_varid')
    import c_char, c_int
    implicit none
    integer(c_int), value :: ncid
    character(kind=c_char), intent(in) :: name(*)
    integer(c_int), intent(out) :: varid
    integer(c_int) :: ret
  end function nc_inq_varid

  !> Wrapper to nc_inq_varndims
  function nc_inq_varndims(ncid, varid, ndims) result(ret) &
    & bind(c, name='nc_inq_varndims')
    import c_int
    implicit none
    integer(c_int), value :: ncid
    integer(c_int), value :: varid
    integer(c_int), intent(out) :: ndims
    integer(c_int) :: ret
  end function nc_inq_varndims

  !> Wrapper to nc_inq_vardimid
  function nc_inq_vardimid(ncid, varid, dimids) result(ret) &
    & bind(c, name='nc_inq_vardimid')
    import c_int
    implicit none
    integer(c_int), value :: ncid
    integer(c_int), value :: varid
    integer(c_int), intent(out) :: dimids(*)
    integer(c_int) :: ret
  end function nc_inq_vardimid

  !> Wrapper to nc_inq_vartype
  function nc_inq_vartype(ncid, varid, type) result(ret) &
    & bind(c, name='nc_inq_vartype')
    import c_int
    implicit none
    integer(c_int), value :: ncid
    integer(c_int), value :: varid
    integer(c_int), intent(out) :: type
    integer(c_int) :: ret
  end function nc_inq_vartype

  !> Wrapper to nc_inq_varnatts
  function nc_inq_varnatts(ncid, varid, natts) result(ret) &
    & bind(c, name='nc_inq_varnatts')
    import c_int
    implicit none
    integer(c_int), value :: ncid
    integer(c_int), value :: varid
    integer(c_int), intent(out) :: natts
    integer(c_int) :: ret
  end function nc_inq_varnatts

  !> Wrapper to nc_inq_dimlen
  function nc_inq_dimlen(ncid, dimid, len) result(ret) &
    & bind(c, name='nc_inq_dimlen')
    import c_int, c_size_t
    implicit none
    integer(c_int), value :: ncid
    integer(c_int), value :: dimid
    integer(c_size_t), intent(out) :: len
    integer(c_int) :: ret
  end function nc_inq_dimlen

  !> Wrapper to nc_inq_dimname
  function nc_inq_dimname(ncid, dimid, name) result(ret) &
    & bind(c, name='nc_inq_dimname')
    import c_int, c_char
    implicit none
    integer(c_int), value :: ncid
    integer(c_int), value :: dimid
    character(kind=c_char), intent(out) :: name(*)
    integer(c_int) :: ret
  end function nc_inq_dimname

  !> Wrapper to nc_inq_attname
  function nc_inq_attname(ncid, varid, attnum, name) result(ret) &
    & bind(c, name='nc_inq_attname')
    import c_int, c_char
    implicit none
    integer(c_int), value :: ncid
    integer(c_int), value :: varid
    integer(c_int), value :: attnum
    character(kind=c_char), intent(out) :: name(*)
    integer(c_int) :: ret
  end function nc_inq_attname

  !> Wrapper to nc_inq_att
  function nc_inq_att(ncid, varid, name, type, len) result(ret) &
    & bind(c, name='nc_inq_att')
    import c_int, c_char, c_size_t
    implicit none
    integer(c_int), value :: ncid, varid
    character(kind=c_char), intent(in) :: name(*)
    integer(c_int), intent(out) :: type
    integer(c_size_t), intent(out) :: len
    integer(c_int) :: ret
  end function nc_inq_att

  !> Wrapper to nc_close
  function nc_close(ncid) result(ret) &
    & bind(c, name='nc_close')
    import c_int
    implicit none
    integer(c_int), value :: ncid
    integer(c_int) :: ret
  end function nc_close

  !> Wrapper to nc_get_var_short
  function nc_get_var_short(ncid, varid, var) result(ret) &
    & bind(c, name='nc_get_var_short')
    import c_int, c_short
    implicit none
    integer(c_int), value :: ncid, varid
    integer(c_short), intent(out) :: var(*)
    integer(c_int) :: ret
  end function nc_get_var_short

  !> Wrapper to nc_get_att_double
  function nc_get_att_double(ncid, varid, name, value) result(ret) &
    & bind(c, name='nc_get_att_double')
    import c_int, c_char, c_double
    implicit none
    integer(c_int), value :: ncid, varid
    character(kind=c_char), intent(in) :: name(*)
    real(c_double), intent(out) :: value(*)
    integer(c_int) :: ret
  end function nc_get_att_double

end interface

end module module_cwrapper
