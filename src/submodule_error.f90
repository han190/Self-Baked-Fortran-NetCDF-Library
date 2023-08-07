submodule(module_netcdf) submodule_error
  implicit none
contains

  !> Check function with error messages. Credit:
  !> https://github.com/Unidata/netcdf-fortran/blob/main/fortran/nf_misc.F90
  module function strip(cstring, nlen) result(string)
    character(len=*), intent(in) :: cstring
    integer, intent(in) :: nlen
    character(:), allocatable :: string
    integer :: ie, inull

    ie = len_trim(cstring)
    inull = scan(cstring, c_null_char)

    if (inull > 1) ie = inull - 1
    ie = max(1, min(ie, nlen))
    string = cstring(1:ie)
  end function strip

  !> Check function with error messages. Credit:
  !> https://github.com/Unidata/netcdf-fortran/blob/main/fortran/nf_misc.F90
  module subroutine handle_error(status, error_message)
    integer(c_int), intent(in) :: status
    character(*), intent(in), optional :: error_message
    character(:), pointer :: fptr => null()
    type(c_ptr) :: cptr
    integer :: inull, iptr, nptr
    character(:), allocatable :: message

    if (status /= nc_noerr) then

      nptr = 80
      allocate (character(len=nptr + 1) :: fptr)

      cptr = nc_strerror(status)
      call c_f_pointer(cptr, fptr)

      iptr = len_trim(fptr)
      inull = scan(fptr, c_null_char)
      if (inull /= 0) iptr = inull - 1
      iptr = max(1, min(iptr, nptr))

      if (present(error_message)) then
        message = fptr(1:iptr)//" ("//error_message//")"
      else
        message = fptr(1:iptr)
      end if
      error stop message

    end if
    nullify(fptr)
  end subroutine handle_error

end submodule submodule_error
