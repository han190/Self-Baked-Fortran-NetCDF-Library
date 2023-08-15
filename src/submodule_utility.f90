submodule(module_interface) submodule_utility
  implicit none
contains

  !> Convert string to cstring
  module pure function to_cstr(string) result(cstring)
    character(len=*), intent(in) :: string
    character(kind=c_char, len=:), allocatable :: cstring

    cstring = adjustl(adjustr(string))//c_null_char
  end function to_cstr

  !> Check function with error messages. Credit:
  !> https://github.com/Unidata/netcdf-fortran/blob/main/fortran/nf_misc.F90
  module function strip(cstring) result(string)
    character(len=*), intent(in) :: cstring
    character(:), allocatable :: string
    integer :: ie, inull

    ie = len_trim(cstring)
    inull = scan(cstring, c_null_char)

    if (inull > 1) ie = inull - 1
    ie = max(1, min(ie, nc_max_name)) ! nx_max_name = 256
    string = cstring(1:ie)
  end function strip

  !> Check function with error messages. Credit:
  !> https://github.com/Unidata/netcdf-fortran/blob/main/fortran/nf_misc.F90
  module subroutine handle_error(stat, err_msg)
    integer(c_int), intent(in) :: stat
    character(*), intent(in), optional :: err_msg
    character(:), pointer :: fptr => null()
    type(c_ptr) :: cptr
    integer :: inull, iptr, nptr
    character(:), allocatable :: message

    if (stat /= nc_noerr) then

      nptr = nc_max_name
      allocate (character(len=nptr + 1) :: fptr)

      cptr = nc_strerror(stat)
      call c_f_pointer(cptr, fptr)

      iptr = len_trim(fptr)
      inull = scan(fptr, c_null_char)
      if (inull /= 0) iptr = inull - 1
      iptr = max(1, min(iptr, nptr))

      if (present(err_msg)) then
        message = fptr(1:iptr)//" ("//err_msg//")"
      else
        message = fptr(1:iptr)
      end if
      error stop message

    end if
    nullify (fptr)
  end subroutine handle_error

end submodule submodule_utility
