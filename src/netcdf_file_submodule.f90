submodule(netcdf_file_module) netcdf_file_submodule

  implicit none
  integer, parameter :: filename_max_len = 1000

contains

  !> Wrapper of nf90_strerror
  module function strerror_(self) result(ret)
    class(netcdf_file), intent(in) :: self
    character(len=80) :: ret

    ret = nf90_strerror(self%status)
  end function strerror_

  !> Wrapper of nf90_create
  module function create_(self) result(ret)
    class(netcdf_file), intent(inout) :: self
    integer :: ret

    ret = nf90_create(self%path, self%mode, self%ncid)
  end function create_

  !> Wrapper of nf90_open
  module function open_(self) result(ret)
    class(netcdf_file), intent(inout) :: self
    integer :: ret

    ret = nf90_open(self%path, self%mode, self%ncid)
  end function open_

  !> Wrapper of nf90_redef
  module function redef_(self) result(ret)
    class(netcdf_file), intent(inout) :: self
    integer :: ret

    ret = nf90_redef(self%ncid)
  end function redef_

  !> wrapper of nf90_enddef
  module function enddef_(self) result(ret)
    class(netcdf_file), intent(inout) :: self
    integer :: ret

    ret = nf90_enddef(self%ncid)
  end function enddef_

  !> Wrapper of nf90_close
  module function close_(self) result(ret)
    class(netcdf_file), intent(in) :: self
    integer :: ret

    ret = nf90_close(self%ncid)
  end function close_

  !> Wrapper of nf90_inquire family
  module function inquire_(self) result(ret)
    class(netcdf_file), intent(inout) :: self
    integer :: ret

    ret = nf90_inquire(self%ncid, self%ndim, self%nvar, &
      self%natt, self%unlimited_dimid)
  end function inquire_

  !> Check
  module subroutine check(self)
    class(netcdf_file), intent(inout) :: self
    character(:), allocatable :: message

    ! error stop trim(self%strerror_()) ! Don't work
    ! gfortran 11.3.0
    message = trim(self%strerror_())
    if (self%status /= nf90_noerr) error stop message
  end subroutine check

end submodule netcdf_file_submodule
