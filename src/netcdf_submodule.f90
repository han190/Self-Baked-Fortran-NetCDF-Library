submodule(netcdf_module) netcdf_submodule

  implicit none

contains

  !> Wrapper of nf90_strerror
  module function strerror_(self) result(ret)
    class(dataset), intent(in) :: self
    character(len=80) :: ret

    ret = nf90_strerror(self%status)
  end function strerror_

  !> Check
  module subroutine check(self)
    class(dataset), intent(inout) :: self
    character(:), allocatable :: message

    ! error stop trim(self%strerror_()) ! Don't work
    ! gfortran 11.3.0
    message = trim(self%strerror_())
    if (self%status /= nf90_noerr) error stop message
  end subroutine check

  !> Wrapper of nf90_open (TODO: make it fully functional)
  module function open_(self) result(ret)
    class(dataset), intent(inout) :: self
    integer :: ret

    ret = nf90_open(self%filename, self%mode, self%ncid)
  end function open_

  !> Wrapper of nf90_create (TODO: make it fully functional)
  module function create_(self) result(ret)
    class(dataset), intent(inout) :: self
    integer :: ret

    ret = nf90_create(self%filename, self%mode, self%ncid)
  end function create_

  !> Wrapper of nf90_close
  module function close_(self) result(ret)
    class(dataset), intent(in) :: self
    integer :: ret

    ret = nf90_close(self%ncid)
  end function close_

  module subroutine read_formatted(self, unit, iotype, v_list, iostat, iomsg)
    class(dataset), intent(inout) :: self
    integer, intent(in) :: unit
    character(len=*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(len=*), intent(inout) :: iomsg
    character(len=10) :: action

    associate (iotype_ => iotype, v_list_ => v_list)
    end associate

    block
      character(len=filename_max_len) :: name
      inquire (unit, name=name, action=action)
      self%filename = trim(name)
    end block

    select case (action)
    case ("READ") ! Case sensitive.
      self%mode = nf90_nowrite
      self%status = self%open_()
      iostat = self%status
      iomsg = "Normal."
    case default
      iostat = 1
      iomsg = "action /= 'READ'"
      error stop iomsg
    end select

    call self%check()
  end subroutine read_formatted

end submodule netcdf_submodule
