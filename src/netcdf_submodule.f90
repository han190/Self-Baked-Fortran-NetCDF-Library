submodule(netcdf_module) netcdf_submodule

  implicit none

contains

  !> Wrapper of nf90_strerror
  module function strerror_(self) result(ret)
    class(netcdf_file), intent(in) :: self
    character(len=80) :: ret

    ret = nf90_strerror(self%status)
  end function strerror_

  !> Check
  module subroutine check(self)
    class(netcdf_file), intent(inout) :: self
    character(:), allocatable :: message

    ! error stop trim(self%strerror_()) ! Don't work
    ! gfortran 11.3.0
    message = trim(self%strerror_())
    if (self%status /= nf90_noerr) error stop message
  end subroutine check

  !> Wrapper of nf90_open (TODO: make it fully functional)
  module function open_(self) result(ret)
    class(netcdf_file), intent(inout) :: self
    integer :: ret

    ret = nf90_open(self%filename, self%mode, self%ncid)
  end function open_

  !> Wrapper of nf90_create (TODO: make it fully functional)
  module function create_(self) result(ret)
    class(netcdf_file), intent(inout) :: self
    integer :: ret

    ret = nf90_create(self%filename, self%mode, self%ncid)
  end function create_

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

    ret = nf90_inquire(self%ncid, self%ndim, self%nvar, self%natt, &
      self%unlimited_dimid, self%format_num)
  end function inquire_

  module function inq_dimids_(self) result(ret)
    class(netcdf_dataset), intent(inout) :: self
    integer :: ret

    ret = nf90_inq_dimids(self%ncid, self%ndims, &
      self%dimids, self%include_parents)
  end function inq_dimids_

  module subroutine read_dataset(self, unit, iotype, v_list, iostat, iomsg)
    class(netcdf_dataset), intent(inout) :: self
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
    case ("READ")

      self%mode = nf90_nowrite
      self%status = self%open_()
      call self%check()

      self%status = self%inquire_()
      call self%check()

      !> Trick the compiler by association
      !> Works on gfortran 11.3.0
      associate (attributes_ => self%attributes)
      end associate
      associate (dimensions_ => self%dimensions)
      end associate
      associate (variables_ => self%variables)
      end associate

      if (allocated(self%dimids)) deallocate (self%dimids)
      allocate (self%dimids(self%ndim))

      self%status = self%inq_dimids_()
      call self%check()

      self%dimids = self%dimids(1:self%ndims)

    case default

      error stop

    end select
  end subroutine read_dataset

end submodule netcdf_submodule
