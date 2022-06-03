submodule(netcdf_group_module) netcdf_group_submodule

  implicit none
  integer, parameter :: max_ncids = 100
  integer, parameter :: max_dimids = 100

contains

  module function inq_grps_root(self) result(ret)
    class(netcdf_dataset), intent(inout) :: self
    integer :: ret

    ! allocate (self%ncids(max_ncids))
    self%ncids = [integer::]
    ret = nf90_inq_grps(self%ncid, self%numgrps, self%ncids)

    if (self%numgrps == 0) then

      !> If root group is the only group,
      !> we allocate only one group, and
      !> some initializations to it.
      deallocate (self%ncids)
      allocate (self%grps(1))

      associate (grp_ => self%grps(1)%grp)
        grp_%name = ""
        grp_%ncid = self%ncid
      end associate

    else

      !> Reallocate ncids to appropriate lenght.
      deallocate (self%ncids)
      allocate (self%ncids(1:self%numgrps))
      ret = nf90_inq_grps(self%ncid, self%numgrps, self%ncids)

    end if
  end function inq_grps_root

  !> Wrapper of nf90_inq_dimids
  module function inq_dimids_(self) result(ret)
    class(netcdf_group), intent(inout) :: self
    integer :: ret

    allocate (self%dimids(max_dimids))
    ret = nf90_inq_dimids(self%ncid, self%ndim, self%dimids, self%parents)
    self%dimids = self%dimids(1:self%ndim)
  end function inq_dimids_

  !> Open dataset
  module subroutine open_dataset(self, filename, mode)
    class(netcdf_dataset), intent(inout) :: self
    character(*), intent(in) :: filename
    character(*), intent(in) :: mode

    select case (mode)
    case ("read", "r")

      !> Open dataset
      self%mode = nf90_nowrite
      self%path = filename
      self%status = self%open_()
      call self%check()

      !> Inquire root group
      self%status = self%inq_grps_()
      call self%check()

    case default

      !> If no support.
      error stop "Invalid open mode."

    end select
  end subroutine open_dataset

end submodule netcdf_group_submodule
