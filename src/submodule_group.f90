submodule(module_netcdf) submodule_group

implicit none
contains

!> Group constructor
module function new_group(path, mode) result(group)
  character(*), intent(in) :: path, mode
  type(group_type) :: group
  integer :: status
  character(kind=c_char, len=:), allocatable :: filename

  select case (trim(mode))
  case ("r", "read")

    group%filename = path
    group%mode = nc_nowrite

    !> Open file
    filename = group%filename//c_null_char
    status = nc_open(filename, group%mode, group%id)
    call check(status, group%filename)

  case default
    error stop "Invalid mode."
  end select
end function new_group

!> Group destructor
module subroutine close_dataset(group)
  type(group_type) :: group
  integer :: status

  status = nc_close(group%id)
  if (allocated(group%name)) deallocate(group%name)
end subroutine close_dataset

end submodule submodule_group
