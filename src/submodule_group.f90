submodule(module_netcdf) submodule_group

implicit none
contains

!> Open base group
module function open_dataset(path, mode) result(group)
  character(*), intent(in) :: path, mode
  type(group_type) :: group
  integer :: status
  character(kind=c_char, len=:), allocatable :: temp

  select case (trim(mode))
  case ("r", "read")

    !> Copy metadata
    group%filename = path
    group%mode = nc_nowrite

    !> Open file
    temp = group%filename//c_null_char
    status = nc_open(temp, group%mode, group%id)
    call handle_error(status, group%filename)

    !> Inquire format
    status = nc_inq_format(group%id, group%format)
    call handle_error(status, "nc_inq_format")

    !> Copy dimension info
    call inquire_dimensions(group)

  case default
    error stop "Invalid mode."
  end select
end function open_dataset

!> Group destructor
module subroutine close_dataset(group)
  type(group_type) :: group
  integer :: status

  !> Close group
  status = nc_close(group%id)
  call handle_error(status, "nc_close")

  !> Deallocate allocatable components
  if (allocated(group%filename)) deallocate (group%filename)
  if (allocated(group%name)) deallocate (group%name)
  if (allocated(group%groups)) deallocate (group%groups)
  if (allocated(group%dimensions)) deallocate (group%dimensions)
  if (allocated(group%attributes)) deallocate (group%attributes)
  if (allocated(group%variables)) deallocate (group%variables)
end subroutine close_dataset

end submodule submodule_group
