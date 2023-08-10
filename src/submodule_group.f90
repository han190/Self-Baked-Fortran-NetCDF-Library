submodule(module_netcdf) submodule_group
  implicit none
contains

  !> Open base group
  module function open_dataset(path, mode, inq_att, inq_var) result(group)
    character(*), intent(in) :: path, mode
    logical, intent(in), optional :: inq_att, inq_var
    type(group_type) :: group
    integer :: status
    character(kind=c_char, len=:), allocatable :: tmp

    select case (trim(mode))
    case ("r", "read")

      !> Copy metadata
      group%filename = path
      group%mode = nc_nowrite

      !> Open file
      tmp = group%filename//c_null_char
      status = nc_open(tmp, group%mode, group%id)
      call handle_error(status, group%filename)

      !> Inquire format
      status = nc_inq_format(group%id, group%format)
      call handle_error(status, "nc_inq_format")

      !> Inquire root group
      deallocate (tmp)
      allocate (character(len=num_chars, kind=c_char) :: tmp)
      status = nc_inq_grpname(group%id, tmp)
      call handle_error(status, "nc_inq_grpname")
      group%name = strip(tmp, num_chars)

      !> Copy dimension info
      call inquire_dimensions(group)

      !> Copy variable info (default: true)
      if (present(inq_var)) then
        if (inq_var) call inquire_variables(group)
      else
        call inquire_variables(group)
      end if

      !> Copy attributes info (default: true)
      if (present(inq_att)) then
        if (inq_att) call inquire_attributes(group)
      else
        call inquire_attributes(group)
      end if

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
