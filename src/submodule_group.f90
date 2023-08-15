submodule(module_interface) submodule_group
  implicit none
contains

  !> Open dataset
  module function dataset(path, mode, inq_atts, inq_vars) result(file)
    character(len=*), intent(in) :: path, mode
    logical, intent(in), optional :: inq_atts, inq_vars
    type(group_type) :: file
    integer :: stat
    character(kind=c_char, len=nc_max_name) :: tmp

    select case (trim(mode))
    case ("r", "read")

      !> copy metadata
      file%filename = path
      file%mode = nc_nowrite

      !> open file
      stat = nc_open(trim(file%filename)//c_null_char, &
        & file%mode, file%id)
      call handle_error(stat, "nc_open")

      !> inquire format
      stat = nc_inq_format(file%id, file%format)
      call handle_error(stat, "nc_inq_format")

      !> inquire file type (root group)
      stat = nc_inq_grpname(file%id, tmp)
      call handle_error(stat, "nc_inq_grpname")
      file%name = strip(tmp)

      !> copy dimension info
      call inq_grp_dims(file)

      !> copy attribute info (default: true)
      if (present(inq_atts)) then
        if (inq_atts) call inq_grp_atts(file)
      end if

      !> copy variable info
      if (present(inq_vars)) then
        if (inq_vars) call inq_grp_vars(file)
      end if

    case default
      error stop "Invalid mode."
    end select
  end function dataset

end submodule submodule_group
