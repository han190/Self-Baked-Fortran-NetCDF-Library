submodule(module_interface) submodule_group
  implicit none
contains

  !> Open dataset
  module function dataset(path, mode, inq_atts, inq_vars) result(grp)
    character(len=*), intent(in) :: path, mode
    logical, intent(in), optional :: inq_atts, inq_vars
    type(group_type) :: grp
    integer :: stat
    character(kind=c_char, len=nc_max_name) :: tmp

    select case (trim(mode))
    case ("r", "read")

      !> copy metadata
      grp%filename = path
      grp%mode = nc_nowrite

      !> open grp
      stat = nc_open(to_cstr(grp%filename), grp%mode, grp%id)
      call handle_error(stat, "nc_open")

      !> inquire format
      stat = nc_inq_format(grp%id, grp%format)
      call handle_error(stat, "nc_inq_format")

      !> inquire grp type (root group)
      stat = nc_inq_grpname(grp%id, tmp)
      call handle_error(stat, "nc_inq_grpname")
      grp%name = strip(tmp)

      !> copy dimension info
      call inq_grp_dims(grp)

      !> copy attribute info (default: true)
      if (present(inq_atts)) then
        if (inq_atts) call inq_grp_atts(grp)
      end if

      !> copy variable info
      if (present(inq_vars)) then
        if (inq_vars) call inq_grp_vars(grp)
      end if

    case ("w", "write")

      !> copy metadata
      grp%filename = path
      grp%mode = ior(nc_netcdf4, nc_clobber)

      !> create
      stat = nc_create(to_cstr(path), grp%mode, grp%id)
      call handle_error(stat, "nc_create")

      !> rootgroup name
      grp%name = to_cstr("/")

    case default
      error stop "Invalid mode."
    end select
  end function dataset

end submodule submodule_group
