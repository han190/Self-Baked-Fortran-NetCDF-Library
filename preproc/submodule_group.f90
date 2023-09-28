submodule(module_fortran_interface) submodule_group
implicit none
contains

module function new_grp(vars, name, atts) result(grp)
  type(nc_var), intent(in) :: vars(:)
  character(len=*), intent(in) :: name
  type(nc_att), intent(in), optional :: atts(:)
  type(nc_grp) :: grp
end function new_grp

module function from_netcdf_grp(path) result(file)
  character(len=*), intent(in) :: path
  type(nc_file) :: file
  integer(c_int) :: stat
  character(kind=c_char, len=nc_max_name) :: tmp

  file%filename = path
  file%mode = nc_nowrite

  stat = nc_open(cstr(file%filename), file%mode, file%ID)
  call handle_error(stat, "nc_open")

  stat = nc_inq_format(file%ID, file%fmt)
  call handle_error(stat, "nc_inq_format")

  stat = nc_inq_grpname(file%ID, tmp)
  call handle_error(stat, "nc_inq_grpname")
  file%name = cstrip(tmp)

  call inq_grp_dims(file)
  call inq_grp_atts(file)
end function from_netcdf_grp

end submodule submodule_group
