submodule(module_fortran_interface) submodule_group
implicit none
contains

pure function pair2dim(pair) result(dim)
  type(pair_type), intent(in) :: pair
  type(nc_dim) :: dim

  dim = pair%key.dim.pair%val
end function pair2dim

pure function pairs2dims(pairs) result(dims)
  type(pair_type), intent(in) :: pairs(:)
  type(nc_dim), allocatable :: dims(:)
  integer :: i

  if (allocated(dims)) deallocate (dims)
  dims = [(pair2dim(pairs(i)), i=1, size(pairs))]
end function pairs2dims

module function new_file(vars, atts) result(file)
  type(nc_var), intent(in) :: vars(:)
  type(nc_att), intent(in), optional :: atts(:)
  type(nc_file) :: file
  type(dict_type) :: file_dims
  integer :: loc, val
  character(:), allocatable :: key
  integer :: i, j

  file_dims = new_dict()
  do i = 1, size(vars)
    do j = 1, size(vars(i)%dims)
      key = vars(i)%dims(j)%name
      val = int(vars(i)%dims(j)%len)

      loc = scan(file_dims, key)
      if (loc == invalid) &
        & call append(file_dims, key.pair.val)
    end do
  end do

  file%name = cstr("/") ! Root group
  if (present(atts)) file%atts = atts
  file%vars = vars
  file%dims = pairs2dims(to_array(file_dims))
end function new_file

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

module subroutine to_netcdf_grp(file)
  type(nc_file), target, intent(in) :: file
  integer(c_int) :: stat, i
  type(nc_var) :: var

  if (.not. allocated(file%vars) .and. .not. allocated(file%grps)) &
    & error stop "[to_netcdf_grp] Empty file type."

  select case (file%mode)
  case (xor(nc_netcdf4, nc_clobber))
    stat = nc_create(cstr(file%filename), file%mode, file%ID)
    call handle_error(stat, "nc_create")

    call def_grp_dim(file)
    if (allocated(file%atts)) call put_grp_atts(file)

    do i = 1, size(file%vars)
      var = file%vars(i)
      var%grpID => file%ID
      call def_var(var)
      if (allocated(var%atts)) call put_var_atts(var)
      call put_var(var)
      nullify (var%grpID)
    end do
  case default
    error stop "[to_netcdf_grp] File mode not supported yet."
  end select

  stat = nc_close(file%ID)
  call handle_error(stat, "nc_close")
end subroutine to_netcdf_grp

end submodule submodule_group
