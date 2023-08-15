program simple_xy_wr

  use :: module_netcdf
  implicit none

  integer(c_int), parameter :: NDIMS = 2
  integer(c_size_t), parameter :: NX = 6
  integer(c_size_t), parameter :: NY = 12
  character(kind=c_char, len=*), parameter :: &
    & filename = "simple_xy.nc"//c_null_char

  integer(c_int) :: ncid, varid, dimids(NDIMS)
  integer(c_int) :: data_out(NX, NY)
  integer(c_int) :: i, j, stat

  do concurrent (i=1:NX, j=1:NY)
    data_out(i, j) = i*NY + j
  end do

  !> Open file
  stat = nc_create(filename, ior(nc_netcdf4, nc_clobber), ncid)
  call handle_error(stat, "nc_create")

  !> Define dimension
  stat = nc_def_dim(ncid, "x"//c_null_char, NX, dimids(1))
  call handle_error(stat, "nc_def_dim")
  stat = nc_def_dim(ncid, "y"//c_null_char, NY, dimids(2))
  call handle_error(stat, "nc_def_dim")

  !> Define variable
  stat = nc_def_var(ncid, "data"//c_null_char, nc_int, NDIMS, dimids, varid)
  call handle_error(stat, "nc_def_var")

  !> In NetCDF4 not enddef is needed
  ! stat = nc_enddef(ncid)

  !> Put variable
  stat = nc_put_var_int(ncid, varid, data_out)
  call handle_error(stat, "nc_put_var_int")
  
  stat = nc_close(ncid)
  print "(a,/)", "*** SUCCESS writing example file simple_xy.nc!"

end program simple_xy_wr