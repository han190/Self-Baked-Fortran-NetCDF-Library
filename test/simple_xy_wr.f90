program simple_xy_wr

  use module_netcdf
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

  stat = nc_create(filename, nc_clobber, ncid)
  stat = nc_def_dim(ncid, "x"//c_null_char, NX, dimids(1))
  stat = nc_def_dim(ncid, "y"//c_null_char, NY, dimids(2))
  stat = nc_def_var(ncid, "data"//c_null_char, nc_int, NDIMS, dimids, varid)
  stat = nc_enddef(ncid)
  stat = nc_put_var_int(ncid, varid, data_out)
  stat = nc_close(ncid)
  print "(a,/)", "*** SUCCESS writing example file simple_xy.nc!"

end program simple_xy_wr