program test

  use :: module_test
  implicit none

  character(*), parameter :: path = "./data/"

  call execute_command_line("mkdir -p "//path)
  if (test_write(path)) then
    print "(a)", "Test write to NetCDF... passed."
  else
    print "(a)", "Test write to NetCDF... failed."
    stop
  end if

  if (test_read(path)) then
    print "(a)", "Test read from NetCDF... passed."
  else
    print "(a)", "Test read from NetCDF... failed."
    stop
  end if

end program test