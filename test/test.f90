program test

  use :: module_test
  implicit none

  character(*), parameter :: path = "./data/"

  call execute_command_line("mkdir -p "//path)
  if (test_simple_xy_wr(path)) then
    print "(a)", "Test simple_xy_wr... passed."
  else
    print "(a)", "Test simple_xy_wr... failed."
    stop
  end if

  if (test_simple_xy_rd(path)) then
    print "(a)", "Test simple_xy_wr... passed."
  else
    print "(a)", "Test simple_xy_wr... failed."
    stop
  end if

end program test