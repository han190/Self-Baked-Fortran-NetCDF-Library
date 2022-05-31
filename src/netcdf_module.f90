module netcdf_module

  use netcdf
  implicit none

  public :: dataset

  private

  type :: dataset

    private
    character(:), allocatable :: filename
    integer :: status = 0
    integer :: mode = 0
    integer :: ncid = 0

  contains

    private
    procedure :: strerror_
    procedure :: check
    procedure :: open_
    procedure :: create_
    procedure :: close_
    procedure :: read_formatted
    generic :: read (formatted) => read_formatted

  end type dataset

  interface

    module function strerror_(self) result(ret)
      class(dataset), intent(in) :: self
      character(len=80) :: ret
    end function strerror_

    module subroutine check(self)
      class(dataset), intent(inout) :: self
    end subroutine check

    module function open_(self) result(ret)
      class(dataset), intent(inout) :: self
      integer :: ret
    end function open_

    module function create_(self) result(ret)
      class(dataset), intent(inout) :: self
      integer :: ret
    end function create_

    module function close_(self) result(ret)
      class(dataset), intent(in) :: self
      integer :: ret
    end function close_

    module subroutine read_formatted(self, unit, iotype, v_list, iostat, iomsg)
      class(dataset), intent(inout) :: self
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg
    end subroutine read_formatted

  end interface

  integer, parameter :: filename_max_len = 1000

end module netcdf_module
