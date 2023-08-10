submodule(module_netcdf) submodule_attribute
  implicit none
contains

  !> Inquire attributes based on group id
  module subroutine inquire_group_attributes(group)
    type(group_type), intent(inout) :: group
    integer(c_int) :: status
    integer :: num_atts, i
    character(kind=c_char, len=num_chars) :: att_name

    !> Inquire number of attributes
    status = nc_inq_natts(group%id, num_atts)
    call handle_error(status, "status")

    !> Allocate group attributes
    if (allocated(group%attributes)) deallocate (group%attributes)
    allocate (group%attributes(num_atts))

    !> Iteratively copy metadata to attributes
    do i = 1, num_atts
      associate (att => group%attributes(i))
        status = nc_inq_attname(group%id, nc_global, i - 1, att_name)
        call handle_error(status, "nc_inq_attname")
        att%name = strip(att_name)

        status = nc_inq_att(group%id, nc_global, att_name, att%type, att%length)
        call handle_error(status, "nc_inq_att")
      end associate
    end do

    call get_grp_att_(group)
  end subroutine inquire_group_attributes

  !> Inquire attributes based on nc_id
  module subroutine inquire_variable_attributes(group, variable)
    type(group_type), intent(inout) :: group
    type(variable_type), intent(inout) :: variable
    integer(c_int) :: status
    integer :: num_atts, i
    character(kind=c_char, len=num_chars) :: att_name
    logical :: scale_offset(2)

    !> Inquire number of attributes
    status = nc_inq_varnatts(group%id, variable%id, num_atts)
    call handle_error(status, "nc_inq_natts")

    !> Reallocate attributes
    if (allocated(variable%attributes)) deallocate (variable%attributes)
    allocate (variable%attributes(num_atts))

    !> Iteratively copy metadata to attributes.
    scale_offset = .false.
    do i = 1, num_atts
      associate (att => variable%attributes(i))
        status = nc_inq_attname(group%id, variable%id, i - 1, att_name)
        call handle_error(status, "nc_inq_attname")
        att%name = strip(att_name)

        if (att%name == "add_offset") then
          scale_offset(1) = .true.
        else if (att%name == "scale_factor") then
          scale_offset(2) = .true.
        end if

        status = nc_inq_att(group%id, variable%id, att_name, att%type, att%length)
        call handle_error(status, "nc_inq_att")
      end associate
    end do

    variable%scale_offset = all(scale_offset)
    call get_var_att_(group, variable)
  end subroutine inquire_variable_attributes

  !> Get variable attribute
  subroutine get_grp_att_(group)
    type(group_type), intent(inout) :: group
    integer(c_int) :: status
    integer :: i

    do i = 1, size(group%attributes)
      associate (att => group%attributes(i))
        select case (att%type)
        case (nc_char)
          allocate (character(kind=c_char, len=att%length) :: att%values(1))
        case (nc_short)
          allocate (integer(int16) :: att%values(att%length))
        case (nc_int)
          allocate (integer(int32) :: att%values(att%length))
        case (nc_int64)
          allocate (integer(int64) :: att%values(att%length))
        case (nc_float)
          allocate (real(real32) :: att%values(att%length))
        case (nc_double)
          allocate (real(real64) :: att%values(att%length))
        end select

        select type (val_ => att%values)
        type is (character(kind=c_char, len=*))
          status = nc_get_att_text(group%id, nc_global, att%name, val_)
          call handle_error(status, "nc_get_att_text")
        type is (integer(int16))
          status = nc_get_att_short(group%id, nc_global, att%name, val_)
          call handle_error(status, "nc_get_att_short")
        type is (integer(int32))
          status = nc_get_att_int(group%id, nc_global, att%name, val_)
          call handle_error(status, "nc_get_att_int")
        type is (integer(int64))
          status = nc_get_att_longlong(group%id, nc_global, att%name, val_)
          call handle_error(status, "nc_get_att_longlong")
        type is (real(real32))
          status = nc_get_att_float(group%id, nc_global, att%name, val_)
          call handle_error(status, "nc_get_att_float")
        type is (real(real64))
          status = nc_get_att_double(group%id, nc_global, att%name, val_)
          call handle_error(status, "nc_get_att_double")
        end select
      end associate
    end do
  end subroutine get_grp_att_

  !> Get variable attribute
  subroutine get_var_att_(group, variable)
    type(group_type), intent(in) :: group
    type(variable_type), intent(inout) :: variable
    integer(c_int) :: status
    integer :: i

    do i = 1, size(variable%attributes)
      associate (att => variable%attributes(i))
        select case (att%type)
        case (nc_char)
          allocate (character(kind=c_char, len=att%length) :: att%values(1))
        case (nc_short)
          allocate (integer(int16) :: att%values(att%length))
        case (nc_int)
          allocate (integer(int32) :: att%values(att%length))
        case (nc_int64)
          allocate (integer(int64) :: att%values(att%length))
        case (nc_float)
          allocate (real(real32) :: att%values(att%length))
        case (nc_double)
          allocate (real(real64) :: att%values(att%length))
        end select

        select type (val_ => att%values)
        type is (character(kind=c_char, len=*))
          status = nc_get_att_text(group%id, variable%id, att%name, val_)
          call handle_error(status, "nc_get_att_text")
        type is (integer(int16))
          status = nc_get_att_short(group%id, variable%id, att%name, val_)
          call handle_error(status, "nc_get_att_short")
        type is (integer(int32))
          status = nc_get_att_int(group%id, variable%id, att%name, val_)
          call handle_error(status, "nc_get_att_int")
        type is (integer(int64))
          status = nc_get_att_longlong(group%id, variable%id, att%name, val_)
          call handle_error(status, "nc_get_att_longlong")
        type is (real(real32))
          status = nc_get_att_float(group%id, variable%id, att%name, val_)
          call handle_error(status, "nc_get_att_float")
        type is (real(real64))
          status = nc_get_att_double(group%id, variable%id, att%name, val_)
          call handle_error(status, "nc_get_att_double")
        end select
      end associate
    end do
  end subroutine get_var_att_

end submodule submodule_attribute
