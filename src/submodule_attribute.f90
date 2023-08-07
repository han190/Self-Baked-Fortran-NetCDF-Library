submodule(module_netcdf) submodule_attribute
  implicit none
contains

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
        att%name = strip(att_name, num_chars)

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
  end subroutine inquire_variable_attributes

end submodule submodule_attribute
