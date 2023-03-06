module module_cwrapper

use iso_c_binding
implicit none
public

interface

  !> NetCDF File and Data I/O
  !> ------------------------

  !> nc__create
  function nc__create(path, cmode, initialsz, chunksizehintp, ncidp) &
    & bind(c, name='nc__create')
    import c_char, c_int, c_size_t
    implicit none
    character(kind=c_char) :: path(*)
    integer(c_int), value :: cmode
    integer(c_size_t), value :: initialsz
    integer(c_size_t) :: chunksizehintp
    integer(c_int) :: ncidp
    integer(c_int) :: nc__create
  end function nc__create

  !> nc__enddef
  function nc__enddef(ncid, h_minfree, v_align, v_minfree, r_align) &
    & bind(c, name='nc__enddef')
    import c_int, c_size_t
    implicit none
    integer(c_int), value :: ncid
    integer(c_size_t), value :: h_minfree, v_align, v_minfree, r_align
    integer(c_int) :: nc__enddef
  end function nc__enddef

  !> nc__open
  function nc__open(path, omode, chunksizehintp, ncidp) &
    & bind(c, name='nc__open')
    import c_char, c_int, c_size_t
    implicit none
    character(kind=c_char) :: path(*)
    integer(c_int), value :: omode
    integer(c_size_t) :: chunksizehintp
    integer(c_int) :: ncidp
    integer(c_int) :: nc__open
  end function nc__open

  !> nc_abort
  function nc_abort(ncid) &
    & bind(c, name='nc_abort')
    import c_int
    implicit none
    integer(c_int), value :: ncid
    integer(c_int) :: nc_abort
  end function nc_abort

  !> nc_close
  function nc_close(ncid) &
    & bind(c, name='nc_close')
    import c_int
    implicit none
    integer(c_int), value :: ncid
    integer(c_int) :: nc_close
  end function nc_close

  !> nc_close_memio
  function nc_close_memio(ncid, memio) &
    & bind(c, name='nc_close_memio')
    import c_int, c_ptr
    implicit none
    integer(c_int), value :: ncid
    type(c_ptr) :: memio
    integer(c_int) :: nc_close_memio
  end function nc_close_memio

  !> nc_create
  function nc_create(path, cmode, ncidp) &
    & bind(c, name='nc_create')
    import c_char, c_int, c_size_t
    implicit none
    character(kind=c_char) :: path(*)
    integer(c_int), value :: cmode
    integer(c_int) :: ncidp
    integer(c_int) :: nc_create
  end function nc_create

  !> nc_create_mem
  function nc_create_mem(path, mode, initialsize, ncidp) &
    & bind(c, name='nc_create_mem')
    import c_char, c_int, c_size_t
    implicit none
    character(kind=c_char) :: path(*)
    integer(c_int), value :: mode
    integer(c_size_t), value :: initialsize
    integer(c_int) :: ncidp
    integer(c_int) :: nc_create_mem
  end function nc_create_mem

  !> nc_create_par
  !> (not implemented yet)

  !> nc_create_par_fortran
  function nc_create_par_fortran(path, cmode, comm, info, ncidp) &
    & bind(c, name='nc_create_par_fortran')
    import c_int, c_char
    implicit none
    character(kind=c_char) :: path(*)
    integer(c_int), value :: cmode, comm, info
    integer(c_int) :: ncidp
    integer(c_int) :: nc_create_par_fortran
  end function nc_create_par_fortran

  !> nc_def_user_format
  !> (not implemented yet)

  !> nc_enddef
  function nc_enddef(ncid) &
    & bind(c, name='nc_enddef')
    import c_int
    implicit none
    integer(c_int), value :: ncid
    integer(c_int) :: nc_enddef
  end function nc_enddef

  !> nc_get_alignment
  function nc_get_alignment(thresholdp, alignmentp) &
    & bind(c, name='nc_get_alignment')
    import c_int
    implicit none
    integer(c_int) :: thresholdp, alignmentp
    integer(c_int) :: nc_get_alignment
  end function nc_get_alignment

  !> nc_get_chunk_cache
  function nc_get_chunk_cache(sizep, nelemsp, preemptionp) &
    & bind(c, name='nc_get_chunk_cache')
    import c_int, c_size_t, c_float
    implicit none
    integer(c_size_t) :: sizep, nelemsp
    real(c_float) :: preemptionp
    integer(c_int) :: nc_get_chunk_cache
  end function nc_get_chunk_cache

  !> nc_inq
  function nc_inq(ncid, ndimsp, nvarsp, nattsp, unlimdimidp) &
    & bind(c, name='nc_inq')
    import c_int
    implicit none
    integer(c_int), value :: ncid
    integer(c_int) :: ndimsp, nvarsp, nattsp, unlimdimidp
    integer(c_int) :: nc_inq
  end function nc_inq

  !> nc_inq_format
  function nc_inq_format(ncid, formatp) &
    & bind(c, name='nc_inq_format')
    import c_int
    implicit none
    integer(c_int), value :: ncid
    integer(c_int) :: formatp
    integer(c_int) :: nc_inq_format
  end function nc_inq_format

  !> nc_inq_format_extended
  function nc_inq_format_extended(ncid, formatp, modep) &
    & bind(c, name='nc_inq_format_extended')
    import c_int
    implicit none
    integer(c_int), value :: ncid
    integer(c_int) :: formatp, modep
    integer(c_int) :: nc_inq_format_extended
  end function nc_inq_format_extended

  !> nc_inq_path
  function nc_inq_path(ncid, pathlen, path) &
    & bind(c, name='nc_inq_path')
    import c_int, c_size_t, c_char
    implicit none
    integer(c_int), value :: ncid
    integer(c_size_t) :: pathlen
    character(kind=c_char) :: path(*)
    integer(c_int) :: nc_inq_path
  end function nc_inq_path

  !> nc_inq_type
  !> nc_type not implemented, use c_int for now.
  function nc_inq_type(ncid, xtype, name, size) &
    & bind(c, name='nc_inq_type')
    import c_int, c_char, c_size_t
    implicit none
    integer(c_int), value :: ncid
    integer(c_int), value :: xtype
    character(kind=c_char) :: name(*)
    integer(c_size_t) :: size
    integer(c_int) :: nc_inq_type
  end function nc_inq_type

  !> nc_inq_user_format
  !> (not implemented yet)

  !> nc_open
  function nc_open(path, omode, ncidp) &
    & bind(c, name='nc_open')
    import c_int, c_char
    implicit none
    character(kind=c_char) :: path(*)
    integer(c_int), value :: omode
    integer(c_int) :: ncidp
    integer(c_int) :: nc_open
  end function nc_open

  !> nc_open_mem
  function nc_open_mem(path, omode, size, memory, ncidp) &
    & bind(c, name='nc_open_mem')
    import c_int, c_size_t, c_ptr, c_char
    implicit none
    character(kind=c_char) :: path(*)
    integer(c_int), value :: omode
    integer(c_size_t), value :: size
    type(c_ptr), value :: memory
    integer(c_int) :: ncidp
    integer(c_int) :: nc_open_mem
  end function nc_open_mem

  !> nc_open_memio
  !> (not implemented yet)

  !> nc_open_par
  !> (not implemented yet)

  !> nc_open_par_fortran
  function nc_open_par_fortran(path, omode, comm, info, ncidp) &
    & bind(c, name='nc_open_par_fortran')
    import c_int, c_char
    implicit none
    character(kind=c_char) :: path(*)
    integer(c_int), value :: omode, comm, info
    integer(c_int) :: ncidp
    integer(c_int) :: nc_open_par_fortran
  end function nc_open_par_fortran

  !> nc_redef
  function nc_redef(ncid) &
    & bind(c, name='nc_redef')
    import c_int
    implicit none
    integer(c_int), value :: ncid
    integer(c_int) :: nc_redef
  end function nc_redef

  !> nc_set_alignment
  function nc_set_alignment(threshold, alignment) &
    & bind(c, name='nc_set_alignment')
    import c_int
    implicit none
    integer(c_int), value :: threshold, alignment
    integer(c_int) :: nc_set_alignment
  end function nc_set_alignment

  !> nc_set_chunk_cache
  function nc_set_chunk_cache(size, nelems, preemption) &
    & bind(c, name='nc_set_chunk_cache')
    import c_int, c_size_t, c_float
    implicit none
    integer(c_size_t), value :: size, nelems
    real(c_float), value :: preemption
    integer(c_int) :: nc_set_chunk_cache
  end function nc_set_chunk_cache

  !> nc_set_fill
  function nc_set_fill(ncid, fillmode, old_modep) &
    & bind(c, name='nc_set_fill')
    import c_int
    implicit none
    integer(c_int), value :: ncid, fillmode
    integer(c_int) :: old_modep
    integer(c_int) :: nc_set_fill
  end function nc_set_fill

  !> nc_sync
  function nc_sync(ncid) &
    & bind(c, name='nc_sync')
    import c_int
    implicit none
    integer(c_int), value :: ncid
    integer(c_int) :: nc_sync
  end function nc_sync

  !> nc_var_par_access
  function nc_var_par_access(ncid, varid, par_access) &
    & bind(c, name='nc_var_par_access')
    import c_int
    implicit none
    integer(c_int), value :: ncid, varid, par_access
    integer(c_int) :: nc_var_par_access
  end function nc_var_par_access

  !> Dimension
  !> ---------

  !> nc_def_dim
  function nc_def_dim(ncid, name, len, idp) &
    & bind(c, name='nc_def_dim')
    import c_int, c_char, c_size_t
    implicit none
    integer(c_int), value :: ncid
    character(kind=c_char) :: name
    integer(c_size_t), value :: len
    integer(c_int) :: idp
    integer(c_int) :: nc_def_dim
  end function nc_def_dim

  !> nc_inq_dim
  function nc_inq_dim(ncid, dimid, name, lenp) &
    & bind(c, name='nc_inq_dim')
    import c_int, c_char, c_size_t
    implicit none
    integer(c_int), value :: ncid, dimid
    character(kind=c_char) :: name(*)
    integer(c_size_t) :: lenp
    integer(c_int) :: nc_inq_dim
  end function nc_inq_dim

  !> nc_inq_dimid
  function nc_inq_dimid(ncid, name, idp) &
    & bind(c, name='nc_inq_dimid')
    import c_int, c_char
    implicit none
    integer(c_int), value :: ncid
    character(kind=c_char) :: name(*)
    integer(c_int) :: idp
    integer(c_int) :: nc_inq_dimid
  end function nc_inq_dimid

  !> nc_inq_dimlen
  function nc_inq_dimlen(ncid, dimid, lenp) &
    & bind(c, name='nc_inq_dimlen')
    import c_int, c_size_t
    implicit none
    integer(c_int), value :: ncid, dimid
    integer(c_size_t) :: lenp
    integer(c_int) :: nc_inq_dimlen
  end function nc_inq_dimlen

  !> nc_inq_dimname
  function nc_inq_dimname(ncid, dimid, name) &
    & bind(c, name='nc_inq_dimname')
    import c_int, c_char
    implicit none
    integer(c_int), value :: ncid, dimid
    character(kind=c_char) :: name(*)
    integer(c_int) :: nc_inq_dimname
  end function nc_inq_dimname

  !> nc_inq_ndims
  function nc_inq_ndims(ncid, ndimsp) &
    & bind(c, name='nc_inq_ndims')
    import c_int
    implicit none
    integer(c_int), value :: ncid
    integer(c_int) :: ndimsp
    integer(c_int) :: nc_inq_ndims
  end function nc_inq_ndims

  !> nc_inq_unlimdim
  function nc_inq_unlimdim(ncid, unlimdimidp) &
    & bind(c, name='nc_inq_unlimdim')
    import c_int
    implicit none
    integer(c_int), value :: ncid
    integer(c_int) :: unlimdimidp
    integer(c_int) :: nc_inq_unlimdim
  end function nc_inq_unlimdim

  !> nc_rename_dim
  function nc_rename_dim(ncid, dimid, name) &
    & bind(c, name='nc_rename_dim')
    import c_int, c_char
    implicit none
    integer(c_int), value :: ncid, dimid
    character(kind=c_char) :: name(*)
    integer(c_int) :: nc_rename_dim
  end function nc_rename_dim

  !> Attributes
  !> ----------

  !> nc_del_att
  function nc_del_att(ncid, varid, name) &
    & bind(c, name='nc_del_att')
    import c_int, c_char
    implicit none
    integer(c_int), value :: ncid, varid
    character(kind=c_char) :: name(*)
    integer(c_int) :: nc_del_att
  end function nc_del_att

  !> nc_get_att
  function nc_get_att(ncid, varid, name, value) &
    & bind(c, name='nc_get_att')
    import c_int, c_char, c_ptr
    implicit none
    integer(c_int), value :: ncid, varid
    character(kind=c_char) :: name(*)
    type(c_ptr) :: value(*)
    integer(c_int) :: nc_get_att
  end function nc_get_att

  !> nc_get_att_double
  function nc_get_att_double(ncid, varid, name, value) &
    & bind(c, name='nc_get_att_double')
    import c_int, c_char, c_double
    implicit none
    integer(c_int), value :: ncid, varid
    character(kind=c_char) :: name(*)
    real(c_double) :: value(*)
    integer(c_int) :: nc_get_att_double
  end function nc_get_att_double

  !> nc_get_att_float
  function nc_get_att_float(ncid, varid, name, value) &
    & bind(c, name='nc_get_att_float')
    import c_int, c_char, c_float
    implicit none
    integer(c_int), value :: ncid, varid
    character(kind=c_char) :: name(*)
    real(c_float) :: value(*)
    integer(c_int) :: nc_get_att_float
  end function nc_get_att_float

  !> nc_get_att_int
  function nc_get_att_int(ncid, varid, name, value) &
    & bind(c, name='nc_get_att_int')
    import c_int, c_char
    implicit none
    integer(c_int), value :: ncid, varid
    character(kind=c_char) :: name(*)
    integer(c_int) :: value(*)
    integer(c_int) :: nc_get_att_int
  end function nc_get_att_int

  !> nc_get_att_long
  function nc_get_att_long(ncid, varid, name, value) &
    & bind(c, name='nc_get_att_long')
    import c_int, c_char, c_long
    implicit none
    integer(c_int), value :: ncid, varid
    character(kind=c_char) :: name(*)
    integer(c_long) :: value(*)
    integer(c_int) :: nc_get_att_long
  end function nc_get_att_long

  !> nc_get_att_longlong
  function nc_get_att_longlong(ncid, varid, name, value) &
    & bind(c, name='nc_get_att_longlong')
    import c_int, c_char, c_long_long
    implicit none
    integer(c_int), value :: ncid, varid
    character(kind=c_char) :: name(*)
    integer(c_long_long) :: value(*)
    integer(c_int) :: nc_get_att_longlong
  end function nc_get_att_longlong

  !> nc_get_att_schar
  function nc_get_att_schar(ncid, varid, name, value) &
    & bind(c, name='nc_get_att_schar')
    import c_int, c_char, c_signed_char
    implicit none
    integer(c_int), value :: ncid, varid
    character(kind=c_char) :: name(*)
    character(kind=c_signed_char), dimension(*) :: value(*)
    integer(c_int) :: nc_get_att_schar
  end function nc_get_att_schar

  !> nc_get_att_short
  function nc_get_att_short(ncid, varid, name, value) &
    & bind(c, name='nc_get_att_short')
    import c_int, c_char, c_short
    implicit none
    integer(c_int), value :: ncid, varid
    character(kind=c_char) :: name(*)
    integer(c_short) :: value(*)
    integer(c_int) :: nc_get_att_short
  end function nc_get_att_short

  !> nc_get_att_string
  function nc_get_att_string(ncid, varid, name, value) &
    & bind(c, name='nc_get_att_string')
    import c_int, c_char
    implicit none
    integer(c_int), value :: ncid, varid
    character(kind=c_char) :: name(*)
    character(kind=c_char), dimension(*) :: value(*)
    integer(c_int) :: nc_get_att_string
  end function nc_get_att_string

  !> nc_get_att_text
  function nc_get_att_text(ncid, varid, name, value) &
    & bind(c, name='nc_get_att_text')
    import c_int, c_char
    implicit none
    integer(c_int), value :: ncid, varid
    character(kind=c_char) :: name(*), value(*)
    integer(c_int) :: nc_get_att_text
  end function nc_get_att_text

  !> nc_inq_att
  function nc_inq_att(ncid, varid, name, xtypep, lenp) &
    & bind(c, name='nc_inq_att')
    import c_int, c_char, c_size_t
    implicit none
    integer(c_int), value :: ncid, varid
    character(kind=c_char) :: name(*)
    integer(c_int) :: xtypep
    integer(c_size_t) :: lenp
    integer(c_int) :: nc_inq_att
  end function nc_inq_att

  !> nc_inq_natts
  function nc_inq_natts(ncid, nattsp) &
    & bind(c, name='nc_inq_natts')
    import c_int
    implicit none
    integer(c_int), value :: ncid
    integer(c_int) :: nattsp
    integer(c_int) :: nc_inq_natts
  end function nc_inq_natts

  !> nc_inq_attname
  function nc_inq_attname(ncid, varid, attnum, name) &
    & bind(c, name='nc_inq_attname')
    import c_int, c_char
    implicit none
    integer(c_int), value :: ncid, varid, attnum
    character(kind=c_char) :: name(*)
    integer(c_int) :: nc_inq_attname
  end function nc_inq_attname

  !> Variables
  !> ---------

  !> nc_inq_vardimid
  function nc_inq_vardimid(ncid, varid, dimidsp) &
    & bind(c, name='nc_inq_vardimid')
    import c_int
    implicit none
    integer(c_int), value :: ncid, varid
    integer(c_int) :: dimidsp(*)
    integer(c_int) :: nc_inq_vardimid
  end function nc_inq_vardimid

  !> nc_inq_varid
  function nc_inq_varid(ncid, name, varidp) &
    & bind(c, name='nc_inq_varid')
    import c_int, c_char
    implicit none
    integer(c_int), value :: ncid
    character(kind=c_char) :: name(*)
    integer(c_int) :: varidp
    integer(c_int) :: nc_inq_varid
  end function nc_inq_varid

  !> nc_inq_varnatts
  function nc_inq_varnatts(ncid, varid, nattsp) &
    & bind(c, name='nc_inq_varnatts')
    import c_int
    implicit none
    integer(c_int), value :: ncid, varid
    integer(c_int) :: nattsp
    integer(c_int) :: nc_inq_varnatts
  end function nc_inq_varnatts

  !> nc_inq_varndims
  function nc_inq_varndims(ncid, varid, ndimsp) &
    & bind(c, name='nc_inq_varndims')
    import c_int
    implicit none
    integer(c_int), value :: ncid, varid
    integer(c_int) :: ndimsp
    integer(c_int) :: nc_inq_varndims
  end function nc_inq_varndims

  !> nc_inq_vartype
  function nc_inq_vartype(ncid, varid, typep) &
    & bind(c, name='nc_inq_vartype')
    import c_int
    implicit none
    integer(c_int), value :: ncid, varid
    integer(c_int) :: typep
    integer(c_int) :: nc_inq_vartype
  end function nc_inq_vartype

  !> nc_get_var_double
  function nc_get_var_double(ncid, varid, ip) &
    & bind(c, name='nc_get_var_double')
    import c_int, c_double
    implicit none
    integer(c_int), value :: ncid, varid
    real(c_double) :: ip(*)
    integer(c_int) :: nc_get_var_double
  end function nc_get_var_double

  !> nc_get_var_float
  function nc_get_var_float(ncid, varid, ip) &
    & bind(c, name='nc_get_var_float')
    import c_int, c_float
    implicit none
    integer(c_int), value :: ncid, varid
    real(c_float) :: ip(*)
    integer(c_int) :: nc_get_var_float
  end function nc_get_var_float

  !> nc_get_var_int
  function nc_get_var_int(ncid, varid, ip) &
    & bind(c, name='nc_get_var_int')
    import c_int
    implicit none
    integer(c_int), value :: ncid, varid
    integer(c_int) :: ip(*)
    integer(c_int) :: nc_get_var_int
  end function nc_get_var_int

  !> nc_get_var_longlong
  function nc_get_var_longlong(ncid, varid, ip) &
    & bind(c, name='nc_get_var_longlong')
    import c_int, c_long_long
    implicit none
    integer(c_int), value :: ncid, varid
    integer(c_long_long) :: ip(*)
    integer(c_int) :: nc_get_var_longlong
  end function nc_get_var_longlong

  !> nc_get_var_short
  function nc_get_var_short(ncid, varid, ip) &
    & bind(c, name='nc_get_var_short')
    import c_int, c_short
    implicit none
    integer(c_int), value :: ncid, varid
    integer(c_short) :: ip(*)
    integer(c_int) :: nc_get_var_short
  end function nc_get_var_short

  !> Groups
  !> ------

  !> nc_def_grp
  function nc_def_grp(parent_ncid, name, new_ncid) &
    & bind(c, name='nc_def_grp')
    import c_int, c_char
    implicit none
    integer(c_int), value :: parent_ncid
    character(kind=c_char) :: name(*)
    integer(c_int) :: new_ncid
    integer(c_int) :: nc_def_grp
  end function nc_def_grp

  !> nc_inq_dimids
  function nc_inq_dimids(ncid, ndims, dimids, include_parents) &
    & bind(c, name='nc_inq_dimids')
    import c_int, c_char
    implicit none
    integer(c_int), value :: ncid, include_parents
    integer(c_int) :: ndims, dimids(*)
    integer(c_int) :: nc_inq_dimids
  end function nc_inq_dimids

  !> nc_inq_grp_full_ncid
  function nc_inq_grp_full_ncid(ncid, full_name, grp_ncid) &
    & bind(c, name='nc_inq_grp_full_ncid')
    import c_int, c_char
    implicit none
    integer(c_int), value :: ncid
    character(kind=c_char) :: full_name
    integer(c_int) :: grp_ncid
    integer(c_int) :: nc_inq_grp_full_ncid
  end function nc_inq_grp_full_ncid

  !> nc_inq_grp_ncid
  function nc_inq_grp_ncid(ncid, grp_name, grp_ncid) &
    & bind(c, name='nc_inq_grp_ncid')
    import c_int, c_char
    implicit none
    integer(c_int), value :: ncid
    character(kind=c_char) :: grp_name(*)
    integer(c_int) :: grp_ncid
    integer(c_int) :: nc_inq_grp_ncid
  end function nc_inq_grp_ncid

  !> nc_inq_grp_parent
  function nc_inq_grp_parent(ncid, parent_ncid) &
    & bind(c, name='nc_inq_grp_parent')
    import c_int
    implicit none
    integer(c_int), value :: ncid
    integer(c_int) :: parent_ncid
    integer(c_int) :: nc_inq_grp_parent
  end function nc_inq_grp_parent

  !> nc_inq_grpname
  function nc_inq_grpname(ncid, name) &
    & bind(c, name='nc_inq_grpname')
    import c_int, c_char
    integer(c_int), value :: ncid
    character(kind=c_char) :: name(*)
    integer(c_int) :: nc_inq_grpname
  end function nc_inq_grpname

  !> nc_inq_grpname_full
  function nc_inq_grpname_full(ncid, lenp, full_name) &
    & bind(c, name='nc_inq_grpname_full')
    import c_int, c_size_t, c_char
    implicit none
    integer(c_int), value :: ncid
    integer(c_size_t) :: lenp
    character(kind=c_char) :: full_name(*)
    integer(c_int) :: nc_inq_grpname_full
  end function nc_inq_grpname_full

  !> nc_inq_grpname_len
  function nc_inq_grpname_len(ncid, lenp) &
    & bind(c, name='nc_inq_grpname_len')
    import c_int, c_size_t
    implicit none
    integer(c_int), value :: ncid
    integer(c_size_t) :: lenp
    integer(c_int) :: nc_inq_grpname_len
  end function nc_inq_grpname_len

  !> nc_inq_grps
  function nc_inq_grps(ncid, numgrps, ncids) &
    & bind(c, name='nc_inq_grps')
    import c_int
    implicit none
    integer(c_int), value :: ncid
    integer(c_int) :: numgrps
    integer(c_int) :: ncids
    integer(c_int) :: nc_inq_grps
  end function nc_inq_grps

  !> nc_inq_ncid
  function nc_inq_ncid(ncid, name, grp_ncid) &
    & bind(c, name='nc_inq_ncid')
    import c_int, c_char
    implicit none
    integer(c_int), value :: ncid
    character(kind=c_char) :: name(*)
    integer(c_int) :: grp_ncid
    integer(c_int) :: nc_inq_ncid
  end function nc_inq_ncid

  !> nc_inq_typeids
  function nc_inq_typeids(ncid, ntypes, typeids) &
    & bind(c, name='nc_inq_typeids')
    import c_int
    implicit none
    integer(c_int), value :: ncid
    integer(c_int) :: ntypes, typeids
    integer(c_int) :: nc_inq_typeids
  end function nc_inq_typeids

  !> nc_inq_varids
  function nc_inq_varids(ncid, nvars, nvarids) &
    & bind(c, name='nc_inq_varids')
    import c_int
    implicit none
    integer(c_int), value :: ncid
    integer(c_int) :: nvars, nvarids
    integer(c_int) :: nc_inq_varids
  end function nc_inq_varids

  !> nc_rename_grp
  function nc_rename_grp(grpid, name) &
    & bind(c, name='nc_rename_grp')
    import c_int, c_char
    implicit none
    integer(c_int), value :: grpid
    character(kind=c_char) :: name(*)
    integer(c_int) :: nc_rename_grp
  end function nc_rename_grp

  !> nc_show_metadata
  function nc_show_metadata(ncid) &
    & bind(c, name='nc_show_metadata')
    import c_int
    implicit none
    integer(c_int), value :: ncid
    integer(c_int) :: nc_show_metadata
  end function nc_show_metadata

  !> NetCDF Error Handling
  !> ---------------------

  !> nc_strerror
  function nc_strerror(ncerr1) &
    & bind(c, name='nc_strerror')
    import c_int, c_ptr
    implicit none
    integer(c_int) :: ncerr1
    type(c_ptr) :: nc_strerror
  end function nc_strerror

end interface

end module module_cwrapper
