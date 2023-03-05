module module_constant

use iso_c_binding
implicit none

integer(c_int), parameter :: nc_nat = 0
integer(c_int), parameter :: nc_byte = 1
integer(c_int), parameter :: nc_char = 2
integer(c_int), parameter :: nc_short = 3
integer(c_int), parameter :: nc_int = 4
integer(c_int), parameter :: nc_long = nc_int
integer(c_int), parameter :: nc_float = 5
integer(c_int), parameter :: nc_double = 6
integer(c_int), parameter :: nc_ubyte = 7
integer(c_int), parameter :: nc_ushort = 8
integer(c_int), parameter :: nc_uint = 9
integer(c_int), parameter :: nc_int64 = 10
integer(c_int), parameter :: nc_uint64 = 11
integer(c_int), parameter :: nc_string = 12
integer(c_int), parameter :: nc_max_atomic_type = nc_string
integer(c_int), parameter :: nc_vlen = 13
integer(c_int), parameter :: nc_opaque = 14
integer(c_int), parameter :: nc_enum = 15
integer(c_int), parameter :: nc_compound = 16

integer(c_int), parameter :: nc_firstusertypeid = 32
integer(c_signed_char), parameter :: nc_fill_byte = -127
integer(c_signed_char), parameter :: nc_fill_char = 0
integer(c_short), parameter :: nc_fill_short = -32767
integer(c_int), parameter :: nc_fill_int = -2147483647
real(c_float), parameter :: nc_fill_float = 9.9692099683868690e+36
real(c_double), parameter :: nc_fill_double = 9.9692099683868690e+36
! #define nc_fill_ubyte   (255)
! #define nc_fill_ushort  (65535)
! #define nc_fill_uint    (4294967295u)
integer(c_long_long), parameter :: nc_fill_int64 = 9223372036854775806_c_long_long
! #define nc_fill_uint64  ((unsigned long long)18446744073709551614ull)
character(kind=c_char, len=*), parameter :: nc_fill_string = ""

integer(c_int), parameter :: nc_max_byte = 127
integer(c_int), parameter :: nc_min_byte = -nc_max_byte - 1
integer(c_int), parameter :: nc_max_char = 255
integer(c_int), parameter :: nc_max_short = 32767
integer(c_int), parameter :: nc_min_short = -nc_max_short - 1
integer(c_int), parameter :: nc_max_int = 2147483647
integer(c_int), parameter :: nc_min_int = -nc_max_int - 1
real(c_float), parameter :: nc_max_float = 3.402823466e+38
real(c_float), parameter :: nc_min_float = -nc_max_float
real(c_double), parameter :: nc_max_double = 1.7976931348623157e+308_c_double
real(c_double), parameter :: nc_min_double = -nc_max_double
integer(c_int), parameter :: nc_max_ubyte = nc_max_char
! #define nc_max_ushort 65535u
! #define nc_max_uint 4294967295u
integer(c_long_long), parameter :: nc_max_int64 = 9223372036854775807_c_long_long
integer(c_long_long), parameter :: nc_min_int64 = -nc_max_int64 - 1
! #define nc_max_uint64 (18446744073709551615ull)

character(kind=c_char, len=*), parameter :: nc_fill_value = "_fillvalue"
integer(c_int), parameter :: nc_fill = 0
integer(c_int), parameter :: nc_nofill = int(z'100')
integer(c_int), parameter :: nc_nowrite = int(z'0000')
integer(c_int), parameter :: nc_write = int(z'0001')
integer(c_int), parameter :: nc_clobber = int(z'0000')
integer(c_int), parameter :: nc_noclobber = int(z'0004')
integer(c_int), parameter :: nc_diskless = int(z'0008')
integer(c_int), parameter :: nc_mmap = int(z'0010')
integer(c_int), parameter :: nc_64bit_data = int(z'0020')
integer(c_int), parameter :: nc_cdf5 = nc_64bit_data
integer(c_int), parameter :: nc_udf0 = int(z'0040')
integer(c_int), parameter :: nc_udf1 = int(z'0080')
integer(c_int), parameter :: nc_classic_model = int(z'0100')
integer(c_int), parameter :: nc_64bit_offset = int(z'0200')
integer(c_int), parameter :: nc_lock = int(z'0400')
integer(c_int), parameter :: nc_share = int(z'0800')
integer(c_int), parameter :: nc_netcdf4 = int(z'1000')
integer(c_int), parameter :: nc_mpiio = int(z'2000')
integer(c_int), parameter :: nc_mpiposix = nc_mpiio
integer(c_int), parameter :: nc_pnetcdf = nc_mpiio
integer(c_int), parameter :: nc_persist = int(z'4000')
integer(c_int), parameter :: nc_inmemory = int(z'8000')
integer(c_int), parameter :: nc_noattcreord = int(z'20000')
integer(c_int), parameter :: nc_nodimscale_attach = int(z'40000')
integer(c_int), parameter :: nc_max_magic_number_len = 8
integer(c_int), parameter :: nc_format_classic = 1
integer(c_int), parameter :: nc_format_64bit_offset = 2
integer(c_int), parameter :: nc_format_64bit = nc_format_64bit_offset
integer(c_int), parameter :: nc_format_netcdf4 = 3
integer(c_int), parameter :: nc_format_netcdf4_classic = 4
integer(c_int), parameter :: nc_format_64bit_data = 5
integer(c_int), parameter :: nc_format_cdf5 = nc_format_64bit_data
integer(c_int), parameter :: nc_formatx_nc3 = 1
integer(c_int), parameter :: nc_formatx_nc_hdf5 = 2
integer(c_int), parameter :: nc_formatx_nc4 = nc_formatx_nc_hdf5
integer(c_int), parameter :: nc_formatx_nc_hdf4 = 3
integer(c_int), parameter :: nc_formatx_pnetcdf = 4
integer(c_int), parameter :: nc_formatx_dap2 = 5
integer(c_int), parameter :: nc_formatx_dap4 = 6
integer(c_int), parameter :: nc_formatx_udf0 = 8
integer(c_int), parameter :: nc_formatx_udf1 = 9
integer(c_int), parameter :: nc_formatx_nczarr = 10
integer(c_int), parameter :: nc_formatx_undefined = 0

integer(c_int), parameter :: nc_sizehint_default = 0
integer(c_size_t), parameter :: nc_align_chunk = -1
integer(c_long), parameter :: nc_unlimited = 0
integer(c_int), parameter :: nc_global = -1

integer(c_int), parameter :: nc_max_dims = 1024
integer(c_int), parameter :: nc_max_attrs = 8192
integer(c_int), parameter :: nc_max_vars = 8192
integer(c_int), parameter :: nc_max_name = 256
integer(c_int), parameter :: nc_max_var_dims = 1024
integer(c_int), parameter :: nc_max_hdf4_name = nc_max_name

integer(c_int), parameter :: nc_endian_native = 0
integer(c_int), parameter :: nc_endian_little = 1
integer(c_int), parameter :: nc_endian_big = 2

integer(c_int), parameter :: nc_chunked = 0
integer(c_int), parameter :: nc_contiguous = 1
integer(c_int), parameter :: nc_compact = 2
integer(c_int), parameter :: nc_unknown_storage = 3
integer(c_int), parameter :: nc_virtual = 4

integer(c_int), parameter :: nc_nochecksum = 0
integer(c_int), parameter :: nc_fletcher32 = 1

integer(c_int), parameter :: nc_noshuffle = 0
integer(c_int), parameter :: nc_shuffle = 1

integer(c_int), parameter :: nc_min_deflate_level = 0
integer(c_int), parameter :: nc_max_deflate_level = 9

integer(c_int), parameter :: nc_szip_nn = 32
integer(c_int), parameter :: nc_szip_ec = 4

integer(c_int), parameter :: nc_noquantize = 0
integer(c_int), parameter :: nc_quantize_bitgroom = 1
integer(c_int), parameter :: nc_quantize_granularbr = 2
integer(c_int), parameter :: nc_quantize_bitround = 3

character(kind=c_char, len=*), parameter :: &
  & nc_quantize_bitgroom_att_name = &
  & "_quantizebitgroomnumberofsignificantdigits"
character(kind=c_char, len=*), parameter :: &
  & nc_quantize_granularbr_att_name = &
  & "_quantizegranularbitroundnumberofsignificantdigits"
character(kind=c_char, len=*), parameter :: &
  & nc_quantize_bitround_att_name = &
  & "_quantizebitroundnumberofsignificantbits"

integer(c_int), parameter :: nc_quantize_max_float_nsd = 7
integer(c_int), parameter :: nc_quantize_max_float_nsb = 23

integer(c_int), parameter :: nc_quantize_max_double_nsd = 15
integer(c_int), parameter :: nc_quantize_max_double_nsb = 52

! #define nc_issyserr(err)        ((err) > 0)
integer(c_int), parameter :: nc_noerr = 0
integer(c_int), parameter :: nc2_err = -1
integer(c_int), parameter :: nc_ebadid = -33
integer(c_int), parameter :: nc_enfile = -34
integer(c_int), parameter :: nc_eexist = -35
integer(c_int), parameter :: nc_einval = -36
integer(c_int), parameter :: nc_eperm = -37
integer(c_int), parameter :: nc_enotindefine = -38
integer(c_int), parameter :: nc_eindefine = -39
integer(c_int), parameter :: nc_einvalcoords = -40
integer(c_int), parameter :: nc_emaxdims = -41
integer(c_int), parameter :: nc_enameinuse = -42
integer(c_int), parameter :: nc_enotatt = -43
integer(c_int), parameter :: nc_emaxatts = -44
integer(c_int), parameter :: nc_ebadtype = -45
integer(c_int), parameter :: nc_ebaddim = -46
integer(c_int), parameter :: nc_eunlimpos = -47

integer(c_int), parameter :: nc_emaxvars = -48
integer(c_int), parameter :: nc_enotvar = -49
integer(c_int), parameter :: nc_eglobal = -50
integer(c_int), parameter :: nc_enotnc = -51
integer(c_int), parameter :: nc_ests = -52
integer(c_int), parameter :: nc_emaxname = -53
integer(c_int), parameter :: nc_eunlimit = -54
integer(c_int), parameter :: nc_enorecvars = -55
integer(c_int), parameter :: nc_echar = -56

integer(c_int), parameter :: nc_eedge = -57
integer(c_int), parameter :: nc_estride = -58
integer(c_int), parameter :: nc_ebadname = -59

integer(c_int), parameter :: nc_erange = -60
integer(c_int), parameter :: nc_enomem = -61
integer(c_int), parameter :: nc_evarsize = -62
integer(c_int), parameter :: nc_edimsize = -63
integer(c_int), parameter :: nc_etrunc = -64
integer(c_int), parameter :: nc_eaxistype = -65

integer(c_int), parameter :: nc_edap = -66
integer(c_int), parameter :: nc_ecurl = -67
integer(c_int), parameter :: nc_eio = -68
integer(c_int), parameter :: nc_enodata = -69
integer(c_int), parameter :: nc_edapsvc = -70
integer(c_int), parameter :: nc_edas = -71
integer(c_int), parameter :: nc_edds = -72
integer(c_int), parameter :: nc_edmr = nc_edds
integer(c_int), parameter :: nc_edatadds = -73
integer(c_int), parameter :: nc_edatadap = nc_edatadds
integer(c_int), parameter :: nc_edapurl = -74
integer(c_int), parameter :: nc_edapconstraint = -75
integer(c_int), parameter :: nc_etranslation = -76
integer(c_int), parameter :: nc_eaccess = -77
integer(c_int), parameter :: nc_eauth = -78

integer(c_int), parameter :: nc_enotfound = -90
integer(c_int), parameter :: nc_ecantremove = -91
integer(c_int), parameter :: nc_einternal = -92
integer(c_int), parameter :: nc_epnetcdf = -93

integer(c_int), parameter :: nc4_first_error = -100
integer(c_int), parameter :: nc_ehdferr = -101
integer(c_int), parameter :: nc_ecantread = -102
integer(c_int), parameter :: nc_ecantwrite = -103
integer(c_int), parameter :: nc_ecantcreate = -104
integer(c_int), parameter :: nc_efilemeta = -105
integer(c_int), parameter :: nc_edimmeta = -106
integer(c_int), parameter :: nc_eattmeta = -107
integer(c_int), parameter :: nc_evarmeta = -108
integer(c_int), parameter :: nc_enocompound = -109
integer(c_int), parameter :: nc_eattexists = -110
integer(c_int), parameter :: nc_enotnc4 = -111
integer(c_int), parameter :: nc_estrictnc3 = -112
integer(c_int), parameter :: nc_enotnc3 = -113
integer(c_int), parameter :: nc_enopar = -114
integer(c_int), parameter :: nc_eparinit = -115
integer(c_int), parameter :: nc_ebadgrpid = -116
integer(c_int), parameter :: nc_ebadtypid = -117
integer(c_int), parameter :: nc_etypdefined = -118
integer(c_int), parameter :: nc_ebadfield = -119
integer(c_int), parameter :: nc_ebadclass = -120
integer(c_int), parameter :: nc_emaptype = -121
integer(c_int), parameter :: nc_elatefill = -122
integer(c_int), parameter :: nc_elatedef = -123
integer(c_int), parameter :: nc_edimscale = -124
integer(c_int), parameter :: nc_enogrp = -125
integer(c_int), parameter :: nc_estorage = -126
integer(c_int), parameter :: nc_ebadchunk = -127
integer(c_int), parameter :: nc_enotbuilt = -128
integer(c_int), parameter :: nc_ediskless = -129
integer(c_int), parameter :: nc_ecantextend = -130
integer(c_int), parameter :: nc_empi = -131

integer(c_int), parameter :: nc_efilter = -132
integer(c_int), parameter :: nc_ercfile = -133
integer(c_int), parameter :: nc_enullpad = -134
integer(c_int), parameter :: nc_einmemory = -135
integer(c_int), parameter :: nc_enofilter = -136
integer(c_int), parameter :: nc_enczarr = -137
integer(c_int), parameter :: nc_es3 = -138
integer(c_int), parameter :: nc_eempty = -139
integer(c_int), parameter :: nc_eobject = -140
integer(c_int), parameter :: nc_enoobject = -141
integer(c_int), parameter :: nc_eplugin = -142

integer(c_int), parameter :: nc4_last_error = -142
integer(c_int), parameter :: nc_eurl = nc_edapurl
integer(c_int), parameter :: nc_econstraint = nc_edapconstraint
character(kind=c_char), parameter :: &
  & dim_without_variable = &
  & "this is a netcdf dimension but not a netcdf variable."
integer(c_int), parameter :: nc_have_new_chunking_api = 1

end module module_constant
