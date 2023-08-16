module module_constant

  !> This module is converted from netcdf.h
  !> https://github.com/Unidata/netcdf-c/blob/main/include/netcdf.h
  use :: iso_c_binding
  implicit none
  public

  ! /*! The nc_type type is just an int. */
  integer(c_int), parameter :: nc_type = c_int

  ! /*
  !  *  the netcdf external data types
  !  */
  integer(c_int), parameter :: nc_nat = 0 ! /**< not a type */
  integer(c_int), parameter :: nc_byte = 1 ! /**< signed 1 byte integer */
  integer(c_int), parameter :: nc_char = 2 ! /**< iso/ascii character */
  integer(c_int), parameter :: nc_short = 3 ! /**< signed 2 byte integer */
  integer(c_int), parameter :: nc_int = 4 ! /**< signed 4 byte integer */
  integer(c_int), parameter :: nc_long = nc_int ! /**< \deprecated required for backward compatibility. */
  integer(c_int), parameter :: nc_float = 5 ! /**< single precision floating point number */
  integer(c_int), parameter :: nc_double = 6 ! /**< double precision floating point number */
  integer(c_int), parameter :: nc_ubyte = 7 ! /**< unsigned 1 byte int */
  integer(c_int), parameter :: nc_ushort = 8 ! /**< unsigned 2-byte int */
  integer(c_int), parameter :: nc_uint = 9 ! /**< unsigned 4-byte int */
  integer(c_int), parameter :: nc_int64 = 10 ! /**< signed 8-byte int */
  integer(c_int), parameter :: nc_uint64 = 11 ! /**< unsigned 8-byte int */
  integer(c_int), parameter :: nc_string = 12 ! /**< string */

  integer(c_int), parameter :: nc_max_atomic_type = nc_string ! /**< @internal largest atomic type. */

  ! /* the following are use internally in support of user-defines
  !  * types. they are also the class returned by nc_inq_user_type. */
  integer(c_int), parameter :: nc_vlen = 13 ! /**< vlen (variable-length) types */
  integer(c_int), parameter :: nc_opaque = 14 ! /**< opaque types */
  integer(c_int), parameter :: nc_enum = 15 ! /**< enum types */
  integer(c_int), parameter :: nc_compound = 16 ! /**< compound types */

  ! /** @internal define the first user defined type id (leave some
  !  * room) */
  integer(c_int), parameter :: nc_firstusertypeid = 32

  ! /** default fill value. this is used unless _fillvalue attribute
  !  * is set.  these values are stuffed into newly allocated space as
  !  * appropriate.  the hope is that one might use these to notice that a
  !  * particular datum has not been set. */
  ! /**@{*/
  integer(c_signed_char), parameter :: nc_fill_byte = -127
  integer(c_signed_char), parameter :: nc_fill_char = 0
  integer(c_short), parameter :: nc_fill_short = -32767
  integer(c_int), parameter :: nc_fill_int = -2147483647
  real(c_float), parameter :: nc_fill_float = 9.9692099683868690e+36_c_float ! /* near 15 * 2^119 */
  real(c_double), parameter :: nc_fill_double = 9.9692099683868690e+36_c_double
  integer(c_int), parameter :: nc_fill_ubyte = 255
  integer(c_int), parameter :: nc_fill_ushort = 65535
  ! #define nc_fill_uint    (4294967295u)
  integer(c_long_long), parameter :: nc_fill_int64 = -9223372036854775806_c_long_long
  ! #define nc_fill_uint64  ((unsigned long long)18446744073709551614ull)
  character(kind=c_char, len=*), parameter :: nc_fill_string = ""
  ! /**@}*/

  ! /*! max or min values for a type. nothing greater/smaller can be
  !  * stored in a netcdf file for their associated types. recall that a c
  !  * compiler may define int to be any length it wants, but a nc_int is
  !  * *always* a 4 byte signed int. on a platform with 64 bit ints,
  !  * there will be many ints which are outside the range supported by
  !  * nc_int. but since nc_int is an external format, it has to mean the
  !  * same thing everywhere. */
  ! /**@{*/
  integer(c_int), parameter :: nc_max_byte = 127
  integer(c_int), parameter :: nc_min_byte = -nc_max_byte - 1
  integer(c_int), parameter :: nc_max_char = 255
  integer(c_int), parameter :: nc_max_short = 32767
  integer(c_int), parameter :: nc_min_short = -nc_max_short - 1
  integer(c_int), parameter :: nc_max_int = 2147483647
  integer(c_int), parameter :: nc_min_int = -nc_max_int - 1
  real(c_float), parameter :: nc_max_float = 3.402823466e+38_c_float
  real(c_float), parameter :: nc_min_float = -nc_max_float
  real(c_double), parameter :: nc_max_double = 1.7976931348623157e+308_c_double
  real(c_double), parameter :: nc_min_double = -nc_max_double
  integer(c_int), parameter :: nc_max_ubyte = nc_max_char
  ! #define nc_max_ushort 65535u
  ! #define nc_max_uint 4294967295u
  integer(c_long_long), parameter :: nc_max_int64 = 9223372036854775807_c_long_long
  integer(c_long_long), parameter :: nc_min_int64 = -9223372036854775807_c_long_long - 1
  ! #define nc_max_uint64 (18446744073709551615ull)
  ! /**@}*/

  ! /** Name of fill value attribute.  If you wish a variable to use a
  !  * different value than the above defaults, create an attribute with
  !  * the same type as the variable and this reserved name. The value you
  !  * give the attribute will be used as the fill value for that
  !  * variable. */
  character(kind=c_char, len=*), parameter :: nc_fill_value = "_fillvalue"
  integer(c_int), parameter :: nc_fill = 0
  integer(c_int), parameter :: nc_nofill = int(z'100')

  ! /* Define the ioflags bits for nc_create and nc_open.
  !    Currently unused in lower 16 bits:
  !         0x0002
  !    All upper 16 bits are unused except
  !         0x20000
  ! */

  ! /* Lower 16 bits */
  integer(c_int), parameter :: nc_nowrite = int(z'0000') ! /**< Set read-only access for nc_open(). */
  integer(c_int), parameter :: nc_write = int(z'0001') ! /**< Set read-write access for nc_open(). */

  integer(c_int), parameter :: nc_clobber = int(z'0000') ! /**< Destroy existing file. Mode flag for nc_create(). */
  integer(c_int), parameter :: nc_noclobber = int(z'0004') ! /**< Don't destroy existing file. Mode flag for nc_create(). */
  integer(c_int), parameter :: nc_diskless = int(z'0008') ! /**< Use diskless file. Mode flag for nc_open() or nc_create(). */
  integer(c_int), parameter :: nc_mmap = int(z'0010') ! /**< \deprecated Use diskless file with mmap. Mode flag for nc_open() or nc_create()*/

  integer(c_int), parameter :: nc_64bit_data = int(z'0020') ! /**< CDF-5 format: classic model but 64 bit dimensions and sizes */
  integer(c_int), parameter :: nc_cdf5 = nc_64bit_data ! /**< Alias NC_CDF5 to NC_64BIT_DATA */

  integer(c_int), parameter :: nc_udf0 = int(z'0040') ! /**< User-defined format 0. */
  integer(c_int), parameter :: nc_udf1 = int(z'0080') ! /**< User-defined format 1. */

  integer(c_int), parameter :: nc_classic_model = int(z'0100') ! /**< Enforce classic model on netCDF-4. Mode flag for nc_create(). */
  integer(c_int), parameter :: nc_64bit_offset = int(z'0200') ! /**< Use large (64-bit) file offsets. Mode flag for nc_create(). */

  ! /** \deprecated The following flag currently is ignored, but use in
  !  * nc_open() or nc_create() may someday support use of advisory
  !  * locking to prevent multiple writers from clobbering a file
  !  */
  integer(c_int), parameter :: nc_lock = int(z'0400')

  ! /** Share updates, limit caching.
  ! Use this in mode flags for both nc_create() and nc_open(). */
  integer(c_int), parameter :: nc_share = int(z'0800')

  integer(c_int), parameter :: nc_netcdf4 = int(z'1000') ! /**< Use netCDF-4/HDF5 format. Mode flag for nc_create(). */

  ! /** The following 3 flags are deprecated as of 4.6.2. Parallel I/O is now
  !  * initiated by calling nc_create_par and nc_open_par, no longer by flags.
  !  */
  integer(c_int), parameter :: nc_mpiio = int(z'2000') ! /**< \deprecated */
  integer(c_int), parameter :: nc_mpiposix = nc_mpiio ! /**< \deprecated */
  integer(c_int), parameter :: nc_pnetcdf = nc_mpiio ! /**< \deprecated */

  integer(c_int), parameter :: nc_persist = int(z'4000') ! /**< Save diskless contents to disk. Mode flag for nc_open() or nc_create() */
  integer(c_int), parameter :: nc_inmemory = int(z'8000') ! /**< Read from memory. Mode flag for nc_open() or nc_create() */

  ! /* Upper 16 bits */
  integer(c_int), parameter :: nc_noattcreord = int(z'20000') ! /**< Disable the netcdf-4 (hdf5) attribute creation order tracking */
  integer(c_int), parameter :: nc_nodimscale_attach = int(z'40000') ! /**< Disable the netcdf-4 (hdf5) attaching of dimscales to variables (#2128) */

  integer(c_int), parameter :: nc_max_magic_number_len = 8 ! /**< Max len of user-defined format magic number. */

  ! /** Format specifier for nc_set_default_format() and returned
  !  *  by nc_inq_format. This returns the format as provided by
  !  *  the API. See nc_inq_format_extended to see the true file format.
  !  *  Starting with version 3.6, there are different format netCDF files.
  !  *  4.0 introduces the third one. \see netcdf_format
  !  */
  ! /**@{*/
  integer(c_int), parameter :: nc_format_classic = 1
  ! /* After adding CDF5 support, the NC_FORMAT_64BIT
  !    flag is somewhat confusing. So, it is renamed.
  !    Note that the name in the contributed code
  !    NC_FORMAT_64BIT was renamed to NC_FORMAT_CDF2
  ! */
  integer(c_int), parameter :: nc_format_64bit_offset = 2
  integer(c_int), parameter :: nc_format_64bit = nc_format_64bit_offset ! /**< \deprecated Saved for compatibility.  Use NC_FORMAT_64BIT_OFFSET or NC_FORMAT_64BIT_DATA, from netCDF 4.4.0 onwards. */
  integer(c_int), parameter :: nc_format_netcdf4 = 3
  integer(c_int), parameter :: nc_format_netcdf4_classic = 4
  integer(c_int), parameter :: nc_format_64bit_data = 5

  ! /* Alias */
  integer(c_int), parameter :: nc_format_cdf5 = nc_format_64bit_data

  ! /* Define a mask covering format flags only */
  !> Use line continuation sign to avoid [-Werror=line-truncation]
  integer(c_int), parameter :: nc_format_all = &
    & ior(nc_64bit_offset, ior(nc_64bit_data, ior(nc_classic_model, ior(nc_netcdf4, ior(nc_udf0, nc_udf1)))))

  ! /**@}*/

  ! /** Extended format specifier returned by  nc_inq_format_extended()
  !  *  Added in version 4.3.1. This returns the true format of the
  !  *  underlying data.
  !  * The function returns two values
  !  * 1. a small integer indicating the underlying source type
  !  *    of the data. Note that this may differ from what the user
  !  *    sees from nc_inq_format() because this latter function
  !  *    returns what the user can expect to see thru the API.
  !  * 2. A mode value indicating what mode flags are effectively
  !  *    set for this dataset. This usually will be a superset
  !  *    of the mode flags used as the argument to nc_open
  !  *    or nc_create.
  !  * More or less, the #1 values track the set of dispatch tables.
  !  * The #1 values are as follows.
  !  * Note that CDF-5 returns NC_FORMAT_NC3, but sets the mode flag properly.
  !  */
  ! /**@{*/

  integer(c_int), parameter :: nc_formatx_nc3 = 1
  integer(c_int), parameter :: nc_formatx_nc_hdf5 = 2 ! /**< netCDF-4 subset of HDF5 */
  integer(c_int), parameter :: nc_formatx_nc4 = nc_formatx_nc_hdf5 ! /**< alias */
  integer(c_int), parameter :: nc_formatx_nc_hdf4 = 3 ! /**< netCDF-4 subset of HDF4 */
  integer(c_int), parameter :: nc_formatx_pnetcdf = 4
  integer(c_int), parameter :: nc_formatx_dap2 = 5
  integer(c_int), parameter :: nc_formatx_dap4 = 6
  integer(c_int), parameter :: nc_formatx_udf0 = 8
  integer(c_int), parameter :: nc_formatx_udf1 = 9
  integer(c_int), parameter :: nc_formatx_nczarr = 10
  integer(c_int), parameter :: nc_formatx_undefined = 0

  ! /* To avoid breaking compatibility (such as in the python library),
  !  we need to retain the NC_FORMAT_xxx format as well. This may come
  ! out eventually, as the NC_FORMATX is more clear that it's an extended
  ! format specifier.*/

  integer(c_int), parameter :: nc_format_nc3 = nc_formatx_nc3 ! /**< \deprecated as of 4.4.0, use nc_formatx_nc3 */
  integer(c_int), parameter :: nc_format_nc_hdf5 = nc_formatx_nc_hdf5 ! /**< \deprecated as of 4.4.0, use nc_formatx_nc_hdf5 */
  integer(c_int), parameter :: nc_format_nc4 = nc_formatx_nc4 ! /**< \deprecated as of 4.4.0, use nc_formatx_nc4 */
  integer(c_int), parameter :: nc_format_nc_hdf4 = nc_formatx_nc_hdf4 ! /**< \deprecated as of 4.4.0, use nc_formatx_hdf4 */
  integer(c_int), parameter :: nc_format_pnetcdf = nc_formatx_pnetcdf ! /**< \deprecated as of 4.4.0, use nc_formatx_pnetcdf */
  integer(c_int), parameter :: nc_format_dap2 = nc_formatx_dap2 ! /**< \deprecated as of 4.4.0, use nc_formatx_dap2 */
  integer(c_int), parameter :: nc_format_dap4 = nc_formatx_dap4 ! /**< \deprecated as of 4.4.0, use nc_formatx_dap4 */
  integer(c_int), parameter :: nc_format_undefined = nc_formatx_undefined ! /**< \deprecated as of 4.4.0, use nc_formatx_undefined */

  ! /**@}*/

  ! /** Let nc__create() or nc__open() figure out a suitable buffer size. */
  integer(c_int), parameter :: nc_sizehint_default = 0

  ! /** In nc__enddef(), align to the buffer size. */
  integer(c_size_t), parameter :: nc_align_chunk = -1

  ! /** Size argument to nc_def_dim() for an unlimited dimension. */
  integer(c_long), parameter :: nc_unlimited = 0

  ! /** Attribute id to put/get a global attribute. */
  integer(c_int), parameter :: nc_global = -1

  ! /**
  ! Maximum for classic library.

  ! In the classic netCDF model there are maximum values for the number of
  ! dimensions in the file (\ref NC_MAX_DIMS), the number of global or per
  ! variable attributes (\ref NC_MAX_ATTRS), the number of variables in
  ! the file (\ref NC_MAX_VARS), and the length of a name (\ref
  ! NC_MAX_NAME).

  ! These maximums are enforced by the interface, to facilitate writing
  ! applications and utilities.  However, nothing is statically allocated
  ! to these sizes internally.

  ! These maximums are not used for netCDF-4/HDF5 files unless they were
  ! created with the ::NC_CLASSIC_MODEL flag.

  ! As a rule, NC_MAX_VAR_DIMS <              = NC_MAX_DIMS.

  ! NOTE: The NC_MAX_DIMS, NC_MAX_ATTRS, and NC_MAX_VARS limits
  !       are *not* enforced after version 4.5.0
  ! */
  ! /**@{*/
  integer(c_int), parameter :: nc_max_dims = 1024
  integer(c_int), parameter :: nc_max_attrs = 8192
  integer(c_int), parameter :: nc_max_vars = 8192
  integer(c_int), parameter :: nc_max_name = 256
  integer(c_int), parameter :: nc_max_var_dims = 1024
  ! /**@}*/

  ! /** The max size of an SD dataset name in HDF4 (from HDF4
  !  * documentation) is 64. But in in the wild we have encountered longer
  !  * names. As long as the HDF4 name is not greater than NC_MAX_NAME,
  !  * our code will be OK. */
  integer(c_int), parameter :: nc_max_hdf4_name = nc_max_name

  ! /** In HDF5 files you can set the endianness of variables with
  !     nc_def_var_endian(). This define is used there. */
  ! /**@{*/
  integer(c_int), parameter :: nc_endian_native = 0
  integer(c_int), parameter :: nc_endian_little = 1
  integer(c_int), parameter :: nc_endian_big = 2
  ! /**@}*/

  ! /** In HDF5 files you can set storage for each variable to be either
  !  * contiguous or chunked, with nc_def_var_chunking().  This define is
  !  * used there. Unknown storage is used for further extensions of HDF5
  !  * storage models, which should be handled transparently by netcdf */
  ! /**@{*/
  integer(c_int), parameter :: nc_chunked = 0
  integer(c_int), parameter :: nc_contiguous = 1
  integer(c_int), parameter :: nc_compact = 2
  integer(c_int), parameter :: nc_unknown_storage = 3
  integer(c_int), parameter :: nc_virtual = 4
  ! /**@}*/

  ! /**@{*/
  ! /** Control the HDF5 shuffle filter. In HDF5 files you can specify
  !  * that a shuffle filter should be used on each chunk of a variable to
  !  * improve compression for that variable. This per-variable shuffle
  !  * property can be set with the function nc_def_var_deflate(). */
  integer(c_int), parameter :: nc_nochecksum = 0
  integer(c_int), parameter :: nc_fletcher32 = 1
  ! /**@}*/

  ! /**@{*/
  ! /** Control the HDF5 shuffle filter. In HDF5 files you can specify
  !  * that a shuffle filter should be used on each chunk of a variable to
  !  * improve compression for that variable. This per-variable shuffle
  !  * property can be set with the function nc_def_var_deflate(). */
  integer(c_int), parameter :: nc_noshuffle = 0
  integer(c_int), parameter :: nc_shuffle = 1
  ! /**@}*/

  integer(c_int), parameter :: nc_min_deflate_level = 0 ! /**< Minimum deflate level. */
  integer(c_int), parameter :: nc_max_deflate_level = 9 ! /**< Maximum deflate level. */

  integer(c_int), parameter :: nc_szip_nn = 32 ! /**< SZIP NN option mask. */
  integer(c_int), parameter :: nc_szip_ec = 4 ! /**< SZIP EC option mask. */

  integer(c_int), parameter :: nc_noquantize = 0 ! /**< No quantization in use. */
  integer(c_int), parameter :: nc_quantize_bitgroom = 1 ! /**< Use BitGroom quantization. */
  integer(c_int), parameter :: nc_quantize_granularbr = 2 ! /**< Use Granular BitRound quantization. */
  integer(c_int), parameter :: nc_quantize_bitround = 3 ! /**< Use BitRound quantization. */

  ! /**@{*/
  ! /** When quantization is used for a variable, an attribute of the
  !  * appropriate name is added. */
  character(kind=c_char, len=*), parameter :: nc_quantize_bitgroom_att_name = "_QuantizeBitGroomNumberOfSignificantDigits"
  character(kind=c_char, len=*), parameter :: nc_quantize_granularbr_att_name = "_QuantizeGranularBitRoundNumberOfSignificantDigits"
  character(kind=c_char, len=*), parameter :: nc_quantize_bitround_att_name = "_QuantizeBitRoundNumberOfSignificantBits"
  ! /**@}*/

  ! /**@{*/
  ! /** For quantization, the allowed value of number of significant
  !  * decimal and binary digits, respectively, for float. */
  integer(c_int), parameter :: nc_quantize_max_float_nsd = 7
  integer(c_int), parameter :: nc_quantize_max_float_nsb = 23
  ! /**@}*/

  ! /**@{*/
  ! /** For quantization, the allowed value of number of significant
  !  * decimal and binary digits, respectively, for double. */
  integer(c_int), parameter :: nc_quantize_max_double_nsd = 15
  integer(c_int), parameter :: nc_quantize_max_double_nsb = 52
  ! /**@}*/

  ! /** The netcdf version 3 functions all return integer error status.
  !  * These are the possible values, in addition to certain values from
  !  * the system errno.h.
  !  */
  ! #define nc_issyserr(err)        ((err) > 0)

  integer(c_int), parameter :: nc_noerr = 0 ! /**< No Error */
  integer(c_int), parameter :: nc2_err = -1 ! /**< Returned for all errors in the v2 API. */

  ! /** Not a netcdf id.

  ! The specified netCDF ID does not refer to an
  ! open netCDF dataset. */
  integer(c_int), parameter :: nc_ebadid = -33
  integer(c_int), parameter :: nc_enfile = -34 ! /**< Too many netcdfs open */
  integer(c_int), parameter :: nc_eexist = -35 ! /**< netcdf file exists && NC_NOCLOBBER */
  integer(c_int), parameter :: nc_einval = -36 ! /**< Invalid Argument */
  integer(c_int), parameter :: nc_eperm = -37 ! /**< Write to read only */

  ! /** Operation not allowed in data mode. This is returned for netCDF
  ! classic or 64-bit offset files, or for netCDF-4 files, when they were
  ! been created with ::NC_CLASSIC_MODEL flag in nc_create(). */
  integer(c_int), parameter :: nc_enotindefine = -38

  ! /** Operation not allowed in define mode.

  ! The specified netCDF is in define mode rather than data mode.

  ! With netCDF-4/HDF5 files, this error will not occur, unless
  ! ::NC_CLASSIC_MODEL was used in nc_create().
  !  */
  integer(c_int), parameter :: nc_eindefine = -39

  ! /** Index exceeds dimension bound.

  ! The specified corner indices were out of range for the rank of the
  ! specified variable. For example, a negative index or an index that is
  ! larger than the corresponding dimension length will cause an error. */
  integer(c_int), parameter :: nc_einvalcoords = -40

  ! /** NC_MAX_DIMS exceeded. Max number of dimensions exceeded in a
  ! classic or 64-bit offset file, or an netCDF-4 file with
  ! ::NC_CLASSIC_MODEL on. */
  integer(c_int), parameter :: nc_emaxdims = -41 ! /* not enforced after 4.5.0 */

  integer(c_int), parameter :: nc_enameinuse = -42 ! /**< String match to name in use */
  integer(c_int), parameter :: nc_enotatt = -43 ! /**< Attribute not found */
  integer(c_int), parameter :: nc_emaxatts = -44 ! /**< NC_MAX_ATTRS exceeded - not enforced after 4.5.0 */
  integer(c_int), parameter :: nc_ebadtype = -45 ! /**< Not a netcdf data type */
  integer(c_int), parameter :: nc_ebaddim = -46 ! /**< Invalid dimension id or name */
  integer(c_int), parameter :: nc_eunlimpos = -47 ! /**< NC_UNLIMITED in the wrong index */

  ! /** NC_MAX_VARS exceeded. Max number of variables exceeded in a
  ! classic or 64-bit offset file, or an netCDF-4 file with
  ! ::NC_CLASSIC_MODEL on. */
  integer(c_int), parameter :: nc_emaxvars = -48 ! /* not enforced after 4.5.0 */

  ! /** Variable not found.

  ! The variable ID is invalid for the specified netCDF dataset. */
  integer(c_int), parameter :: nc_enotvar = -49
  integer(c_int), parameter :: nc_eglobal = -50 ! /**< Action prohibited on NC_GLOBAL varid */
  integer(c_int), parameter :: nc_enotnc = -51 ! /**< Not a netcdf file */
  integer(c_int), parameter :: nc_ests = -52 ! /**< In Fortran, string too short */
  integer(c_int), parameter :: nc_emaxname = -53 ! /**< NC_MAX_NAME exceeded */
  integer(c_int), parameter :: nc_eunlimit = -54 ! /**< NC_UNLIMITED size already in use */
  integer(c_int), parameter :: nc_enorecvars = -55 ! /**< nc_rec op when there are no record vars */
  integer(c_int), parameter :: nc_echar = -56 ! /**< Attempt to convert between text & numbers */

  ! /** Start+count exceeds dimension bound.

  ! The specified edge lengths added to the specified corner would have
  ! referenced data out of range for the rank of the specified
  ! variable. For example, an edge length that is larger than the
  ! corresponding dimension length minus the corner index will cause an
  ! error. */
  integer(c_int), parameter :: nc_eedge = -57 ! /**< Start+count exceeds dimension bound. */
  integer(c_int), parameter :: nc_estride = -58 ! /**< Illegal stride */
  integer(c_int), parameter :: nc_ebadname = -59 ! /**< Attribute or variable name contains illegal characters */
  ! /* N.B. following must match value in ncx.h */

  ! /** Math result not representable.

  ! One or more of the values are out of the range of values representable
  ! by the desired type. */
  integer(c_int), parameter :: nc_erange = -60
  integer(c_int), parameter :: nc_enomem = -61 ! /**< Memory allocation (malloc) failure */
  integer(c_int), parameter :: nc_evarsize = -62 ! /**< One or more variable sizes violate format constraints */
  integer(c_int), parameter :: nc_edimsize = -63 ! /**< Invalid dimension size */
  integer(c_int), parameter :: nc_etrunc = -64 ! /**< File likely truncated or possibly corrupted */
  integer(c_int), parameter :: nc_eaxistype = -65 ! /**< Unknown axis type. */

  ! /* Following errors are added for DAP */
  integer(c_int), parameter :: nc_edap = -66 ! /**< Generic DAP error */
  integer(c_int), parameter :: nc_ecurl = -67 ! /**< Generic libcurl error */
  integer(c_int), parameter :: nc_eio = -68 ! /**< Generic IO error */
  integer(c_int), parameter :: nc_enodata = -69 ! /**< Attempt to access variable with no data */
  integer(c_int), parameter :: nc_edapsvc = -70 ! /**< DAP server error */
  integer(c_int), parameter :: nc_edas = -71 ! /**< Malformed or inaccessible DAS */
  integer(c_int), parameter :: nc_edds = -72 ! /**< Malformed or inaccessible DDS */
  integer(c_int), parameter :: nc_edmr = nc_edds ! /**< Dap4 alias */
  integer(c_int), parameter :: nc_edatadds = -73 ! /**< Malformed or inaccessible DATADDS */
  integer(c_int), parameter :: nc_edatadap = nc_edatadds ! /**< Dap4 alias */
  integer(c_int), parameter :: nc_edapurl = -74 ! /**< Malformed DAP URL */
  integer(c_int), parameter :: nc_edapconstraint = -75 ! /**< Malformed DAP Constraint*/
  integer(c_int), parameter :: nc_etranslation = -76 ! /**< Untranslatable construct */
  integer(c_int), parameter :: nc_eaccess = -77 ! /**< Access Failure */
  integer(c_int), parameter :: nc_eauth = -78 ! /**< Authorization Failure */

  ! /* Misc. additional errors */
  integer(c_int), parameter :: nc_enotfound = -90 ! /**< No such file */
  integer(c_int), parameter :: nc_ecantremove = -91 ! /**< Can't remove file */
  integer(c_int), parameter :: nc_einternal = -92 ! /**< NetCDF Library Internal Error */
  integer(c_int), parameter :: nc_epnetcdf = -93 ! /**< Error at PnetCDF layer */

  ! /* The following was added in support of netcdf-4. Make all netcdf-4
  !    error codes < -100 so that errors can be added to netcdf-3 if
  !    needed. */
  integer(c_int), parameter :: nc4_first_error = -100 ! /**< @internal All HDF5 errors < this. */
  integer(c_int), parameter :: nc_ehdferr = -101 ! /**< Error at HDF5 layer. */
  integer(c_int), parameter :: nc_ecantread = -102 ! /**< Can't read. */
  integer(c_int), parameter :: nc_ecantwrite = -103 ! /**< Can't write. */
  integer(c_int), parameter :: nc_ecantcreate = -104 ! /**< Can't create. */
  integer(c_int), parameter :: nc_efilemeta = -105 ! /**< Problem with file metadata. */
  integer(c_int), parameter :: nc_edimmeta = -106 ! /**< Problem with dimension metadata. */
  integer(c_int), parameter :: nc_eattmeta = -107 ! /**< Problem with attribute metadata. */
  integer(c_int), parameter :: nc_evarmeta = -108 ! /**< Problem with variable metadata. */
  integer(c_int), parameter :: nc_enocompound = -109 ! /**< Not a compound type. */
  integer(c_int), parameter :: nc_eattexists = -110 ! /**< Attribute already exists. */
  integer(c_int), parameter :: nc_enotnc4 = -111 ! /**< Attempting netcdf-4 operation on netcdf-3 file. */
  integer(c_int), parameter :: nc_estrictnc3 = -112 ! /**< Attempting netcdf-4 operation on strict nc3 netcdf-4 file. */
  integer(c_int), parameter :: nc_enotnc3 = -113 ! /**< Attempting netcdf-3 operation on netcdf-4 file. */
  integer(c_int), parameter :: nc_enopar = -114 ! /**< Parallel operation on file opened for non-parallel access. */
  integer(c_int), parameter :: nc_eparinit = -115 ! /**< Error initializing for parallel access. */
  integer(c_int), parameter :: nc_ebadgrpid = -116 ! /**< Bad group ID. */
  integer(c_int), parameter :: nc_ebadtypid = -117 ! /**< Bad type ID. */
  integer(c_int), parameter :: nc_etypdefined = -118 ! /**< Type has already been defined and may not be edited. */
  integer(c_int), parameter :: nc_ebadfield = -119 ! /**< Bad field ID. */
  integer(c_int), parameter :: nc_ebadclass = -120 ! /**< Bad class. */
  integer(c_int), parameter :: nc_emaptype = -121 ! /**< Mapped access for atomic types only. */
  integer(c_int), parameter :: nc_elatefill = -122 ! /**< Attempt to define fill value when data already exists. */
  integer(c_int), parameter :: nc_elatedef = -123 ! /**< Attempt to define var properties, like deflate, after enddef. */
  integer(c_int), parameter :: nc_edimscale = -124 ! /**< Problem with HDF5 dimscales. */
  integer(c_int), parameter :: nc_enogrp = -125 ! /**< No group found. */
  integer(c_int), parameter :: nc_estorage = -126 ! /**< Can't specify both contiguous and chunking. */
  integer(c_int), parameter :: nc_ebadchunk = -127 ! /**< Bad chunksize. */
  integer(c_int), parameter :: nc_enotbuilt = -128 ! /**< Attempt to use feature that was not turned on when netCDF was built. */
  integer(c_int), parameter :: nc_ediskless = -129 ! /**< Error in using diskless  access. */
  integer(c_int), parameter :: nc_ecantextend = -130 ! /**< Attempt to extend dataset during ind. I/O operation. */
  integer(c_int), parameter :: nc_empi = -131 ! /**< MPI operation failed. */

  integer(c_int), parameter :: nc_efilter = -132 ! /**< Filter operation failed. */
  integer(c_int), parameter :: nc_ercfile = -133 ! /**< RC file failure */
  integer(c_int), parameter :: nc_enullpad = -134 ! /**< Header Bytes not Null-Byte padded */
  integer(c_int), parameter :: nc_einmemory = -135 ! /**< In-memory file error */
  integer(c_int), parameter :: nc_enofilter = -136 ! /**< Filter not defined on variable. */
  integer(c_int), parameter :: nc_enczarr = -137 ! /**< Error at NCZarr layer. */
  integer(c_int), parameter :: nc_es3 = -138 ! /**< Generic S3 error */
  integer(c_int), parameter :: nc_eempty = -139 ! /**< Attempt to read empty NCZarr map key */
  integer(c_int), parameter :: nc_eobject = -140 ! /**< Some object exists when it should not */
  integer(c_int), parameter :: nc_enoobject = -141 ! /**< Some object not found */
  integer(c_int), parameter :: nc_eplugin = -142 ! /**< Unclassified failure in accessing a dynamically loaded plugin> */

  integer(c_int), parameter :: nc4_last_error = -142 ! /**< @internal All netCDF errors > this. */

  ! /* Errors for all remote access methods(e.g. DAP and CDMREMOTE)*/
  integer(c_int), parameter :: nc_eurl = nc_edapurl ! /**< Malformed URL */
  integer(c_int), parameter :: nc_econstraint = nc_edapconstraint ! /**< Malformed Constraint*/

  ! /** @internal This is used in netCDF-4 files for dimensions without
  !  * coordinate vars. */
  character(kind=c_char, len=*), parameter :: dim_without_variable = "Not a netCDF variable."

  ! /** @internal This is here at the request of the NCO team to support
  !  * our mistake of having chunksizes be first ints, then
  !  * size_t. Doh! */
  integer(c_int), parameter :: nc_have_new_chunking_api = 1

end module module_constant
