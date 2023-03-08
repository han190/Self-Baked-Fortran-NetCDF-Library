# Status of supported cwrappers

The API list can be checked from [the NetCDF documentation site](https://docs.unidata.ucar.edu/netcdf-c/current/modules.html).

| NetCDF C API | C Return | C Arguments | Fortran Return | Fortran Arguments | Status |
|:-------------|:---------|:------------|:---------------|:------------------|:------:|
| NetCDF File and Data I/O |
| `nc__create` | `int` | `const char *path`<br />`int cmode`<br />`size_t initialsz`<br />`size_t *chunksizehintp`<br />`int *ncidp` | `c_int` | `character(kind=c_char) :: path(*)`<br />`integer(c_int), value :: cmode`<br />`integer(c_size_t), value :: initialsz`<br />`integer(c_size_t) :: chunksizehintp, integer(c_int) :: ncidp` | :laughing: |
| `nc__enddef` | `int` | `int ncid`<br />`size_t h_minfree`<br />`size_t v_align`<br />`size_t v_minfree`<br />`size_t r_align` | `c_int` | `integer(c_int), value :: ncid`<br />`integer(c_size_t), value :: h_minfree`<br />`integer(c_size_t), value :: v_align`<br />`integer(c_size_t), value :: v_minfree`<br />`integer(c_size_t), value :: r_align` | :laughing: |
| `nc__open` | `int` | `const char *path`<br />`int omode`<br />`size_t *chunksizehintp`<br />`int *ncidp` | `c_int` | `character(kind=c_char) :: path(*)`<br />`integer(c_int), value :: omode`<br />`integer(c_size_t) :: chunksizehintp`<br />`integer(c_int) :: ncidp` | :laughing: |
| `nc_abort` | `int` | `int ncid` | `c_int` | `integer(c_int), value :: ncid` | :laughing: |
| `nc_close` | `int` | `int ncid` | `c_int` | `integer(c_int), value :: ncid` | :laughing: |
| `nc_close_memio` | `int` | `int ncid`<br />`NC_memio *memio` | | | :imp: |
| `nc_create` | `int` | `const char *path`<br />`int cmode`<br />`int *ncidp` | `c_int` | `character(kind=c_char) :: path(*)`<br /> `integer(c_int), value :: cmode`<br />`integer(c_int) :: ncidp` | :laughing: |
| `nc_create_mem` | `int` | `const char *path`<br />`int mode`<br />`size_t initialsize`<br />`int *ncidp` | `c_int` | `character(kind=c_char) :: path(*)`<br />`integer(c_int), value :: mode`<br />`integer(c_size_t), value :: initialsize`<br />`integer(c_int) :: ncidp` | :laughing: |
