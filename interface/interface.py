import re
from dataclasses import dataclass
import sys

# This script is used to convert all "EXTERNL" functions
# into fortran interface, so it is NOT a general purpose c-interface generator.

TYPES = {
    "int": "integer(c_int)",
    "unsigned int": "integer(c_int)",  # ?
    "unsigned short": "integer(c_short)",
    "unsigned long long": "integer(c_long_long)",
    "short": "integer(c_short)",
    "short int": "integer(c_short)",
    "long": "integer(c_long)",
    "long int": "integer(c_long)",
    "long long": "integer(c_long_long)",
    "long long int": "integer(c_long_long)",
    "signed char": "integer(c_signed_char)",
    "unsigned char": "integer(c_signed_char)",
    "size_t": "integer(c_size_t)",
    "int8_t": "integer(c_int8_t)",
    "int16_t": "integer(c_int16_t)",
    "int32_t": "integer(c_int32_t)",
    "int64_t": "integer(c_int64_t)",
    "int_least8_t": "integer(c_int_least8_t)",
    "int_least16_t": "integer(c_int_least16_t)",
    "int_least32_t": "integer(c_int_least32_t)",
    "int_least64_t": "integer(c_int_least64_t)",
    "int_fast8_t": "integer(c_int_fast8_t)",
    "int_fast16_t": "integer(c_int_fast16_t)",
    "int_fast32_t": "integer(c_int_fast32_t)",
    "int_fast64_t": "integer(c_int_fast64_t)",
    "intmax_t": "integer(c_intmax_t)",
    "intptr_t": "integer(c_intptr_t)",
    "ptrdiff_t": "integer(c_ptrdiff_t)",
    "float": "real(c_float)",
    "double": "real(c_double)",
    "long double": "real(c_long_double)",
    "float _Complex": "complex(c_float_complex)",
    "double _Complex": "complex(c_double_complex)",
    "long double _Complex": "complex(c_long_double_complex)",
    "_Bool": "logical(c_bool)",
    "char": "character(kind=c_char, len=*)",
    "void": "type(c_ptr)",
    "nc_type": "integer(nc_type)",
    "NC_Dispatch": "type(NC_Dispatch)",
    "nc_vlen_t": "type(nc_vlen_t)"}

COMMENT = r"/\*(?:.|[\n])*?\*/"
C_NAME = r"[a-zA-Z_][a-zA-Z0-9_]*"
C_TYPE = r"".join(["(const\s)?" + t + "|" for t in TYPES])[:-1]
C_DECL = r"({})(\s|\n)({})".format(C_TYPE, C_NAME)
NC_API = r"EXTERNL\s((.|\n)*?);"
IS_DIM = r"\[\]|ids"
IS_DIM_FUNC_NAME = r"nc_(get|put)_(var|att)_(short|int|long|float|double|longlong|uchar)"
IS_DIM_VAR_NAME = r"(op|ip)"


def type_name(type):
    if 'character' not in type:
        tmp = re.findall('\((.*)\)', type)
        if len(tmp) == 0:
            sys.exit('{}'.format(type))
        return tmp[0]
    else:
        return 'c_char'


def nc_vlen_t():
    return \
        "!> type nc_vlen_t\n" + \
        "type, bind(c) :: nc_vlen_t\n" + \
        "  integer(c_size_t) :: len\n" + \
        "  type(c_ptr) :: p\n" + \
        "end type nc_vlen_t\n\n"


def nc_dispatch():
    return \
        "!> type NC_Dispatch\n" + \
        "type, bind(c) :: NC_Dispatch\n" + \
        "end type NC_Dispatch\n\n"


class c_variable:
    def __init__(self, type, name, attrs):
        if type in TYPES:
            self.type = type
        else:
            sys.exit("Invalid type {}".format(type))
        self.name = name
        self.attrs = attrs

    def to_fortran_declaration(self):
        attr_str = []
        for attr in self.attrs:
            if attr == "dimension":
                dim = self.attrs[attr]
                attr_str.append(", dimension({})".format(dim))
            elif attr == "value":
                is_value = self.attrs[attr]
                if is_value:
                    attr_str.append(", value")
        attr_str = ''.join(attr_str)

        string = "{}{} :: {}".format(TYPES[self.type], attr_str, self.name)
        return string


class c_function:
    def __init__(self, source_code):
        self.src = source_code

        def replace_star(src):
            return re.sub(' +', ' ', src.replace('*', '')).strip().rsplit(' ', 1)

        _func_type, _func_name = replace_star(
            re.findall('.*?(?=\()', source_code)[0])

        # if the function returns a const char*
        # we wrap it with a c_ptr
        if _func_type == 'char':
            _func_type = 'void'

        self.function_name = c_variable(
            _func_type, _func_name, {"value": False})

        arg_str = re.findall('\((.*?)\)', source_code)[0].strip().split(',')
        self.arguments = []
        for arg in arg_str:
            attrs = {}
            _arg_type, _arg_name = replace_star(arg)

            dim_attr = False
            if (re.search(IS_DIM_VAR_NAME, _arg_name) and
                    re.search(IS_DIM_FUNC_NAME, _func_name)):
                dim_attr = True
            elif re.search(r"ids", _arg_name):
                dim_attr = True
            elif re.search(r"\[\]", _arg_name):
                _arg_name = _arg_name.replace('[]', '')
                dim_attr = False

            if dim_attr:
                # _arg_name = _arg_name.replace('[]', '')
                attrs |= {"dimension": "*"}
            attrs |= {"value": not '*' in arg}
            self.arguments.append(c_variable(_arg_type, _arg_name, attrs))

    def to_fortran(self, current_indent, indent=2):
        _func = self.function_name.name
        _args = "".join(["{}, ".format(arg.name)
                        for arg in self.arguments])[:-2]

        tmp = ["{}".format(type_name(TYPES[arg.type]))
               for arg in self.arguments + [self.function_name]]
        _imports = ", ".join(list(set(tmp)))
        _spac = " "*current_indent
        _spac2 = " "*(current_indent + indent)

        strings = \
            ["{}!> interface to {}\n".format(_spac, _func),
             "{}!> Original line in netcdf.h\n".format(_spac),
             "{}!> EXTERNL {}\n".format(_spac, self.src),
             "{}function {}({}) &\n".format(_spac, _func, _args),
             "{}& bind(c, name=\"{}\")\n".format(_spac2, _func),
             "{}import {}\n".format(_spac2, _imports),
             "{}implicit none\n\n".format(_spac2)] + \
            ["{}{}\n".format(_spac2, arg.to_fortran_declaration()) for
             arg in self.arguments] + \
            ["{}{}\n".format(_spac2, self.function_name.to_fortran_declaration()),
             "{}end function {}\n\n".format(_spac, _func)]
        return "".join(strings)


class nc_interface:
    def __init__(self, filename):
        '''Initialization'''
        with open(filename, "r") as file:
            code = file.read()
        code = code.split('/* Begin v2.4 backward compatibility */')[0]
        code = re.sub(COMMENT, '', code)
        self.filename = filename

        def f(x): return x.replace('\n', ' ').replace(
            '*', ' * ').replace('const ', '')
        self._api = [re.sub(' +', ' ', f(x[0]))
                     for x in re.findall(NC_API, code)]

        with open("module_interface.f90", "w") as file:
            file.write("module module_interface\n\n")
            file.write("!> Automatically generated by interface.py\n")
            file.write("use iso_c_binding\n")
            file.write("use module_constant, only: nc_type\n")
            file.write("implicit none\n\n")
            file.write(nc_vlen_t())
            file.write(nc_dispatch())
            file.write("interface\n\n")
            for string in self._api[3:]:
                if '#' in string or '(' not in string:
                    continue
                file.write(c_function(string).to_fortran(2))
            file.write("end interface\n\n")
            file.write("end module module_interface\n")
            file.close


if __name__ == "__main__":
    nc_interface("netcdf.h")
