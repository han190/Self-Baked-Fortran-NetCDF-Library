import re
import requests
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
    "char": "character(kind=c_char)",
    "void": "type(c_ptr)",
    "nc_type": "integer(nc_type)",
    "NC_Dispatch": "type(NC_Dispatch)",
    "nc_vlen_t": "type(nc_vlen_t)",
}

COMMENT = r"/\*(?:.|[\n])*?\*/"
C_NAME = r"[a-zA-Z_][a-zA-Z0-9_]*"
C_TYPE = r"".join(["(const\s)?" + t + "|" for t in TYPES])[:-1]
C_DECL = r"({})(\s|\n)({})".format(C_TYPE, C_NAME)
NC_API = r"EXTERNL\s((.|\n)*?);"
NC_CONST = r"#define\s{}.*\n".format(C_NAME)
IS_DIM_FUNC_NAME = (
    r"nc_(get|put)_(var|att)_"
    + r"(short|int|long|float|double|longlong|uchar|schar|text)"
)
IS_DIM_VAR_NAME = r"(op|ip)"
NC_FORMAT_ALL_STR = (
    "&\n  & ior(nc_64bit_offset, ior(nc_64bit_data, &\n"
    + "  & ior(nc_classic_model, ior(nc_netcdf4, ior(nc_udf0, nc_udf1)))))"
)


def type_name(type):
    if "character" not in type:
        tmp = re.findall("\((.*)\)", type)
        if len(tmp) == 0:
            sys.exit("{}".format(type))
        return tmp[0]
    else:
        return "c_char"


def type_bindc(type_name, components, indent=2, level=0):
    """Generate c binding derived type"""
    base_indent = " " * level * indent
    lvl2_indent = " " * (level + 1) * indent
    _components = [
        "{}{} :: {}\n".format(lvl2_indent, type, components[type])
        for type in components
    ]
    source = (
        "{}!> type {}\n".format(base_indent, type_name)
        + "{}type, bind(c) :: {}\n".format(base_indent, type_name)
        + "".join(comp for comp in _components)
        + "{}end type {}\n\n".format(base_indent, type_name)
    )
    return source


def nc_vlen_t(indent, level):
    type_name = "nc_vlen_t"
    components = {"integer(c_size_t)": "len", "type(c_ptr)": "p"}
    return type_bindc(type_name, components, indent, level)


def nc_dispatch(indent, level):
    type_name = "NC_Dispatch"
    components = {}
    return type_bindc(type_name, components, indent, level)


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
        attr_str = "".join(attr_str)

        string = "{}{} :: {}".format(TYPES[self.type], attr_str, self.name)
        return string


class c_function:
    def __init__(self, source_code):
        self.src = source_code

        def replace_star(src):
            return re.sub(" +", " ", src.replace("*", "")).strip().rsplit(" ", 1)

        _func_type, _func_name = replace_star(re.findall(".*?(?=\()", source_code)[0])

        # if the function returns a const char*
        # we wrap it with a c_ptr
        if _func_type == "char":
            _func_type = "void"

        self.function_name = c_variable(_func_type, _func_name, {"value": False})

        arg_str = re.findall("\((.*?)\)", source_code)[0].strip().split(",")
        self.arguments = []
        for arg in arg_str:
            attrs = {}
            _arg_type, _arg_name = replace_star(arg)

            dim_attr = False
            if re.search(IS_DIM_VAR_NAME, _arg_name) and re.search(
                IS_DIM_FUNC_NAME, _func_name
            ):
                dim_attr = True
            elif re.search(r"ids", _arg_name):
                dim_attr = True
            elif re.search(r"\[\]", _arg_name):
                _arg_name = _arg_name.replace("[]", "")
                dim_attr = False
            elif re.search(r"char", _arg_type):
                dim_attr = True

            if dim_attr:
                # _arg_name = _arg_name.replace('[]', '')
                attrs |= {"dimension": "*"}
            attrs |= {"value": not "*" in arg}
            self.arguments.append(c_variable(_arg_type, _arg_name, attrs))

    def to_fortran(self, current_indent, indent=2):
        _func = self.function_name.name
        _args = "".join(["{}, ".format(arg.name) for arg in self.arguments])[:-2]

        tmp = [
            "{}".format(type_name(TYPES[arg.type]))
            for arg in self.arguments + [self.function_name]
        ]
        _imports = ", ".join(sorted(list(set(tmp))))
        _spac = " " * current_indent
        _spac2 = " " * (current_indent + indent)

        strings = (
            [
                "{}!> interface to {}\n".format(_spac, _func),
                "{}!> Original line in netcdf.h\n".format(_spac),
                "{}!> EXTERNL {}\n".format(_spac, self.src),
                "{}function {}({}) &\n".format(_spac, _func, _args),
                '{}& bind(c, name="{}")\n'.format(_spac2, _func),
                "{}import {}\n".format(_spac2, _imports),
                "{}implicit none\n\n".format(_spac2),
            ]
            + [
                "{}{}\n".format(_spac2, arg.to_fortran_declaration())
                for arg in self.arguments
            ]
            + [
                "{}{}\n".format(_spac2, self.function_name.to_fortran_declaration()),
                "{}end function {}\n\n".format(_spac, _func),
            ]
        )
        return "".join(strings)


def extract_numeric(s):
    # This pattern will capture numbers, including their optional sign (+ or -)
    match = re.search(r"([-+]?\d+(\.\d+)?([eE][-+]?\d+)?)", s)
    return match.group(0) if match else None


def extract_letters_and_underscores(s):
    # This pattern captures sequences of alphabetic characters and underscores
    return re.findall(C_NAME, s)[0]


def cdef2fpar(str_list):
    """convert a list of C macros to Fortran parameters"""
    declares = {}
    for element in str_list:
        if "\n" in element:
            str_split = element.replace("\n", "").split()
        else:
            str_split = element.split()
        str_proc = [str_split[1], "".join(str_split[2:])]

        str_clean = []
        for _str in str_proc:
            if _str in ("#define", ""):
                continue

            if _str[0] == "(" and _str[-1] == ")":
                tmp = _str[1:-1]
            else:
                tmp = _str
            str_clean.append(tmp)

        if len(str_clean) == 2:
            name = str_clean[0]
            try:
                contains_name = extract_letters_and_underscores(str_clean[1])
            except:
                contains_name = str_clean[1]

            if "NC_FORMAT_ALL" in str_clean[0]:
                _value = NC_FORMAT_ALL_STR
                _type = "integer"
                _kind = "c_int"
                declares[name] = {"type": _type, "kind": _kind, "value": _value}
            elif str_clean[1] in declares:
                declare = declares[str_clean[1]]
                if type(declare) is dict:
                    _value = str_clean[1]
                    _type = declare["type"]
                    _kind = declare["kind"]
                    declares[name] = {"type": _type, "kind": _kind, "value": _value}
                else:
                    declares[name] = "!" + " ".join(str_clean)
            elif contains_name in declares:
                declare = declares[contains_name]
                if type(declare) is dict:
                    _value = str_clean[1]
                    _type = declare["type"]
                    _kind = declare["kind"]
                    declares[name] = {"type": _type, "kind": _kind, "value": _value}
                else:
                    declares[name] = "!" + " ".join(str_clean)
            elif str_clean[1].isdigit() or (
                str_clean[1][1:].isdigit() and str_clean[1][0] == "-"
            ):
                _value = extract_numeric(str_clean[1])
                _type = "integer"
                _kind = "c_int"
                declares[name] = {"type": _type, "kind": _kind, "value": _value}
            elif "signedchar" in str_clean[1]:
                _value = extract_numeric(str_clean[1])
                _type = "integer"
                _kind = "c_signed_char"
                declares[name] = {"type": _type, "kind": _kind, "value": _value}
            elif "short" in str_clean[1]:
                _value = extract_numeric(str_clean[1])
                _type = "integer"
                _kind = "c_short"
                declares[name] = {"type": _type, "kind": _kind, "value": _value}
            elif "_FLOAT" in str_clean[0]:
                _value = extract_numeric(str_clean[1])
                _type = "real"
                _kind = "c_float"
                declares[name] = {"type": _type, "kind": _kind, "value": _value}
            elif "_DOUBLE" in str_clean[0]:
                _value = str_clean[1] + "_c_double"
                _type = "real"
                _kind = "c_double"
                declares[name] = {"type": _type, "kind": _kind, "value": _value}
            elif "(size_t)" in str_clean[1]:
                _value = extract_numeric(str_clean[1])
                _type = "integer"
                _kind = "c_size_t"
                declares[name] = {"type": _type, "kind": _kind, "value": _value}
            elif "(longlong)" in str_clean[1]:
                _value = extract_numeric(str_clean[1]) + "_c_long_long"
                _type = "integer"
                _kind = "c_long_long"
                declares[name] = {"type": _type, "kind": _kind, "value": _value}
            elif "(char*)" in str_clean[1]:
                _value = str_clean[1].replace("(char*)", "")
                _type = "character"
                _kind = "kind=c_char, len=*"
                declares[name] = {"type": _type, "kind": _kind, "value": _value}
            elif "_INT64" in str_clean[0]:
                _value = extract_numeric(str_clean[1]) + "_c_int64_t"
                _type = "integer"
                _kind = "c_int64_t"
                declares[name] = {"type": _type, "kind": _kind, "value": _value}
            elif "NC_UNLIMITED" in str_clean[0]:
                _value = extract_numeric(str_clean[1]) + "_c_long"
                _type = "integer"
                _kind = "c_long"
                declares[name] = {"type": _type, "kind": _kind, "value": _value}
            elif bool(re.match("0x[0-9]{3,}", str_clean[1])):
                _value = "int(z'{}')".format(str_clean[1][2:])
                _type = "integer"
                _kind = "c_int"
                declares[name] = {"type": _type, "kind": _kind, "value": _value}
            else:
                declares[name] = "!" + " ".join(str_clean)

    ret = []
    unsolved = 0
    for name in declares:
        if type(declares[name]) is dict:
            declare = declares[name]
            line = "{}({}), parameter :: {} = {}".format(
                declare["type"], declare["kind"], name, declare["value"]
            )
            ret.append(line.lower())
        else:
            ret.append(declares[name])
            unsolved += 1
    return ret, unsolved


if __name__ == "__main__":
    url = "https://raw.githubusercontent.com/Unidata/netcdf-c/main/include/netcdf.h"
    resp = requests.get(url)
    code = resp.text

    # ignore v2.4 backward compatibility
    code = code.split("/* Begin v2.4 backward compatibility */")[0]

    # remove all comments
    code = re.sub(COMMENT, "", code)

    def f(x):
        return x.replace("\n", " ").replace("*", " * ").replace("const ", "")

    api = [re.sub(" +", " ", f(x[0])) for x in re.findall(NC_API, code)]

    const, unsolved = cdef2fpar(re.findall(NC_CONST, code))
    # print("{}/{}".format(unsolved, len(const)))

    with open("module_c_interface.f90", "w") as file:
        file.write("module module_c_interface\n\n")
        file.write("!> Automatically generated by module_c_interface.py\n")
        file.write("use :: iso_c_binding\n")
        file.write("use :: iso_fortran_env\n")
        # file.write("use module_constant, only: nc_type\n")
        file.write("implicit none\n")
        file.write("public\n\n")
        file.write(nc_vlen_t(2, 1))
        file.write(nc_dispatch(2, 1))
        file.write("integer(c_int), parameter :: nc_type = c_int\n")
        file.write("!> Unsolved parameters {}/{}\n".format(
            unsolved, len(const)))
        for _const in const:
            file.write("{}\n".format(_const))
        file.write("\n")
        file.write("interface\n\n")
        for string in api[3:]:
            if "#" in string or "(" not in string:
                continue
            file.write(c_function(string).to_fortran(2))
        file.write("end interface\n\n")
        file.write("end module module_c_interface\n")
        file.close
