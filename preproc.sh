#!/usr/bin/env bash
num_ranks=6
repodir=$(pwd)
src="${repodir}/src"
preproc="${repodir}/preproc"
interface="${repodir}/interface"
fypp_files=("module_interface" "submodule_attribute" \
  "submodule_io" "submodule_variable")
fprettify_flags="-i 2 --strict-indent --disable-indent-mod -r"
fypp_var="-Dnum_ranks=${num_ranks}"

echo "Preprocessing using fypp..."
cd $preproc
for fypp_file in ${fypp_files[@]}; do
  # echo "fypp ${fypp_file}.fypp ${src}/${fypp_file}.f90"
  fypp ${fypp_var} ${fypp_file}.fypp ${src}/${fypp_file}.f90
done

echo "Generating C interface..."
# echo "python module_c_interface.py"
python module_c_interface.py
mv ${preproc}/module_c_interface.f90 ${src}
cd ..

echo "Copy remaining source code..."
cp ${preproc}/*.f90 ${src}

echo "Reindent source code using fprettify..."
fprettify ${fprettify_flags} ${src}

echo "Source file generated successfully."
echo "Compiler Fortran source code..."
fpm test
