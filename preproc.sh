#!/usr/bin/env bash
repodir=$(pwd)
compiler="gfortran"
src="${repodir}/src"
preproc="${repodir}/preproc"
interface="${repodir}/interface"
fypp_files=("submodule_attribute" "submodule_io" "submodule_variable")
fprettify_flags="-i 2 --strict-indent --disable-indent-mod -r"

echo "Preprocessing using fypp..."
cd $preproc
for fypp_file in ${fypp_files[@]}; do
  echo "  Preprocessing ${fypp_file}.fypp..."
  fypp ${fypp_file}.fypp ${src}/${fypp_file}.f90
done

echo "Generating C interface..."
python module_c_interface.py
mv ${preproc}/module_c_interface.f90 ${src}
cd ..

echo "Copy remaining source code..."
cp ${preproc}/*.f90 ${src}

echo "Source file generated successfully."
echo "Compile Fortran source code..."
if [ -d "./build" ]; then
  rm -rf ./build/
fi
fpm build --compiler ${compiler}
echo "Running tests..."
fpm test --compiler ${compiler}
