#!/usr/bin/env bash
repodir=$(pwd)
src="${repodir}/src"
preproc="${repodir}/preproc"
interface="${repodir}/interface"
num_ranks="4"
fypp_files=("module_netcdf" "submodule_attribute" "submodule_variable")

while true; do
  case "$1" in
  -r | --remove)
    for fypp_file in ${fypp_files[@]}; do
      if [ -f ${src}/${fypp_file}.f90 ]; then
        echo "rm ${src}/${fypp_file}.f90"
        rm ${src}/${fypp_file}.f90
      fi
      if [ -f ${src}/module_interface.f90 ]; then
        echo "rm ${src}/module_interface.f90"
        rm ${src}/module_interface.f90
      fi
    done
    if [ -d "build" ]; then
      echo "rm -r ./build"
      rm -r ./build
    fi
    exit 1
    ;;
  *)
    break
    ;;
  esac
done

cd ${preproc}
for fypp_file in ${fypp_files[@]}; do
  echo "fypp ${fypp_file}.fypp ${src}/${fypp_file}.f90"
  fypp -D num_ranks=${num_ranks} ${fypp_file}.fypp ${src}/${fypp_file}.f90
done
echo "python module_interface.py"
python module_interface.py
mv module_interface.f90 ${src}
cd ..

fpm build