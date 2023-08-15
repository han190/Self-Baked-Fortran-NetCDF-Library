#!/usr/bin/env bash
repodir=$(pwd)
src="${repodir}/src"
preproc="${repodir}/preproc"
interface="${repodir}/interface"
fypp_files=("submodule_attribute" "submodule_variable")

cd $preproc
for fypp_file in ${fypp_files[@]}; do
  echo "fypp ${fypp_file}.fypp ${src}/${fypp_file}.f90"
  fypp ${fypp_file}.fypp ${src}/${fypp_file}.f90
done
echo "python module_interface.py"
python module_interface.py
mv module_interface.f90 ${src}
cd ..

fprettify -i 2 -r ./src