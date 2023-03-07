
src="./src"
fypp_files=("module_netcdf" "submodule_attribute" "submodule_variable")

while true; do
  case "$1" in
  -r | --remove)
    for fypp_file in ${fypp_files[@]}; do
      if [ -f ${src}/${fypp_file}.f90 ]; then
        echo "rm ${src}/${fypp_file}.f90"
        rm ${src}/${fypp_file}.f90
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

for fypp_file in ${fypp_files[@]}; do
  echo "fypp ${src}/${fypp_file}.fypp ${src}/${fypp_file}.f90"
  fypp ${src}/${fypp_file}.fypp ${src}/${fypp_file}.f90
done

fpm build