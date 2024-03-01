cmake_minimum_required(VERSION 3.10)
project(pycads)
find_package(OpenMp)
enable_language(Fortran)
find_package(PythonLibs REQUIRED)
find_package(Python3 REQUIRED COMPONENTS NumPy)
include_directories(${PYTHON_INCLUDE_DIRS})
include_directories(${_Python3_NumPy_INCLUDE_DIR})

set(CADS_DIR "/perm/ecmv1501/CADS/CADS/")
set(CADS_NL "${CADS_DIR}/namelist")
set(CADS_SRC "${CADS_DIR}/src")

set(F2PY_EXECUTABLE "f2py")

enable_language(Fortran)
find_package(OpenMP)

if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
    set(ompFlag "-liomp5")
else()
    set(ompFlag "-lgomp")
endif()
file( GLOB CADS_SOURCES ${CADS_SRC}/*.F90 )
file( GLOB CADS_IFACES ${CADS_SRC}/*.intfb )
#file( GLOB CADS_NLS ${CADS_NL}/*.NL )

add_library(cads SHARED ${CADS_SOURCES} ${CADS_IFACES})

set(fortran_src_file "${CMAKE_CURRENT_SOURCE_DIR}/wrap/wrap_cads_detect_cloud.F90")
set(f2py_module_name "cads_wrap")
set(generated_module_file ${CMAKE_CURRENT_BINARY_DIR}/${f2py_module_name}${PYTHON_EXTENSION_MODULE_SUFFIX})

#_dumb is used to avoid circular dependency which breaks Ninja.
add_custom_target(${f2py_module_name}_dumb ALL
  DEPENDS ${generated_module_file} ${cads}
  )

add_custom_command(
  OUTPUT ${generated_module_file}
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMAND  ${F2PY_EXECUTABLE} -c ${fortran_src_file} -L. -I. ${ompFlag} -lcads -m ${f2py_module_name}
  DEPENDS ${cads}
  )

add_dependencies(${f2py_module_name}_dumb cads)
list(GET _Python3_INTERPRETER_PROPERTIES 6 version)
set(produced_so ${CMAKE_CURRENT_BINARY_DIR}/${f2py_module_name}.${version}.so)
set(cads_so ${CMAKE_CURRENT_BINARY_DIR}/libcads.so)
set(cads_mod ${CMAKE_CURRENT_BINARY_DIR}/cads_module.mod)

#Move to pycads directory
install(FILES ${produced_so} ${cads_so} ${cads_mod} PERMISSIONS WORLD_READ WORLD_EXECUTE OWNER_WRITE OWNER_EXECUTE OWNER_READ DESTINATION pycads )
file( GLOB PYSCRIPTS ${CMAKE_CURRENT_SOURCE_DIR}/go*.py)
file( GLOB PYSCRIPTSPLOT ${CMAKE_CURRENT_SOURCE_DIR}/plot*.py)
install( FILES ${PYSCRIPTS} ${PYSCRIPTSPLOT} PERMISSIONS WORLD_READ WORLD_EXECUTE OWNER_READ OWNER_WRITE OWNER_EXECUTE DESTINATION pycads/bin/ )
file( GLOB ODB_HELP ${CMAKE_CURRENT_SOURCE_DIR}/useful_stuff.py)
install( FILES ${ODB_HELP} PERMISSIONS WORLD_READ WORLD_EXECUTE OWNER_READ OWNER_WRITE OWNER_EXECUTE DESTINATION pycads/ )
file( GLOB ODB_HELP ${CMAKE_CURRENT_SOURCE_DIR}/maps.py)
install( FILES ${ODB_HELP} PERMISSIONS WORLD_READ WORLD_EXECUTE OWNER_READ OWNER_WRITE OWNER_EXECUTE DESTINATION pycads/ )
