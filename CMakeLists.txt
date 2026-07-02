# Set up project
#
# Xoptfoil2
#

cmake_minimum_required(VERSION 3.5)

set(XOPTFOIL_VERSION_STRING "" CACHE STRING "Version string embedded into binaries.")

if("${XOPTFOIL_VERSION_STRING}" STREQUAL "")
  if(NOT EXISTS "${CMAKE_SOURCE_DIR}/VERSION")
    message(FATAL_ERROR "Missing VERSION file at ${CMAKE_SOURCE_DIR}/VERSION")
  endif()
  file(READ "${CMAKE_SOURCE_DIR}/VERSION" XOPTFOIL_VERSION_FROM_FILE)
  string(STRIP "${XOPTFOIL_VERSION_FROM_FILE}" XOPTFOIL_VERSION_FROM_FILE)
  set(XOPTFOIL_VERSION_STRING "${XOPTFOIL_VERSION_FROM_FILE}")
endif()

project(Xoptfoil2 VERSION ${XOPTFOIL_VERSION_STRING} LANGUAGES Fortran)
set(XOPTFOIL_VERSION_STRING "${PROJECT_VERSION}" CACHE STRING "Version string embedded into binaries." FORCE)

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  if(CMAKE_HOST_WIN32)
    set(CMAKE_INSTALL_PREFIX "${CMAKE_SOURCE_DIR}/windows" CACHE PATH "Install path prefix." FORCE)
  elseif(CMAKE_HOST_UNIX)
    set(CMAKE_INSTALL_PREFIX "${CMAKE_SOURCE_DIR}/linux" CACHE PATH "Install path prefix." FORCE)
  endif()
endif()

if(DEFINED ENV{XOPTFOIL_VERSION})
  message(WARNING "Legacy env var XOPTFOIL_VERSION is ignored. Use VERSION file or XOPTFOIL_VERSION_STRING cache override.")
endif()

if (CMAKE_HOST_UNIX)
  set (XOPTFOIL_OS_DEFINE "UNIX=1")
  # under Unix do dynamic linking so additional libs are required at runtime 
  set (STATIC_FLAG "")
  message (STATUS "\n \n Building XFoptfoil2 Version ${XOPTFOIL_VERSION_STRING} for UNIX\n\n")
elseif (CMAKE_HOST_WIN32)
  set (XOPTFOIL_OS_DEFINE "WIN32=1")
  # under Window do static linking so no additional libs are required at runtime 
  set (STATIC_FLAG "-static")
  message (STATUS "\n \n Building XFoptfoil2 Version ${XOPTFOIL_VERSION_STRING} for Windows\n\n")
else()
  message (FATAL_ERROR "Unsupported OS")
endif (CMAKE_HOST_UNIX)

# that's needed to avoid strange make behaviour when installing the exe
unset(CMAKE_SYSTEM_NAME)

# Some default directories
set(DATADIR ${CMAKE_INSTALL_PREFIX}/share CACHE PATH "Directory to install shared data and examples.")
 
# Determine compiler
get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
message(STATUS "Fortran compiler: " ${CMAKE_Fortran_COMPILER})

if (NOT CMAKE_CONFIGURATION_TYPES AND NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the build type." FORCE)
endif()

set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "RelWithDebInfo" "MinSizeRel")

# Option to build with/without OpenMP support - overwrites Release and Debug
# 	! build directory has to be deleted to activate change of option !
option(ENABLE_OPENMP "Activate OpenMP" ON)

# Enable OpenMP by default for Release builds, but disable by default for Debug
if (CMAKE_BUILD_TYPE STREQUAL "Release")
  set (OPENMP_FLAG "-fopenmp -DOPENMP=1")
  if (NOT ENABLE_OPENMP)
    MESSAGE(STATUS "Disabling OpenMP support since ENABLE_OPENMP=FALSE")
    set (OPENMP_FLAG "")
  endif (NOT ENABLE_OPENMP)
elseif (CMAKE_BUILD_TYPE STREQUAL "Debug")
  set (OPENMP_FLAG "")
  # if (ENABLE_OPENMP)
  #   MESSAGE(STATUS "Enabling OpenMP support for debug build since ENABLE_OPENMP=TRUE")
  #   set (OPENMP_FLAG "-fopenmp  -DOPENMP=1")
  # endif (ENABLE_OPENMP)
endif (CMAKE_BUILD_TYPE STREQUAL "Release")

# Default compiler flags
#     gfortran:  -Wno-uninitialized is needed to suppress false positive warnings
if (Fortran_COMPILER_NAME MATCHES "gfortran")
  set (CMAKE_Fortran_FLAGS_RELEASE "-std=gnu -fpic -O2 ${OPENMP_FLAG} -cpp ${STATIC_FLAG}")
  set (CMAKE_Fortran_FLAGS_DEBUG "-std=gnu -g -fdump-core -fcheck=all -Wno-uninitialized -Wno-maybe-uninitialized -fbacktrace -ffpe-trap=invalid,zero,overflow,denormal -Wall ${OPENMP_FLAG} -cpp ${STATIC_FLAG}")
  set (Fortran_REAL8_FLAG "-fdefault-real-8")
elseif (Fortran_COMPILER_NAME MATCHES "ifort")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O3 ${OPENMP_FLAG}")
  set (CMAKE_Fortran_FLAGS_DEBUG "-g -warn all ${OPENMP_FLAG}")
  set (Fortran_REAL8_FLAG "-r8")
else (Fortran_COMPILER_NAME MATCHES "gfortran")
  message(FATAL_ERROR "Fortran compiler not supported.")
endif (Fortran_COMPILER_NAME MATCHES "gfortran")

# Set source files (exclude programs here)
configure_file(
  "${CMAKE_SOURCE_DIR}/src/fortran/xoptfoil_version.f90.in"
  "${CMAKE_BINARY_DIR}/generated/xoptfoil_version.f90"
  @ONLY
)

file(GLOB SOURCESDBLE "src/fortran/*.f90" "src/fortran/test_cases/*.f90" "src/fortran/xfoil_deps/*.f90")
file(GLOB SOURCESREAL "src/fortran/xfoil_deps/*.f")
list(APPEND SOURCESDBLE "${CMAKE_BINARY_DIR}/generated/xoptfoil_version.f90")
list(REMOVE_ITEM SOURCESDBLE "${CMAKE_SOURCE_DIR}/src/fortran/main.f90")
list(REMOVE_ITEM SOURCESDBLE "${CMAKE_SOURCE_DIR}/src/fortran/worker.f90")
list(REMOVE_ITEM SOURCESDBLE "${CMAKE_SOURCE_DIR}/src/fortran/test_cases/xoptfoil2_testdriver.f90")

# Real-8 flag for xfoil source files
set_source_files_properties(${SOURCESREAL} PROPERTIES COMPILE_FLAGS ${Fortran_REAL8_FLAG})

# CMake "object libraries" which allow sharing object files for multiple executables
# https://cmake.org/Wiki/CMake/Tutorials/Object_Library
# (Introduced in CMake 2.8.8)
add_library(MYOBJLIB OBJECT ${SOURCESDBLE} ${SOURCESREAL})

set(XOPTFOIL_COMMON_DEFINES ${XOPTFOIL_OS_DEFINE})
target_compile_definitions(MYOBJLIB PUBLIC ${XOPTFOIL_COMMON_DEFINES})

# Build executables
add_executable("xoptfoil2"             $<TARGET_OBJECTS:MYOBJLIB> "src/fortran/main.f90")
add_executable("worker"                $<TARGET_OBJECTS:MYOBJLIB> "src/fortran/worker.f90")
add_executable("xoptfoil2_testdriver"  $<TARGET_OBJECTS:MYOBJLIB> "src/fortran/test_cases/xoptfoil2_testdriver.f90")

target_compile_definitions(xoptfoil2 PRIVATE ${XOPTFOIL_COMMON_DEFINES})
target_compile_definitions(worker PRIVATE ${XOPTFOIL_COMMON_DEFINES})
target_compile_definitions(xoptfoil2_testdriver PRIVATE ${XOPTFOIL_COMMON_DEFINES})


# Installation
install(TARGETS xoptfoil2 worker      DESTINATION bin)
install(TARGETS xoptfoil2_testdriver  DESTINATION bin)
