# Set up project
#
# Xoptfoil2
#
# - added static linking of libs so no runtime libs are needed

# Variables from build_windows / build_linux
#
#       INSTALLDIR                   e.g. %CD%\windows
#       XOPTFOIL_VERSION             e.g. 1.5.0beta

if (CMAKE_HOST_UNIX)
  set (OS_FLAG "-DUNIX=1")
  # under Unix do dynamic linking so additional libs are required at runtime 
  set (STATIC_FLAG "")
  message (STATUS "\n \n Building XFoptfoil2 Version $ENV{XOPTFOIL_VERSION} for UNIX\n\n")
elseif (CMAKE_HOST_WIN32)
  set (OS_FLAG "-DWIN32=1")
  # under Window do static linking so no additional libs are required at runtime 
  set (STATIC_FLAG "-static")
  message (STATUS "\n \n Building XFoptfoil2 Version $ENV{XOPTFOIL_VERSION} for Windows\n\n")
else()
  message (FATAL_ERROR "Unsupported OS")
endif (CMAKE_HOST_UNIX)

# that's needed to avoid strange make behaviour when installing the exe
unset(CMAKE_SYSTEM_NAME)

cmake_minimum_required(VERSION 3.5)
project(Xoptfoil2)
enable_language(Fortran)

# Some default directories
set(DATADIR ${CMAKE_INSTALL_PREFIX}/share CACHE PATH "Directory to install shared data and examples.")
 
# Determine compiler
get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
message(STATUS "Fortran compiler: " ${CMAKE_Fortran_COMPILER})

if (NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "Release")
endif(NOT CMAKE_BUILD_TYPE)

# Option to build with/without OpenMP support - overwrites Release and Debug
# 	! build directory has to be deleted to activate change of option !
option(ENABLE_OPENMP "Activate OpenMP" ON)

# Enable OpenMP by default for Release builds, but disable by default for Debug
if (CMAKE_BUILD_TYPE MATCHES "Release")
  set (OPENMP_FLAG "-fopenmp -DOPENMP=1")
  if (NOT ENABLE_OPENMP)
    MESSAGE(STATUS "Disabling OpenMP support since ENABLE_OPENMP=FALSE")
    set (OPENMP_FLAG "")
  endif (NOT ENABLE_OPENMP)
elseif (CMAKE_BUILD_TYPE MATCHES "Debug")
  set (OPENMP_FLAG "")
  if (ENABLE_OPENMP)
    MESSAGE(STATUS "Enabling OpenMP support for debug build since ENABLE_OPENMP=TRUE")
    set (OPENMP_FLAG "-fopenmp  -DOPENMP=1")
  endif (ENABLE_OPENMP)
endif (CMAKE_BUILD_TYPE MATCHES "Release")

# Default compiler flags
#     gfortran:  -Wno-uninitialized is needed to suppress false positive warnings
if (Fortran_COMPILER_NAME MATCHES "gfortran")
  # changed optimzation into -O2 (from -O3)
  set (CMAKE_Fortran_FLAGS_RELEASE "-fpic -O2 ${OPENMP_FLAG} -cpp ${OS_FLAG} ${STATIC_FLAG}")
  set (CMAKE_Fortran_FLAGS_DEBUG "-g -fdump-core -fcheck=all -Wno-uninitialized -Wno-maybe-uninitialized -fbacktrace -ffpe-trap=invalid,zero,overflow -Wall ${OPENMP_FLAG} -cpp ${OS_FLAG} ${STATIC_FLAG}")
  set (Fortran_REAL8_FLAG "-fdefault-real-8")
elseif (Fortran_COMPILER_NAME MATCHES "ifort")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O3 ${OPENMP_FLAG}")
  set (CMAKE_Fortran_FLAGS_DEBUG "-g -warn all ${OPENMP_FLAG}")
  set (Fortran_REAL8_FLAG "-r8")
else (Fortran_COMPILER_NAME MATCHES "gfortran")
  message(FATAL_ERROR "Fortran compiler not supported.")
endif (Fortran_COMPILER_NAME MATCHES "gfortran")

# Preprocessor definitions
add_definitions(-DPACKAGE_VERSION=\"$ENV{XOPTFOIL_VERSION}\")

# Set source files (exclude programs here)
file(GLOB SOURCESDBLE "src/fortran/*.f90" "src/fortran/test_cases/*.f90" "src/fortran/xfoil_deps/*.f90")
file(GLOB SOURCESREAL "src/fortran/xfoil_deps/*.f")
list(REMOVE_ITEM SOURCESDBLE "${CMAKE_SOURCE_DIR}/src/fortran/main.f90")
list(REMOVE_ITEM SOURCESDBLE "${CMAKE_SOURCE_DIR}/src/fortran/worker.f90")
list(REMOVE_ITEM SOURCESDBLE "${CMAKE_SOURCE_DIR}/src/fortran/test_cases/Xoptfoil2_testdriver.f90")

# Real-8 flag for xfoil source files
set_source_files_properties(${SOURCESREAL} PROPERTIES COMPILE_FLAGS ${Fortran_REAL8_FLAG})

# CMake "object libraries" which allow sharing object files for multiple executables
# https://cmake.org/Wiki/CMake/Tutorials/Object_Library
# (Introduced in CMake 2.8.8)
add_library(MYOBJLIB OBJECT ${SOURCESDBLE} ${SOURCESREAL})

# Build executables
add_executable("xoptfoil2"   $<TARGET_OBJECTS:MYOBJLIB> "src/fortran/main.f90")
add_executable("worker"      $<TARGET_OBJECTS:MYOBJLIB> "src/fortran/worker.f90")
add_executable("xoptfoil2_testdriver"  $<TARGET_OBJECTS:MYOBJLIB> "src/fortran/test_cases/Xoptfoil2_testdriver.f90")

# Build dll
# add_library(x5_api SHARED $<TARGET_OBJECTS:MYOBJLIB>)

# Installation
install(TARGETS xoptfoil2 worker      DESTINATION bin)
install(TARGETS xoptfoil2_testdriver  DESTINATION bin)

