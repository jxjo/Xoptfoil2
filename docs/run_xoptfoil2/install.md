---
layout: home
title: Installation
parent: Run Xoptfoil2
nav_order: 1
---


## Installation 
{: .no_toc }

The current version of Xoptfoil2 is available in the [Releases section](https://github.com/jxjo/Xoptfoil2/releases) of the GitHub repository. The assets include zip files for:
- a ready-built version for Windows
- the source files for building Xoptfoil2 under Linux

<span>Windows</span>{: .label .label-blue }

Download the Windows zip file and extract it into any directory, for example directly on the Desktop for a first try. Xoptfoil2 is a lightweight installation and does not install other artifacts on your PC.

To use Xoptfoil2 from any directory, add its installation folder to the Windows `Path` environment variable. This avoids having to copy `xoptfoil2.exe` into each new project folder.

<span>Linux</span>{: .label .label-red } (Debian-based)

Download the `Source Code` tar file and extract it in any folder. In addition to the standard development tools of a typical Linux distribution, the Fortran compiler and CMake are required. These tools can be installed with:

```
sudo apt install gfortran
sudo apt install cmake
```

In the project root, run the `build_linux.sh` script:

```
bash build_linux.sh
```

This builds and installs Xoptfoil2 to the repository-local default location `linux/bin`.

By copying `xoptfoil2` and `worker` to `/usr/local/bin`, they can be accessed from every directory. In some cases, you may need to mark the files as executable:

```
sudo chmod +x xoptfoil2
sudo chmod +x worker
```

If you prefer a system-wide installation via CMake, configure with `/usr/local` as install prefix and run install with `sudo`:

```
cmake -S . -B build -DCMAKE_INSTALL_PREFIX=/usr/local
sudo cmake --build build --target install
```

Note: CMake appends `bin` because the project installs executables with `DESTINATION bin`, so the final location becomes `/usr/local/bin`.

Linux command names are case-sensitive. Use `xoptfoil2` (lowercase), not `Xoptfoil2`.

### CMake build

Independent of the helper scripts, the standard CMake workflow can be used on both platforms:

```
cmake -S . -B build
cmake --build build
cmake --build build --target install
```

To override the default install location:

```
cmake -S . -B build -DCMAKE_INSTALL_PREFIX=<your_path>
```

The binary version string is defined centrally in the root `VERSION` file. For ad-hoc builds it can be overridden during configuration with `-DXOPTFOIL_VERSION_STRING=<value>`.

The default install locations are `windows/bin` (Windows) and `linux/bin` (Linux).