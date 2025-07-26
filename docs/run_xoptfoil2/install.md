---
layout: home
title: Installation
parent: Run Xoptfoil2
nav_order: 1
---


## Installation 
{: .no_toc }

The actual version of Xoptfoil2 can be found in the [Releases section](https://github.com/jxjo/Xoptfoil2/releases) of the Xoptfoil2 Github repo. In 'Assets' there some zip files: 
- a ready build version for Windows 
- the source files for building Xoptfoil2 under Linux

<span>Windows</span>{: .label .label-blue } 

Download the Windows zip-file and extract it in any subdirectory - maybe for the first tries directly on the Windows Desktop. Xoptfoil2 is a very lightweight installation, which doesn't install any other artefacts on your PC.

If you just want to try the examples, go to the folder `.\examples`, select an example and double click on `make.bat`.

Your own optimization projects should be created in seperate directories. Extend the Windows `Path` environment variable with the path to folder Xoptfoil2 was installed. Then you don't need to copy `xoptfoil2.exe`each time you create a new project.

<span>Linux</span>{: .label .label-red } 

Download the `Source Code` tar file and extract it in any folder. In addition to the standard development tools of a typical Linux distribution, the FORTRAN compiler and cMake is needed. These tools can be installed with: 

```
sudo apt install gfortran
sudo apt install cmake
```

In the `src` folder you'll find the script `build_linux.sh` for compilation. Run the script 

```
bash build_linx.sh
```

and Xoptfoil2 should be ready to go.

By copying `xoptfoil2`and `worker` to '/usr/local/bin' it can be accessed in every directory. Sometimes it is needed to mark the files as executables with 

```
sudo chmod +x xoptfoil2
sudo chmod +x worker
```