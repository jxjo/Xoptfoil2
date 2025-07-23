

![XO2](docs/images/Xoptfoil2.png "Xoptfoil2")

# Xoptfoil2 - The Airfoil Optimizer 

Optimize an airfoil based on its aerodynamic characteristics. 

Xoptfoil2 follows an approach to airfoil design that could be called 'design by polars' - in contrast to the classic design methods such as 'inverse design' or 'direct design'. 

A new, optimized airfoil is described by its aerodynamic properties with objectives at some operating points. The optimizer will try to design an airfoil geometry which satisfies these objectives best possible.  

Xoptfoil2 was already used to develop some [high end airfoil families](https://github.com/jxjo/Airfoils) for F3B/F3F model gliders. 


[Get started](https://jxjo.github.io/Xoptfoil2/docs/getting_started) ... and run your first airfoil optimizations. 

---


## Main features

* Optimization using 'Particle Swarm Optimization'
  - particle retry and rescue 
  - dynamic weighting of operating points 
* Aerodynamic evaluation based on Xfoil
  - retry of unconverged operating points 
  - outlier detection of xfoil results  
* Available shape functions 
  - Hicks Henne bump functions
  - Bezier curves  
  - Geometry parameter modification 
* Definition of an optimization task with operating points by
  - min cd, max cl/cd, max cl, min sink 
  - target values for cd, cl/cd, cm 
  - flap angle or flap angle optimization  
* Geometry targets thickness and camber 
* Curvature control 
  - bump detection for Hicks Henne shape type 
  - max curvature at trailing edge 
* Rerun optimization with refined targets 
* Worker tool for automization of typical tasks 


## Documentation 

For usage and background information on airfoil optimization please visit the [Xoptfoil2 documentation]( https://jxjo.github.io/Xoptfoil2)


## Installation

The actual version of Xoptfoil2 can be found in the [Releases section](https://github.com/jxjo/Xoptfoil2/releases) of this repo. In 'Assets' there some zip files: 
- a ready build version for Windows 
- the source files for building Xoptfoil2 under Linux

#### Windows

Download the Windows zip-file and extract it in any subdirectory - maybe for the first tries directly on the Windows Desktop. Xoptfoil2 is a very lightweight installation, which doesn't install any other artefacts on your PC.

#### Linux (Debian based) 

Download the `Source Code` tar file and extract it in any folder. In addition to the standard development tools of a typical Linux distribution, the FORTRAN compiler and cMake is needed. These tools can be installed with: 

```
sudo apt install gfortran
sudo apt install cmake
```

In the `src` folder you'll find the script `build_linux.sh` for compilation. Run this script and Xoptfoil2 should be ready to go.


## Examples

There are a few examples ready-to-run in the folder `examples`. As a first "Hello World" to optimization the example based on the SD7003 airfoil is well suited. Just double click on `make.bat` (Windows) or run `make.sh` (Linux) and the optimization will start.

You'll find much more informationen about this example in the [Xoptfoil2 documentation]( https://jxjo.github.io/Xoptfoil2)


## About the project

Xoptfoil2 is the successor of the awesome [Xoptfoil by Daniel Prosser](https://github.com/montagdude/Xoptfoil)  and [Xoptfoil-JX](https://github.com/jxjo/Xoptfoil-JX/tree/master), a branch of the original Xoptfoil. The objectives of this project are:
- building a robust and reliable airfoil optimization engine supporting future extensions in various aspects. 
- an airfoil optimizer UI, which allows to define, visualize and analyze an optimization task.
Have a look at the [AirfoilEditor project](https://github.com/jxjo/AirfoilEditor).

### Have fun! 

:+1:

Jochen Guenzel
