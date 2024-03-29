

![XO2](images/Xoptfoil2.png "Xoptfoil2")

# Xoptfoil2 - The Airfoil Optimizer 
Optimize an airfoil based on its aerodynamic characteristics. 

Xoptfoil2 is the successor of Xoptfoil and [Xoptfoil-JX](https://github.com/jxjo/Xoptfoil-JX/tree/master), which is a branch of the original, awesome Xoptfoil by Daniel Prosser.

>[!NOTE]
>The current status of the project is *in development*. 

Therefore no version 1.0 is available yet, but only regular beta versions, which are published here under 'Releases'. 


## Main features

* Optimization with 'Particle Swarm Optimization'
  - particle retry and rescue 
  - dynamic weighting of operating points 
* Aerodynamic evaluation based on Xfoil
  - retry of unconverged operating points 
  - outlier detection of xfoil results  
* Available shape functions 
  - Hicks Henne bump functions
  - Bezier curves for top and bot side 
  - Modification of geometry parameters 
* Definition of an optimization tasks with operating points by
  - min cd, max cl/cd, max cl, min sink 
  - target values for cd, cl/cd, cm 
  - flap angle or flap angle optimization  
* Definition of geometry targets thickness and camber 
* Curvature control 
  - bump detection for Hicks Henne shape type 
  - max curvature at trailing edge 
* 'Worker' tool for automization of typical tasks 


## Software Design 

The system consists out the Xoptfoil2-Engine, which has a command line interface controlled by an input file - and the Airfoil Optimizer UI, which allows to define, visualize and analyze an optimization task (the UI is not published yet) 

## Documentation 

For usage and background information please visit [Xoptfoil2 homepage]( https://jxjo.github.io/Xoptfoil2)


## Examples 

There are a few exmaples ready-to-run in the folder './exmaples'. As a first "Hello World" to optimization the example with the SD7003 is well suited. 

## Installation

Under 'Releases you'll find a ready build beta version for Wondows as a zip-File. Download this file und extract the zip in a directory of your choice. A double click on one of the example 'make.bat' should start the optimization. 

### Have fun! 

:+1:

Jochen Guenzel, February 2024 
