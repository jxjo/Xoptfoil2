---
layout: home
title: Home
nav_order: 1
description: "A tool for optimizing airfoils"
permalink: /
---

![XO2](./images/Xoptfoil2.png "Xoptfoil2")


# The Airfoil Optimizer 
{: .fs-9 }

Optimize an airfoil based on its aerodynamic characteristics. 
{: .fs-6 .fw-300 }

[Get started now]({% link getting_started/getting_started.md %}#Getting started){: .btn .btn-primary .fs-5 .mb-4 .mb-md-0 .mr-2 }
[View it on GitHub]("https://github.com/jxjo/Xoptfoil2"){: .btn .fs-5 .mb-4 .mb-md-0 }

{: .note }
The current status of the project is *in development*. Therefore no version 1.0 is available yet, but only regular beta versions, which are published here under 'Releases'. 

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


## About the project

Xoptfoil2 is the successor of Xoptfoil and [Xoptfoil-JX](https://github.com/jxjo/Xoptfoil-JX/tree/master), which is a branch of the original, awesome Xoptfoil by Daniel Prosser.

The objective of the project are:
- building a robust and reliable airfoil optimization kernel supporting future extensions in various aspects. 
- an airfoil optimizer GUI, which allows to define, visualize and analyze an optimization task - WiP

Jochen Guenzel, 2024 