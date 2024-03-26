---
layout: home
title: Optimization Task  
nav_order: 6
parent: Airfoil Optimization
permalink: docs/objectives
---

# Optimization Task 
{: .no_toc}

In this chapter, we will learn how to define the actual optimization task for Xoptfoil2.  The previous chapters should have been well understood.
{: .fs-6 .fw-300 }


### Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

---


## Aerodynamic Objectives

The aerodynamic objectives of the optimization are defined with the help of so-called 'operating points', each of which defines a point on a polar curve of the airfoil. 

For an 'operating point', it is then defined which property is to be changed by the optimization and how. The most common specification is: 'Minimize drag' at this operating point. 

Depending on the degrees of freedom (number of design variables) resulting from the selected #shape function, 3 to a maximum of 15 operating points are or must be defined in order to achieve optimum results. 
Choosing the right operating points and defining the appropriate optimization target for this point is the key to successful airfoil optimization. 

### Defining an Operating Point

Two parameters are sufficient to clearly define an operating point on a polar curve:
- the Re-number   
- the angle of attack 'alpha' or alternatively the lift coefficient cl

In most cases, better results are achieved if the operating point is defined on the basis of the lift coefficient cl. This should be the default for your optimizations.

In the case of an 'alpha' operating point, it can happen that the optimizer wants to 'sneak' a low drag coefficient cd by reducing the lift.  However, an 'alpha' operating point is needed if the objective is to optimize cl-max, i.e. cl is the value to be optimized. 

### Min/Max Objectives  

### Target Objectives 

#### Allow improved targets 

## Geometric Objectives

### Constraints vs. Targets 
### Camber & Thickness 
#### Presetting 

## Objective function 

### Multi objectives - Pareto Front - Weighting
### Dynamic weighting 
### Local minima

## Example 