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

In most cases, better results are achieved if the operating point is defined on the basis of the lift coefficient cl. This is also the default for an operating point.

In the case of an 'alpha' operating point, it can happen that the optimizer wants to 'sneak' a low drag coefficient cd by reducing the lift.  However, an 'alpha' operating point is needed if the objective is to optimize cl-max, i.e. cl is the value to be optimized. 

To specify the Reynolds number of the operatings points, there are several convinience paramters to set default Re numbers values. The special option `re_default_as_resqrtcl` allows to use T2 polars as the base of optimization. In case of T2 (constant lift polar) the Re number is interpreted as Re*sqrt(cl)  


```fortran
&operating_conditions                            ! options to describe the optimization task
  ...
  re_default             = 400000                ! use this Reynolds number for operating points
  re_default_as_resqrtcl = .false.               ! interpret re number as type 2 (Re*sqrt(cl)) 
  mach_default           = 0.0                   ! use this Mach number for operating points 
  ...
/
```


### Min/Max Objectives  

Once an 'operating point' has been defined on a polar curve, the optimization target to be achieved for this point is now defined.  The 'optimzation type' option is used for this. 

If it is a min/max optimization, possible values for this option are


| `optimization_type`| Description                               |
|:-------------------|:------------------------------------------|
|  `min-drag`        | Minimize drag - more correctly: minimize the drag coefficient cd  |
|  `max-glide`       | Maximize glide ratio 'cl/cd'. This is equivalent to 'min_drag' if the operating point is defined on the basis of cl  |
|  `min-sink`       | Minimize the sink rate cl³/cd² |
|  `max-lift`        | Maximize lift - more correctly maximize the lift coefficient cl. Only makes sense if the operating point is defined on the basis of alpha |
|  `max-xtr`         | Move the laminar-turbulent transition location as far towards the trailing edge as possible. The mean value of upper and lower side is taken as the objective|

Min/max objectives should be used as sparingly as possible, as operating points usually have competing objectives and the optimization results are then no longer deterministic (see Pareto Front).  

### Target Objectives 

Target objectives are more powerful and more versatile than min/max objectives. A `target_value` is specified for the operating point. If this `target_value` is reached during optimization, the optimizer can concentrate on the other operating points, so to speak, which leads to more reproducible results overall.

| `optimization_type`| Description                               |
|:-------------------|:------------------------------------------|
|  `target-drag`     | Drag coefficient cd to be achieved for this operating point |
|  `target-glide`    | Glide ratio cl/cd to be achieved for this operating point. This is equivalent to `target-drag` if the operating point is defined on the basis of cl. |
|  `target-moment`   | Moment coefficient cm to be achieved for this operating point|

The default setting is that the target values of an operating point may also be exceeded ('allow_improved_target = .true.'). In rare cases, it can be useful to reach a target value exactly and not to exceed it ('allow_improved_target = .false').

{: .tip }
Design by Polar

An interesting variant of optimization can be implemented with 'Target Objectives': Reverse engineering of a polar profile. 

To do this, an existing polar curve, for example from a publication, is mapped with the help of 5-10 operating points and 'target_drag'. The optimizer then generates the airfoil associated with this polar curve. If the operating points are well chosen, the result is an amazingly good reproduction of the original airfoil. 


### Flap Optimization 

## Geometric Objectives

### Constraints vs. Targets 
### Camber & Thickness 
#### Presetting 

## Objective function 

### Multi objectives - Pareto Front - Weighting
### Dynamic weighting 
### Local minima

## Example 