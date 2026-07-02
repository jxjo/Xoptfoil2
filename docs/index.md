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

Xoptfoil2 follows an airfoil design approach that can be described as 'design by polars', in contrast to classic methods such as 'inverse design' or 'direct design'.

A new optimized airfoil is defined by aerodynamic objectives at selected operating points. The optimizer then searches for an airfoil geometry that satisfies these objectives as closely as possible.

Xoptfoil2 has been successfully used to develop the [JX airfoil families](https://github.com/jxjo/Airfoils) for F3B/F3F model gliders.


[Get started]({% link getting_started/getting_started.md %}#Getting started){: .btn .btn-primary .btn-green } ... run your first example optimizations. 
{: .mt-6}

---


## Main features

* Optimization using Particle Swarm Optimization
  - particle retry for invalid geometric designs
  - goal attainment balancing to handle diverse Pareto front objectives
* Aerodynamic evaluation based on Xfoil
  - retry of unconverged operating points
  - outlier detection of Xfoil results
* Available shape functions
  - Bezier curves
  - Hicks Henne bump functions
  - B-Spline curves (not for production)
* Definition of an optimization task with operating points by
  - min cd, max cl/cd, max cl, min sink
  - target values for cd, cl/cd, cm, cp_min
  - flap angle or flap angle optimization
* Geometry targets for thickness and camber
* Geometry and curvature constraints
* Curvature control
  - control curvature reversals for rear-loaded or reflexed airfoils
  - bump detection and suppression
  - max curvature at trailing edge
* Rerun optimization with refined targets
* Worker tool for automation of typical tasks

---

[Download](https://github.com/jxjo/Xoptfoil2/releases){: .btn .btn-purple} ... the latest release from Github.
{: .mt-6 }

---

## About the project

Xoptfoil2 is the successor of the awesome [Xoptfoil by Daniel Prosser](https://github.com/montagdude/Xoptfoil). The objectives of the project are:
- generate high-end airfoils with smooth, clean geometry ready for CAD use
- provide a CLI command-line tool for airfoil optimization
- serve as the optimization engine in [AirfoilEditor](https://github.com/jxjo/AirfoilEditor) with a graphical UI for airfoil optimization

Jochen Guenzel 