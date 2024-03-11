---
layout: home
title: Shape functions 
nav_order: 4
parent: Airfoil Optimization
permalink: docs/shape_functions
---

# Shaping Airfoil 

As Xoptfoil2 lore ipsum. 
{: .fs-5 .fw-300 }

## Camber-Thickness

The shape function `camb-thick` uses an airfoils geometry parameters to modify the airfoil. It is the most convinient methodas no care has to be taken for curvature artefacts or geometry constraints violations.
![Camber-Thickness](../images/shape_camb-thick.png)

Per default these 6 parameteres of an airfoils geometry are used respectively modified: 

- Thickness 
- Thickness highpoint position 
- Camber   
- Camber highpoint position
- Leading edge radius 
- Leading edge radius blending distance  

Each of these parameters can de-activated so they won't be changed during optimization. 

The shape function `camb-thick` is ideal for getting a quick estimation of the possible capabilties of an existing airfoil which should be adapted for a certain task - or to adapt an airfoil for a new Reynolds number 
(see [the Getting started example]({% link ../docs/getting_started.md %}#the_optimization_task) for more information). 

An adavantage of `camb-thick` is the fast convergence of an optimization 



## Bezier 
lore ipsum
Some text here
![Bezier](../images/shape_bezier.png)
## Hicks-Henne
lore ipsum
![Hicks-Henne](../images/shape_hicks-henne.png)
## Geometry constraints 
lore ipsum
