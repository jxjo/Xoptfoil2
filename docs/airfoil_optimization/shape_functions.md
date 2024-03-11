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

Per default these 6 parameteres - or design variables - of an airfoils geometry are used respectively modified: 

- Thickness 
- Thickness highpoint position 
- Camber   
- Camber highpoint position
- Leading edge radius 
- Leading edge radius blending distance  

Each of these parameters can de-activated so they won't be changed during optimization. 

The shape function `camb-thick` is ideal for getting a quick estimation of the possible capabilties of an existing airfoil which should be adapted for a certain task - or to adapt an airfoil for a new Reynolds number 
(see [the Getting started example]({{ site.baseurl }}/getting_started/getting_started) for more information). just --->
<!--- 
(see [the Getting started example](docs/getting_started/getting_started.md) for more information). ---> 

As the solution space for new designs is limited it is not advisable to define more than 2,3 or 4 operating points as optimization objectives. In contrast to the shape function `bezier` and `hicks-henne` there is quite seldom a need to define 'helper operating points' to avoid side effects. 

The big advantage of `camb-thick` is the fast convergence of an optimization. Typically after 50 iterations as the design radius is below the `min_radius`. 

### Input Options

Normally no input paramters are needed for shape function `camb-thick` as the defaults activate all possible geometry modifications. 

In Case you want to fix a certain geometry paramter to its current value, set the corresponding option to `.false`. 

```fortran
&camb_thick_options                              ! options for shape_function 'camb_thick'
  thickness        = .true.                      ! optimize thickness 
  thickness_pos    = .true.                      ! optimize max. thickness position
  camber           = .true.                      ! optimize camber
  camber_pos       = .true.                      ! optimize max. camber position
  le_radius        = .true.                      ! optimize leading edge radius
  le_radius_blend  = .true.                      ! optimize blending distance for le radius change 
  initial_perturb  = 0.1d0                       ! max. perturb when creating initial designs 
/
```


## Bezier 
lore ipsum
Some text here
![Bezier](../images/shape_bezier.png)
## Hicks-Henne
lore ipsum
![Hicks-Henne](../images/shape_hicks-henne.png)
## Geometry constraints 
lore ipsum
