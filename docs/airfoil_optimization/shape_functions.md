---
layout: home
title: Shape functions 
nav_order: 4
parent: Airfoil Optimization
permalink: docs/shape_functions
---

# Shaping Airfoil 
{: .no_toc }

The creation of new airfoil designs during optimization is made by a 'shape function' which convert the set of design varibales into a new airfoil shape. There are three different 'shape functions' implemented each having its own advantages and disadvantages depending on the optimization task.
{: .fs-5 .fw-300 }

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

---
## Camb-Thick shape function 

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
<!---
(see [the Getting started example]({{ site.baseurl }}/getting_started/getting_started) for more information). just --->
 
(see [the Getting started example]({% link getting_started/getting_started.md %}#Getting started) for more information). 

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


## Bezier shape function 

[Bezier curves](https://en.wikipedia.org/wiki/B%C3%A9zier_curve) are an elegant variant of parametric curves. A set of control points define in an intuitive way the shape of the curve.
To shape an airfoil with Bezier curves, two Bezier curves are used: One for the top and one for the bottom side of the airfoil.

The more control points are used for a Bezier curve, the higher is the solution space to shape the surface of the airfoil. Certain control points have a 'fixed role' when a Bezier curve is used as shape function: 

- Point 1: The leading edge point, which is fixed to 0,0 
- Point 2: Defines the tangent and (very much) the curvature at the leading. As the tangent at leading edge has to be vertical, the x-coordinate is fixed to 0 and only the y-coordinate varies.  
- Point n: The trailing edge point fixed at x=1.0. The y-coordinate defines the 'trailing edge gap' 

Typically 4 to 5 control points are enough to cover most of the optimization tasks and wii result in smooth, well formed geometry of the airfoil. 
If the airfoil should have a curve reversal (on the upper side for a reflexed airfoil, on the lower side for an airfoil with rear-loading) on more control point should be taken to cover the curvature requirement. 


![Bezier](../images/shape_bezier.png)
Control points of the Bezier curves for upper and lower side. The upper side has one more control point to allow a curve reversal (reflexed airfoil for a flying wing) 
{: .fs-2}

The number of design variables needed for a Bezier curve is calculated by.
```
ndv = (ncp - 3) * 2  + 1        (ncp = no of control points)
```
Therefore, it will need 7 design variables for a single Bezier curve with 6 control points.


### Input Options

Normally no input paramters are needed for shape function `bezier` as the defaults activate all possible geometry modifications. Increase the number of Bezier control points only if you have advanced requirements like 'curvature reversals'. 

```fortran
&bezier_options                                  ! options for shape_function 'bezier'
  ncp_top          = 5                           ! no of bezier control points on top side              
  ncp_bot          = 5                           ! no of bezier control points on bot side
  initial_perturb  = 0.1                         ! max. perturb when creating initial designs
/
```

### Airfoil preprocessing 

If a normal '.dat' airfoil file is used as the seed airfoil for an optimization, a 'transformation' of a coordinate based geometry to a Bezier based geometry has to be made to achieve an inital 'Bezier design'.

For this, an internal, very fast optimization run is started, which uses a Simplex (Nelder-Mead) optimization to find a Bezier curve which matches as good as possible the original '.dat' airfoil. 

During this 'match-foil' optimization, particular attention is paid to the curvature of the leading and trailing edges in order to obtain a geometrically clean seed airfoil for the subsequent main optimization.

With the option 'show_details' some further information about this 'match-foil' optimization is displayed.  


### Output files 

After the optimization has finished there will be an additional airfoil file beside the normal '.dat' file which is a '.bez' file which holds the information about the final Bezier control poin coordinates. 

```
My-Example-Airfoil
Top Start
  0.0000000000  0.0000000000
  0.0000000000  0.0132540570
  0.6491545620  0.0261382462
  1.0000000000  0.0001545000
Top End
Bottom Start
  0.0000000000  0.0000000000
  0.0000000000 -0.0137343844
  0.5252227777 -0.0088367053
  1.0000000000 -0.0001545000
Bottom End
```

{: .tip }
The [Airfoil Editor](https://github.com/jxjo/PlanformCreator2) is able to read a '.bez' file and visualize the Bezier curves with its control points. It can also be used to modify the Bezier curves.

{: .tip } 
A '.bez' file can also be used as a seed airfoil for Xoptfoil2. As a Bezier based airfoil is per definiton normalized no preprocessing of the seed airfoil will be performed and the optimization will begin on the exact Bezier airfoil definition. This makes '.bez' files ideal for repeated optimization runs.  


### Curvature Aspects

#### Control poins and curvature 

The less control points are used, the more 'friendly' and uncomplicated Bezier curves are regarding curvature artefacts which make them ideal for optimization. As only a few control points are not able to shape bumps or other artefacts on the surface, the number of 'helper' operatings points can also be reduced which will result in a fast optimization with a good convergence. 

To squeeze out the best performance, it may be needed to enlarge the theorectical solution space with more control points. In this case more and more care has to be taken to avoid curvature artefacts. 

At trailing edge the 2 options `check_curvature`and `auto_curvature` take care for a smooth trailing edge without 'spoilers' or other artefacts. 

#### Leading edge curvature 

A typical problem arises when two independent curves are combined to form an airfoil. For Bezier curves the zeroth and first derivatives are continuous at leading edge as the tangent at leading edge is vertical for both upper and lower side. 

But the second derivate of both curves will differ at leading edge meaning the combind curve won't have C² continuity. When rebuilding such a combined Bezier airfoil out of coordinates and spline, the curvature will show some oscillation artefacts at leading edge. 

The question is what influence these artifacts have on the aerodynamic properties.

To achieve the geometric cleanest possible airfoil, an additional 'geometric constraint' is active when using Bezier curves: The difference of the curvature of the two Bezier curves at leading edge may not exceed the value of the seed airfoil. This constraint adds a quite challenging task to the optimizer. 

When `show_details` is activated, the number of this type of constraint violations is labeled as `max_le_curv_diff`. 







## Hicks-Henne
lore ipsum
![Hicks-Henne](../images/shape_hicks-henne.png)
## Geometry constraints 
lore ipsum
