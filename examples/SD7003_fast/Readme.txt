An easy going example to get a first impression of airfoil optimization. 

Optimization Task: 

The low reynolds airfoil SD7003 is adapted to little higher Reynolds number at 400,000.
Optimization target is to minimize drag at lower cd - while maintaining glide ration at cl = 0.7. 

The optimized airfoil should have 8% thickness.

Shape_functions 'camb-thick' will be used, which modifies the airfoil by changing the geometry
paramters camber, thickness, their highpoints and leading edge radius and its blending distance.

This results in a total of just six design variables leading to a fast convergence towards the 
final airfoil. 

As there is no danger of geometry artefacts only a few operting points are needed to achieve good results.

Run the example by: 

-> Double click 'make.bat' to start the optimizer 

Enjoy!





