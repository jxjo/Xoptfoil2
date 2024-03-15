---
layout: default
title: Basics of Optimization 
nav_order: 1
has_children: false
parent: Airfoil Optimization
has_toc: false
---

# Basics of Optimization  
The basics of airfoil optimization are explained in this short introduction. In doing so, we will take a targeted approach to the ‘objective function’ - the core of an optimization task.
{: .fs-5 .fw-300 }


## Basic Principle

The basic principle of airfoil optimization is very simple. An 'optimizer' generates an airfoil design, it is examined whether this design has better properties in the sense of our target, if yes, it is further improved in this direction, if no, something new is tried. 

![Bascis 1](../images/optimization_basics_1.png)

This interplay is repeated until no further improvement can be achieved.


## Optimizing Domains – The Objective Function

In the sense of ‘Separation of Concern’, it makes sense to divide the overall task into two separate domains. 

The optimization algorithm, such as ‘Particle Swarm Optimization’, is implemented in the optimizer domain. This is where the iterations are controlled, or the achievement of an end condition is checked. The optimiser domain has no knowledge of an airfoil or its aerodynamic properties. 

In the ‘airfoil domain’, an airfoil design is evaluated according to the formulated optimisation goals. New airfoils are created, the geometry is checked and an aerodynamic calculation is carried out with Xfoil. For its part, the airfoil domain does not know that it is running within an optimization - it is only responsible for the evaluation.

![Bascis 2](../images/optimization_basics_2.png)

The connection or interface between the two domains is very narrow: 

The optimizer passes a set of **Design variables** to the ‘Airfoil Evaluation’. In the case of Xoptfoil2, these variables are normalized between 0 and 1. 

As a result of the evaluation, a number is returned - the famous **Objective Function**.

For the initial airfoil, the 'Objective Function' is exactly equal to 1.0. A better airfoil in terms of the objectives has a value of less than 1.0 - a worse design has a value greater than 1.0 

The 'Objective Function' is the only information that the optimizer has available to find the best design.  It's a bit like the children's game 'Hot, cold' where the optimizer only has this one piece of information as a clue in which direction to improve the design variables.

*(That is still amazing to me...)*


## Create Shape and Evaluate 

Let's take a closer look at the 'Airfoil Evaluation' module. 

![Bascis 3](../images/optimization_basics_3.png)

In a first processing step, "Create Airfoil" contains the instructions on how to rebuild an airfoil from the 'Design variables' with the help of a 'Shape function'. The 'Shape function' can be a Bezier curve, for example, which is defined by its control points which are mapped from the 'Design variables'. 

The newly created airfoil then undergoes a geometry check in which designs that make no sense or violate geometric constraints are discarded. In this case, the optimizer is informed via a high value of the 'Objective Function': "That was nothing!"

After a successful geometric check, the actual aerodynamic evaluation is carried out on the basis of an Xfoil calculation at the operating points of our optimization task. The individual results are added up to the final value of the 'Objective Function'.     

A special feature of Xoptfoil2 is the ability to include geometric values such as thickness and camber in the evaluation and thus optimize an airfoil with regard to these values. 

Finally the evaluated value is passed back to the 'Optimizer'.


## Prepare and Initialize 
... and there 
![Bascis 3](../images/optimization_basics_4.png)
Some text here
