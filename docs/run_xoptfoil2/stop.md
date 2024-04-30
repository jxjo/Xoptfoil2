---
layout: home
title: Stopping an optimization
parent: Run Xoptfoil2
nav_order: 4
---


## Stopping an optimization
{: .no_toc }

Once an optimization is started there is only limited possibility stop an optimization.

The easiest way is just to close the command / shell window. The execution of Xoptfoil2 will be 
stopped immediately without creating an optimized airfoil. This is fine for all the cases where you early identify, the optimization is going in the wrong direction. 

In cases where the optimizier is still trying to improve a little bit with no or too little success, a regular 'stop' is possible: 

When Xoptfoil2 is up and running a little file named `run_control` is created in the current directory. 
Open this file with a text editor, write 'stop' in the first line and save the file. Xoptfoil2 will then have a friendly shut-down, creating the final airfoil. 
