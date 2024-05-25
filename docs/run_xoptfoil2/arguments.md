---
layout: home
title: Command line arguments
parent: Run Xoptfoil2
nav_order: 2
---


## Command line arguments 
{: .no_toc }


Following options are supported 
   
| Argument                         | Usage     | Description                               |
|:---------------------------------|:----------|:------------------------------------------|
|  input_file <nobr>-i input_file</nobr> | mandatory | name of input file which holds all the parameters used for optimization  |
| <nobr>-o output_prefix</nobr>    | optional  | The output_prefix will be the name of the generated airfoil as well as the subdirectory prefix for the intermediate data files for visualization |
| <nobr>-a airfoil_file</nobr>     | optional  | Name of airfoil file which will be used as the seed airfoil - overwrites `airfoil_file` in namelist `&optimization_options` of the input file |
| <nobr>-r reynolds</nobr>         | optional  | Default Reynolds number for operating points - overwrites  `re_default` in namelist `&operating_conditions` of the input file |
| <nobr>-h</nobr>                  | optional  | Shows command line help |


If the output_prefix is omitted, the stem of the input file name will be taken as the ‘output prefix’.

Example:
 
`Xoptfoil2 -i JX-GT-15.inp ` will have the output_prefix `JX-GT-15` and the name of the optimized airfoil will be `JX-GT-15.dat`. 

Both arguments airfoil_file and reynolds can be used for automization by using one input file for different optimization tasks. 


{: .tip }
Put the command to run Xoptfoil2 in a little batch file (shell script) so you can start the optimization with a double click in the Explorer.

{: .tip }
You can also connect the file type of the input file e.g. `.inp` to Xoptfoil2. So a double click on the input file will start the optimizer. 
