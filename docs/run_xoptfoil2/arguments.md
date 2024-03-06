---
layout: home
title: Command line arguments
parent: Run Xoptfoil2
nav_order: 1
---


## Command line arguments 
{: .no_toc }



Following options are supported 

| Argument                        | usage     | description                               |
|:--------------------------------|:----------|:------------------------------------------|
|  <nobr>&lt;input_file></nobr> <nobr>-i &lt;input_file></nobr> | mandatory | ‘input file’  which holds all the parameters used for optimization  |
| <nobr>-o &lt;output_prefix></nobr> | optional  | The ‘output prefix’ will be the name of the generated airfoil as well as the subdirectory prefix for the intermediate data files for visualization |
| <nobr>-a &lt;airfoil_file></nobr>  | optional  | Airfoil .dat-file which will be used as the seed airfoil - equals to `airfoil_file` in namelist `&optimization_options` |
| <nobr>-r &lt;reynolds></nobr>      | optional  | Default Reynolds number for operating points - equals to  `re_default` in namelist `&operating_conditions` |
| <nobr>-h</nobr>                 | optional  | Shows command line help |



Tip: 

Put the command to run Xoptfoil-JX in a little batch file (shell script) so you can start the optimization with a double click in the Explorer.

It’s a good advice to name the “input file” like <output_prefix>.inp so the newly created airfoil and its corresponding input file belong together.
Together with Xfoil_worker, a little tool coming with Xoptfoil-JX, it is straightforward to automate the airfoils design process including setting flaps for the new airfoil or calculating complete polar sets which can be directly imported into Xflr5.



