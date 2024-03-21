---
layout: home
title: Format
parent: Input File
nav_order: 1
---


## Format of the Input File 
{: .no_toc }

The parameters of the input file are grouped in sections called ‘namelists’ which start with `&namelist_name` and end with an `/` character. The format of the input file is also an indication of the language in which the program is written: Fortran.   

```fortran
&option_list
  logical_option  = .true.                      ! this is a remark 
  number_option   = 1.0 
  string_option   = 'bezier' 
/
```

An option can either be  
- logical with the values `.true.` or `.false.`
- numerical with a point as decimal separator  
- a string enclosed in apostrophes like `'myString'`

Remarks or comment are introduced by an exclamation mark. It is a good advice to use 'remarks' extensively in your input file to remember modifications made between several optimizations runs.



### Minimum example

Only a few parameters are mandatory and most of the numerous Xoptfoil2 paramters are optional having a default value. Here is an example of a minimum input file for a valid optimization task 

```fortran
! Optimize cd while trying to preserve glide ratio of SD7003 at Reynolds 400,000 

&optimization_options
  airfoil_file     = 'SD7003.dat'                ! the seed airfoil 
  shape_functions  = 'camb-thick'                ! just optimize geometry parameters like camber 
/  

&operating_conditions
  re_default       = 400000                      ! Reynolds number for all operating points
  noppoint         = 2                           ! we define only 2 operating points

  op_mode(1) = 'spec-cl'                         ! operating point based on cl-value
  op_point(1) = 0.2                              ! cl=0.2
  optimization_type(1) = 'min-drag'              ! minimize cd (drag) at this point 

  op_mode(2) = 'spec-cl'                         ! operating point based on cl-value
  op_point(2) = 0.7                              ! cl = 0.7
  optimization_type(2) = 'target-glide'          ! target is the glide ratio cl/cd  
  target_value(2) = -1.0                         ! try to preserve the value of SD7003
/
```

### Parameters as arguments 

There are a few optimization parameters which can be provided as command line arguments. 
In this case the command line argument will overwrite the value in the input file. 

- `-a <filename>`  overwrites `<airfoil_file>` of `&optimization_options`
- `-r <number>`    overwrites `<re_default>` of `&operating_conditions`

Using command line arguments can be useful when a row of optimization tasks should be automized for example to build a series of airfoils for a single wing. 
