---
layout: home
title: Getting started
nav_order: 2
has_children: false
permalink: /docs/getting_started
---

# Getting started 

In this little tour we want to run our first airfoil optimization. It is not about understanding everything what is happening 'behind the curtain' but much more to get a first impression what Xoptfoil2 can achieve - and to experience, that airfoil optimization can be quite easy, if we (the user) don't make it too complicated. Let's go ...

### The optimization task 

We want to use the fine SD7003 airfoil for our new project. Being one of the first 'bubble ramp' airfoils the SD7003 was one of the conerstones in airfoil development. It is working perfectly a low Reynolds numbers. Because of that it is often used as a tip airfoil in a wing airfoil 'strak'.

![SD7003](../images/getting_started_SD7003.png)

In our new project the SD7003 shall be used as the base airfoil for the complete wing. To get best performance the SD7003 should be modified to handle the higher Reynolds number when used in the root wing section. Our new glider should become a quite fast allrounder with a good overall performance.

Therefore we define the optimization task as follows: 
1. optimize for Re=400000 at the root section
2. minimize drag at cl=0.2
3. retain the glide ratio of the original SD7003 at cl=0.7
4. the final airfoil should have 8% thickness

One way to go from here would be to fiddle around in Xflr5 trying to adapt geometry parameters like thickness highpoint (generations of users did this in endless sessions) ...
Or we use Xoptfoil2 to do the job.

### Get and run Xoptfoil2

In the Xoptfoil2 GitHub repo we find the actual version in the [Releases section](https://github.com/jxjo/Xoptfoil2/releases). In 'Assets' there some zip files: 
- a ready build version for Windows 
- the source files for building Xoptfoil2 under Linux

<span>Windows</span>{: .label .label-blue } 
Just download the Windows zip-file and extract it in any subdirectory - maybe for the first tries directly on the Windows Desktop. Xoptfoil2 is a very lightweight installation, which doesn't 'install' any other artefacts on your PC. Go to the folder `.\examples\SD7003_camb_thick` and double click on `make.bat`.


<span>Linux</span>{: .label .label-red } 
Please have a look in the [installation guide] for making your own build of Xoptfoil2. 
After a successful build open a shell in `.\examples\SD7003_camb_thick` and enter 
`Xoptfoil2 -i SD7003_cdmin.inp -o SD7003_cdmin_opt`

The optimization will start.




![XO2 First run](../images/getting_started_first_run.png)