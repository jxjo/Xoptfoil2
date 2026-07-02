---
layout: home
title: CPU load during optimization
parent: Run Xoptfoil2
nav_order: 5
---


## CPU load during optimization
{: .no_toc }

Xoptfoil2 makes intensive use of multithreading during particle swarm optimization. Each particle is assigned its own thread to perform the CPU-intensive viscous aerodynamic analysis with Xfoil.

A special parameter in the input file allows to control how many CPU threads Xoptfoil2 should use during optimization: 

```fortran
&optimization_options                            
  cpu_threads      = nThreads                         
/  
```

If `nThreads` is positive, that number of threads is used. If `nThreads` is negative, the maximum available thread count is reduced by `nThreads`.

In this example an optimization was just started using all available threads.

![XO2](../images/CPU_load.png "cpu load")

The default setting is `cpu_threads = -1` leaving one thread free to allow more or less normal working with the PC.  