# Changelog

All notable changes to this project will be documented in this file.

## v 1.0.10 

This is a just a maintenance release 

### Fixes

- Worker: Polar generation - ensure starting at AOA 0°
- Xoptfoil2: Shape function Bezier - fix solution space 
- Worker: Polar generation up to Reynolds 1d8 - 1
- Minor fixes


## v 1.0.9

### Added 

- Worker: Bubble detection in polar generation (AirfoilEditor)  


## v 1.0.8

### Fixes
- Worker: #20 - Outlier detection for T2 polar


## v 1.0.7

### Fixes

- Worker: #19 - Segmentation fault when generating a single T2 polar


## v 1.0.6

### Added
- Xoptfoil2: Executable is now lowercase: 'xoptfoil2'
- Xoptfoil2: Revised examples 
- Xoptfoil2: Get ready for the [AirfoilEditor](https://github.com/jxjo/AirfoilEditor)
- Worker: New worker action 'polar-flapped' to create all polars in a single directory
- Worker: Executable is now lowercase: 'worker'

### Fixes
- Minor bug fixes


## v 1.0.5

### Fixes

- Worker: Linux - blank in an airfoil filename caused crash
- Xoptfoil2: y value of flap hinge couldn't be set to 0.0 or 1.0 ( thanks @robe191 )


## v 1.0.4

### Fixes

- Worker: Polar generation got hung up 
- Worker: Error messages for mkdir and rmdir operations 
- Worker: Colors in terminal output  
- revised build scripts 


## v 1.0.3

### Fixes

- Worker: Some fixes of 'auto_range' in polar generation 
- Worker: Polar name with ncrit >= 10.0 
- Xoptfoil2: NACA 23112
- Revised build jobs (thanks @andir !)


## v 1.0.2

### Added

- Worker: Republished - see the [Worker documentation](https://jxjo.github.io/Xoptfoil2/docs/worker) for details or run the example in the examples folder
- Worker: Improved polar generation with new 'auto_range' feature to obtain an optimal alpha range of a polar
- Worker: 'Match Bezier' to create a Bezier based airfoil from an existing one 

### Fixes
- Xoptfoil2 minor fixes


## v 1.0.1

### Added

- Xoptfoil2: Optimization with Mach numbers is now properly supported. `mach_default` allows to set a default Mach number. 
- Worker: Generation of polars with Mach numbers now possible - see [Quick Reference](https://jxjo.github.io/Xoptfoil2/docs/input_file#quick-reference) section `polar_generation`.