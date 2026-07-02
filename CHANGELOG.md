# Changelog

All notable changes to this project will be documented in this file.

## v 2.0.0

This major release focuses on consolidating the feature set to what has proven useful and robust over the years.
Several rarely-used settings and parameters have been intentionally removed to keep the code base clean.

These **breaking changes** may require existing Xoptfoil2 input files to be updated before they can be used with the new version.


### Revised

- **Particle Swarm Optimization**
  - Retry logic for geometrically invalid designs reworked
  - Goal (target) attainment as part of the objective function replaces the former dynamic weighting
  - Soft penalties as part of the objective function when a design only slightly violates geometric or curvature constraints
  - Removed "rescue stucked particles" logic
  - Tuned PSO coefficients for `quick` and `exhaustive` convergence profiles
  - `convergence_profile 'quick_camb_thick'` removed
- **Seed airfoil preparation** — the seed airfoil is now always first matched to a Bezier curve to produce a clean, well-defined starting geometry
- **Constraint evaluation** refactored into a unified penalty framework with per-type statistics and scaling
- **Examples** revised and updated to reflect the new feature set


### Added

- **Optimization objectives**
  - New op-point type `target-cp-min` to optimize minimum pressure coefficient
  - `cp_min` included in polar file output
- **Geometric constraints** — new parameters: `max_thickness`, `min_thickness`, `max_camber`, `min_camber`, `min_thickness_at_x`, `min_te_top_angle`, `max_te_bot_angle`
- **Curvature constraints**
  - `check_le_curvature` — ensures leading-edge curvature decreases monotonically
  - `check_curvature_bumps` / `bump_threshold` — suppresses curvature bumps on top and bottom sides
- **B-Spline shape function** — experimental support for B-spline based airfoil parameterization
- **Worker** — `match-bezier` and `match-bspline` now respect `auto_curvature` constraints read from the input file


### Removed / Deprecated

- Shape function `camb-thick` removed
- PSO: `rescue_particle` / `stucked_threshold` / `rescue_frequency` options removed
- PSO: `init_attempts` option removed; `convergence_profile 'quick_camb_thick'` removed
- Geo target mode `match-foil` removed
- Seed airfoil option `preset_to_target` removed
- Hicks-Henne option `smooth_seed` removed
- Curvature constraint parameters `curv_top_spec`, `curv_bot_spec`, `max_spikes_top`, `max_spikes_bot`, `spike_threshold`, `max_le_curvature_diff` removed
- Dynamic weighting within PSO removed (replaced by goal attainment)
- Negative target value interpreted as a scale factor — removed
- Worker: option `-w check` removed
- Worker: option `-w polar-csv` removed
- Worker: option `-w polar-flapped` deprecated - flapped polars now unified in `-w polar`


## v 1.0.11 

This is a just a maintenance release 

### Added 

- Support of forced transition in polar generation. 


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