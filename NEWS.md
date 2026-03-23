# 0.0.4

- *new* `plot_adaptive_1d` and `plot_adaptive_2d` to plot the smoothing parameter as a function of the covariate when doing adaptive smoothing. See https://calgary.converged.yt/articles/adaptive_smoothing.html#bonus-plotting-the-smoothing-parameter-over-the-covariate for more information. Thanks to Philip Dixon for the suggestion.

- Bug fixes and improvements to clognorm:
  - bug: example, thanks Mike Dunbar
  - bug: when base is exponential, thanks to Gavin Simpson
  - bug: sign error in variance, thanks to me
  - improvement: clognorm now does transformation internally


# 0.0.3

- Work on censored log-normal helpers `clognorm` and `fitted_values_clognorm` were funded by the Department of Environment, Food and Rural Affairs, Natural Capital Ecosystem Assessment Programme. It was managed by the Environment Agency and delivered by the UK Centre for Ecology and Hydrology, under Research, Development and Evidence Framework contract RDE945.

# 0.0.2

- Thanks to Zachary Susswein for [spotting a bug](https://github.com/dill/calgary/issues/1) regarding a [change in the syntax](https://cran.r-project.org/web/packages/mgcv/ChangeLog) for `mgcv`.
- Added `vis_nei` to visualise neighbourhood structure for neighbourhood cross-validation

# 0.0.1.9000

* Add `smooth.construct.spde.smooth.spec` to build the Matérn model from Lindgren et al (2011)
* Add `simulate.gam` from `mgcViz`
* Added a `NEWS.md` file to track changes to the package.
