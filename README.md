# LMS-model

## Reference
__Article Title__: Reference values of body composition parameters and visceral adipose tissue (VAT) by DXA in adults aged 18–81 years—results from the LEAD cohort

__DOI__: 10.1038/s41430-020-0596-5, 2019EJCN0971

__Link__: https://www.nature.com/articles/s41430-020-0596-5

__Citation__: Ofenheimer, A., Breyer-Kohansal, R., Hartl, S. et al. Reference values of body composition parameters and visceral adipose tissue (VAT) by DXA in adults aged 18–81 years—results from the LEAD cohort. Eur J Clin Nutr (2020).


## Description
This is a repository with the entire code used to create the LMS models, plots and statistical analysis for the paper referenced above. The code should be easy to understand and use for other projects including LMS model generation for body composition data of large cohorts, in particular, to generate age dependent (in a continuous manner) percentile curves which can be used as reference values.

The code is written in R.


## Data & Applications
While the raw input data is not publicly available, the generated LMS model output data (LMS values and percentile curves for children and adults) can be found [here](https://github.com/FlorianKrach/scientific-LMS-zscore-app/tree/master/data). It is part of the [Scientific LMS Z-Score App](https://github.com/FlorianKrach/scientific-LMS-zscore-app), a python application to compute z-scores based on our generated reference values.

A more easily accessible version of this is the corresponding [WebApp](https://floriankrach.github.io/pyscripts/zscore-app-pyscript/index-scientific.html).



## Citation
If you find this code useful or if you use it for your own work, please cite the paper referenced above.
