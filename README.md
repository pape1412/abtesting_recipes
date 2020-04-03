# ABTesting Recipes
A/B testing recipe collection.

## Intro
This repository serves as growing collection of theory and methodologies around the topic of A/B testing. As of now it covers topics such as
- Sample size estimation
- Power analysis
- Permutation tests
- Bootstrapping tests
- Group sequential designs
- Bayesian estimation

## Files
Each of the topics listed above is presented in a jupyter notebook including both a brief overview of theoretical foundations and a coded example. At the moment all coded examples are written in ```R```. And because I'm sometimes too lazy to switch back and forth between different tools I've added a couple of handy custom functions to run on my SQL client directly (works on Exasol).
```
R\
  bayesian_estimation_R.ipynb			# notebooks
  bootstrapping_test_R.ipynb
  group_sequential_designs_R.ipynb
  permutation_test_R.ipynb
  power_analysis_R.ipynb
  survey_sample_size_R.ipynb
data\
  pwr_data_control.csv				# toy examples
  test_data_control.csv
  test_data_test.csv
sql\
  bootstrapping_test.sql			# custom sql functions
  descriptive_statistics.sql
  t_test.sql
```
