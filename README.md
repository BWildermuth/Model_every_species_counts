Updated in August 2025
# Model_every_species_counts
Code for the manuscript under review:
Every species counts: Arthropod species loss, but not their identity, underpins biomass declines

The R code was previously run on R 4.2.2 and newer versions. Required packages can be found in the code. Installation of R takes a couple of minutes, installation of the required packages a few seconds.

The code is split into a file for the Jena Experiment and a file for the Biodiversity Exploratories. The code is annotated.
The first part (Price equation comparisons) can be run with the raw data provided in the submission system, be aware that the runtime can exceed multiple hours.
The second part (1,000 linear mixed-effects models based on random subsamples for each of the 9 responses (total of 9,000 iterations)) can be run with the full processed dataset of all arthropods together, also provided in the submission system. Runtime should not exceed a couple of hours per response - if all responses are run, it may take a day. This code can reproduce the main results of the paper, including model outputs and predictions.
