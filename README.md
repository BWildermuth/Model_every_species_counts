# Model_every_species_counts
Code for the manuscript under review:
Every species counts: Arthropod species loss, but not their identity, underpins biomass declines

The R code was previously run on R 4.2.2 and newer versions. Required packages can be found in the code. Installation of R takes a couple of minutes, installation of the required packages a few seconds.

The code is split into a file for the Jena Experiment and a file for the Biodiversity Exploratories. The code is annotated.
The first part (Price equation comparisons) can be run with the example data provided here (a subset of the original raw data), be aware that the runtime can still exceed a couple of minutes.
The second part (linear mixed-effects models) can be run with the full dataset on all arthropods together, provided via the submission system. Runtime should not exceed a couple of seconds per iteration - if all iterations are run, it may take a couple of minutes. This code can reproduce the main results of the paper, including model outputs and predictions.
