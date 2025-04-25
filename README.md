#  Estimating the duration of RT-PCR positivity for SARS-CoV-2 from doubly interval censored data with undetected infections

This repository contains the code to reproduce the manuscript ``Estimating the duration of RT-PCR positivity for SARS-CoV-2 from doubly interval censored data with undetected infections".
Once in the directory, `make all` should simply work, producing all figures and the pdf.
Note that figures are included in the directory `figures/output` and will only be reproduced if deleted first.

Three R packages, available on GitHub support the code.

- [atacccDurationEstimates](https://github.com/joshuablake/atacccDurationEstimates) contains duration estimates based on the ATACCC data used as a prior within the paper.
- [cisDurationModel](https://github.com/joshuablake/cisDurationModel) defines the statistical models, including Stan code, to analyses both the simulated and real data.
- [cisSimulation](https://github.com/joshuablake/cisSimulation) contains code to simulate a CIS-like study.

## Source of underlying data

The manuscript uses both simulated and real data.
The real data can only be analysed within the ONS's Secure Research Service (SRS), available on request to bona fide researchers; results of analyses within the SRS are included here.
Simulated results are also included.

### Simulations

Simulations can be reproduced [using this repository](https://github.com/joshuablake/CIS_survival_analysis_sims/tree/main).
This will produce a file `output/all_posteriors.R` which should match the file in this repository `data/all_posteriors.R`.
The repository also contains code to generate `data/input_curves.rds`, containing information on duration distributions from previous analyses to those presented in the manuscript; these distributions are used as the ground truth in the simulation and for the strongly informative prior for the analyses.

## Real data

These analyses are extracted from the SRS and contained in the folders starting `data/STATS`.
These files are statistical data from ONS which is Crown Copyright. The use of the ONS statistical data in this work does not imply the endorsement of the ONS in relation to the interpretation or analysis of the statistical data. This work uses research datasets which may not exactly reproduce National Statistics aggregates.
Code to implement the analyses is defined using models [in this R package](https://github.com/joshuablake/cisDurationModel).

## UK population data

The source for the data in the files `data/pop-estimates-2020.csv` and `data/by-ethnicity-5-groups-table.csv` is the Office for National Statistics, licensed under the [Open Government License v.3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).

