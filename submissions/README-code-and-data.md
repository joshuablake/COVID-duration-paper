# Code and data for "Estimating the duration of RT-PCR positivity for SARS-CoV-2 from doubly interval censored data with undetected infections"

Joshua Blake, Paul Birrell, A. Sarah Walker, Koen B. Pouwels, Thomas House,
Brian D M Tom, Theodore Kypraios, Daniela De Angelis.

*Biometrics*, manuscript BIOM2025402P.

This archive contains the R scripts and the released data behind every figure
and table in the manuscript and its Supplementary Materials. Each script starts
with a header saying what it produces, which inputs it needs, and how to run
it.

The complete repository, including its version history, is at
<https://github.com/joshuablake/COVID-duration-paper>.

## Contents

```
figures/R/    the scripts that produce each figure and table
data/         the data those scripts read
```

## Which script produces what

| Script | Produces | Reads |
| --- | --- | --- |
| `figures/R/challenges.R` | Figure 1 | nothing (schematic) |
| `figures/R/regions_diag.R` | Figure 2 | nothing (schematic) |
| `figures/R/sim_survival.R` | Figure 3 | `data/all_posteriors.rds`, `data/input_curves.rds` |
| `figures/R/CIS_survival.R` | Figures 4 and 5, and the posterior summaries quoted in the text | `data/STATS18744/draws.rds`, `data/STATS17701/draws.rds` |
| `figures/R/surv_priors.R` | Web Figure 1 | `atacccDurationEstimates` package |
| `figures/R/CIS_ntot.R` | Web Figure 2 | `data/STATS18744/means.rds` |
| `figures/R/demographics_table.R` | Web Table 1 | `data/STATS22850/*.csv`, `data/pop-estimates-2020.csv`, `data/by-ethnicity-5-groups-table.csv` |
| `figures/R/utils.R` | shared plotting helpers, sourced by the others | — |
| `data/demographics.R` | console-only cross-check of the Web Table 1 figures | `data/STATS22850/*.csv`, `data/pop-estimates-2020.csv` |

## Running the scripts

The scripts expect to be run from the root of this archive, and write into
`figures/output/`:

```sh
mkdir -p figures/output
Rscript figures/R/challenges.R
Rscript figures/R/regions_diag.R
Rscript figures/R/sim_survival.R
Rscript figures/R/CIS_survival.R
Rscript figures/R/surv_priors.R
Rscript figures/R/CIS_ntot.R
Rscript figures/R/demographics_table.R
```

They need R (4.2 or later) and these CRAN packages: `dplyr`, `ggplot2`,
`ggdist`, `tidybayes`, `tidyr`, `tibble`, `purrr`, `readr`, `stringr`,
`patchwork`, `latex2exp`, `glue`, `scales`, `rlang`, `here`, `mvtnorm`.

Three further packages, written for this work, are on GitHub:

```r
remotes::install_github("joshuablake/atacccDurationEstimates")
remotes::install_github("joshuablake/cisDurationModel")
remotes::install_github("joshuablake/cisSimulation")
```

- **atacccDurationEstimates** holds the ATACCC-based duration estimates that
  the manuscript uses as a prior. `CIS_survival.R` and `surv_priors.R` need it.
- **cisDurationModel** defines the statistical models, including the Stan code,
  used to fit both the simulated and the real data. This is the package to
  start from to apply the method to your own data.
- **cisSimulation** simulates a CIS-like study.

## What is here, and what is not

The Coronavirus Infection Survey (CIS) individual-level records cannot leave
the Office for National Statistics (ONS) Secure Research Service (SRS), so they
are not in this archive and cannot be redistributed. They are available to bona
fide researchers on application:
<https://www.ons.gov.uk/aboutus/whatwedo/statistics/requestingstatistics/secureresearchservice>.

What the archive does contain is the output of those in-SRS analyses, cleared
for release by the SRS, in the folders named `data/STATS*`. Starting from these
files, every figure and number in the paper can be reproduced outside the SRS.
The models that generated them are in the `cisDurationModel` package above, so
the analysis can be replicated on other data.

The simulation study can be reproduced in full from
<https://github.com/joshuablake/CIS_survival_analysis_sims>, which regenerates
`data/all_posteriors.rds` and `data/input_curves.rds`.

Two files are omitted here only because of their size: the posterior draws
`data/STATS17701/draws.rds` and `data/STATS18744/draws.rds` (about 64 MB each),
which `CIS_survival.R` reads to draw Figures 4 and 5. Download them from
<https://github.com/joshuablake/COVID-duration-paper> and drop them into those
folders to run that script. The summarised versions, `means.rds`, are included.

## Data sources and licensing

`data/STATS17701`, `data/STATS18744`, and `data/STATS22850` are statistical
data from ONS which is Crown Copyright. The use of the ONS statistical data in
this work does not imply the endorsement of the ONS in relation to the
interpretation or analysis of the statistical data. This work uses research
datasets which may not exactly reproduce National Statistics aggregates. The
analyses were carried out in the Secure Research Service, part of the Office
for National Statistics. `data/STATS17701/README.txt` records the SRS clearance
for release.

`data/pop-estimates-2020.csv` (ONS 2020 mid-year population estimates) and
`data/by-ethnicity-5-groups-table.csv` (2021 England and Wales census
ethnicity) are also from the Office for National Statistics, licensed under the
[Open Government Licence v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).
