OMMProcessing
================

**OMMProcessing** is a repository containing two Shiny applications that
support Oral Minimal Model (OMM) workflows using the **SAMM** software.

The repository is designed to support a practical end-to-end pipeline:

1.  Start with a study dataset containing OGTT-style glucose, insulin,
    and C-peptide values
2.  Generate one SAMM input file per observation (participant x visit)
3.  Run SAMM externally (batch processing)
4.  Convert SAMM’s text-based output into clean, analysis-ready results
    tables

> **Development status**  
> Both apps are under active development. Core functionality is
> implemented, but the user interface and model specification are still
> evolving.

------------------------------------------------------------------------

## How to run the apps

### 1) Clone or download the repository

Either clone using Git:

    git clone https://github.com/<alicelouisejane>/OMMProcessing.git

Or download the repository as a ZIP from GitHub and unzip it.

### 2) Install required R packages

    install.packages(c(
      "shiny", "shinyFiles", "shinyjs", "DT",
      "rio", "dplyr", "tidyr", "stringr", "stringi",
      "zip", "ggplot2", "gtsummary", "gt"
    ))

### 3) Run an app

Make sure your working directory is set to the repository root. From the
repository root, run:

#### Create SAMM input files app

    shiny::runApp("create_SAMM_files_app/app")

#### Extract SAMM results app

    shiny::runApp("extract_SAMM_results_app/app")

## 

App 1: Create SAMM input files

### Overview

The Create SAMM Input Files Shiny application converts a single uploaded
trial/cohort dataset (`.csv` or `.xlsx`) into SAMM-ready input files,
generating one file per observation for batch execution in SAMM. This
uploaded file is expected to be in a certain format and will have
undergone preprocessing checks before uploading. See Required input data
structure section for overview of data structure.

This tool is intended to reduce manual formatting and support
reproducible, scalable SAMM workflows for large clinical trial and
cohort datasets.

### Required input data structure

The uploaded dataset can contain as many rows (observations) as require
but it must include the following variables using standard nomenclature:

- `id`
- `age_at_visit`
- `bmi`
- `weight`

OGTT variables must follow the naming conventions below:

- `glu*`: Glucose
- `cpep*`: C-peptide
- `ins*`: Insulin

Where:

- `m` indicates a negative timepoint (e.g., `m10` = -10 minutes)
- numeric suffixes indicate OGTT sampling timepoints in minutes (e.g.,
  `0`, `30`, `60`, `90`, `120`)

For example, valid column names include:

- `glum10`, `glu0`, `glu30`, `glu60`, `glu90`, `glu120`
- `insm10`, `ins0`, `ins30`, `ins60`, `ins90`, `ins120`
- `cpepm10`, `cpep0`, `cpep30`, `cpep60`, `cpep90`, `cpep120`

An example input dataset is provided in this repository:

- `test_data.csv`

<!-- -->

    ## Warning in attr(x, "align"): 'xfun::attr()' is deprecated.
    ## Use 'xfun::attr2()' instead.
    ## See help("Deprecated")

    ## Warning in attr(x, "format"): 'xfun::attr()' is deprecated.
    ## Use 'xfun::attr2()' instead.
    ## See help("Deprecated")

| id                | age_at_visit |      bmi | weight | cpepm10 | cpep0 | cpep30 | cpep60 | cpep90 | cpep120 | insm10 |  ins0 |  ins30 |  ins60 |  ins90 | ins120 | glum10 | glu0 | glu30 | glu60 | glu90 | glu120 |
|:------------------|-------------:|---------:|-------:|--------:|------:|-------:|-------:|-------:|--------:|-------:|------:|-------:|-------:|-------:|-------:|-------:|-----:|------:|------:|------:|-------:|
| 202830_2023-09-01 |            3 | 14.61027 |   16.2 |    0.70 |  0.64 |   3.34 |   4.59 |   3.64 |    3.25 |   3.69 |  2.93 |  30.87 |  26.04 |  22.20 |  12.25 |     86 |   83 |   157 |   130 |   110 |    102 |
| 203300_2023-10-10 |           12 | 21.31113 |   51.2 |    3.20 |  3.03 |  11.27 |  12.68 |  13.09 |   10.68 |  17.59 | 14.71 | 168.89 | 129.50 | 132.41 |  98.57 |    107 |  104 |   159 |   153 |   136 |    115 |
| 204130_2024-04-23 |           13 | 17.07925 |   40.4 |    1.60 |  1.46 |   6.08 |   7.21 |   6.93 |    6.79 |   5.43 |  5.40 |  73.92 |  55.53 |  49.68 |  46.74 |     82 |   83 |   142 |   127 |   117 |    117 |
| 210509_2024-03-13 |           11 | 19.82172 |   48.3 |    1.13 |  1.14 |   5.53 |   3.97 |   3.42 |    2.42 |   7.57 |  8.19 |  92.25 |  20.08 |  28.53 |  14.60 |     89 |   89 |   139 |    89 |    97 |     86 |
| 216332_2024-03-27 |            5 | 15.10435 |   20.5 |    0.35 |  0.29 |   1.22 |   1.35 |   1.27 |    1.20 |   1.53 |  0.87 |  10.56 |   6.87 |   4.23 |   3.81 |     66 |   64 |   102 |    98 |    95 |     92 |
| 218315_2023-12-15 |           37 | 22.09593 |   68.6 |    1.34 |  1.33 |   2.75 |   3.63 |   4.32 |    3.71 |   2.32 |  2.38 |  10.78 |  12.76 |  14.09 |   9.50 |    108 |  107 |   203 |   263 |   267 |    186 |

### Mock up interface

<figure>
<img src="docs/extract_samm_app_demo.gif"
alt="Extract SAMM results app demo" />
<figcaption aria-hidden="true">Extract SAMM results app
demo</figcaption>
</figure>

#### Model

Choose which SAMM model input files to generate:

- **glucose**: generates SAMM input files for the glucose minimal model
  (used to estimate insulin sensitivity).

- **cpeptide**: generates SAMM input files for the C-peptide minimal
  model (used to estimate beta-cell responsiveness / Phi parameters).

In most workflows, both models are run separately in SAMM, so users
typically generate both sets of input files.

#### Choose CSV file

Upload the cleaned trial/cohort dataset containing the required
anthropometrics and OGTT variables.

- Supported format: `.csv`

The uploaded dataset is displayed in the main panel for review.

#### Baseline fasted timepoint

Indicate whether your OGTT includes a fasted baseline sample collected
**before** the 0-minute timepoint (e.g., a -10 minute sample).

- **No**: assumes the earliest OGTT timepoint is 0 minutes

- **Yes**: enables entry of a negative timepoint (e.g., `-10`)

This option affects how fasting values are derived and how the
`STARTDATA` and `CONST` sections are populated in the SAMM input file.
**Info button:**explains what a baseline fasted timepoint means and when
it should be used.

#### Number of points

Choose the OGTT sampling schedule used in your dataset.

- **5 points**: uses the default timepoints  
  `0, 30, 60, 90, 120`

- **7 points**: uses the default timepoints

  `0, 10, 20, 30, 60, 90, 120`

- **5 (custom)** or **7 (custom)**: allows the user to define their own
  OGTT timepoints.

**Info button:** describes the default schedules and how custom
schedules work.

#### Custom timepoints (only for custom schedules)

If a custom schedule is selected, users must enter timepoints as a
comma-separated list

Requirements:

- Must contain exactly 5 or 7 timepoints (depending on selection) ie.
  `0, 15, 30, 60, 90`

- The first timepoint must be `0`

- Values must be numeric

A “Submit Timepoints” button becomes enabled once the custom schedule is
valid.

**Info button:** describes the formatting requirements and validation
rules.

#### Model-specific settings

**If model = glucose the app displays glucose-model specific settings:**

- **FSD** (default `0.02`)  
  The fractional standard deviation used in the SAMM data weighting
  specification.

- **Glucose protocol** (default `1`)

  Used to specify the glucose dose protocol (e.g., standard 75g OGTT).

**If model = cpeptide the app displays C-peptide-model specific
settings:**

- **GEN1** (default `2000`)

- **GEN2** (default `0.001`)

- **GEN3** (default `2`)These parameters control model settings for the
  C-peptide/Phi model template.

Once all required inputs are provided and valid, the **Create SAMM
Files** button becomes enabled.

After successful generation, a **Download SAMM Files** button appears.

This downloads a `.zip` containing all generated SAMM input files for
the selected model type.

### Example created SAMM file output

A SAMM file is a plain-text model specification that combines:

- the **model structure** and solver settings
- the **parameter definitions** (including bounds and initial values)
- the **observation-level OGTT data**
- the observation-specific **constants** required by the model

Current implementation outputs a `.txt` file which would then be
converted to the appropriate file type for SAMM.

**See `docs/example_samm_input.txt` for a complete example of the SAMM
output file structure**

In the example file:

- The `MODEL / COMPDEF / COMPART / XFER / LOSS` blocks define the
  compartmental glucose model and its governing equations.
- The `PARAM` blocks define parameters to be estimated (e.g., `SI`, `T`,
  `p1`), including starting values and bounds.
- The `STARTDATA ... DATA ... END` section contains the
  observation-level OGTT data, where each row is a timepoint and each
  column is a measured variable (e.g., glucose `G` and insulin `I`).
- The `CONST` lines at the bottom provide observation-specific constants
  such as:
  - `BW` = body weight
  - `Gss` = fasting glucose steady-state value
  - `Iss` = fasting insulin steady-state value
  - `D` = dose term (derived from body weight)

The Create SAMM Input Files app automates generation of these files by
inserting the observation-specific `STARTDATA` and `CONST` sections for
each row of an uploaded dataset.

> **Note:** The SAMM template and model specification used by this app
> are subject to change as the tool is developed (e.g., to support
> additional sampling schedules and model variants).

## App 2: Extract SAMM output

### Overview

SAMM generates model results as long, text-based output files for each
observation.  
These output files typically include separate model runs for the
**glucose model and** the **C-peptide model**

While these outputs contain all required information (parameter
estimates, convergence information, fit diagnostics), the format is not
directly usable for downstream statistical analysis, reporting, or
merging back into a trial/cohort dataset.

This app provides an interface to convert SAMM output text files into
clean, analysis-ready tables.

### Example from the SAMM output before processing

The app expects a separate raw (`.csv`) file output from the C-peptide
model run and/or from the glucose model run, combining the processed
results where both are present. All IDs ran in the SAMM batch will be
output within this one `.csv` file. The verbose output from the
C-peptide model for one ID file is below:

<figure>
<img src="docs/raw_SAMM_output.png" alt="Verbose raw SAMM output" />
<figcaption aria-hidden="true">Verbose raw SAMM output</figcaption>
</figure>

### Mock up interface

Users upload one or both raw SAMM outputs:

- **Phi results** (C-peptide model output)

- **SI results** (glucose model output)

After clicking **Process Data**, the app extracts key model outputs into
a clean table (one row per observation) captures SAMM warning/error
messages and generates QC flags as explained in detail in the next
section.

Tabs across the top provides a quick summary of model failures, skipped
runs, and zero-valued parameters and generates basic visualisations of
parameter distributions.

A **Download Cleaned Results** button appears after processing, allowing
export of the cleaned dataset.

<figure>
<img src="docs/create_samm_app_demo.gif"
alt="Create SAMM input files app demo" />
<figcaption aria-hidden="true">Create SAMM input files app
demo</figcaption>
</figure>

### Example outputted analysis-ready table

The app produces a single combined results table (`.csv`) containing one
row per observation (`id`).  
This table is designed to be merged back into the original trial/cohort
dataset using `id`.

| id                | phi_has_parameter | si_has_parameter | phi_is_skipped | si_is_skipped | phib_is_zero | phid_is_zero | phis_is_zero | phitotOB_is_zero | SI2_is_zero | phi_message | si_message | phib_value | phib_sd | phib_cv | phib_ci_lower | phib_ci_upper | phid_value |  phid_sd |  phid_cv | phid_ci_lower | phid_ci_upper | phis_value |  phis_sd |  phis_cv | phis_ci_lower | phis_ci_upper | phitotOB_value | phitotOB_sd | phitotOB_cv | phitotOB_ci_lower | phitotOB_ci_upper |  SI2_GE | SI2_value |   SI2_sd |  SI2_cv | SI2_ci_lower | SI2_ci_upper |
|:------------------|:------------------|:-----------------|:---------------|:--------------|:-------------|:-------------|:-------------|:-----------------|:------------|:------------|:-----------|-----------:|--------:|--------:|--------------:|--------------:|-----------:|---------:|---------:|--------------:|--------------:|-----------:|---------:|---------:|--------------:|--------------:|---------------:|------------:|------------:|------------------:|------------------:|--------:|----------:|---------:|--------:|-------------:|-------------:|
| 202830_2023-09-01 | TRUE              | TRUE             | FALSE          | FALSE         | FALSE        | FALSE        | FALSE        | FALSE            | FALSE       | NA          | NA         |    3.68992 |       0 |       0 |       3.68992 |       3.68992 |   224.0558 |  59.0156 | 26.33970 |      36.24152 |      411.8701 |   35.86120 | 1.908850 |  5.32289 |      29.78637 |      41.93602 |       39.97098 |    2.419410 |     6.05291 |          32.27134 |          47.67062 | 0.02538 |  75.81328 | 10.93150 | 14.4189 |    -63.08421 |     214.7108 |
| 203300_2023-10-10 | TRUE              | TRUE             | FALSE          | FALSE         | FALSE        | FALSE        | FALSE        | FALSE            | FALSE       | NA          | NA         |    4.52995 |       0 |       0 |       4.52995 |       4.52995 |   141.9228 |  31.4657 | 22.17100 |      41.78480 |      242.0608 |    1.73721 | 0.425855 | 24.51380 |       0.38195 |       3.09247 |        3.48175 |    0.358394 |    10.29350 |           2.34118 |           4.62232 | 0.02538 |  82.26501 | 11.27600 | 13.7069 |    -61.01030 |     225.5403 |
| 204130_2024-04-23 | TRUE              | TRUE             | FALSE          | FALSE         | FALSE        | FALSE        | FALSE        | FALSE            | FALSE       | NA          | NA         |    7.74895 |       0 |       0 |       7.74895 |       7.74895 |  1074.7279 |  80.4944 |  7.48975 |     818.55852 |     1330.8973 |   42.89934 | 5.007190 | 11.67190 |      26.96421 |      58.83446 |       59.21152 |    6.071380 |    10.25370 |          39.88966 |          78.53338 | 0.02538 |  45.07580 |  6.26298 | 13.8943 |    -34.50289 |     124.6545 |
| 210509_2024-03-13 | TRUE              | TRUE             | FALSE          | FALSE         | FALSE        | FALSE        | FALSE        | FALSE            | FALSE       | NA          | NA         |    6.74456 |       0 |       0 |       6.74456 |       6.74456 |   623.2160 | 124.7670 | 20.01990 |     226.14983 |     1020.2822 |   27.96498 | 1.909020 |  6.82648 |      21.88961 |      34.04036 |       35.70477 |    2.263460 |     6.33939 |          28.50141 |          42.90813 | 0.02538 |  46.17331 |  6.63206 | 14.3634 |    -38.09497 |     130.4416 |
| 216332_2024-03-27 | TRUE              | TRUE             | FALSE          | FALSE         | FALSE        | FALSE        | FALSE        | FALSE            | FALSE       | NA          | NA         |    3.32714 |       0 |       0 |       3.32714 |       3.32714 |   600.1751 | 104.1910 | 17.36010 |     268.59307 |      931.7572 |   10.88228 | 3.130380 | 28.76590 |       0.91999 |      20.84456 |       19.88120 |    2.935270 |    14.76410 |          10.53983 |          29.22256 | 0.02538 | 178.87271 | 25.12220 | 14.0447 |   -140.33443 |     498.0799 |
| 218315_2023-12-15 | TRUE              | TRUE             | FALSE          | FALSE         | FALSE        | FALSE        | FALSE        | FALSE            | FALSE       | NA          | NA         |    3.31641 |       0 |       0 |       3.31641 |       3.31641 |   454.7736 |  76.5203 | 16.82600 |     211.25152 |      698.2957 |   26.57369 | 2.063320 |  7.76452 |      20.00728 |      33.14011 |       34.60003 |    2.760310 |     7.97778 |          25.81547 |          43.38459 | 0.02538 | 139.46815 | 19.57010 | 14.0319 |   -109.19293 |     388.1292 |

#### Model run status variables

The output includes variables indicating whether model estimation was
attempted and whether it succeeded:

- `phi_has_parameter`  
  Indicates whether the model successfully produced Phi parameter
  estimates.

- `si_has_parameter`  
  Indicates whether the model successfully produced insulin sensitivity
  (SI) estimates.

- `phi_is_skipped`, `si_is_skipped`  
  Indicates whether estimation was intentionally skipped (e.g., due to
  insufficient data or pre-specified model rules).

In downstream analyses, it can be useful to distinguish **skipped** runs
from runs where estimation was attempted but failed. For example:

- *Attempted Phi estimate but failed*  
  (`phi_has_parameter == FALSE` and `phi_is_skipped == FALSE`)

- *Attempted SI estimate but failed*  
  (`si_has_parameter == FALSE` and `si_is_skipped == FALSE`)

#### Zero-value flags (convergence / boundary issues)

SAMM outputs may contain cases where parameters are set to zero due to
convergence problems or boundary constraints.  
The extraction output includes flags for these cases, such as:

- `phib_is_zero` (Phi basal set to 0)
- `phid_is_zero` (Phi dynamic set to 0)
- `phis_is_zero` (Phi static set to 0)
- `phitotOB_is_zero` (Phi total set to 0)
- `SI2_is_zero` (SI set to 0)

These flags allow users to quantify and report the frequency of
optimisation failures or boundary solutions.

#### Parameter estimates and uncertainty

For each extracted parameter, the output includes:

- estimated value (`*_value`)
- standard deviation (`*_sd`)
- coefficient of variation (`*_cv`)
- confidence interval bounds (`*_ci_lower`, `*_ci_upper`)

Example extracted parameters include:

- `phib` (basal beta-cell responsiveness)
- `phid` (dynamic beta-cell responsiveness)
- `phis` (static beta-cell responsiveness)
- `phitotOB` (overall beta-cell responsiveness)
- `SI2` (insulin sensitivity)

#### Warning/Error Messages

SAMM outputs commonly contain warnings and errors that indicate
optimisation failure, boundary solutions, or unreliable uncertainty
estimates. This app preserves the raw message text in `phi_message`
(C-peptide / Phi model) and `si_message` (glucose / SI model) in
addition to structured flags so that failure rates and boundary
solutions can be summarised across large datasets.

These messages are generated by SAMM and may vary depending on model
settings, solver options, and sampling schedules.

#### Phi model (C-peptide) messages

![](docs/phi_messages.png)

#### SI model (glucose) messages

![](docs/si_messages.png)
