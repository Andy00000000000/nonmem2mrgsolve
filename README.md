nonmem2mrgsolve
================

## Purpose

Provide an R package that automates NONMEM to mrgsolve translation to
reduce human errors and improve efficiency.

## Introduction

Pharmacometricians often utilize multiple software, with NONMEM (ICON
plc, Dublin, Ireland) widely regarded as the current gold standard for
population pharmacokinetic (PK) and pharmacokinetic-pharmacodynamic
(PK-PD) modeling. While NONMEM has the ability to simulate, it is often
more convenient and practical to perform simulations using free
open-source software such as R which can also perform post-processing of
the output. A popular pharmacometrics-orientated R package for solving
ODE systems is mrgsolve. Although mrgsolve has built-in functionality to
streamline translation, to the best of our knowledge a
previously-developed and publicly-available R package for fully
automated NONMEM to mrgsolve translation does not exist.

## Installation

If not already installed, install R. Then within R run the following
command:

``` r
devtools::install_github("Andy00000000000/nonmem2mrgsolve")
```

## Getting Started

As an orientation, assume that a theoretical NONMEM run has finished
estimating and has explicitly-defined ODEs. The ctl and ext files are
both named pbpk-101 and are located at C:/Documents/NONMEM/Project.

Load R and run:

``` r
setwd("C:/Documents/NONMEM/Project") # set the working directory to where the NONMEM files are located
library(nonmem2mrgsolve)             # load the nonmem2mrgsolve package
nonmem2mrgsolve("pbpk-101")          # run the nonmem2mrgsolve function with default settings
```

The resulting mrgsolve code will be wrote to
mrgsolve-code-V0\_pbpk-101.R within the C:/Documents/NONMEM/Project
folder. The user should then validate the translation, for which there
is an intuitive and established framework
[(Here)](https://mrgsolve.org/blog/posts/2023-update-validation.html).

## Usage Statement

``` r
nonmem2mrgsolve::nonmem2mrgsolve(
  filename = "nonmem-model.ctl", # String of the NONMEM model name with or without the .ctl extension
  dir = "./folder",              # String of the directory path to the NONMEM files (if not already given in the file name input or the working directory already set)
  sigdig = NULL,                 # Numeric of the number of significant digits to round non-fixed thetas and etas to; default NULL for no rounding
  write = T,                     # [T or F] for whether to write the resulting mrgsolve code to an R file
  return.orig = F,               # [T or F] for whether to return the original NONMEM ctl and ext (or cnv) files to the R environment
  out.filename = NULL,           # String of the name for the mrgsolve output file without the .R extension; NULL for default naming
  use.cnv = F                    # [T or F] for whether to use the NONMEM cnv file for parameter estimates instead of the ext file
)
```

## Getting Help

Please feel free to report problems using the [Issue
Tracker](https://github.com/Andy00000000000/nonmem2mrgsolve/issues) or
to reach out for help on the [Discussion
Board](https://github.com/Andy00000000000/nonmem2mrgsolve/discussions).

## Our Team

[Enhanced Pharmacodynamics, LLC (ePD)](https://www.epd-llc.com/) is a
contract research organization that assists clients with the design and
implementation of model-informed drug development strategies in a broad
range of therapeutic areas. The executive management team is led by
[Dr. Donald E. Mager](https://www.linkedin.com/in/don-mager-2652615/)
and [Dr. Scott A. Van
Wart](https://www.linkedin.com/in/scott-van-wart-2b6002a/).

[Finch Studio](https://finchstudio.io/) is an integrated modeling
environment for pharmacometricians, clinical pharmacologists, and other
team members to visualize, develop, and organize PK/PD models and data.
It includes a modern NONMEM code editor and workbench, interactive data
tables and visualizations, and a PK/PD model library. The development
team is led by [Dr. Mohamed
Ismail](https://www.linkedin.com/in/mohamed-ismail-bb54a367/).

nonmem2mrgsolve was developed at ePD by [Andrew
Santulli](https://www.linkedin.com/in/andrew-santulli-219034156/).

## Vignette

Three case study models are provided within the vignette folder. The
following section will use these examples to provide a brief tutorial on
how to implement nonmem2mrgsolve into the model development and
simulation workflow. To get started, download nonmem2mrgsolve and copy
the vignette folder to a convenient file location.

### Example 1: A 1-compartment model with first-order absorption

Within the vignette &gt;&gt; models directory, you should find three
folders:

<img src="readme_images/vignette_01.png" style="width:40.0%" />

This first example is drugx-oral-1cmt-101, in which you will find the
NONMEM control stream (.ctl) and output files (.ext and .lst). The
nonmem2mrgsolve package does not require the .lst file, it is provided
solely for user reference. The nonmem2mrgsolve-translated mrgsolve code,
along with the mrgsolve code following any necessary user modifications,
are provided as well for reference, but could be deleted at this step
and recreated during the vignette. The run\_nonmem2mrgsolve\_drugx.R
file contains all the code presented for this example of the vignette.

<img src="readme_images/vignette_02.png" style="width:40.0%" />

Now that the directory structure is clear, start R, load the
nonmem2mrgsolve package, and set the working directory to immediately
before entering the vignette folder:

``` r
library("nonmem2mrgsolve")
setwd("Your file path to vignette folder here")
```

Translating the NONMEM model into mrgsolve code is accomplished using a
single function call:

``` r
nonmem2mrgsolve(
  "mod1.ctl", # the NONMEM run name
  "./vignette/models/drugx-oral-1cmt-101/", # path to mod1.ctl and mod1.ext, from the working directory (which was set earlier)
  out.filename = "mrgsolve_code_drugx-oral", # name for the mrgsolve code .R output file
  sigdig = 3                                 # number of significant digits to report thetas and omegas to within the mrgsolve code (optional)
)
```

During the translation, the final parameter estimates (thetas and
omegas) are obtained from the NONMEM .ext file, while the parameter and
differential equations are pulled from the NONMEM .ctl file. There are
several optional inputs to the nonmem2mrgsolve function, as detailed in
the [usage statement](#link_1).

When the translation is finished, the mrgsolve code will print to the R
console and will also be written to a .R file:

<img src="readme_images/vignette_03.png" style="width:100.0%" />
<img src="readme_images/vignette_04.png" style="width:40.0%" />

Now is a good time to check whether the nonmem2mrgsolve-translated
mrgsolve code requires any user modifications in order to compile:

``` r
source("./vignettes/models/drugx-oral-1cmt-101/mrgsolve_code_drugx-oral.R")
mrgsolve::mcode("Test_Unmodified_Translation",code)
```

In this example manual user changes to the mrgsolve code are not needed,
since the model successfully compiled:

<img src="readme_images/vignette_05.png" style="width:50.0%" />

If certain model complexities are present (such as time-varying
covariates), the nonmem2mrgsolve-translated mrgsolve code may fail to
compile into an mrgsolve model object. The compilation failure will
provide informative error messages for locating the line of code that
needs attention. The compilation failure will also prevent the user from
running simulations without first addressing problems in the code.

As a final step before conducting simulations, the user should validate
the mrgsolve model. There is an intuitive and established framework
[(Here)](https://mrgsolve.org/blog/posts/2023-update-validation.html).
Model validation remains the responsibility of the user and should
always be performed, regardless of the robustness of the translation
software or the experience of the user.

### Example 2: A QSS Target-Mediated Drug Disposition (TMDD) Model and Indirect Response PK-PD Model

This example is located within the evolocumab-tmdd-qss-pkpd-101 folder
of the vignette &gt;&gt; models directory. Since the steps for
translating the NONMEM model into mrgsolve code are equivalent to those
presented in [Example 1](#link_2), the tutorial for this case study will
start at compiling the nonmem2mrgsolve-translated mrgsolve code into an
mrgsolve model object. The complete code for running the earlier steps
as well is included within the run\_nonmem2mrgsolve\_evolocumab.R file.

Attempting to compile the nonmem2mrgsolve-translated mrgsolve code
results in a fatal error:

``` r
source("./vignette/models/evolocumab-tmdd-qss-pkpd-101/mrgsolve_code_evolocumab-tmdd.R")
mrgsolve::mcode("Test_Unmodified_Translation",code)
```

<img src="readme_images/vignette_06.png" style="width:100.0%" />

From the error message it is clear that the issue occurred when the
NONMEM dataset column “TIME” was being used in the differential equation
code block ($DES). Mrgsolve syntax requires this to be labeled as
“SOLVERTIME”.

The user can manually make this single change while leaving the rest of
the nonmem2mrgsolve code unmodified:

<img src="readme_images/vignette_07.png" style="width:100.0%" />

Compiling the corrected mrgsolve code is successful:

``` r
source("./vignette/models/evolocumab-tmdd-qss-pkpd-101/mrgsolve_code_evolocumab-tmdd_user-modified.R")
mrgsolve::mcode("Test_Modified_Translation",code)
```

<img src="readme_images/vignette_08.png" style="width:50.0%" />

### Example 3: A Whole-body Physiologically-based Pharmacokinetic (PBPK) Model

This example is located within the mavoglurant-pbpk-101 folder of the
vignette &gt;&gt; models directory. Since the steps for translating the
NONMEM model into mrgsolve code are equivalent to those presented in
[Example 1](#link_2), the tutorial for this case study will start at
compiling the nonmem2mrgsolve-translated mrgsolve code into an mrgsolve
model object. The complete code for running the earlier steps as well is
included within the run\_nonmem2mrgsolve\_mavoglurant.R file.

Attempting to compile the nonmem2mrgsolve-translated mrgsolve code
results in a fatal error:

``` r
source("./vignette/models/mavoglurant-pbpk-101/mrgsolve_code_mavoglurant-pbpk.R")
mrgsolve::mcode("Test_Unmodified_Translation",code)
```

<img src="readme_images/vignette_09.png" style="width:80.0%" />

From the error message it is clear that the issue occurred because
“RATE” was not declared in the parameter block ($PARAM). Additionally,
this should be renamed since “RATE” is a reserved term.

The user can manually make these two changes while leaving the rest of
the nonmem2mrgsolve code unmodified:

<img src="readme_images/vignette_10.png" style="width:100.0%" />
<img src="readme_images/vignette_11.png" style="width:100.0%" />

Compiling the corrected mrgsolve code is successful:

``` r
source("./vignette/models/mavoglurant-pbpk-101/mrgsolve_code_mavoglurant-pbpk_user-modified.R")
mrgsolve::mcode("Test_Modified_Translation",code)
```

<img src="readme_images/vignette_12.png" style="width:50.0%" />
