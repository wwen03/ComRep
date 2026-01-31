# ComRep - Comprehensiveness and Representativeness 

## Overview

This repo provides a set of R scripts for "Conservation planning for biodiversity of ice-free Antarctica: A focus on comprehensiveness and representativeness".

## Code Structure

```
ComRep/
├── Outputs/
├── Scripts/
│   ├── 01_Data_Preparation_AH_SpeciesRange.R
│   ├── 02_Access_Cost_Function.R
│   ├── 03_Sensivity_Analysis_Budget.R
│   ├── 04_Sensivity_Analysis_Target.R
│   ├── 05_Targets_Analysis_Comparison.R
│   ├── 06_Costs_Analysis_Comparison.R
│   └── 07_Evaluating_Candidate_ProtectedAreas.R
└── ComRep.Rproj
└── LICENSE
└── README.md
```

## Usage

Run the scripts in numerical order for a complete analysis:

```r
# Data preparation
source("Scripts/01_Data_Preparation_AH_SpeciesRange.R")
source("Scripts/02_Access_Cost_Function.R")

# Perform sensitivity analyses
source("Scripts/03_Sensitivity_Analysis_Budget.R")
source("Scripts/04_Sensitivity_Analysis_Target.R")

# Comparison analyses
source("Scripts/05_Targets_Analysis_Comparison.R")
source("Scripts/06_Costs_Analysis_Comparison.R")

# Evaluate candidate protected areas, correlation test,  visualizations
source("Scripts/07_Evaluating_Scenarios_Plots.R")
```

## Requirements

- R (version 4.0 or higher recommended)
- Required R packages (install as needed):

## Installation

```bash
# Clone the repository
git clone https://github.com/wwen03/ComRep.git

# Navigate to the project directory
cd ComRep
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## Contact

For questions or feedback, please open an issue on GitHub.