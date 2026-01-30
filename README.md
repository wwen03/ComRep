# ComRep - Comprehensiveness and Representativeness 

## Overview

This repo provides a set of R scripts for "Conservation planning for biodiversity of ice-free Antarctica: A focus on comprehensiveness and representativeness".

## Code Structure

```
ComRep/
├── Outputs/
├── Scripts/
│   ├── 01_Basic_Scenario_ComRep.R
│   ├── 02_Species_Range_Scenario_ComRep.R
│   ├── 03_Adaptive_Scenario_ComRep.R
│   ├── 04_Integrated_Scenario_ComRep.R
│   ├── 05_Sensitivity_Analysis_Targets.R
│   ├── 06_Sensitivity_Analysis_Budget.R
│   └── 07_Evaluating_Scenarios_Plots.R
└── ComRep.Rproj
└── LICENSE
└── README.md
```

## Usage

Run the scripts in numerical order for a complete analysis:

```r
# Run scenarios sequentially
source("Scripts/01_Basic_Scenario_ComRep.R")
source("Scripts/02_Species_Range_Scenario_ComRep.R")
source("Scripts/03_Adaptive_Scenario_ComRep.R")
source("Scripts/04_Integrated_Scenario_ComRep.R")

# Perform sensitivity analyses
source("Scripts/05_Sensitivity_Analysis_Targets.R")
source("Scripts/06_Sensitivity_Analysis_Budget.R")

# Evaluate scenarios, correlation test,  visualizations
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