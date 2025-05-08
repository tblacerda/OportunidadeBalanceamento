# Balanceamento_Trafego

This repository contains R scripts and resources for analyzing and processing data related to cellular network traffic balancing. Below is a summary of the main scripts in this project:

## Scripts

### `teste.R`
This script is a test module for processing data related to cellular networks. It includes:
- Importing data from text files.
- Filtering specific cells for analysis.
- Checking consistency between datasets.

### `metis_analise_subutilizacao.R`
This script analyzes underutilized cells in a specific region. It:
- Processes data to identify cells with low utilization.
- Exports the results to an Excel file for further analysis.

### `modulo_tratamento_nextim.R`
This module handles inconsistencies, duplicates, and missing data in cellular network datasets. It:
- Processes data related to azimuths, frequencies, and bandwidths.
- Cleans and deduplicates records.
- Exports cleaned data and duplicate records to an Excel file.

## Project Structure
- **Inputs/**: Contains input data files such as Excel sheets and CSV files.
- **Outputs/**: Stores processed data and results, including Excel files.
- **Scripts/**: Contains additional R scripts for specific tasks.
- **PYTHON_CODE/**: Includes Python scripts and Jupyter notebooks for supplementary analysis.

## Requirements
- R (with the following libraries):
  - `dplyr`
  - `stringr`
  - `readxl`
  - `writexl`
  - `reticulate`
- Python (for integration with R using `reticulate`).

## Usage
1. Clone the repository.
2. Set up the required R and Python environments.
3. Run the scripts as needed for data analysis and processing.

## License
This project is licensed under the MIT License.