# Annual mapping of soil organic carbon stock in Brazil 1985-present. Training field soil data

## Introduction

Mapping soil organic carbon (SOC) stock is crucial for understanding and managing the carbon cycle, a key component of climate regulation. SOC plays a significant role in maintaining soil health, enhancing agricultural productivity, and supporting biodiversity. Monitoring SOC stocks allows researchers and policymakers to assess the impacts of land-use changes, agricultural practices, and conservation efforts on soil carbon sequestration.

This repository is part of an ongoing effort by the MapBiomas Soil Working Group to monitor and analyze changes in SOC stocks across Brazil. It contains the source code used to standardize and harmonize field soil data from various sources, especially the SoilData repository, which are collated together. This data is then used to train machine learning algorithms and environmental covariates that produce the series of annual maps of soil organic carbon stock in Brazil from 1985 to the present.

## Installation and usage

### Repository Structure

The content of the repository is organized as follows:

* `data`: Text files (CSV, TSV, TXT) with input and temporary data.
* `doc`: Markdown files (MD) documenting the data processing steps.
* `res`: Project results.
  * `fig`: Image files (PNG) with plots and maps.
  * `tab`: Text files (CSV, TSV, TXT) with tables, including the final output data.
* `src`: Source code (R).
* `tmp`: Other temporary files.

### Requirements

To run the scripts in this repository, you'll need R and the following R packages:

* `caret`
* `data.table`
* `geobrfebr`
* `ranger`
* `remotes`
* `rgee`
* `rnaturalearth`
* `sf`

### Usage

The source code is composed of R scripts that process the input data and generate the output data.
The input data is processed sequentially, with each script performing a specific task.
The scripts are organized as follows:

1. [src/01a-process-febr-data-time.r](src/01a-process-febr-data-time.r)
2. [src/01b-process-febr-data-rondonia.r](src/01b-process-febr-data-rondonia.r)
3. [src/02-merge-external-data.r](src/02-merge-external-data.r)
4. [src/03-prepare-soil-covars.r](src/03-prepare-soil-covars.r)
5. [src/04-prepare-env-covars.r](src/04-prepare-env-covars.r)
6. [src/05-estimate-soil-bd.r](src/05-estimate-soil-bd.r)
7. [src/06-compute-carbon-stock.r](src/06-compute-carbon-stock.r)
8. [src/07-create-figures.r](src/07-create-figures.r)
9. [src/08-gee-trainning-data.r](src/08-gee-trainning-data.r)

To process the training field soil data, run the scripts in the `src` directory in sequential order.
For example:

```bash
Rscript src/01a-process-febr-data-time.r
```

### Output Data

The output data, `pontos-estoque-cos`, is stored in the `res/tab` folder.
File names include the date of creation, allowing users to identify the most recent version.
The dataset used to produce the annual maps of soil organic carbon stock in Brazil is available in
the [SoilData repository](https://soildata.mapbiomas.org/).

## Contributing

We welcome contributions to this project. If you have suggestions for improvements or bug fixes, please open an issue or submit a pull request.

<!-- ## License -->

<!-- This project is licensed under the MIT License - see the LICENSE.md file for details. -->

## Contact

For questions or support, please contact us at https://brasil.mapbiomas.org/contato/.
