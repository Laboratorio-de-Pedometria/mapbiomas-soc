# mapbiomas-solo



## Repository structure

The content of the repository is organized as follows:

* `data`: text files (CSV, TSV, TXT) with input and temporary data
* `doc`: markdown files (MD) documenting the data processing steps
* `res`: project results
  * `fig`: image files (PNG) with plots and maps
  * `tab`: text files (CSV, TSV, TXT) with tables, including the final, output data
* `src`: source code (R)
* `tmp`: other temporary files

## Source code

The source code is composed of R scripts that process the input data and generate the output data.
The input data is processed sequentially, with each script performing a specific task.
The scripts are organized as follows:

* [src/01a-process-febr-data-time.r](src/01a-process-febr-data-time.r)
* [src/01b-process-febr-data-rondonia.r](src/01b-process-febr-data-rondonia.r)
* [src/02-merge-external-data.r](src/02-merge-external-data.r)
* [src/03-prepare-soil-covars.r](src/03-prepare-soil-covars.r)
* [src/04-prepare-env-covars.r](src/04-prepare-env-covars.r)
* [src/05-estimate-soil-bd.r](src/05-estimate-soil-bd.r)
* [src/06-compute-carbon-stock.r](src/06-compute-carbon-stock.r)
* [src/07-create-figures.r](src/07-create-figures.r)
* [src/08-gee-trainning-data.r](src/08-gee-trainning-data.r)

## Output data

The output data (`pontos-estoque-cos`) is stored in the `res/tab` folder.
File names include the date of creation so that users can identify the most recent version.
