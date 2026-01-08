# Kandidatarbete
This repository contains code and data for my bachelors thesis titled: "Laven Caloplaca Exsecuta utgör ett komplex av tre arter, varav två kryptiska". Please read the thesis before further exploring this repository. Below is a description for each folder and some relevant files that exist here.

## Data
This folder contains data gathered for the examined specimens. This includes measurements and observations for morphological and anatomical characteristics but no genetic data. Data is given in the `xlsx` or `csv` formats.

## Protocol
This folder contains scanned copies of the protocols which were filled out by hand when examining each specimen.

## main
This folder contains code for all analysis, including PCA, various statistial tests and geographic distribution. All code here assumes the availability of data from `/data/caloplaca_exsecuta.csv` and `/data/spores.csv`

### initial_pca.r
Creates a biplot showing results from an initial PCA aswell as a textfile which includes contributions from each variable. 

### secondary_pca.r
Creates a biplot showing results from an secondary PCA aswell as a textfile which includes contributions from each variable.

### hypothesis_tests.r
Runs various statistical tests comparing differences in characters between clades.

### character_graphs.r
Creates graphs for characters which are discussed in the thesis.

### map.html
Generates a map showing the point at which specimens that were determined to belong to a clade were collected. It can be opened in any browser.

## Functions
Contains abstractions to be used in the main folder, includes things such as generating graphs. 

## config.r
Contains various configuration options such as characters to be used in PCA or default output folders. By default, running code that generates files will output to the following paths: `output`,
`output/initial_pca`, 
`output/hypothesis_tests`,
`output/secondary_pca`,
`output/character_graphs`,
