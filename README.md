# Impact of COVID-19 Restrictions on Book Consumption
This repository presents additonal material related to my master thesis [The Impact of COVID-19 Restrictions on Book Consumption](https://github.com/Mikeverweij96/Influence-of-COVID-19-on-Bookreading-behaviour/blob/48ec56e474920f0f5635807bd9ae9926960f8fc2/Verweij%20(2022).pdf). Specificically, this repository contains the workflow for data preperation and analysis used for my thesis. 

In my thesis, I have investigated how COVID-19 restrictions have affected the amount people read, consumers’ reading speed, evaluation of books and types of books read and how these effects vary across age groups, genders, types of readers and nationalities. The expected relationships that have been investigated are shown below:

![image](https://user-images.githubusercontent.com/90783740/172004153-8f8bd9f6-b5d4-46be-b173-8e4a027c8ce9.png)

##Data Description
To investigate the impact of COVID-19 restrictions on book consumption, we use data scraped from the reading community website Goodreads. We collected 18,252,877 book reading records from 112,087 unique Goodreads users that were found via the 31 largest country-specific subgroups on Goodreads. Our dataset covers the consumption of books over a 15-year timeframe, including almost two years after the outbreak of COVID-19. 


## Repository overview

```
├── README.md
├── makefile
└── src
    ├── analysis
    ├── data-collection
    ├── data-preparation
    └── pricing-app
```

## Dependencies
Please follow the installation guide on http://tilburgsciencehub.com/.

Please follow the installation guide on http://tilburgsciencehub.com/.

- Python. [Installation guide](https://tilburgsciencehub.com/building-blocks/configure-your-computer/statistics-and-computation/python/).
- R. [Installation guide](https://tilburgsciencehub.com/building-blocks/configure-your-computer/statistics-and-computation/r/).
- Make. [Installation guide](https://tilburgsciencehub.com/building-blocks/configure-your-computer/automation-and-workflows/make/).

- For Python, make sure you have installed below packages:
```
pip install bs4
pip install selenium
```

- For R, make sure you have installed below packages:
```
install.packages("tidyverse")
install.packages("ggfortify")
install.packages("yaml")
install.packeges("shiny")
install.packages("googledrive")
install.packages("tidypredict")
install.packages("car")
install.packages("base")
install.packages("data.table")
install.packages("broom")
install.packages("haven")
install.packages("readxl")
```

### Running the code
Follow below instructions to run the code:
1. Fork this repository
2. Open your command line/terminal:

```
git clone https://github.com/[your username]/Airbnb-pricing.git
```
3. To directly run all code use the following command in your directory "airbnb-pricing":

```
make
```
4. After running all code a http link is generated. Copy paste this link in your browser to launch the app.
    Note: do not close/terminate the command line/terminal before you are finished with using the app. When the command line/terminal is closed and/or stops running, the website will not be available anymore.

Another option will be to run all code in the following order:
- ../src/data-preparation -> data_preparation.R
- ../src/analysis -> regression.R
- ../src/pricing-app -> pricing_app.R


#### Running the data collection and amenities matrix preparation
Note: Above worflow does not include the data collection steps and the preparation of the amenities matrix. Running these steps typically takes around 20 hours in total. Therefore, the output files of these two steps are stored on our shared google drive folder, from which they are downloaded in the data_preparation.R file. If one is interested in running these steps too, one should run below files in the following order:
- ../src/data-collection -> Inside_Airbnb_link_scraper.py
- ../src/data-collection -> data_download.R
- ../src/data-preparation -> ammenities_matrix.R

## Author

[Mike Verweij](https://github.com/Mikeverweij96)
