\docType{data}
\name{Data}
\alias{TempIst}
\alias{TempCop}
\alias{temperature_turkey}
\alias{temperature_denmark}
\title{Temperature Datasets}
\usage{
TempIst
TempCop
temperature_turkey
temperature_denmark
}
\description{
There are two kinds of datas and four kinds of datasets in this package.
The first two datasets are called TempCop and TempIst and they show some mean temperatures for every day in 2015 in Copenhagen and Istanbul.
}

}
\format{
TempIst and TempCop are dataframes with 31 cases(rows) and 12 variables(columns). Rows are the days of a month and the columns are names of the months of a year.
temperature_denmark and temperature_turkey are also dataframes with 12 cases(rows) and 25 variables(columns). Rows are names of the months of a year and columns are the years between 1991-2015.
}
\References{
The temperature_denmark and temperature_turkey data was used from this website
http://sdwebx.worldbank.org/climateportal/index.cfm
TempCop and TempIst data are used from
https://www.holiday-weather.com/country/turkey/
}
