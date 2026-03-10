install.packages("readxl")
install.packages("tidyverse")
library(readxl)
library(tidyverse)

datos <- read_excel("Web_Analytics.xls")


View(datos)
head(datos)