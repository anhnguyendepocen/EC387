library(XML)
# from https://sdw-wsrest.ecb.europa.eu/
doc <- xmlParse("https://sdw-wsrest.ecb.europa.eu/service/datastructure/ECB/ECB_EXR1")
da <- xmlToDataFrame(url)
  