library(leaflet)
library(tidyverse)
library(ows4R)

csw_mercator <- 'https://resources.marine.copernicus.eu/?option=com_csw&view=details&tab=info&product_id=GLOBAL_ANALYSIS_FORECAST_PHY_001_024&format=xml'

csw <- CSWClient$new(csw_mercator, "2.0.2", logger = "DEBUG")

caps <- csw$getCapabilities()

md <- csw$getRecordById("identificationInfo", outputSchema = "http://www.isotc211.org/2005/gmd")


###################

wms_mercator <- 'http://nrt.cmems-du.eu/thredds/wms/global-analysis-forecast-phy-001-024'



leaflet() %>% 
  setView(lng = 4.287638, lat = 50.703039, zoom = 15) %>% 
  addWMSTiles(
    wms_mercator,
    layers = "GRB_BSK",
    options = WMSTileOptions(format = "image/png", transparent = TRUE)
  )
