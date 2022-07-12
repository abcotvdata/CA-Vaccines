#load libraries
library(rgdal)
library(tidyverse)
library(leaflet)
library(leaflet.providers)
library(htmlwidgets)
library(leaflet.extras)
library(htmltools)

#read in data
ca_vax <- read.csv('https://data.chhs.ca.gov/dataset/e283ee5a-cf18-4f20-a92c-ee94a2866ccd/resource/130d7ba2-b6eb-438d-a412-741bde207e1c/download/covid19vaccinesbycounty.csv')
#filter out stuff
ca_vax1 <- filter(ca_vax, california_flag != "Not in California")
ca_vax2 <- filter(ca_vax1, county != "All CA and Non-CA Counties")
#View(ca_vax2)

#read in population data
ca_pop <- read.csv("~/CA-Vaccines/scripts/ca-county-pops-new.csv")
#View(ca_pop)

#merge vaccine data with population data
ca_vax_pop <- merge(ca_vax2, ca_pop, by='county', all=TRUE)

#calculate percent of the population
ca_vax_pop$pct_partially_vaxed <- round(((ca_vax_pop$total_partially_vaccinated/ca_vax_pop$Total.Population)*100), digits=2)
ca_vax_pop$pct_atleast_onedose <- round(((ca_vax_pop$cumulative_at_least_one_dose/ca_vax_pop$Total.Population)*100), digits=2)
ca_vax_pop$pct_fully_vaxed <- round(((ca_vax_pop$cumulative_fully_vaccinated/ca_vax_pop$Total.Population)*100), digits=2)
ca_vax_pop$pct_boostered_fully_vaxed <- round(((ca_vax_pop$cumulative_booster_recip_count/ca_vax_pop$cumulative_fully_vaccinated)*100), digits=2)

#View(ca_vax_pop)

#clean up the headers
ca_vax_pop_clean_headers <- ca_vax_pop %>% 
  dplyr::rename(`Cumulative doses administered` = cumulative_total_doses) %>%
  dplyr::rename(`Daily doses administered` = total_doses) %>%
  dplyr::rename(`% partially vaccinated` = pct_partially_vaxed) %>%
  dplyr::rename(`% with at least 1 shot` = pct_atleast_onedose) %>%
  dplyr::rename(`% fully vaccinated` = pct_fully_vaxed) %>% 
  dplyr::rename(`% boostered` = pct_boostered_fully_vaxed)

#View(ca_vax_pop_clean_headers)

# save as csv in data folder
write.csv(ca_vax_pop_clean_headers, '~/CA-Vaccines/ca-vaccine-data/CA-Vaccines-By-County_Historical.csv')

#make sure the date is actually a date
ca_vax_pop_clean_headers$administered_date <- as.Date(ca_vax_pop_clean_headers$administered_date,'%Y-%m-%d')
#View(ca_vax_pop_clean_headers)

#filter out for most recent date
ca_vax_pop_clean_headers_most_recent <- ca_vax_pop_clean_headers %>% group_by(county) %>% filter(administered_date == max(administered_date))
ca_vax_pop_clean_headers_most_recent$pretty_date <- gsub(" 0"," ", format(as.character(ca_vax_pop_clean_headers_most_recent$administered_date, format="%b. %d")))


#View(ca_vax_pop_clean_headers_most_recent)

#read in json counties mapping file
#ca_counties <- rgdal::readOGR("https://opendata.arcgis.com/datasets/a61c138d0a6946da8d1ebb8d1c9db13a_0.geojson")
ca_counties <- rgdal::readOGR("~/CA-Vaccines/scripts/stanford-jm667wq2232-geojson.json")


#merge mapping file with vax data
ca_vax_geo <- geo_join(ca_counties, ca_vax_pop_clean_headers_most_recent, "name", "county")
#View(ca_vax_geo)

# new chart: daily COVID-19 vaccine doses per county

all_vaccines1 <- ca_vax %>% 
  select("county", "administered_date", "total_doses") %>%
  filter(!county %in% c("All CA and Non-CA Counties", "Outside California", "Unknown"))

all_vaccines2 <- split(all_vaccines1, all_vaccines1$county)

new_names <- c("Alameda", "All CA Counties", "Alpine", "Amador", "Butte", "Calaveras", "Colusa", "Contra Costa", "Del Norte", "El Dorado", "Fresno", "Glenn", "Humboldt", "Imperial", "Inyo", "Kern", "Kings", "Lake", "Lassen", "Los Angeles", "Madera", "Marin", "Mariposa", "Mendocino", "Merced", "Modoc", "Mono", "Monterey", "Napa", "Nevada", "Orange", "Placer", "Plumas", "Riverside", "Sacramento", "San Benito", "San Bernardino", "San Diego", "San Francisco", "San Joaquin", "San Luis Obispo", "San Mateo", "Santa Barbara", "Santa Clara", "Santa Cruz", "Shasta", "Sierra", "Siskiyou", "Solano", "Sonoma", "Stanislaus", "Sutter", "Tehama", "Trinity", "Tulare", "Tuolumne", "Ventura", "Yolo", "Yuba")

for (i in 1:length(all_vaccines2)) {
  assign(new_names[i], all_vaccines2[[i]])
}

counties <- list(`All CA Counties`, Alameda, Alpine, Amador, Butte, Calaveras, Colusa, `Contra Costa`, `Del Norte`, `El Dorado`, Fresno, Glenn, Humboldt, Imperial, Inyo, Kern, Kings, Lake, Lassen, `Los Angeles`, Madera, Marin, Mariposa, Mendocino, Merced, Modoc, Mono, Monterey, Napa, Nevada, Orange, Placer, Plumas, Riverside, Sacramento, `San Benito`, `San Bernardino`, `San Diego`, `San Francisco`, `San Joaquin`, `San Luis Obispo`, `San Mateo`, `Santa Barbara`, `Santa Clara`, `Santa Cruz`, Shasta, Sierra, Siskiyou, Solano, Sonoma, Stanislaus, Sutter, Tehama, Trinity, Tulare, Tuolumne, Ventura, Yolo, Yuba)

final <- lapply(counties, function(x) {  
  mutate(x,
         `Rolling average` = rollmean(`total_doses`, k = 7, fill = NA))})

final_combined <- bind_rows(final, .id = "column_label")

final_combined$`administered_date` <- as.Date(final_combined$`administered_date`, format = "%Y-%m-%d")

final_combined1 <- final_combined %>% 
  select(-c(`column_label`)) %>% 
  rename(`Daily doses` = `total_doses`)

# save as csv in data folder
write.csv(final_combined1, '~/CA-Vaccines/ca-vaccine-data/CA-Vaccines-By-County_daily.csv')

#______________map_________________

pal <- colorNumeric(
  palette = "PuBu",
  domain = ca_vax_geo$X..fully.vaccinated,
  reverse = FALSE
)

popups <- paste(sep = "",
                paste(sep = "","<font size='3'><b> <p style='color:navy'>", ca_vax_geo$county," County</b><br> <font size='2'> <p style='color:black'> There have been <b>" ,format(ca_vax_geo$Cumulative.doses.administered ,big.mark=",",scientific=FALSE),"</b> vaccine doses<br>administered. <b><br><br>",format(ca_vax_geo$cumulative_at_least_one_dose ,big.mark=",",scientific=FALSE),"</b> people, about <b>",ca_vax_geo$X..with.at.least.1.shot,"%</b> of<br>of the county population, have been given<br>at least one dose. <b> <br><br>",format(ca_vax_geo$cumulative_fully_vaccinated ,big.mark=",",scientific=FALSE),"</b> people, about <b>",ca_vax_geo$X..fully.vaccinated,"%</b> of<br>of the county population, are<br>fully vaccinated.<br><br><b>",format(ca_vax_geo$cumulative_booster_recip_count ,big.mark=",",scientific=FALSE),"</b> people, about <b>",ca_vax_geo$X..boostered ,"%</b> of<br>of the fully vaccinated population, have received a <br>booster shot.</font size>", "<br><br> <font size='1'>Data as of <b>",ca_vax_geo$pretty_date)) %>% lapply(htmltools::HTML)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title {
    position: fixed !important;
    right: 0%;
    bottom: 6px;
    width: 50%;
    text-align: right;
    padding: 10px;
    background: rgba(255,255,255,0.75);
    font-style: italic;
    font-size: 10px;
  }
  @media only screen and (max-width: 460px) {
    .leaflet-control.map-title {
      width: 25%;
      font-size: 8px;
    }
  }
"))

title <- tags$div(
  tag.map.title, HTML("NOTE: Totals for the state's Vaccine Progress Dashboard and this dataset may not match, as the dashboard is by report date and this data is by administration date. Dose numbers may also change as data is updated.")
)

ca_vaccine_map <-leaflet(options = leafletOptions(zoomControl = FALSE, hoverToWake=FALSE)) %>%
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }") %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(-120.1484334,37.2427456, zoom = 6) %>%
  addPolygons(data = ca_vax_geo, 
              color = "black", 
              fillColor = ~pal(X..fully.vaccinated),
              label = popups, 
              labelOptions = labelOptions(
                direction = "auto",
                style = list("max-width" = "300px")),
              weight = 1, 
              fillOpacity = 0.9, 
              highlight = highlightOptions(
                weight = 3,
                color = "black",
                opacity = 1.0,
                bringToFront = TRUE,
                sendToBack = TRUE)) %>%
  addLegend(values = ca_vax_geo$X..fully.vaccinated, title = "Percent of people<br>fully vacccinated",
            pal = pal,
            position = 'bottomleft',
            opacity = 1) %>%
  addControl(title, position = "bottomright", className="map-title") 

saveWidget(ca_vaccine_map, '~/CA-Vaccines/ca_vaccine_map.html', selfcontained = TRUE)
