library(ggplot2)
library(plotly)
library(htmlwidgets)
library(bsselectR)
library(tidyverse)
library(zoo)
library(ggtext)
library(janitor)


vaccines <- read.csv("https://data.chhs.ca.gov/dataset/e283ee5a-cf18-4f20-a92c-ee94a2866ccd/resource/130d7ba2-b6eb-438d-a412-741bde207e1c/download/covid19vaccinesbycounty.csv")

vaccines$administered_date <- as.Date(vaccines$administered_date, format = "%Y-%m-%d")

vax_since_jan <- vaccines %>% 
  filter(administered_date >= "2021-01-01") %>% 
  rename(`Daily doses` = total_doses) %>% 
  rename(`County` = county) %>% 
  mutate(pretty_date = gsub(" 0"," ", format(as.character(administered_date, format="%b. %d, %Y"))))
  


our_counties <- c('All CA Counties','Alameda','Alpine','Amador','Butte','Calaveras','Colusa','Contra Costa','Del Norte','El Dorado','Fresno','Glenn','Humboldt','Imperial','Inyo','Kern','Kings','Lake','Lassen','Los Angeles','Madera','Marin','Mariposa','Mendocino','Merced','Modoc','Mono','Monterey','Napa','Nevada','Orange','Placer','Plumas','Riverside','Sacramento','San Benito','San Bernardino','San Diego','San Francisco','San Joaquin','San Luis Obispo','San Mateo','Santa Barbara','Santa Clara','Santa Cruz','Shasta','Sierra','Siskiyou','Solano','Sonoma','Stanislaus','Sutter','Tehama','Trinity','Tulare','Tuolumne','Ventura','Yolo','Yuba')

county_vax <- vax_since_jan %>% 
  filter(`County` == 'All CA Counties')

vax_with_ave <- data.frame()

colors <- c("7-day average" = "navy", "Daily doses" = "dodgerblue")

font <- list(
    family = "Gotham-Book",
    size = 15,
    color = "white"
)

for (county in our_counties) {
  print(county)
  county_nospace <- gsub(" ", "_", county)
  print(county_nospace)
  county_vax <- vax_since_jan %>% 
    filter(`County` == county) %>% 
    arrange(administered_date) %>% 
    mutate(`7-day average` = round(rollmean(`Daily doses`, k=7, align = 'right', fill = "N/A"), digits = 1)) %>% 
    select(`County`, administered_date, pretty_date, `Daily doses`, `7-day average`)
  
  vax_plot <- ggplot() +
  geom_bar(data=county_vax,
           lineend = "butt",
           linejoin = "round",
           stat="identity",
            aes(x=administered_date,
                y=`Daily doses`,
                group=1,
                text = paste(sep = "",pretty_date ,": <br>", prettyNum(`Daily doses`, big.mark = ","), " Daily Doses"),
                color="Daily doses",
                ), 
            size = 1.2) +
  geom_path(data=county_vax,
            lineend = "butt",
            linejoin = "round",
            aes(x=administered_date, 
                y=`7-day average`,
                group=1,
                text = paste(sep = "","On ", pretty_date ,", <br>the 7-day average was about ", prettyNum(`7-day average`, big.mark = ",")),
                color="7-day average",
                ),
            size = 1.2) +
  #labs(color = "Legend") +
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = scales::comma) +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = 'grey'),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        axis.text.y = element_text(vjust = -0.5,
                                   hjust = 0,
                                   margin = margin(
                                     l = 2, 
                                     r = -35)
                                   ),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0, "pt"),
        legend.position = "none",
        legend.title = element_blank() #,
        #legend.position = "top",
        #legend.direction = "horizontal",
        #legend.spacing.x = unit(1.0, 'cm')
        ) 


  font <- list(
    family = "Gotham-Book",
    size = 15,
    color = "white"
  )
  
  label <- list(
    bgcolor = "dodgerblue",
    bordercolor = "transparent",
    font = font
  )
  
  vax_plot<- ggplotly(vax_plot, tooltip = "text", height=400) %>% 
    config(displayModeBar = F) %>% 
    layout(font=font #, 
           #legend = list(orientation="v", x=0.1, y=1)
           ) %>% 
    style(hoverlabel = label)
  

  
  assign(paste0("vax_with_ave_",county_nospace), county_vax)
  
  assign(paste0("vax_plot_",county_nospace), vax_plot)
  
  saveWidget(vax_plot, paste0("~/R/CA-Vaccines/ca_daily_doses/plots/vax_with_ave_",county_nospace, ".html"), title = paste0("vax_with_ave_",county_nospace), selfcontained = TRUE)
  
  
  #number_of_days_RFW_zone <- ggplot(days_in_zone, aes(x=year)) + 
  #  geom_bar(color="purple", fill="#3e0045") +
  #  ggtitle(paste0("Number of days that had a red flag warning in the ", cwa, " area"))
  
  #number_of_days_RFW_zone <- ggplotly(number_of_days_RFW_zone)
  #print(number_of_days_RFW_zone)
  #assign(paste0("number_of_days_RFW_",cwa), number_of_days_RFW_zone)
  
  vax_with_ave <- rbind(vax_with_ave, county_vax)
  
}
