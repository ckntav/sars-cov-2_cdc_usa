setwd("/Users/chris/Desktop/sars-cov-2_cdc_usa")

library(tidyverse)
library(lubridate)

map_hhs_region <- data.frame(
  nb = as.character(seq(1, 10)),
  hhs_region = c("Region 1 - Boston",
                 "Region 2 - New York",
                 "Region 3 - Philadelphia",
                 "Region 4 - Atlanta",
                 "Region 5 - Chicago",
                 "Region 6 - Dallas",
                 "Region 7 - Kansas City",
                 "Region 8 - Denver",
                 "Region 9 - San Francisco",
                 "Region 10 - Seattle"),
  hhs_color = c("#827660", "#9E516C",
                         "#393D69", "#BA514C",
                         "#447586", "#687725",
                         "#F1B142", "#71518C",
                         "#873129", "#3879B0")
)

# load raw data
date_release <- "20230203"
rawData <- read_csv(file.path("input", paste(sep = "_", date_release, "SARS-CoV-2_Variant_Proportions.csv")))

rawData_bis <- rawData %>% 
  mutate(published_date_tmp = str_split(pattern = " ", published_date) %>% map(1) %>% unlist,
         published_date_ymd = mdy(published_date_tmp))

# get last published data
last_published_date <- rawData_bis$published_date_ymd %>% max
message("Last published date : ", last_published_date)

last_df <- rawData_bis %>% 
  dplyr::filter(published_date_ymd == last_published_date)

# get tidy data
xbb_df <- last_df %>% 
  dplyr::filter(variant == "XBB.1.5",
                usa_or_hhsregion != "USA") %>% 
  mutate(date_tmp = str_split(pattern = " ", week_ending) %>% map(1) %>% unlist,
         date = mdy(date_tmp),
         percent = share*100) %>% 
  dplyr::select(-week_ending, -date_tmp, -share) %>%
  dplyr::filter(date >= ymd("20221029")) %>% 
  left_join(map_hhs_region, by = c("usa_or_hhsregion" = "nb")) %>% 
  dplyr::select(date, hhs_region, percent, everything(), -usa_or_hhsregion, -variant,
                -published_date, -share_hi, -share_lo, -time_interval, -hhs_color) %>%
  arrange(date)

xbb_df$hhs_region <- factor(xbb_df$hhs_region, levels = map_hhs_region$hhs_region)
cols_hhs <- map_hhs_region$hhs_color

xbb_df %>% arrange(hhs_region) %>% 
  as.data.frame 

#
#
source("/Users/chris/Desktop/vaccintrackerqc.ca/_codes/scripts/set_env.R")

#
# date_variant <- ymd(date_release) + ddays(1)
date_variant <- ymd(date_release)
date_fr_variant <- format_date_fr(ymd(date_variant))
date_subtitle <- paste("En date du", date_fr_variant, sep = " ")

#
options(highcharter.lang = hcoptslang_fr)

xbb_df %>% 
  hchart("spline", hcaes(x = date, y = percent, group = hhs_region, v = hhs_region)) %>% 
  hc_tooltip(crosshairs = TRUE, table = TRUE, sort = TRUE, borderColor = "grey",
             xDateFormat = '%A %e %B %Y', valueDecimals = 2,
             headerFormat = "<small>{point.key}</small><table>",
             pointFormat = paste0('<tr><td style="color: {series.color}\">&#11044;</td>',
                                  '<td>{point.v}</td>',
                                  # '<td style="text-align: right">{point.z}</td>',
                                  '<td style="text-align: right">{point.y} %</td></tr>'),
             footerFormat = "</table>") %>% 
  hc_yAxis(title = list(text = "Proportion (%)"), 
           labels = list(format = "{value} %"),
           min = 0, max = 100) %>%
  hc_xAxis(title = list(text = "Semaine de prélèvement (se terminant le)")) %>% 
  hc_plotOptions(spline = list(lineWidth = 3,
                               marker = list(radius = 0,
                                             symbol = "circle"))) %>% 
  hc_colors(cols_hhs) %>% 
  hc_subtitle(text = paste0(date_subtitle, " | @vaccintrackerqc"), align = "left") %>%
  hc_title(text = "Évolution de la proportion du variant XBB.1.5 (Kraken) aux États-Unis", align = "left") %>%
  hc_credits(text = "source: CDC - Centers for Disease Control and Prevention", enabled = TRUE)
