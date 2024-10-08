setwd("/Users/chris/Desktop/sandbox7/20221231_sars-cov-2_USA")

library(tidyverse)
library(lubridate)

rawData <- read_csv("SARS-CoV-2_Variant_Proportions.csv")

colnames(rawData)
rawData$usa_or_hhsregion %>% unique

last_published_date <- rawData$published_date %>% max

last_df <- rawData %>% 
  dplyr::filter(published_date == last_published_date)


variants_list <- c("XBB.1.5",
                   "BQ.1.1",
                   "BQ.1",
                   "BA.5",
                   "XBB",
                   "BN.1",
                   "BF.7")

usa_df <- last_df %>% 
  dplyr::filter(usa_or_hhsregion == "USA",
                variant %in% variants_list) %>% 
  mutate(date_tmp = str_split(pattern = " ", week_ending) %>% map(1) %>% unlist,
         date = mdy(date_tmp),
         percent = share*100) %>% 
  dplyr::select(-week_ending, -date_tmp, -share) %>%
  dplyr::filter(date >= ymd("20220901"))

usa_df %>% 
  ggplot(aes(x = date, y = percent, color = variant)) +
  geom_line()

#
source("/Users/chris/Desktop/vaccintrackerqc.ca/_codes/scripts/set_env.R")

#
date_variant <- "20221231"
date_fr_variant <- format_date_fr(ymd(date_variant))
date_subtitle <- paste("En date du", date_fr_variant, sep = " ")

nb_variants <- variants_list %>% length

# make charts
cols <- wes_palette("Zissou1", nb_variants, type = "continuous") %>% as.vector
options(highcharter.lang = hcoptslang_fr)

  usa_df %>% 
    hchart("spline", hcaes(x = date, y = percent, group = variant, v = variant)) %>% 
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
    hc_colors(cols) %>% 
    hc_subtitle(text = paste0(date_subtitle, " | @vaccintrackerqc"), align = "left") %>%
    hc_title(text = "Évolution de la proportion des variants du SRAS-CoV-2 aux États-Unis", align = "left")
    
#
  r1 <- last_df %>% 
    dplyr::filter(usa_or_hhsregion == "1",
                  variant %in% variants_list) %>% 
    mutate(date_tmp = str_split(pattern = " ", week_ending) %>% map(1) %>% unlist,
           date = mdy(date_tmp),
           percent = share*100) %>% 
    dplyr::select(-week_ending, -date_tmp, -share) %>%
    dplyr::filter(date >= ymd("20220901"))
  
  r1 %>% 
    hchart("spline", hcaes(x = date, y = percent, group = variant, v = variant)) %>% 
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
    hc_colors(cols) %>% 
    hc_subtitle(text = paste0("Région 1 : Connecticut, Maine, Massachusetts, New Hampshire, Rhode Island and Vermont.<br>", date_subtitle, " | @vaccintrackerqc"), align = "left") %>%
    hc_title(text = "Évolution de la proportion des variants du SRAS-CoV-2 | Région 1, États-Unis", align = "left")

#
  r2 <- last_df %>% 
    dplyr::filter(usa_or_hhsregion == "2",
                  variant %in% variants_list) %>% 
    mutate(date_tmp = str_split(pattern = " ", week_ending) %>% map(1) %>% unlist,
           date = mdy(date_tmp),
           percent = share*100) %>% 
    dplyr::select(-week_ending, -date_tmp, -share) %>%
    dplyr::filter(date >= ymd("20220901"))
  
  r2 %>% 
    hchart("spline", hcaes(x = date, y = percent, group = variant, v = variant)) %>% 
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
    hc_colors(cols) %>% 
    hc_subtitle(text = paste0("Région 2 : New Jersey, New York, Puerto Rico and the Virgin Islands.<br>", date_subtitle, " | @vaccintrackerqc"), align = "left") %>%
    hc_title(text = "Évolution de la proportion des variants du SRAS-CoV-2 | Région 2, États-Unis", align = "left")

  #
  r3 <- last_df %>% 
    dplyr::filter(usa_or_hhsregion == "3",
                  variant %in% variants_list) %>% 
    mutate(date_tmp = str_split(pattern = " ", week_ending) %>% map(1) %>% unlist,
           date = mdy(date_tmp),
           percent = share*100) %>% 
    dplyr::select(-week_ending, -date_tmp, -share) %>%
    dplyr::filter(date >= ymd("20220901"))
  
  r3 %>% 
    hchart("spline", hcaes(x = date, y = percent, group = variant, v = variant)) %>% 
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
    hc_colors(cols) %>% 
    hc_subtitle(text = paste0("Région 3 : Delaware, District of Columbia, Maryland, Pennsylvanina, Virginia and West Virginia.<br>", date_subtitle, " | @vaccintrackerqc"), align = "left") %>%
    hc_title(text = "Évolution de la proportion des variants du SRAS-CoV-2 | Région 3, États-Unis", align = "left")
  