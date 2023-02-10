setwd("/Users/chris/Desktop/sars-cov-2_cdc_usa")

library(tidyverse)
library(lubridate)

# load raw data
date_release <- "20230210"
rawData <- read_csv(file.path("input", paste(sep = "_", date_release, "SARS-CoV-2_Variant_Proportions.csv")))

colnames(rawData)
rawData$usa_or_hhsregion %>% unique

rawData_bis <- rawData %>% 
  mutate(published_date_tmp = str_split(pattern = " ", published_date) %>% map(1) %>% unlist,
         published_date_ymd = mdy(published_date_tmp))

last_published_date <- rawData_bis$published_date_ymd %>% max

last_df <- rawData_bis %>% 
  dplyr::filter(published_date_ymd == last_published_date)


# variants_list <- c("XBB.1.5",
#                    "BQ.1.1",
#                    "BQ.1",
#                    "BA.5",
#                    "XBB",
#                    "BN.1",
#                    "BF.7")

variants_list <- last_df %>% 
  mutate(date_tmp = str_split(pattern = " ", week_ending) %>% map(1) %>% unlist,
         date = mdy(date_tmp)) %>% 
  dplyr::filter(date >= ymd("20220901"),
                share >= 0.05) %>% pull(variant) %>% unique

usa_df <- last_df %>% 
  dplyr::filter(usa_or_hhsregion == "USA",
                variant %in% variants_list) %>% 
  mutate(date_tmp = str_split(pattern = " ", week_ending) %>% map(1) %>% unlist,
         date = mdy(date_tmp),
         percent = share*100) %>% 
  dplyr::select(-week_ending, -date_tmp, -share) %>%
  dplyr::filter(date >= ymd("20220901")) %>% 
  arrange(date)

usa_df %>% 
  ggplot(aes(x = date, y = percent, color = variant)) +
  geom_line()

#
source("/Users/chris/Desktop/vaccintrackerqc.ca/_codes/scripts/set_env.R")

#
#date_variant <- ymd(date_release) + ddays(1)
date_variant <- ymd(date_release)
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
  hc_title(text = "Évolution de la proportion des variants du SRAS-CoV-2 aux États-Unis", align = "left") %>%
  hc_credits(text = "source: CDC - Centers for Disease Control and Prevention", enabled = TRUE)

#
r1 <- last_df %>% 
  dplyr::filter(usa_or_hhsregion == "1",
                variant %in% variants_list) %>% 
  mutate(date_tmp = str_split(pattern = " ", week_ending) %>% map(1) %>% unlist,
         date = mdy(date_tmp),
         percent = share*100) %>% 
  dplyr::select(-week_ending, -date_tmp, -share) %>%
  dplyr::filter(date >= ymd("20220901")) %>% 
  arrange(date)

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
  hc_subtitle(text = paste0("Région 3 : Delaware, District of Columbia, Maryland, Pennsylvania, Virginia and West Virginia.<br>", date_subtitle, " | @vaccintrackerqc"), align = "left") %>%
  hc_title(text = "Évolution de la proportion des variants du SRAS-CoV-2 | Région 3, États-Unis", align = "left")
