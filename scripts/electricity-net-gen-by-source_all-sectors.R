#  ---------------------------------------------------- INPUT DATA ----------------------------------------------------

data.file = 'Table_7.2a_Electricity_Net_Generation__Total_(All_Sectors).xlsx'

#  --------------------------------------------------- MAIN SCRIPT ---------------------------------------------------

# load libraries -------
  
  library(data.table)
  library(openxlsx)
  library(stringr)
  library(plyr)
  library(lubridate)
  library(ggplot2)
  library(hrbrthemes)
  library(directlabels)
  library(grid)
  library(extrafont)
  
# load fonts -----
# run this line by itself because it requires a y/n answer

  # font_import()

# source color palettes and plot themes ------

  items = list.files(here::here("src"))
  sapply(here::here("src", items), source)
  
# load data ------
  
  dt_month = as.data.table(read.xlsx(here::here("data", data.file), sheet = "Monthly Data", startRow = 11, detectDates = T))
  dt_month = dt_month[2:nrow(dt_month)]
  
  dt_annual = as.data.table(read.xlsx(here::here("data", data.file), sheet = "Annual Data", startRow = 11, detectDates = T))
  dt_annual = dt_annual[2:nrow(dt_annual)]
  
# rename columns -----
  
  colnames(dt_month) = c("month",
                         "Coal",
                         "Petroleum",
                         "Natural Gas",
                         "Other Gases",
                         "Nuclear",
                         "Pumped Storage",
                         "Hydroelectric",
                         "Wood",
                         "Waste",
                         "Geothermal",
                         "Solar",
                         "Wind",
                         "Total")
  
  colnames(dt_annual) = c("year",
                          "Coal",
                          "Petroleum",
                          "Natural Gas",
                          "Other Gases",
                          "Nuclear",
                          "Pumped Storage",
                          "Hydroelectric",
                          "Wood",
                          "Waste",
                          "Geothermal",
                          "Solar",
                          "Wind",
                          "Total")
  
  
# remove pumped storage -----
  
  dt_month[, ("Pumped Storage") := NULL]
  dt_annual[, ("Pumped Storage") := NULL]
  
# melt data table from wide to long format -----
  
  dt_month = melt(dt_month, measure.vars = colnames(dt_month)[2:13],
                  variable.name = "MSN", value.name = "value")
  
  dt_annual = melt(dt_annual, measure.vars = colnames(dt_annual)[2:13],
                   variable.name = "MSN", value.name = "value")
  
# convert value column to numeric ----
  
  dt_month[, value := as.numeric(value)]
  dt_annual[, value := as.numeric(value)]
  
# remove NA month or year entries ----
  
  dt_month = dt_month[!is.na(month)]
  dt_annual = dt_annual[!is.na(year)]
  
# use lubridate package to create year and month columns ------
  
  dt_month[, year := year(month)]
  dt_month[, month_val := month(month)]
  dt_month[, month_name := month(month, label = T)]
  
# remove total ------
  
  dt_month = dt_month[ ! MSN %in% c("Total")]
  dt_annual = dt_annual[ ! MSN %in% c("Total")]
  
# remove NAs -----
  
  dt_month = dt_month[!is.na(value)]
  dt_annual = dt_annual[!is.na(value)]
  
# keep renewables only ------
  
  dt_annual_re = dt_annual[! MSN %in% c("Total", "Coal", "Petroleum", "Natural Gas", "Nuclear", "Other Gases") ]
  dt_month_re = dt_month[! MSN %in% c("Total", "Coal", "Petroleum", "Natural Gas", "Nuclear", "Other Gases") ]
  
# calculate proportion (or percentage) contributed by each fuel type, for each month, for each year -----
  
  dt_month[, prop := value/sum(value), by = c("year", "month_name")]
  dt_annual[, prop := value/sum(value), by = c("year")]
  dt_month_re[, prop := value/sum(value), by = c("year", "month_name")]
  dt_annual_re[, prop := value/sum(value), by = c("year")]
  
#  ------------------------------------------------------- FIGURES -------------------------------------------------------

  # reorder factor levels for plots -----
  
    dt_annual = dt_annual[ ! MSN %in% c("Total")][, MSN := factor(MSN, levels = c("Wind",
                                                                                  "Solar",
                                                                                  "Geothermal",
                                                                                  "Waste",
                                                                                  "Wood",
                                                                                  "Hydroelectric",
                                                                                  "Nuclear",
                                                                                  "Other Gases",
                                                                                  "Natural Gas",
                                                                                  "Petroleum",
                                                                                  "Coal"))]
  
  # line, annual -------
  
    fig_line_annual = ggplot(dt_annual, aes(x = year, y = value/1000, group = MSN, color = MSN)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual U.S. electricity generation by energy source, all sectors (1949-2019)',
           subtitle = 'Data: U.S. Energy Information Administration', 
           x = NULL,
           y = 'Billion Kilowatthours') +
      guides(color = FALSE) +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_comma(expand = c(0.01,0)) +
      scale_color_manual(values = pal_fuel) + 
      theme_line +
      geom_dl(aes(label = MSN), method = list(dl.trans(x = x + .3), 'last.bumpup', 
                                                 cex = 1,
                                                 fontfamily = 'Secca Soft',
                                                 fontface = 'bold')) 
    
    fig_line_annual = ggplotGrob(fig_line_annual)
    fig_line_annual$layout$clip[fig_line_annual$layout$name == "panel"] = "off"
    
    ggsave(fig_line_annual, 
           filename = here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_lts.pdf'), 
           width = 11.75, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_lts.pdf'),
                outfile = here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_lts.pdf'))
    
    # save as png:
    # ggsave(fig_line_annual, 
    #        filename = here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_lts.png'), 
    #        width = 11.75, 
    #        height = 6.25, 
    #        dpi = 600)
  
  # area, annual (absolute) -------
  
    fig_area_annual_abs = ggplot(dt_annual, aes(x = year, y = value/1000, group = MSN, fill = MSN)) + 
      geom_area() +
      labs(title = 'Annual U.S. electricity generation by energy source, all sectors (1949-2019)',
           subtitle = 'Data: U.S. Energy Information Administration', 
           x = NULL,
           y = 'Billion Kilowatthours',
           fill = NULL) +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_comma(expand = c(0,0)) +
      scale_fill_manual(values = pal_fuel) + 
      theme_area
    
    ggsave(fig_area_annual_abs, 
           filename = here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_ats_absolute.pdf'), 
           width = 11.75, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_ats_absolute.pdf'),
                outfile = here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_ats_absolute.pdf'))
    
    # save as png: 
    # ggsave(fig_area_annual_abs, 
    #        filename = here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_ats_absolute.png'), 
    #        width = 11.75, 
    #        height = 6.25, 
    #        dpi = 600)
  
  # area, annual (percentage) -------
  
    fig_area_annual_prop = ggplot(dt_annual, aes(x = year, y = prop, group = MSN, fill = MSN)) + 
      geom_area() +
      labs(title = 'Annual U.S. electricity generation by energy source, all sectors (1949-2019)',
           subtitle = 'Data: U.S. Energy Information Administration', 
           x = NULL,
           y = 'Percentage',
           fill = NULL) +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_percent(expand = c(0,0)) +
      scale_fill_manual(values = pal_fuel) + 
      theme_area
    
    ggsave(fig_area_annual_prop, 
           filename = here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_ats_proportion.pdf'), 
           width = 11.75, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_ats_proportion.pdf'),
                outfile = here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_ats_proportion.pdf'))
    
    # save as png:
    # ggsave(fig_area_annual_prop, 
    #        filename = here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_ats_proportion.png'), 
    #        width = 11.75, 
    #        height = 6.25, 
    #        dpi = 600)
  
  # renewable, line, annual -------
  
    fig_line_annual_re = ggplot(dt_annual_re, aes(x = year, y = value/1000, group = MSN, color = MSN)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual U.S. electricity generation from renewable energy sources, all sectors (1949-2019)',
           subtitle = 'Data: U.S. Energy Information Administration', 
           x = NULL,
           y = 'Billion Kilowatthours') +
      guides(color = FALSE) +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_comma(expand = c(0.01,0)) +
      scale_color_manual(values = pal_fuel) + 
      theme_line +
      geom_dl(aes(label = MSN), method = list(dl.trans(x = x + .3), 'last.bumpup', 
                                              cex = 1,
                                              fontfamily = 'Secca Soft',
                                              fontface = 'bold')) 
    
    fig_line_annual_re = ggplotGrob(fig_line_annual_re)
    fig_line_annual_re$layout$clip[fig_line_annual_re$layout$name == "panel"] = "off"
    
    ggsave(fig_line_annual_re, 
           filename = here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_renewable_lts.pdf'), 
           width = 11.75, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_renewable_lts.pdf'),
                outfile = here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_renewable_lts.pdf'))
    
    # save as png:
    # ggsave(fig_line_annual_re, 
    #        filename = here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_renewable_lts.png'), 
    #        width = 11.75, 
    #        height = 6.25, 
    #        dpi = 600)
  
  # renewable, area, annual (absolute) -------
  
    # create dataset of where to put the labels on area chart
    labs_area_re = dt_annual_re[year == 2019][order(factor(MSN, levels = (c("Wind", "Solar", "Geothermal", "Waste", "Wood", "Hydroelectric"))))]
    labs_area_re[, cum_sum := cumsum(value/1000)] 
    labs_area_re[, difference := diff(c(0,cum_sum))/2]
    labs_area_re[, position := cum_sum - difference]

    fig_area_annual_abs_re = ggplot(dt_annual_re, aes(x = year, y = value/1000, group = MSN, fill = MSN)) + 
      geom_area() +
      labs(title = 'Annual U.S. electricity generation from renewable energy sources, all sectors (1949-2019)',
           subtitle = 'Data: U.S. Energy Information Administration', 
           x = NULL,
           y = 'Billion Kilowatthours',
           fill = NULL) +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_comma(expand = c(0,0)) +
      scale_color_manual(values = pal_fuel) +
      scale_fill_manual(values = pal_fuel) + 
      guides(fill = FALSE,
             color = FALSE) +
      theme_area_labeled + 
      geom_text(data = labs_area_re,
                aes(x = Inf, y = position,
                    label = paste0(' ', MSN), color = MSN), hjust = 0, 
                size = 4.7, fontface = 'bold',
                family = 'Secca Soft')  
    
    fig_area_annual_abs_re = ggplotGrob(fig_area_annual_abs_re)
    fig_area_annual_abs_re$layout$clip[fig_area_annual_abs_re$layout$name == "panel"] = "off"
    
    ggsave(fig_area_annual_abs_re, 
           filename = here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_renewable_ats_absolute.pdf'), 
           width = 11.75, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_renewable_ats_absolute.pdf'),
                outfile = here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_renewable_ats_absolute.pdf'))
    
    # save as png: 
    # ggsave(fig_area_annual_abs_re,
    #        filename = here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_renewable_ats_absolute.png'),
    #        width = 11.75,
    #        height = 6.25,
    #        dpi = 600)
  
  # renewable, area, annual (proportion) -------
    
    # create dataset of where to put the labels on area chart
    labs_area_re_prop = dt_annual_re[year == 2019][order(factor(MSN, levels = (c("Wind", "Solar", "Geothermal", "Waste", "Wood", "Hydroelectric"))))]
    labs_area_re_prop[, cum_sum := cumsum(prop)] 
    labs_area_re_prop[, difference := diff(c(0,cum_sum))/2]
    labs_area_re_prop[, position := cum_sum - difference]
    
    fig_area_annual_prop_re = ggplot(dt_annual_re, aes(x = year, y = prop, group = MSN, fill = MSN)) + 
      geom_area() +
      labs(title = 'Annual U.S. electricity generation from renewable energy sources, all sectors (1949-2019)',
           subtitle = 'Data: U.S. Energy Information Administration', 
           x = NULL,
           y = 'Percentage',
           fill = NULL) +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_percent(expand = c(0,0)) +
      scale_color_manual(values = pal_fuel) +
      scale_fill_manual(values = pal_fuel) + 
      guides(fill = FALSE,
             color = FALSE) +
      theme_area_labeled + 
      geom_text(data = labs_area_re_prop,
                aes(x = Inf, y = position,
                    label = paste0(' ', MSN), color = MSN), hjust = 0, 
                size = 4.7, fontface = 'bold',
                family = 'Secca Soft')  
    
    fig_area_annual_prop_re = ggplotGrob(fig_area_annual_prop_re)
    fig_area_annual_prop_re$layout$clip[fig_area_annual_prop_re$layout$name == "panel"] = "off"

    ggsave(fig_area_annual_prop_re, 
           filename = here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_renewable_ats_proportion.pdf'), 
           width = 11.75, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_renewable_ats_proportion.pdf'),
                outfile = here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_renewable_ats_proportion.pdf'))
    
    # save as png: 
    # ggsave(fig_area_annual_prop_re,
    #        filename = here::here('figures', 'electricity_net-generation-by-source_all-sectors_annual_1949-2019_renewable_ats_proportion.png'),
    #        width = 11.75,
    #        height = 6.25,
    #        dpi = 600)
    
  # bar, 2019 ---------
    
    fig_bar_2019 = ggplot(dt_annual[year == max(year)], aes(x = reorder(MSN, value), y = value/1000, group = MSN, fill = MSN)) + 
      geom_bar(stat = "identity") +
      labs(title = 'Annual U.S. electricity generation by energy source, all sectors (2019)',
           subtitle = 'Data: U.S. Energy Information Administration', 
           x = NULL,
           y = 'Billion Kilowatthours',
           fill = NULL) +
      # scale_x_continuous() +
      scale_y_comma(expand = c(0,0), breaks = seq(0,1600,400), limits = c(0,1700)) +
      scale_color_manual(values = pal_fuel) +
      scale_fill_manual(values = pal_fuel) + 
      guides(fill = FALSE,
             color = FALSE) +
      theme_bar_flipped + 
      geom_text(data = dt_annual[year == max(year)],
                aes(x = MSN, y = value/1000 + 75,
                    label = paste0(signif(prop*100,2), '%'), color = MSN), hjust = 0.5,
                size = 6, fontface = 'bold',
                family = 'Secca Soft') +
      coord_flip()
    
    ggsave(fig_bar_2019, 
           filename = here::here('figures', 'electricity_net-generation-by-source_all-sectors_2019_bar.pdf'), 
           width = 11.75, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'electricity_net-generation-by-source_all-sectors_2019_bar.pdf'),
                outfile = here::here('figures', 'electricity_net-generation-by-source_all-sectors_2019_bar.pdf'))
    
    # save as png: 
    # ggsave(fig_bar_2019,
    #        filename = here::here('figures', 'electricity_net-generation-by-source_all-sectors_2019_bar.png'),
    #        width = 11.75,
    #        height = 6.25,
    #        dpi = 600)
    