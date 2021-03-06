#  ---------------------------------------------------- INPUT DATA ----------------------------------------------------

# data downloaded from:https://www.eia.gov/totalenergy/data/browser/index.php?tbl=T07.02A#/?f=M
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
  
# remove totals -----
  
  dt_month = dt_month[! MSN == 'Total']
  dt_annual = dt_annual[! MSN == 'Total']
  
# keep renewables only ------
  
  dt_annual_re = dt_annual[! MSN %in% c("Coal", "Petroleum", "Natural Gas", "Nuclear", "Other Gases") ]
  dt_month_re = dt_month[! MSN %in% c("Coal", "Petroleum", "Natural Gas", "Nuclear", "Other Gases") ]
  
# combine non-hydro into 'Other Renewables' ------
  
  dt_annual[, fuel := MSN]
  dt_annual[ MSN %in% c('Wood', 'Waste', 'Geothermal', 'Solar', 'Wind'), fuel := 'Other Renewables' ]
  
  dt_month[, fuel := MSN]
  dt_month[ MSN %in% c('Wood', 'Waste', 'Geothermal', 'Solar', 'Wind'), fuel := 'Other Renewables' ]
  
# aggregate by fuel ------
  
  dt_annual_agg = dt_annual[, .(value = sum(value)), by = .(year, fuel)]
  dt_month_agg = dt_month[, .(value = sum(value)), by = .(month, year, month_val, month_name, fuel )]
  
# calculate proportion (or percentage) contributed by each fuel type, for each month, for each year -----
  
  dt_month[, prop := value/sum(value), by = c("year", "month_name")]
  dt_annual[, prop := value/sum(value), by = c("year")]
  dt_month_agg[, prop := value/sum(value), by = c("year", "month_name")]
  dt_annual_agg[, prop := value/sum(value), by = c("year")]
  dt_month_re[, prop := value/sum(value), by = c("year", "month_name")]
  dt_annual_re[, prop := value/sum(value), by = c("year")]
  
  
#  ------------------------------------------------------- FIGURES -------------------------------------------------------

  # bar, 2019 ---------
  
  fig_bar_2019 = ggplot(dt_annual[year == max(year)], aes(x = reorder(MSN, value), y = value/1000, group = MSN, fill = MSN)) + 
    geom_bar(stat = "identity") +
    labs(title = 'Annual U.S. electricity generation by energy source, all sectors (2019)',
         subtitle = NULL, 
         caption = 'Data: U.S. Energy Information Administration',
         x = NULL,
         y = 'Billion Kilowatthours',
         fill = NULL) +
    scale_y_continuous(labels = scales::comma, expand = c(0,0), breaks = seq(0,1600,400), limits = c(0,1700)) +
    scale_color_manual(values = pal_fuel) +
    scale_fill_manual(values = pal_fuel) + 
    guides(fill = 'none',
           color = 'none') +
    theme_bar_flipped + 
    geom_text(data = dt_annual[year == max(year)],
              aes(x = MSN, y = value/1000 + 75,
                  label = paste0(signif(prop*100,2), '%'), color = MSN), hjust = 0.5,
              size = 6, fontface = 'bold',
              family = 'Secca Soft') +
    coord_flip()
  
  ggsave(fig_bar_2019, 
         filename = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_2019_bar.pdf'), 
         width = 11.5, 
         height = 6.25)
  
  embed_fonts(here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_2019_bar.pdf'),
              outfile = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_2019_bar.pdf'))
  
  # save as png: 
  # ggsave(fig_bar_2019,
  #        filename = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_2019_bar.png'),
  #        width = 11.5,
  #        height = 6.25,
  #        dpi = 600)
  
  
  # reorder factor levels for plots ------
  
    dt_annual_agg = dt_annual_agg[, fuel := factor(fuel, levels = c('Other Renewables',
                                                                    "Hydroelectric",
                                                                    "Nuclear",
                                                                    "Other Gases",
                                                                    "Natural Gas",
                                                                    "Petroleum",
                                                                    "Coal"))]
  
    dt_annual = dt_annual[, fuel := factor(fuel, levels = c("Wind",
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
    
    # create dataset of where to put the labels on line chart
    labs_line = dt_annual_agg[year == max(year)]
    labs_line = labs_line[order(rank(value))]
    labs_line[1:2, position := c(0,85)]
    labs_line[is.na(position), position := value/1000]
  
    fig_line_annual = ggplot(dt_annual_agg, aes(x = year, y = value/1000, group = fuel, color = fuel)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual U.S. electricity generation by energy source, all sectors (1949-2019)',
           subtitle = 'Billion Kilowatthours', 
           caption = 'Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
      scale_color_manual(values = pal_fuel) + 
      theme_line +
      geom_text(data = labs_line, aes(x = Inf, y = position, label = paste0(' ', fuel), color = fuel), hjust = 0, 
                size = 6.5, fontface = 'plain', family = 'Secca Soft')  

    fig_line_annual = ggplotGrob(fig_line_annual)
    fig_line_annual$layout$clip[fig_line_annual$layout$name == "panel"] = "off"
    
    ggsave(fig_line_annual, 
           filename = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_annual_1949-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_annual_1949-2019_lts.pdf'),
                outfile = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_annual_1949-2019_lts.pdf'))
    
    # save as png:
    # ggsave(fig_line_annual, 
    #        filename = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_annual_1949-2019_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
    fig_line_annual_2 = ggplot(dt_annual, aes(x = year, y = value/1000, group = MSN, color = MSN)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual U.S. electricity generation by energy source, all sectors (1949-2019)',
           subtitle = 'Billion Kilowatthours', 
           caption = 'Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
      scale_color_manual(values = pal_fuel) + 
      theme_line +
      # geom_text(data = labs_line, aes(x = Inf, y = position, label = paste0(' ', fuel), color = fuel), hjust = 0, 
      #           size = 6.5, fontface = 'plain', family = 'Secca Soft')  
      geom_dl(aes(label = MSN), method = list(dl.trans(x = x + .2), 'last.bumpup', cex = 1.1, fontfamily = 'Secca Soft', fontface = 'plain'))
    
    fig_line_annual_2 = ggplotGrob(fig_line_annual_2)
    fig_line_annual_2$layout$clip[fig_line_annual_2$layout$name == "panel"] = "off"
    
    ggsave(fig_line_annual_2, 
           filename = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_annual_1949-2019_lts_v2.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_annual_1949-2019_lts_v2.pdf'),
                outfile = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_annual_1949-2019_lts_v2.pdf'))
    
  
  # area, annual (absolute) -------
  
    fig_area_annual_abs = ggplot(dt_annual, aes(x = year, y = value/1000, group = MSN, fill = MSN)) + 
      geom_area() +
      labs(title = 'Annual U.S. electricity generation by energy source, all sectors (1949-2019)',
           subtitle = 'Billion Kilowatthours', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL ,
           fill = NULL) +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
      scale_fill_manual(values = pal_fuel) + 
      theme_area
    
    ggsave(fig_area_annual_abs, 
           filename = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_annual_1949-2019_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_annual_1949-2019_ats_absolute.pdf'),
                outfile = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_annual_1949-2019_ats_absolute.pdf'))
    
    # save as png: 
    # ggsave(fig_area_annual_abs, 
    #        filename = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_annual_1949-2019_ats_absolute.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
  
  # area, annual (percentage) -------
  
    fig_area_annual_prop = ggplot(dt_annual, aes(x = year, y = prop, group = MSN, fill = MSN)) + 
      geom_area() +
      labs(title = 'Annual U.S. electricity generation by energy source, all sectors (1949-2019)',
           subtitle = 'Share of electricity generation',
           caption = 'Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL,
           fill = NULL) +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
      scale_fill_manual(values = pal_fuel) + 
      theme_area
    
    ggsave(fig_area_annual_prop, 
           filename = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_annual_1949-2019_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_annual_1949-2019_ats_proportion.pdf'),
                outfile = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_annual_1949-2019_ats_proportion.pdf'))
    
    # save as png:
    # ggsave(fig_area_annual_prop, 
    #        filename = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_annual_1949-2019_ats_proportion.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
  
  # renewable, line, annual -------
    
    # create dataset of where to put the labels on line chart
    labs_line_re = dt_annual_re[year == max(year)]
    labs_line_re = labs_line_re[order(rank(value))]
    labs_line_re[1:3, position := c(9,24,41)]
    labs_line_re[is.na(position), position := value/1000]

    fig_line_annual_re = ggplot(dt_annual_re, aes(x = year, y = value/1000, group = MSN, color = MSN)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual U.S. electricity generation from renewable sources, all sectors (1949-2019)',
           subtitle = 'Billion Kilowatthours', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
      scale_color_manual(values = pal_fuel) + 
      theme_line +
      geom_text(data = labs_line_re, aes(x = Inf, y = position, label = paste0(' ', MSN), color = MSN), hjust = 0, 
                size = 6.5, fontface = 'plain', family = 'Secca Soft')  

    fig_line_annual_re = ggplotGrob(fig_line_annual_re)
    fig_line_annual_re$layout$clip[fig_line_annual_re$layout$name == "panel"] = "off"
    
    ggsave(fig_line_annual_re, 
           filename = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_renewables_annual_1949-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_renewables_annual_1949-2019_lts.pdf'),
                outfile = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_renewables_annual_1949-2019_lts.pdf'))
    
    # save as png:
    # ggsave(fig_line_annual_re, 
    #        filename = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_renewables_annual_1949-2019_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
  
  # renewable, area, annual (absolute) -------
  
    # create dataset of where to put the labels on area chart
    labs_area_re = dt_annual_re[year == 2019][order(factor(MSN, levels = (c("Wind", "Solar", "Geothermal", "Waste", "Wood", "Hydroelectric"))))]
    labs_area_re[, cum_sum := cumsum(value/1000)] 
    labs_area_re[, difference := diff(c(0,cum_sum))/2]
    labs_area_re[, position := cum_sum - difference]
    labs_area_re[ MSN == 'Geothermal', position := position - 2 ]
    labs_area_re[ MSN == 'Waste', position := position + 8 ]
    labs_area_re[ MSN == 'Wood', position := position + 8 ]

    fig_area_annual_abs_re = ggplot(dt_annual_re, aes(x = year, y = value/1000, group = MSN, fill = MSN)) + 
      geom_area() +
      labs(title = 'Annual U.S. electricity generation from renewable sources, all sectors (1949-2019)',
           subtitle = 'Billion Kilowatthours', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL,
           fill = NULL) +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
      scale_color_manual(values = pal_fuel) +
      scale_fill_manual(values = pal_fuel) + 
      guides(fill = 'none',
             color = 'none') +
      theme_area_labeled + 
      geom_text(data = labs_area_re, aes(x = Inf, y = position, label = paste0(' ', MSN), color = MSN), hjust = 0, 
                size = 6.5, fontface = 'plain', family = 'Secca Soft')  
    
    fig_area_annual_abs_re = ggplotGrob(fig_area_annual_abs_re)
    fig_area_annual_abs_re$layout$clip[fig_area_annual_abs_re$layout$name == "panel"] = "off"
    
    ggsave(fig_area_annual_abs_re, 
           filename = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_renewables_annual_1949-2019_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_renewables_annual_1949-2019_ats_absolute.pdf'),
                outfile = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_renewables_annual_1949-2019_ats_absolute.pdf'))
    
    # save as png: 
    # ggsave(fig_area_annual_abs_re,
    #        filename = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_renewables_annual_1949-2019_ats_absolute.png'),
    #        width = 11.5,
    #        height = 6.25,
    #        dpi = 600)
  
  # renewable, area, annual (proportion) -------
    
    # create dataset of where to put the labels on area chart
    labs_area_re_prop = dt_annual_re[year == 2019][order(factor(MSN, levels = (c("Wind", "Solar", "Geothermal", "Waste", "Wood", "Hydroelectric"))))]
    labs_area_re_prop[, cum_sum := cumsum(prop)] 
    labs_area_re_prop[, difference := diff(c(0,cum_sum))/2]
    labs_area_re_prop[, position := cum_sum - difference]
    labs_area_re_prop[ MSN == 'Geothermal', position := position - 0.01 ]
    labs_area_re_prop[ MSN == 'Waste', position := position + 0.01 ]
    labs_area_re_prop[ MSN == 'Wood', position := position + 0.01 ]

    fig_area_annual_prop_re = ggplot(dt_annual_re, aes(x = year, y = prop, group = MSN, fill = MSN)) + 
      geom_area() +
      labs(title = 'Annual U.S. electricity generation from renewable sources, all sectors (1949-2019)',
           subtitle = 'Share of renewable electricity generation', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL,
           fill = NULL) +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
      scale_color_manual(values = pal_fuel) +
      scale_fill_manual(values = pal_fuel) + 
      guides(fill = 'none',
             color = 'none') +
      theme_area_labeled + 
      geom_text(data = labs_area_re_prop, aes(x = Inf, y = position, label = paste0(' ', MSN), color = MSN), hjust = 0, 
                size = 6.5, fontface = 'plain', family = 'Secca Soft')  
    
    fig_area_annual_prop_re = ggplotGrob(fig_area_annual_prop_re)
    fig_area_annual_prop_re$layout$clip[fig_area_annual_prop_re$layout$name == "panel"] = "off"

    ggsave(fig_area_annual_prop_re, 
           filename = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_renewables_annual_1949-2019_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_renewables_annual_1949-2019_ats_proportion.pdf'),
                outfile = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_renewables_annual_1949-2019_ats_proportion.pdf'))
    
    # save as png: 
    # ggsave(fig_area_annual_prop_re,
    #        filename = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_renewables_annual_1949-2019_ats_proportion.png'),
    #        width = 11.5,
    #        height = 6.25,
    #        dpi = 600)
    
    
  # line, monthly (not labeled) -------
    
    labs_line = dt_month_agg[month == max(month)]
    labs_line = labs_line[order(rank(value))]
    labs_line[, position := value/1000]
    
    fig_line_month = ggplot(dt_month_agg, aes(x = month, y = value/1000, group = fuel, color = fuel)) + 
      geom_line(size = 0.5) +
      labs(title = 'Monthly U.S. electricity generation by energy source, all sectors (Jan 1973-May 2020)',
           subtitle = 'Billion Kilowatthours',
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_date(breaks = seq(as.Date('1975-01-01'), as.Date('2020-01-01'), '5 years'), date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,200,50), limits = c(-0.1,210), expand = c(0,0)) +
      scale_color_manual(values = pal_fuel) + 
      geom_dl(aes(label = fuel), method = list(dl.trans(x = x + .3), 'last.bumpup',  cex = 1.2,
                                               fontfamily = 'Secca Soft', fontface = 'plain')) +
      theme_line +
      theme(plot.margin = unit(c(1,8,1,1), "lines"))
    
    fig_line_month = ggplotGrob(fig_line_month)
    fig_line_month$layout$clip[fig_line_month$layout$name == "panel"] = "off"
    
    ggsave(fig_line_month, 
           filename = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_month_Jan1973-May2020_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_month_Jan1973-May2020_lts.pdf'),
                outfile = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_month_Jan1973-May2020_lts.pdf'))
    
    # save as png:
    # ggsave(fig_line_month, 
    #        filename = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_month_Jan1973-May2020_lts_labeled.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
  # line, monthly (labeled) -------
    
    labs_line = dt_month_agg[month == max(month)]
    labs_line = labs_line[order(rank(value))]
    labs_line[, position := value/1000]

    fig_line_month_lab = ggplot(dt_month_agg, aes(x = month, y = value/1000, group = fuel, color = fuel)) + 
      geom_line(size = 0.5) +
      labs(title = 'Monthly U.S. electricity generation by energy source, all sectors (Jan 1973-May 2020)',
           subtitle = 'Billion Kilowatthours',
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_date(breaks = seq(as.Date('1975-01-01'), as.Date('2020-01-01'), '5 years'), date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,200,50), limits = c(-0.1,210), expand = c(0,0)) +
      scale_color_manual(values = pal_fuel) + 
      geom_dl(aes(label = fuel), method = list(dl.trans(x = x + .3), 'last.bumpup',  cex = 1.2,
                                              fontfamily = 'Secca Soft', fontface = 'plain')) +
      geom_segment(x = ymd('2015-04-01'), xend = ymd('2015-04-01'), y = 0, yend = 200, color = 'black', linetype = 2)  +
      annotate('text', x = ymd('2015-04-01'), y = 205, label = 'Natural gas surpassed coal in April 2015', vjust = 0,
               color = '#404040', size = 6, family = 'Secca Soft' ) +
      theme_line +
      theme(plot.margin = unit(c(1,8,1,1), "lines"))
    
    fig_line_month_lab = ggplotGrob(fig_line_month_lab)
    fig_line_month_lab$layout$clip[fig_line_month_lab$layout$name == "panel"] = "off"
    
    ggsave(fig_line_month_lab, 
           filename = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_month_Jan1973-May2020_lts_labeled.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_month_Jan1973-May2020_lts_labeled.pdf'),
                outfile = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_month_Jan1973-May2020_lts_labeled.pdf'))
    
    # save as png:
    # ggsave(fig_line_month, 
    #        filename = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_month_Jan1973-May2020_lts_labeled.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
  # renewable, line, monthly -------
    
    fig_line_month_re = ggplot(dt_month_re, aes(x = month, y = value/1000, group = MSN, color = MSN)) + 
      geom_line(size = 0.5) +
      labs(title = 'Monthly U.S. electricity generation from renewable sources, all sectors (Jan 1973-May 2020)',
           subtitle = 'Billion Kilowatthours', 
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_date(breaks = seq(as.Date('1975-01-01'), as.Date('2020-01-01'), '5 years'), date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,35,5), limits = c(-0.1,35), expand = c(0,0)) +
      scale_color_manual(values = pal_fuel) + 
      geom_dl(aes(label = MSN), method = list(dl.trans(x = x + .3), 'last.bumpup',  cex = 1.2, fontfamily = 'Secca Soft', fontface = 'plain')) +
      theme_line +
      theme(plot.margin = unit(c(1,8,1,1), "lines"))
    
    fig_line_month_re = ggplotGrob(fig_line_month_re)
    fig_line_month_re$layout$clip[fig_line_month_re$layout$name == "panel"] = "off"
    
    ggsave(fig_line_month_re, 
           filename = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_renewables_month_Jan1973-May2020_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_renewables_month_Jan1973-May2020_lts.pdf'),
                outfile = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_renewables_month_Jan1973-May2020_lts.pdf'))
    
    # save as png:
    # ggsave(fig_line_month_re, 
    #        filename = here::here('figures', 'electricity', 'net-generation-by-source_all-sectors_renewables_month_Jan1973-May2020_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
