#  ---------------------------------------------------- INPUT DATA ----------------------------------------------------

prod.month.file         = 'NG_PROD_SUM_DC_NUS_MMCF_M.xlsx' 
prod.annual.file        = 'NG_PROD_SUM_DC_NUS_MMCF_A.xlsx' 
price.month.file        = 'NG_PRI_SUM_DCU_NUS_M.xlsx' 
price.annual.file       = 'NG_PRI_SUM_DCU_NUS_A.xlsx' 

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

# source color palettes and plot themes ------

  items = list.files(here::here('src'))
  sapply(here::here('src', items), source)
  
# load data ------
  
  prod_month = as.data.table(read.xlsx(here::here('data', prod.month.file), sheet = 'Data 1', startRow = 3, cols = c(1,2,12), detectDates = T))
  prod_annual = as.data.table(read.xlsx(here::here('data', prod.annual.file), sheet = 'Data 1', startRow = 3, cols = c(1,2,12), detectDates = T))
  
  price_month = as.data.table(read.xlsx(here::here('data', price.month.file), sheet = 'Data 1', startRow = 3, cols = c(1,2,9), detectDates = T))
  price_annual = as.data.table(read.xlsx(here::here('data', price.annual.file), sheet = 'Data 1', startRow = 3, cols = c(1,2,10), detectDates = T))
  
# merge data tables -----
  
  dt_month = prod_month[price_month, on = 'Date']
  dt_annual = prod_annual[price_annual, on = 'Date']
  
# rename columns ----
  
  colnames(dt_month) = c('date', 'gross_withdrawals', 'dry_production', 'wellhead_price', 'citygate_price')
  colnames(dt_annual) = c('date', 'gross_withdrawals', 'dry_production', 'wellhead_price', 'citygate_price')
  
# set start and end dates ------
  
  dt_month = dt_month[ date >= '1980-01-15' & date <= '2020-05-15' ]
  dt_annual = dt_annual[ date >= '1936-06-30' & date <= '2019-06-30' ]
  
# create year and month columns ------
  
  dt_annual[, year := year(date)]
  
#  ------------------------------------------------------- FIGURES -------------------------------------------------------
  
  
  # line, dry prod vs citygate price, annual ------
  
    fig_line_dry_city_annual = ggplot(dt_annual) + 
      geom_line(aes(x = year, y = dry_production, col = 'prod', linetype = 'prod'), size = 1.2) +
      geom_line(aes(x = year, y = citygate_price*(40e6/9), col = 'price', linetype = 'price'), size = 1.2) +
      labs(title = 'Annual dry natural gas production versus Citygate price (1984-2019)',
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL,
           color = 'guide',
           linetype = 'guide') +
      scale_x_continuous(breaks = seq(1984,2019,5), limits = c(1984,2019), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, 
                         # name = 'Thousand cubic feet per day', 
                         breaks = seq(0,40e6,4e6), limits = c(0,1.3*max(dt_annual[, dry_production])),
                           sec.axis = sec_axis( trans=~./(40e6/9), 
                                                # name = 'Number of rigs', 
                                                breaks = seq(0,9,1))) +
      scale_color_manual(breaks = c('prod', 'price'), 
                         values = c(col_ng, col_rig), 
                         labels = c('Dry natural gas production (million cubic feet)', 'Citygate price (dollars per thousand cubic feet)')) +
      scale_linetype_manual(breaks = c('prod', 'price'), 
                            values = c(1, 2),  
                            labels = c('Dry natural gas production (million cubic feet)', 'Citygate price (dollars per thousand cubic feet)')) +
      annotate("text", label = "Million cubic feet", x = 1984, y = 1.3*max(dt_annual[, dry_production]), 
               size = 6.5, fontface = 'plain', family = 'Secca Soft', hjust = 0.74) +
      annotate("text", label = "Dollars per thousand cubic feet", x = 2019, y = 1.3*max(dt_annual[, dry_production]), 
               size = 6.5, fontface = 'plain', family = 'Secca Soft', hjust = 0.9) +
      guides(color = guide_legend(nrow = 2, title = NULL),
             linetype = guide_legend(title = NULL)) +
      theme_line +
      theme(plot.margin = unit(c(1,2,1,1), "lines"),
            legend.key.width = unit(2.5,"line"))
  
    fig_line_dry_city_annual = ggplotGrob(fig_line_dry_city_annual)
    fig_line_dry_city_annual$layout$clip[fig_line_dry_city_annual$layout$name == "panel"] = "off"
  
    ggsave(fig_line_dry_city_annual, 
           filename = here::here('figures', 'ng-and-oil', 'ng_dry-production-vs-citygate-price_annual_1984-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'ng-and-oil', 'ng_dry-production-vs-citygate-price_annual_1984-2019_lts.pdf'),
                outfile = here::here('figures', 'ng-and-oil', 'ng_dry-production-vs-citygate-price_annual_1984-2019_lts.pdf'))
  
  # line, dry prod vs citygate price, month ------
    
    fig_line_dry_city_month = ggplot(dt_month[date >= '1997-01-15']) + 
      geom_line(aes(x = date, y = dry_production, col = 'prod', linetype = 'prod'), size = 0.5) +
      geom_line(aes(x = date, y = citygate_price*(3e6/15), col = 'price', linetype = 'price'), size = 0.5) +
      labs(title = 'Monthly dry natural gas production versus Citygate price (Jan 1997-May 2020)',
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL,
           color = 'guide',
           linetype = 'guide') +
      scale_x_date(breaks = seq(as.Date('1999-01-15'), as.Date('2020-05-15'), '2 years'), date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, 
                         breaks = seq(0,3e6,6e5), limits = c(0,1.1*max(dt_month[, dry_production], na.rm = T)),
                         sec.axis = sec_axis( trans=~./(3e6/15), 
                                              breaks = seq(0,15,3))) +
      scale_color_manual(breaks = c('prod', 'price'), 
                         values = c(col_ng, col_rig), 
                         labels = c('Dry natural gas production (million cubic feet)', 'Citygate price (dollars per thousand cubic feet)')) +
      scale_linetype_manual(breaks = c('prod', 'price'), 
                            values = c(1, 2),  
                            labels = c('Dry natural gas production (million cubic feet)', 'Citygate price (dollars per thousand cubic feet)')) +
      annotate("text", label = "Million cubic feet", x = as.Date('1997-01-15'), y = 1.1*max(dt_month[, dry_production], na.rm = T), 
               size = 6.5, fontface = 'plain', family = 'Secca Soft', hjust = 0.65) +
      annotate("text", label = "Dollars per thousand cubic feet", x = as.Date('2020-05-15'), y = 1.1*max(dt_month[, dry_production], na.rm = T), 
               size = 6.5, fontface = 'plain', family = 'Secca Soft', hjust = 0.9) +
      guides(color = guide_legend(nrow = 2, title = NULL),
             linetype = guide_legend(title = NULL)) +
      theme_line  +
      theme(plot.margin = unit(c(1,2,1,1), "lines"),
            legend.key.width = unit(2.5,"line"))
    
    fig_line_dry_city_month = ggplotGrob(fig_line_dry_city_month)
    fig_line_dry_city_month$layout$clip[fig_line_dry_city_month$layout$name == "panel"] = "off"
    
    ggsave(fig_line_dry_city_month, 
           filename = here::here('figures', 'ng-and-oil', 'ng_dry-production-vs-citygate-price_month_Jan1997-May2020_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'ng-and-oil', 'ng_dry-production-vs-citygate-price_month_Jan1997-May2020_lts.pdf'),
                outfile = here::here('figures', 'ng-and-oil', 'ng_dry-production-vs-citygate-price_month_Jan1997-May2020_lts.pdf'))
    
    
    
  # line, gross withdrawal vs wellhead price, annual ------
    
    fig_line_with_wellhead_annual = ggplot(dt_annual) + 
      geom_line(aes(x = year, y = gross_withdrawals, col = 'prod', linetype = 'prod'), size = 1.2) +
      geom_line(aes(x = year, y = wellhead_price*(40e6/8), col = 'price', linetype = 'price'), size = 1.2) +
      labs(title = 'Annual natural gas gross withdrawals versus wellhead price (1936-2019)',
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL,
           color = 'guide',
           linetype = 'guide') +
      scale_x_continuous(breaks = seq(1939,2019,5), limits = c(1936,2019), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, 
                         breaks = seq(0,40e6,5e6), limits = c(0,1.07*max(dt_annual[, gross_withdrawals])),
                         sec.axis = sec_axis( trans=~./(40e6/8), 
                                              breaks = seq(0,8,1))) +
      scale_color_manual(breaks = c('prod', 'price'), 
                         values = c(col_ng, col_rig), 
                         labels = c('Natural gas gross withdrawals (million cubic feet)', 'Wellhead price (dollars per thousand cubic feet)')) +
      scale_linetype_manual(breaks = c('prod', 'price'), 
                            values = c(1, 2),  
                            labels = c('Natural gas gross withdrawals (million cubic feet)', 'Wellhead price (dollars per thousand cubic feet)')) +
      annotate("text", label = "Million cubic feet", x = 1936, y = 1.07*max(dt_annual[, gross_withdrawals]), 
               size = 6.5, fontface = 'plain', family = 'Secca Soft', hjust = 0.74) +
      annotate("text", label = "Dollars per thousand cubic feet", x = 2019, y = 1.07*max(dt_annual[, gross_withdrawals]), 
               size = 6.5, fontface = 'plain', family = 'Secca Soft', hjust = 0.9) +
      guides(color = guide_legend(nrow = 2, title = NULL),
             linetype = guide_legend(title = NULL)) +
      theme_line +
      theme(plot.margin = unit(c(1,2,1,1), "lines"),
            legend.key.width = unit(2.5,"line"))
    
    fig_line_with_wellhead_annual = ggplotGrob(fig_line_with_wellhead_annual)
    fig_line_with_wellhead_annual$layout$clip[fig_line_with_wellhead_annual$layout$name == "panel"] = "off"
    
    ggsave(fig_line_with_wellhead_annual, 
           filename = here::here('figures', 'ng-and-oil', 'ng_gross-withdrawals-vs-wellhead-price_annual_1936-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'ng-and-oil', 'ng_gross-withdrawals-vs-wellhead-price_annual_1936-2019_lts.pdf'),
                outfile = here::here('figures', 'ng-and-oil', 'ng_gross-withdrawals-vs-wellhead-price_annual_1936-2019_lts.pdf'))
    
    
    
  # line, dry gross withdrawal vs wellhead price, month ------
    
    fig_line_with_wellhead_month = ggplot(dt_month) + 
      geom_line(aes(x = date, y = gross_withdrawals, col = 'prod', linetype = 'prod'), size = 0.5) +
      geom_line(aes(x = date, y = wellhead_price*(3.5e6/10), col = 'price', linetype = 'price'), size = 0.5) +
      labs(title = 'Monthly natural gas gross withdrawals versus wellhead price (Jan 1980-May 2020)',
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL,
           color = 'guide',
           linetype = 'guide') +
      scale_x_date(breaks = seq(as.Date('1980-01-15	'), as.Date('2020-05-15'), '4 years'), date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, 
                         breaks = seq(0,3.5e6,7e5), limits = c(0,1.08*max(dt_month[, gross_withdrawals], na.rm = T)),
                         sec.axis = sec_axis( trans=~./(3.5e6/10), 
                                              breaks = seq(0,10,2))) +
      scale_color_manual(breaks = c('prod', 'price'), 
                         values = c(col_ng, col_rig), 
                         labels = c('Natural gas gross withdrawals (million cubic feet)', 'Wellhead price (dollars per thousand cubic feet)')) +
      scale_linetype_manual(breaks = c('prod', 'price'), 
                            values = c(1, 2),  
                            labels = c('Natural gas gross withdrawals (million cubic feet)', 'Wellhead price (dollars per thousand cubic feet)')) +
      annotate("text", label = "Million cubic feet", x = as.Date('1980-01-15'), y = 1.08*max(dt_month[, gross_withdrawals], na.rm = T), 
               size = 6.5, fontface = 'plain', family = 'Secca Soft', hjust = 0.65) +
      annotate("text", label = "Dollars per thousand cubic feet", x = as.Date('2020-05-15'), y = 1.08*max(dt_month[, gross_withdrawals], na.rm = T), 
               size = 6.5, fontface = 'plain', family = 'Secca Soft', hjust = 0.9) +
      guides(color = guide_legend(nrow = 2, title = NULL),
             linetype = guide_legend(title = NULL)) +
      theme_line  +
      theme(plot.margin = unit(c(1,2,1,1), "lines"),
            legend.key.width = unit(2.5,"line"))
    
    fig_line_with_wellhead_month = ggplotGrob(fig_line_with_wellhead_month)
    fig_line_with_wellhead_month$layout$clip[fig_line_with_wellhead_month$layout$name == "panel"] = "off"
    
    ggsave(fig_line_with_wellhead_month, 
           filename = here::here('figures', 'ng-and-oil', 'ng_gross-withdrawals-vs-wellhead-price_month_Jan1980-May2020_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'ng-and-oil', 'ng_gross-withdrawals-vs-wellhead-price_month_Jan1980-May2020_lts.pdf'),
                outfile = here::here('figures', 'ng-and-oil', 'ng_gross-withdrawals-vs-wellhead-price_month_Jan1980-May2020_lts.pdf'))
    
