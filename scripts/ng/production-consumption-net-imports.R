#  ---------------------------------------------------- INPUT DATA ----------------------------------------------------

prod.file     = 'Table_1.2_Primary_Energy_Production_by_Source.xlsx' 
cons.file     = 'Table_1.3_Primary_Energy_Consumption_by_Source.xlsx' 
ng.file       = 'Table_4.1_Natural_Gas_Overview.xlsx'

# load libraries -------

  library(data.table)
  library(openxlsx)
  library(stringr)
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
  
  prod_month = as.data.table(read.xlsx(here::here('data', prod.file), sheet = 'Monthly Data', startRow = 11, cols = c(1,3), detectDates = T))
  prod_month = prod_month[2:nrow(prod_month)]
  
  prod_annual = as.data.table(read.xlsx(here::here('data', prod.file), sheet = 'Annual Data', startRow = 11, cols = c(1,3), detectDates = T))
  prod_annual = prod_annual[2:nrow(prod_annual)]
  
  cons_month = as.data.table(read.xlsx(here::here('data', cons.file), sheet = 'Monthly Data', startRow = 11, cols = c(1,3), detectDates = T))
  cons_month = cons_month[2:nrow(cons_month)]
  
  cons_annual = as.data.table(read.xlsx(here::here('data', cons.file), sheet = 'Annual Data', startRow = 11, cols = c(1,3), detectDates = T))
  cons_annual = cons_annual[2:nrow(cons_annual)]
  
  month = as.data.table(read.xlsx(here::here('data', ng.file), sheet = 'Monthly Data', startRow = 11, cols = c(1,5,9,12), detectDates = T))
  month = month[2:nrow(month)]
  
  annual = as.data.table(read.xlsx(here::here('data', ng.file), sheet = 'Annual Data', startRow = 11, cols = c(1,5,9,12), detectDates = T))
  annual = annual[2:nrow(annual)]
  
# rename columns ----
  
  colnames(prod_month) = c('month', 'Dry Production')
  colnames(prod_annual) = c('year', 'Dry Production')
  
  colnames(cons_month) = c('month', 'Consumption')
  colnames(cons_annual) = c('year','Consumption')
  
  colnames(month) = c('month', 'Dry Production', 'Net Imports', 'Consumption')
  colnames(annual) = c('year', 'Dry Production', 'Net Imports', 'Consumption')

# merge primary energy production and consumption data -----
  
  pe_month = prod_month[cons_month, on = 'month']
  pe_annual = prod_annual[cons_annual, on = 'year']
  
# melt data table from wide to long format -----
  
  pe_month = melt(pe_month, measure.vars = colnames(pe_month)[2:3],
                  variable.name = 'type', value.name = 'value')
  
  pe_annual = melt(pe_annual, measure.vars = colnames(pe_annual)[2:3],
                   variable.name = 'type', value.name = 'value')
  
  month = melt(month, measure.vars = colnames(month)[2:4],
                  variable.name = 'type', value.name = 'value')
  
  annual = melt(annual, measure.vars = colnames(annual)[2:4],
                   variable.name = 'type', value.name = 'value')
  
# convert value column to numeric ----
  
  pe_month[, value := as.numeric(value)]
  pe_annual[, value := as.numeric(value)]
  month[, value := as.numeric(value)]
  annual[, value := as.numeric(value)]
  
#  ------------------------------------------------------- FIGURES -------------------------------------------------------
  
  # line, primary, annual ------
  
    fig_line_pe_annual = ggplot(pe_annual, aes(x = year, y = value, group = type, color = type)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual U.S. natural gas consumption and production (1949-2019)',
           subtitle = 'Quadrillion BTU',
           caption = 'Using primary energy consumption and production values. Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,35,5), limits = c(0,35), expand = c(0,0)) +
      scale_color_manual(values = pal_ng) + 
      geom_dl(aes(label = type), method = list(dl.trans(x = x + .3), 'last.bumpup',  cex = 1.5, fontfamily = 'Secca Soft', fontface = 'plain')) +
      theme_line +
      theme(plot.margin = unit(c(1,9,1,1), "lines"))
    
    fig_line_pe_annual = ggplotGrob(fig_line_pe_annual)
    fig_line_pe_annual$layout$clip[fig_line_pe_annual$layout$name == "panel"] = "off"
    
    ggsave(fig_line_pe_annual, 
           filename = here::here('figures', 'ng', 'primary-energy-production-and-consumption-annual_1949-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'ng', 'primary-energy-production-and-consumption-annual_1949-2019_lts.pdf'),
                outfile = here::here('figures', 'ng', 'primary-energy-production-and-consumption-annual_1949-2019_lts.pdf'))
    
    # save as png:
    # ggsave(fig_line_month, 
    #        filename = here::here('figures', 'ng', 'primary-energy-production-and-consumption-annual_1949-2019_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
  # line, primary, monthly ------
    
    fig_line_pe_month = ggplot(pe_month, aes(x = month, y = value, group = type, color = type)) + 
      geom_line(size = 0.5) +
      labs(title = 'Monthly U.S. natural gas consumption and production (Jan 1973-May 2020)',
           subtitle = 'Quadrillion BTU',
           caption = 'Using primary energy consumption and production values. Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_date(breaks = seq(as.Date('1975-01-01'), as.Date('2020-01-01'), '5 years'), date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,3.5,0.5), limits = c(0,3.6), expand = c(0,0)) +
      scale_color_manual(values = pal_ng) + 
      geom_dl(aes(label = type), method = list(dl.trans(x = x + .3), 'last.bumpup',  cex = 1.5, fontfamily = 'Secca Soft', fontface = 'plain')) +
      theme_line +
      theme(plot.margin = unit(c(1,9,1,1), "lines"))
    
    fig_line_pe_month = ggplotGrob(fig_line_pe_month)
    fig_line_pe_month$layout$clip[fig_line_pe_month$layout$name == "panel"] = "off"
    
    ggsave(fig_line_pe_month, 
           filename = here::here('figures', 'ng', 'primary-energy-production-and-consumption-month_Jan1973-May2020_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'ng', 'primary-energy-production-and-consumption-month_Jan1973-May2020_lts.pdf'),
                outfile = here::here('figures', 'ng', 'primary-energy-production-and-consumption-month_Jan1973-May2020_lts.pdf'))
    
    # save as png:
    # ggsave(fig_line_month, 
    #        filename = here::here('figures', 'ng', 'primary-energy-production-and-consumption-month_Jan1973-May2020_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
    
    
    
  # line, overview, annual ------
    
    fig_line_annual = ggplot(annual, aes(x = year, y = value, group = type, color = type)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual U.S. natural gas consumption, production, and net imports (1949-2019)',
           subtitle = 'Billion Cubic Feet',
           caption = 'Using natural gas overview values. Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1949,2019,5), limits = c(1949, 2019), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(-5000, 35000, 5000), limits = c(-5000,35000), expand = c(0,0), labels = scales::comma) +
      scale_color_manual(values = pal_ng) + 
      geom_dl(aes(label = type), method = list(dl.trans(x = x + .3), 'last.bumpup',  cex = 1.5, fontfamily = 'Secca Soft', fontface = 'plain')) +
      theme_line +
      theme(plot.margin = unit(c(1,9,1,1), "lines"))
    
    fig_line_annual = ggplotGrob(fig_line_annual)
    fig_line_annual$layout$clip[fig_line_annual$layout$name == "panel"] = "off"
    
    ggsave(fig_line_annual, 
           filename = here::here('figures', 'ng', 'overview-production-consumption-net-imports-annual_1949-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'ng', 'overview-production-consumption-net-imports-annual_1949-2019_lts.pdf'),
                outfile = here::here('figures', 'ng', 'overview-production-consumption-net-imports-annual_1949-2019_lts.pdf'))
    
    # save as png:
    # ggsave(fig_line_annual, 
    #        filename = here::here('figures', 'ng', 'overview-production-consumption-net-imports-annual_1949-2019_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
    
    
  # line, overview, monthly ------
    
    fig_line_month = ggplot(month, aes(x = month, y = value, group = type, color = type)) + 
      geom_line(size = 0.5) +
      labs(title = 'Monthly U.S. natural gas consumption, production, and net imports (Jan 1973-May 2020)',
           subtitle = 'Billion Cubic Feet',
           caption = 'Using natural gas overview values. Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_date(breaks = seq(as.Date('1975-01-01'), as.Date('2020-01-01'), '5 years'), date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(breaks = seq(-500, 3500, 500), limits = c(-500,3500), expand = c(0,0), labels = scales::comma) +
      scale_color_manual(values = pal_ng) + 
      geom_dl(aes(label = type), method = list(dl.trans(x = x + .3), 'last.bumpup',  cex = 1.5, fontfamily = 'Secca Soft', fontface = 'plain')) +
      theme_line +
      theme(plot.margin = unit(c(1,9,1,1), "lines"))
    
    fig_line_month = ggplotGrob(fig_line_month)
    fig_line_month$layout$clip[fig_line_month$layout$name == "panel"] = "off"
    
    ggsave(fig_line_month, 
           filename = here::here('figures', 'ng', 'overview-production-consumption-net-imports-month_Jan1973-May2020_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'ng', 'overview-production-consumption-net-imports-month_Jan1973-May2020_lts.pdf'),
                outfile = here::here('figures', 'ng', 'overview-production-consumption-net-imports-month_Jan1973-May2020_lts.pdf'))
    
    # save as png:
    # ggsave(fig_line_month, 
    #        filename = here::here('figures', 'ng', 'overview-production-consumption-net-imports-month_Jan1973-May2020_lts.png'), 
    #        width = 11.5, 
    #        height = 6.25, 
    #        dpi = 600)
    
    