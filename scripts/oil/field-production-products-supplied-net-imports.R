#  ---------------------------------------------------- INPUT DATA ----------------------------------------------------

month.file        = 'PET_SUM_SND_D_NUS_MBBL_M_cur.xlsx' 
annnual.file      = 'PET_SUM_SND_D_NUS_MBBL_A_cur.xlsx' 

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

  dt_month = as.data.table(read.xlsx(here::here('data', month.file), sheet = 'Data 1', startRow = 3, cols = c(1:2,5,9:10), detectDates = T))
  dt_annual = as.data.table(read.xlsx(here::here('data', annnual.file), sheet = 'Data 1', startRow = 3, cols = c(1:2,5,9:10), detectDates = T))

# rename columns ----
  
  colnames(dt_month) = c('date', 'Field Production', 'Imports', 'Exports', 'Products Supplied' )
  colnames(dt_annual) = c('date', 'Field Production', 'Imports', 'Exports', 'Products Supplied'   )

# calculate net imports ------
  
  dt_month[!is.na(Imports), `Net Imports` := Imports - Exports ]
  dt_annual[!is.na(Imports), `Net Imports` := Imports - Exports ]
  
# melt data table from wide to long format -----
  
  dt_month = melt(dt_month, measure.vars = colnames(dt_month)[2:6],
                  variable.name = 'type', value.name = 'value')
  
  dt_annual = melt(dt_annual, measure.vars = colnames(dt_annual)[2:6],
                   variable.name = 'type', value.name = 'value')

# create year and month columns ------
  
  dt_annual[, year := year(date)]
  
# remove imports and exports -----
  
  dt_month = dt_month[ ! type %in% c('Imports', 'Exports')]
  dt_annual = dt_annual[ ! type %in% c('Imports', 'Exports')]
  
#  ------------------------------------------------------- FIGURES -------------------------------------------------------
  
  # line, annual ------
  
    fig_line_annual = ggplot(dt_annual, aes(x = year, y = value, group = type, color = type)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual U.S. field production, products supplied, and net imports of\ncrude oil and petroleum products (1981-2019)',
           subtitle = 'Thousand barrels',
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1981,2019,5), limits = c(1981, 2019), expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0,8e6,1e6), limits = c(0,8e6), expand = c(0,0), labels = scales::comma) +
      scale_color_manual(values = pal_oil) + 
      geom_dl(aes(label = type), method = list(dl.trans(x = x + .3), 'last.bumpup',  cex = 1.5, fontfamily = 'Secca Soft', fontface = 'plain')) +
      theme_line +
      theme(plot.margin = unit(c(1,10,1,1), "lines"))
    
    fig_line_annual = ggplotGrob(fig_line_annual)
    fig_line_annual$layout$clip[fig_line_annual$layout$name == "panel"] = "off"
    
    ggsave(fig_line_annual, 
           filename = here::here('figures', 'oil', 'field-production-products-supplied-net-imports_annual_1981-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'oil', 'field-production-products-supplied-net-imports_annual_1981-2019_lts.pdf'),
                outfile = here::here('figures', 'oil', 'field-production-products-supplied-net-imports_annual_1981-2019_lts.pdf'))
    
    
  # line, monthly ------
    
    fig_line_month = ggplot(dt_month[date >= '1981-01-01'], aes(x = date, y = value, group = type, color = type)) + 
      geom_line(size = 0.5) +
      labs(title = 'Monthly U.S. field production, products supplied, and net imports of\ncrude oil and petroleum products (Jan 1981-May 2020)',
           subtitle = 'Thousand barrels',
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_date(breaks = seq(as.Date('1985-01-01'), as.Date('2020-01-01'), '5 years'), date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(breaks = seq(-1e5,7e5,1e5), limits = c(-1e5,7e5), expand = c(0,0), labels = scales::comma) +
      scale_color_manual(values = pal_oil) + 
      geom_dl(aes(label = type), method = list(dl.trans(x = x + .3), 'last.bumpup',  cex = 1.5, fontfamily = 'Secca Soft', fontface = 'plain')) +
      theme_line +
      theme(plot.margin = unit(c(1,10,1,1), "lines"))
    
    fig_line_month = ggplotGrob(fig_line_month)
    fig_line_month$layout$clip[fig_line_month$layout$name == "panel"] = "off"
    
    ggsave(fig_line_month, 
           filename = here::here('figures', 'oil', 'field-production-products-supplied-net-imports_month_Jan1981-May2020_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'oil', 'field-production-products-supplied-net-imports_month_Jan1981-May2020_lts.pdf'),
                outfile = here::here('figures', 'oil', 'field-production-products-supplied-net-imports_month_Jan1981-May2020_lts.pdf'))
    