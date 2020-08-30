#  ---------------------------------------------------- INPUT DATA ----------------------------------------------------

prod.m.file         = 'PET_SUM_SND_D_NUS_MBBL_M_cur.xlsx' 
prod.a.file         = 'PET_SUM_SND_D_NUS_MBBL_A_cur.xlsx' 
price.m.file        = 'PET_PRI_SPT_S1_M.xlsx'
price.a.file        = 'PET_PRI_SPT_S1_A.xlsx'

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
  library(cowplot)

# source color palettes and plot themes ------

  items = list.files(here::here('src'))
  sapply(here::here('src', items), source)

# load data ------
  
  prod_month = as.data.table(read.xlsx(here::here('data', prod.m.file), sheet = 'Data 2', startRow = 3, cols = c(1:2), detectDates = T))
  prod_annual = as.data.table(read.xlsx(here::here('data', prod.a.file), sheet = 'Data 2', startRow = 3, cols = c(1:2), detectDates = T))
  
  price_month = as.data.table(read.xlsx(here::here('data', price.m.file), sheet = 'Data 1', startRow = 3, cols = c(1:2), detectDates = T))
  price_annual = as.data.table(read.xlsx(here::here('data', price.a.file), sheet = 'Data 1', startRow = 3, cols = c(1:2), detectDates = T))
  
# rename columns ----
  
  colnames(prod_month) = c('month', 'field_production')
  colnames(prod_annual) = c('date', 'field_production')
  
  colnames(price_month) = c('month', 'wti')
  colnames(price_annual) = c('date', 'wti')

# merge production and price data -----
  
  dt_month = prod_month[price_month, on = 'month']
  dt_annual = prod_annual[price_annual, on = 'date']

# get year column ------
  
  dt_annual[, year := year(date)]

#  ------------------------------------------------------- FIGURES -------------------------------------------------------
  
  # line, field production vs wti spot price, annual ------
  
    fig_line_annual = ggplot(dt_annual) + 
      geom_line(aes(x = year, y = field_production, col = 'oil', linetype = 'oil'), size = 1.1) +
      geom_line(aes(x = year, y = wti*(5e6/100), col = 'price', linetype = 'price'), size = 1.1) +
      labs(title = 'Annual field production of crude oil vs WTI spot price (1986-2019)',
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL,
           color = 'guide',
           linetype = 'guide') +
      scale_x_continuous(breaks = seq(1986,2019,3), limits = c(1986, 2019), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma,  breaks = seq(0,5e6,1e6), limits = c(0,1.08*5e6), expand = c(0,0),
                           sec.axis = sec_axis( trans=~./(5e6/100), breaks = seq(0,100,20), labels = scales::comma)) +
      scale_color_manual(breaks = c('oil', 'price'),
                         values = c(col_oil, col_rig), 
                         labels = c('Field production of crude oil (thousand barrels)',  'WTI spot price FOB (dollars per barrel)')) +
      scale_linetype_manual(breaks = c('oil', 'price'),
                            values = c(1, 2),  
                            labels = c('Field production of crude oil (thousand barrels)',  'WTI spot price FOB (dollars per barrel)')) +
      annotate("text", label = "Thousand barrels", x = min(dt_annual[, year]), y = 1.08*5e6,
               size = 6.5, fontface = 'plain', family = 'Secca Soft', hjust = 0.65) +
      annotate("text", label = "Dollars per barrel", x = max(dt_annual[, year]), y = 1.08*5e6, 
               size = 6.5, fontface = 'plain', family = 'Secca Soft', hjust = 0.76) +
      guides(color = guide_legend(nrow = 2, title = NULL),
             linetype = guide_legend(title = NULL)) +
      theme_line +
      theme(plot.margin = unit(c(1,2,1,1), "lines"),
            legend.key.width = unit(2.5,"line"))
    
    fig_line_annual = ggplotGrob(fig_line_annual)
    fig_line_annual$layout$clip[fig_line_annual$layout$name == "panel"] = "off"
    
    ggsave(fig_line_annual, 
           filename = here::here('figures', 'oil', 'field-production-vs-wti-spot-price_annual_1986-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'oil', 'field-production-vs-wti-spot-price_annual_1986-2019_lts.pdf'),
                outfile = here::here('figures', 'oil', 'field-production-vs-wti-spot-price_annual_1986-2019_lts.pdf'))
    
    
    
  # line, field production vs wti spot price, monthly ------
    
    fig_line_month = ggplot(dt_month[!is.na(field_production)]) + 
      geom_line(aes(x = month, y = field_production, col = 'oil', linetype = 'oil'), size = 0.5) +
      geom_line(aes(x = month, y = wti*(4e5/160), col = 'price', linetype = 'price'), size = 0.5) +
      labs(title = 'Monthly field production of crude oil vs WTI spot price (Jan 1986-May 2020)',
           caption = 'Data: U.S. Energy Information Administration',
           x = NULL,
           y = NULL,
           color = 'guide',
           linetype = 'guide') +
      scale_x_date(breaks = seq(as.Date('1988-01-15'), as.Date('2020-01-15'), '4 years'), date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma,  breaks = seq(0,4e5,5e4), limits = c(0,1.08*4e5), expand = c(0,0),
                         sec.axis = sec_axis( trans=~./(4e5/160), breaks = seq(0,160,20), labels = scales::comma)) +
      scale_color_manual(breaks = c('oil', 'price'),
                         values = c(col_oil, col_rig), 
                         labels = c('Field production of crude oil (thousand barrels)',  'WTI spot price FOB (dollars per barrel)')) +
      scale_linetype_manual(breaks = c('oil', 'price'),
                            values = c(1, 2),  
                            labels = c('Field production of crude oil (thousand barrels)',  'WTI spot price FOB (dollars per barrel)')) +
      annotate("text", label = "Thousand barrels", x = min(dt_month[, month]), y = 1.08*4e5,
               size = 6.5, fontface = 'plain', family = 'Secca Soft', hjust = 0.55) +
      annotate("text", label = "Dollars per barrel", x = max(dt_month[, month]), y = 1.08*4e5, 
               size = 6.5, fontface = 'plain', family = 'Secca Soft', hjust = 0.76) +
      guides(color = guide_legend(nrow = 2, title = NULL),
             linetype = guide_legend(title = NULL)) +
      theme_line +
      theme(plot.margin = unit(c(1,2,1,1), "lines"),
            legend.key.width = unit(2.5,"line"))
    
    fig_line_month = ggplotGrob(fig_line_month)
    fig_line_month$layout$clip[fig_line_month$layout$name == "panel"] = "off"
    
    ggsave(fig_line_month, 
           filename = here::here('figures', 'oil', 'field-production-vs-wti-spot-price_month_Jan1986-May2020_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'oil', 'field-production-vs-wti-spot-price_month_Jan1986-May2020_lts.pdf'),
                outfile = here::here('figures', 'oil', 'field-production-vs-wti-spot-price_month_Jan1986-May2020_lts.pdf'))
    
    