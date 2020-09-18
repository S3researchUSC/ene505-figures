# plots generated:
  # 
  #
  #
  #
  #

# ------------------------------------ inputs ------------------------------------

  gdp_file      = "international-gdp-by-country-1980-2019.csv"

# ------------------------------------ outputs ------------------------------------
  
  out_gdp_2019  = "gdp-by-country-ranked-2019.csv"
  
# ------------------------------------ main script ------------------------------------
  
# load libraries -------

  library(data.table)
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
  
  # read in csv (without API column)
    gdp_data = fread(here::here('data', gdp_file), skip = 1, select = 2:42, header = T)
  
  # rename first column name
    setnames(gdp_data, 'V2', 'country')

  # extract first row as unit
    unit = gdp_data[1, country]
  
  # remove first row
    gdp_data = gdp_data[ ! country == unit ]
    
  # remove whitespace from country names
    gdp_data[, country := trimws(country, which = 'both')]
    
# melt data from wide to long format -----
    
  gdp_long = melt(gdp_data, measure.vars = as.character(1980:2019),
                  variable.name = 'year', value.name = 'gdp_billion_usd')
  
# convert year and gdp values to numeric type -----
    
  gdp_long[, year := as.numeric(as.character(year))]
  gdp_long[, gdp_billion_usd := as.numeric(gdp_billion_usd)]
  
# get top ten GDP in latest year -----
  
  gdp_most_recent = gdp_long[year == max(year) & ! country == 'World']
  setorder(gdp_most_recent, -'gdp_billion_usd', na.last = T)
  gdp_most_recent[, rank := 1:nrow(gdp_most_recent)]
  
  top_10_gdp = gdp_most_recent[rank %in% 1:10]
  
# export ranked gdp in 2019 to csv file ----
  
  fwrite(gdp_most_recent, here::here('data', out_gdp_2019), row.names = F)
  
# ------------------------------------ figures ------------------------------------
  
  # line, annual -----
    
    labs_line = gdp_long[country %in% top_10_gdp[, country] & year == max(year)]
    labs_line = labs_line[order(rank(gdp_billion_usd))]
    labs_line[, position := gdp_billion_usd]
    labs_line[1:7, position := seq(1200,7100,length.out = 7)]
    # labs_line[is.na(position), position := value/1000]
  
    line_gdp = ggplot(gdp_long[country %in% top_10_gdp[, country]], aes(x = year, y = gdp_billion_usd, color = country)) + geom_line() +
      geom_line(size = 0.9) +
      labs(title = 'Annual gross domestic product (GDP) at purchasing power parities (1980-2019)',
           subtitle = 'Billion 2015$ PPP', 
           caption = 'Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1980,2019,5), limits = c(1980, 2019), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
      scale_color_manual(values = pal_country) + 
      theme_line +
      geom_text(data = labs_line, aes(x = Inf, y = position, label = paste0(' ', country), color = country), hjust = 0,
                size = 5.6, fontface = 'plain', family = 'Secca Soft') +
      theme(plot.margin = unit(c(1,8,1,1), "lines"))
  
    line_gdp = ggplotGrob(line_gdp)
    line_gdp$layout$clip[line_gdp$layout$name == "panel"] = "off"
    
    ggsave(line_gdp, 
           filename = here::here('figures', 'world', 'gdp-by-country_annual_1980-2019_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'gdp-by-country_annual_1980-2019_lts.pdf'),
                outfile = here::here('figures', 'world', 'gdp-by-country_annual_1980-2019_lts.pdf'))
  
    