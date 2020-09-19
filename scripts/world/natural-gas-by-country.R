# ------------------------------------ inputs ------------------------------------

  ng_file       = 'international-natural-gas-by-country-1980-2020.csv'

# ------------------------------------ main script ------------------------------------

# load libraries -------

  library(data.table)
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
  
  # read in csv of ng data
    ng_data = fread(here::here('data', ng_file), skip = 1, header = T)
  
  # rename V2 column
    setnames(ng_data, 'V2', 'country')
  
  # remove whitespace from country names
    ng_data[, country := trimws(country, which = 'both')]

# melt data from wide to long format -----
  
  ng_long = melt(ng_data, measure.vars = as.character(1980:2020), variable.name = 'year', value.name = 'value')
  
# convert year and values to numeric type -----
  
  ng_long[, year := as.numeric(as.character(year))]
  ng_long[, value := as.numeric(value)]
  
# categorize values by type using API -----
  
  ng_long[API %like% 'INTL.26-1', type := 'production']
  ng_long[API %like% 'INTL.26-2', type := 'consumption']
  ng_long[API %like% 'INTL.26-3', type := 'imports']
  ng_long[API %like% 'INTL.26-4', type := 'exports']
  ng_long[API %like% 'INTL.3-6', type := 'reserves']
  
# add units -----
  
  ng_long[, unit := ifelse(type == 'reserves', 'trillion cubic feet', 'billion cubic feet')]
  
# remove rows that are just labels from the raw data ----
  
  ng_long = ng_long[!is.na(type)]
  
# rank values in each category (production, consumption, imports, exports, and reserves) in 2018 ------
  
  ng_max_year = ng_long[!is.na(value) & ! country == 'World']
  ng_max_year = ng_max_year[year == 2017]
  ng_max_year[, rank := frank(-value), by = 'type']
  rank_country = unique(ng_max_year[, c('country', 'type', 'rank')])
  
# merge with main coal data ----
  
  ng_ranked = merge(ng_long, rank_country, by = c('country', 'type'), nomatch = 0)
  
# label all non-top 5 countries as "Other" (for exports, just top 2) -----
  
  ng_ranked[, label := ifelse(rank %in% 1:5, country, 'All Other Countries')]

# aggregate by new country grouping -----
  
  ng_agg = ng_ranked[, .(value = sum(value, na.rm = T)), by = .(year, label, type, unit)]
  setcolorder(ng_agg, c('year', 'type', 'label', 'value', 'unit'))
  
# calculate proportion (or percentage) contributed by type for each year -----
  
  ng_agg[, prop := value/sum(value), by = c('type', 'year')]
  
# get top 5 for each type -----
  
  top5_prod = rank_country[type == 'production' & rank %in% 1:5][order(rank), country]
  top5_cons = rank_country[type == 'consumption' & rank %in% 1:5][order(rank), country]
  top5_imp = rank_country[type == 'imports' & rank %in% 1:5][order(rank), country]
  top5_exp = rank_country[type == 'exports' & rank %in% 1:5][order(rank), country]
  top5_res = rank_country[type == 'reserves' & rank %in% 1:5][order(rank), country]

# ------------------------------------ figures ------------------------------------
  
  # palettes -----
  
    pal_prod = pal_5countries
    names(pal_prod)[2:6] = top5_prod
    
    pal_cons = pal_5countries
    names(pal_cons)[2:6] = top5_cons
    
    pal_imp = pal_5countries
    names(pal_imp)[2:6] = top5_imp
    
    pal_exp = pal_5countries[1:6]
    names(pal_exp)[2:6] = top5_exp
    
    pal_res = pal_5countries
    names(pal_res)[2:6] = top5_res
    
  # line, production -----
    
    labs_line_prod = ng_agg[type == 'production' & year == 2017]
    setorder(labs_line_prod, 'value')
    labs_line_prod[, position := value]
    labs_line_prod[1:3, position := c(4400,6900,9150)]
    
    line_prod = ggplot(ng_agg[type == 'production'], aes(x = year, y = value, color = label)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual dry natural gas production by country (1980-2017)',
           subtitle = 'Billion cubic feet', 
           caption = 'Top 5 natural gas producing countries in 2017 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1980,2018,5), limits = c(1980,2017), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0), breaks = seq(0,60e3,5e3)) +
      scale_color_manual(values = pal_prod) + 
      theme_line +
      geom_text(data = labs_line_prod, aes(x = Inf, y = position, label = paste0(' ', label), color = label), hjust = 0,
                size = 6.1, fontface = 'plain', family = 'Secca Soft')
    
    line_prod = ggplotGrob(line_prod)
    line_prod$layout$clip[line_prod$layout$name == "panel"] = "off"
    
    ggsave(line_prod, 
           filename = here::here('figures', 'world', 'natural-gas-production-by-country_annual_1980-2017_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'natural-gas-production-by-country_annual_1980-2017_lts.pdf'),
                outfile = here::here('figures', 'world', 'natural-gas-production-by-country_annual_1980-2017_lts.pdf'))

  # area, production (absolute) -----
    
    labs_area_prod = ng_agg[type == 'production' & year == 2017][order(factor(label, levels = rev(c('All Other Countries', rev(top5_prod)))))]
    labs_area_prod[, cum_sum := cumsum(value)] 
    labs_area_prod[, difference := diff(c(0,cum_sum))/2]
    labs_area_prod[, position := cum_sum - difference]
    # labs_area_prod[4:6, position := c(65400, 68800,71900)]
    
    area_prod = ggplot(ng_agg[type == 'production'], 
                       aes(x = year, y = value, fill = factor(label, levels = c('All Other Countries', rev(top5_prod))))) + 
      geom_area() +
      labs(title = 'Annual dry natural gas production by country (1980-2017)',
           subtitle = 'Billion cubic feet', 
           caption = 'Top 5 natural gas producing countries in 2017 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1980,2018,5), limits = c(1980,2017), expand = c(0,0)) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0), breaks = seq(0,130e3,10e3)) +
      guides(fill = 'none',
             color = 'none') +
      scale_color_manual(values = pal_prod) + 
      scale_fill_manual(values = pal_prod) + 
      theme_area_labeled +
      geom_text(data = labs_area_prod, aes(x = Inf, y = position, label = paste0(' ', label), color = label), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')
    
    area_prod = ggplotGrob(area_prod)
    area_prod$layout$clip[area_prod$layout$name == "panel"] = "off"
    
    ggsave(area_prod, 
           filename = here::here('figures', 'world', 'natural-gas-production-by-country_annual_1980-2017_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'natural-gas-production-by-country_annual_1980-2017_ats_absolute.pdf'),
                outfile = here::here('figures', 'world', 'natural-gas-production-by-country_annual_1980-2017_ats_absolute.pdf'))
    
    
  # area, production (proportion) -----
    
    labs_area_prod_prop = ng_agg[type == 'production' & year == 2017][order(factor(label, levels = rev(c('All Other Countries', rev(top5_prod)))))]
    labs_area_prod_prop[, cum_sum := cumsum(prop)] 
    labs_area_prod_prop[, difference := diff(c(0,cum_sum))/2]
    labs_area_prod_prop[, position := cum_sum - difference]
    # labs_area_prod_prop[4:6, position := c(0.904, 0.947, 0.99)]
    
    area_prod_prop = ggplot(ng_agg[type == 'production'], 
                            aes(x = year, y = prop, fill = factor(label, levels = c('All Other Countries', rev(top5_prod))))) + 
      geom_area() +
      labs(title = 'Annual dry natural gas production by country (1980-2017)',
           subtitle = 'Share of global dry natural gas production', 
           caption = 'Top 5 natural gas producing countries in 2017 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1980,2019,5), limits = c(1980,2017), expand = c(0,0)) +
      scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
      guides(fill = 'none',
             color = 'none') +
      scale_color_manual(values = pal_prod) + 
      scale_fill_manual(values = pal_prod) + 
      theme_area_labeled +
      geom_text(data = labs_area_prod_prop, aes(x = Inf, y = position, label = paste0(' ', label), color = label), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')
    
    area_prod_prop = ggplotGrob(area_prod_prop)
    area_prod_prop$layout$clip[area_prod_prop$layout$name == "panel"] = "off"
    
    ggsave(area_prod_prop, 
           filename = here::here('figures', 'world', 'natural-gas-production-by-country_annual_1980-2017_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'natural-gas-production-by-country_annual_1980-2017_ats_proportion.pdf'),
                outfile = here::here('figures', 'world', 'natural-gas-production-by-country_annual_1980-2017_ats_proportion.pdf'))
    
  # line, consumption -----
    
    labs_line_cons = ng_agg[type == 'consumption' & year == 2017]
    setorder(labs_line_cons, 'value')
    labs_line_cons[, position := value]
    labs_line_cons[1:3, position := c(3000, 6500, 10000)]
    
    line_cons = ggplot(ng_agg[type == 'consumption'], aes(x = year, y = value, color = label)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual dry natural gas consumption by country (1980-2017)',
           subtitle = 'Billion cubic feet', 
           caption = 'Top 5 natural gas consuming countries in 2017 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1980,2019,5), limits = c(1980,2017), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), breaks = seq(0,70e3,10e3), limits = c(0,70e3), labels = scales::comma) +
      scale_color_manual(values = pal_cons) + 
      theme_line +
      geom_text(data = labs_line_cons, aes(x = Inf, y = position, label = paste0(' ', label), color = label), hjust = 0,
                size = 6.5, fontface = 'plain', family = 'Secca Soft')
    
    line_cons = ggplotGrob(line_cons)
    line_cons$layout$clip[line_cons$layout$name == "panel"] = "off"
    
    ggsave(line_cons, 
           filename = here::here('figures', 'world', 'natural-gas-consumption-by-country_annual_1980-2017_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'natural-gas-consumption-by-country_annual_1980-2017_lts.pdf'),
                outfile = here::here('figures', 'world', 'natural-gas-consumption-by-country_annual_1980-2017_lts.pdf'))
    
  # area, consumption (absolute) -----
    
    labs_area_cons = ng_agg[type == 'consumption' & year == 2017][order(factor(label, levels = rev(c('All Other Countries', rev(top5_cons)))))]
    labs_area_cons[, cum_sum := cumsum(value)] 
    labs_area_cons[, difference := diff(c(0,cum_sum))/2]
    labs_area_cons[, position := cum_sum - difference]
    # labs_area_cons[6, position := 52e3]
    
    area_cons = ggplot(ng_agg[type == 'consumption'], 
                       aes(x = year, y = value, fill = factor(label, levels = c('All Other Countries', rev(top5_cons))))) + 
      geom_area() +
      labs(title = 'Annual dry natural gas consumption by country (1980-2017)',
           subtitle = 'Billion cubic feet', 
           caption = 'Top 5 natural gas consuming countries in 2017 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1980,2019,5), limits = c(1980,2017), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), breaks = seq(0,140e3,20e3), limits = c(0,140e3), labels = scales::comma) +
      guides(fill = 'none',
             color = 'none') +
      scale_color_manual(values = pal_cons) + 
      scale_fill_manual(values = pal_cons) + 
      theme_area_labeled +
      geom_text(data = labs_area_cons, aes(x = Inf, y = position, label = paste0(' ', label), color = label), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')
    
    area_cons = ggplotGrob(area_cons)
    area_cons$layout$clip[area_cons$layout$name == "panel"] = "off"
    
    ggsave(area_cons, 
           filename = here::here('figures', 'world', 'natural-gas-consumption-by-country_annual_1980-2017_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'natural-gas-consumption-by-country_annual_1980-2017_ats_absolute.pdf'),
                outfile = here::here('figures', 'world', 'natural-gas-consumption-by-country_annual_1980-2017_ats_absolute.pdf'))
    
    
  # area, consumption (proportion) -----
    
    labs_area_cons_prop = ng_agg[type == 'consumption' & year == 2017][order(factor(label, levels = rev(c('All Other Countries', rev(top5_cons)))))]
    labs_area_cons_prop[, cum_sum := cumsum(prop)] 
    labs_area_cons_prop[, difference := diff(c(0,cum_sum))/2]
    labs_area_cons_prop[, position := cum_sum - difference]

    area_cons_prop = ggplot(ng_agg[type == 'consumption'], 
                            aes(x = year, y = prop, fill = factor(label, levels = c('All Other Countries', rev(top5_cons))))) + 
      geom_area() +
      labs(title = 'Annual dry natural gas consumption by country (1980-2017)',
           subtitle = 'Billion cubic feet', 
           caption = 'Top 5 natural gas consuming countries in 2017 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1980,2019,5), limits = c(1980,2017), expand = c(0,0)) +
      scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
      guides(fill = 'none',
             color = 'none') +
      scale_color_manual(values = pal_cons) + 
      scale_fill_manual(values = pal_cons) + 
      theme_area_labeled +
      geom_text(data = labs_area_cons_prop, aes(x = Inf, y = position, label = paste0(' ', label), color = label), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')
    
    area_cons_prop = ggplotGrob(area_cons_prop)
    area_cons_prop$layout$clip[area_cons_prop$layout$name == "panel"] = "off"
    
    ggsave(area_cons_prop, 
           filename = here::here('figures', 'world', 'natural-gas-consumption-by-country_annual_1980-2017_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'natural-gas-consumption-by-country_annual_1980-2017_ats_proportion.pdf'),
                outfile = here::here('figures', 'world', 'natural-gas-consumption-by-country_annual_1980-2017_ats_proportion.pdf'))
    
  # line, imports -----
    
    labs_line_imp = ng_agg[type == 'imports' & year == 2017]
    setorder(labs_line_imp, 'value')
    labs_line_imp[, position := value]
    labs_line_imp[1:5, position := seq(1600,5400,length.out = 5)]
    
    line_imp = ggplot(ng_agg[type == 'imports'], aes(x = year, y = value, color = label)) + 
      geom_line(size = 0.9) +
      labs(title = 'Annual dry natural gas imports by country (1980-2017)',
           subtitle = 'Billion cubic feet', 
           caption = 'Top 5 dry natural gas importing countries in 2017 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      guides(color = 'none') +
      scale_x_continuous(breaks = seq(1980,2019,5), limits = c(1980,2017), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), breaks = seq(0,25e3,5e3), labels = scales::comma) +
      scale_color_manual(values = pal_imp) + 
      theme_line +
      geom_text(data = labs_line_imp, aes(x = Inf, y = position, label = paste0(' ', label), color = label), hjust = 0,
                size = 5.5, fontface = 'plain', family = 'Secca Soft')
    
    line_imp = ggplotGrob(line_imp)
    line_imp$layout$clip[line_imp$layout$name == "panel"] = "off"
    
    ggsave(line_imp, 
           filename = here::here('figures', 'world', 'natural-gas-imports-by-country_annual_1980-2017_lts.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'natural-gas-imports-by-country_annual_1980-2017_lts.pdf'),
                outfile = here::here('figures', 'world', 'natural-gas-imports-by-country_annual_1980-2017_lts.pdf'))
    
  # area, imports (absolute) -----
    
    labs_area_imp = ng_agg[type == 'imports' & year == 2017][order(factor(label, levels = rev(c('All Other Countries', rev(top5_imp)))))]
    labs_area_imp[, cum_sum := cumsum(value)] 
    labs_area_imp[, difference := diff(c(0,cum_sum))/2]
    labs_area_imp[, position := cum_sum - difference]
    
    area_imp = ggplot(ng_agg[type == 'imports'], 
                      aes(x = year, y = value, fill = factor(label, levels = c('All Other Countries', rev(top5_imp))))) + 
      geom_area() +
      labs(title = 'Annual dry natural gas imports by country (1980-2017)',
           subtitle = 'Billion cubic feet', 
           caption = 'Top 5 dry natural gas importing countries in 2017 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1980,2019,5), limits = c(1980,2017), expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), breaks = seq(0,45e3,5e3), labels = scales::comma) +
      guides(fill = 'none',
             color = 'none') +
      scale_color_manual(values = pal_imp) + 
      scale_fill_manual(values = pal_imp) + 
      theme_area_labeled +
      geom_text(data = labs_area_imp, aes(x = Inf, y = position, label = paste0(' ', label), color = label), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')
    
    area_imp = ggplotGrob(area_imp)
    area_imp$layout$clip[area_imp$layout$name == "panel"] = "off"
    
    ggsave(area_imp, 
           filename = here::here('figures', 'world', 'natural-gas-imports-by-country_annual_1980-2017_ats_absolute.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'natural-gas-imports-by-country_annual_1980-2017_ats_absolute.pdf'),
                outfile = here::here('figures', 'world', 'natural-gas-imports-by-country_annual_1980-2017_ats_absolute.pdf'))
    
    
  # area, imports (proportion) -----
    
    labs_area_imp_prop = ng_agg[type == 'imports' & year == 2017][order(factor(label, levels = rev(c('All Other Countries', rev(top5_imp)))))]
    labs_area_imp_prop[, cum_sum := cumsum(prop)] 
    labs_area_imp_prop[, difference := diff(c(0,cum_sum))/2]
    labs_area_imp_prop[, position := cum_sum - difference]
    
    area_imp_prop = ggplot(ng_agg[type == 'imports'], 
                           aes(x = year, y = prop, fill = factor(label, levels = c('All Other Countries', rev(top5_imp))))) + 
      geom_area() +
      labs(title = 'Annual dry natural gas imports by country (1980-2017)',
           subtitle = 'Billion cubic feet', 
           caption = 'Top 5 dry natural gas importing countries in 2017 shown individually. All other countries aggregated. Data: U.S. Energy Information Administration', 
           x = NULL,
           y = NULL) +
      scale_x_continuous(breaks = seq(1980,2019,5), limits = c(1980,2017), expand = c(0,0)) +
      scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
      guides(fill = 'none',
             color = 'none') +
      scale_color_manual(values = pal_imp) + 
      scale_fill_manual(values = pal_imp) + 
      theme_area_labeled +
      geom_text(data = labs_area_imp_prop, aes(x = Inf, y = position, label = paste0(' ', label), color = label), 
                hjust = 0, size = 6.5, fontface = 'plain', family = 'Secca Soft')
    
    area_imp_prop = ggplotGrob(area_imp_prop)
    area_imp_prop$layout$clip[area_imp_prop$layout$name == "panel"] = "off"
    
    ggsave(area_imp_prop, 
           filename = here::here('figures', 'world', 'natural-gas-imports-by-country_annual_1980-2017_ats_proportion.pdf'), 
           width = 11.5, 
           height = 6.25)
    
    embed_fonts(here::here('figures', 'world', 'natural-gas-imports-by-country_annual_1980-2017_ats_proportion.pdf'),
                outfile = here::here('figures', 'world', 'natural-gas-imports-by-country_annual_1980-2017_ats_proportion.pdf'))
    
    
    
    
