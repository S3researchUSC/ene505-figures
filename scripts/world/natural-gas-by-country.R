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
  ng_max_year[, max_year := max(year), by = 'type']
  ng_max_year = ng_max_year[year == max_year]
  ng_max_year[, rank := frank(-value), by = 'type']
  rank_country = unique(ng_max_year[, c('country', 'type', 'rank')])
  
# merge with main coal data ----
  
  ng_ranked = merge(ng_long, rank_country, by = c('country', 'type'), nomatch = 0)
  
# label all non-top 5 countries as "Other" -----
  
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
