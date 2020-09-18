# ------------------------------------ inputs ------------------------------------

  oil_file       = 'international-petroleum-crude-by-country-1980-2019.csv'

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

  # read in csv of coal data
    oil_data = fread(here::here('data', oil_file), skip = 1, header = T)
  
  # rename V2 column
    setnames(oil_data, 'V2', 'country')
  
  # remove whitespace from country names
    oil_data[, country := trimws(country, which = 'both')]

# melt data from wide to long format -----

  oil_long = melt(oil_data, measure.vars = as.character(1973:2020), variable.name = 'year', value.name = 'value')

# convert year and values to numeric type -----

  oil_long[, year := as.numeric(as.character(year))]
  oil_long[, value := as.numeric(value)]
  
# remove crude production api -----
  
  oil_long = oil_long[! API %like% 'INTL.55-1']
  
# categorize values by type using API -----
  
  oil_long[API %like% 'INTL.53-1', type := 'production']
  oil_long[API %like% 'INTL.5-2', type := 'consumption']
  oil_long[API %like% 'INTL.57-3', type := 'imports']
  oil_long[API %like% 'INTL.57-4', type := 'exports']
  oil_long[API %like% 'INTL.57-6', type := 'reserves']
  
# add units -----
  
  oil_long[, unit := ifelse(type == 'reserves', 'billion barrels', 'thousand barrels per day')]
  
# remove rows that are just labels from the raw data ----
  
  oil_long = oil_long[!is.na(type)]
  
# rank values in each category (production, consumption, imports, exports, and reserves) in 2018 ------
  
  oil_max_year = oil_long[!is.na(value) & ! country == 'World']
  oil_max_year[, max_year := max(year), by = 'type']
  oil_max_year = oil_max_year[year == max_year]
  oil_max_year[, rank := frank(-value), by = 'type']
  rank_country = unique(oil_max_year[, c('country', 'type', 'rank')])
  
# merge with main coal data ----
  
  oil_ranked = merge(oil_long, rank_country, by = c('country', 'type'))
  
# label all non-top 5 countries as "Other" -----
  
  oil_ranked[, label := ifelse(rank %in% 1:5, country, 'All Other Countries')]
  
# aggregate by new country grouping -----
  
  oil_agg = oil_ranked[, .(value = sum(value, na.rm = T)), by = .(year, label, type, unit)]
  setcolorder(oil_agg, c('year', 'type', 'label', 'value', 'unit'))
  
# calculate proportion (or percentage) contributed by type for each year -----
  
  oil_agg[, prop := value/sum(value), by = c('type', 'year')]
  
# get top 5 for each type -----
  
  top5_prod = rank_country[type == 'production' & rank %in% 1:5][order(rank), country]
  top5_cons = rank_country[type == 'consumption' & rank %in% 1:5][order(rank), country]
  top5_imp = rank_country[type == 'imports' & rank %in% 1:5][order(rank), country]
  top5_exp = rank_country[type == 'exports' & rank %in% 1:5][order(rank), country]
  top5_res = rank_country[type == 'reserves' & rank %in% 1:5][order(rank), country]
  