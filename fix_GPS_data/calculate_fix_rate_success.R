

# --------------- label=setup
# Clean everything before beginning
rm(list = ls())

# Load packages
library(install.load)
install.load::install_load('tidyverse', 'lubridate')
install.load::install_load('amt')

# --------------- label=load_data
# Load Puma data

# Load Puma movement data
# read_csv('movement_data_Pardas_Tiete_all_individuals_2018_12_26.csv') %>% print(width = Inf)
mov.data.puma <- read.table('movement_data_Pardas_Tiete_all_individuals_2019_09_28.csv', header = T, sep = ',') %>% 
  mutate_at('timestamp', as.POSIXct, tz = 'UTC') %>% # timestamp as timestamp
  type_convert() #%>% # check that numeric columns are indeed numeric
#as_tibble()
# mutate_at('timestamp', lubridate::ymd_hms) %>%   
# mutate_at('timestamp', as.POSIXct, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')

head(mov.data.puma)
mov.data.puma %>% print(width = Inf)
str(mov.data.puma)

# Remove duplicated and arrange in chronological order
dupl <- mov.data.puma %>% 
  dplyr::select(name, timestamp) %>% 
  duplicated
sum(dupl)

mov.data <- mov.data.puma[!dupl,] %>% # remove duplicated
  dplyr::filter(timestamp <= lubridate::ymd('2018-12-31')) %>% 
  dplyr::arrange(name, timestamp)

# amt
# use SIRGAS2000, UTM 22S - EPSG 31982
mov.track <- mk_track(mov.data, .x = x_GRS80_utm22S, .y = y_GRS80_utm22S, .t = timestamp, crs = sp::CRS("+init=epsg:31982"),
                      ID, name, species, sex, dispersive.behavior)

# movement basic statistics
planned.schedule <- lubridate::period(1, unit = "hours") # (1h is the most common fix rate)

mov.track %>% 
  dplyr::group_by(name) %>% 
  dplyr::summarise(
    t.begin = min(t_),
    t.end = max(t_),
    t.range = diff(range(t_)),
    n = n(),
    n.intended = as.period(floor(t.range))/planned.schedule,
    success.fixrate = 100*n/n.intended
  ) 
