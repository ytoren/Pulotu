# Data manipulation packages
require(dplyr)
require(readr)
require(tidyr)
require(reshape2)
require(tidytext)
#require(stringr)

# Graphics packages
require(magick)
require(ggplot2)
require(ggmap)
require(grid)
#require(googleVis)


# Load & filter data ------------------------------------------------------

d <- read_tsv('c:/Users/yizhart/Downloads/Datasets/Pulotu_Database_4_7_2015.txt')

# Focusing only on the "pacific" cultures, and adding some columns
d %>% 
  filter(substring(d$Culture, 1, 8) != 'Malagasy') %>%
  mutate(
    Start = parse_date(paste(substring(v1.Traditional_Time_Focus, first = 1, last = 4), '-01-01', sep = ''), format = '%Y-%m-%d'),
    End   = parse_date(paste(substring(v1.Traditional_Time_Focus, first = 6), '-01-01', sep = ''), format = '%Y-%m-%d'),
    n = row_number()
  ) ->
d


# Graph1 : time period overlap --------------------------------------------

d %>% 
  ggplot() + 
    geom_segment(aes(x=Start, xend=End, y=Culture, yend=Culture, color = Culture), size=1) + 
    labs(x = 'Years', y = 'Culture') +
    #theme_minimal() + 
    theme(legend.position="none")

  
# Graph2 : cultures on map, based on population size ----------------------

# Find map center
d %>% summarise(lon = mean(v6.Longitude), lat = mean(v5.Latitude))

# We ended up miving the "center" to the east to be able to focus on the relevant region
qmap(c(lon = 140, lat = -10), zoom = 3) +
  geom_point(
    data = d, 
    aes(x = v6.Longitude, y = v5.Latitude), 
    color = 'red',
    show.legend = TRUE,
    alpha = 0.5, 
    size = 5 + 30 * as.numeric(d$v10.Population)/ max(as.numeric(d$v10.Population), na.rm = TRUE)
  ) +
  geom_text(
    data = d,
    aes(x = v6.Longitude, y = v5.Latitude, label = Culture), 
    check_overlap = FALSE,
    size = 1 + 5 * as.numeric(d$v10.Population)/ max(as.numeric(d$v10.Population), na.rm = TRUE)
  )


# source analysis - what are the main sources of the information?  --------

year_pattern <- '[ ][(][0-9]{4}[A-Z]{0,1}[)][ ]'

d %>% 
  select(ends_with('Source'), Culture) %>%
  # transpose each column to a row
  melt(id = ('Culture')) %>% 
  # split multiple sources in a single line
  mutate(value = strsplit(value, split = '; ')) %>%
  # convert a multi-source line to multiple lines
  unnest(value) %>% 
  # Locate year pattern 
  mutate(regex = gregexpr(pattern = year_pattern, text = value)) %>%
  # In case there's more than one match (still) build a column of DF's and then split again
  mutate(regex_values = Map(function(y) {data.frame(start = unlist(y), length = attr(y, "match.length"))}, regex)) %>%
  select(-regex) %>%
  unnest(regex_values) %>%
  mutate(
    before = substr(value, 1, start-1), 
    matched = substr(value, start, start+length-1), 
    after = substr(value, start+length, stop = nchar(value))
  ) %>%
  # everythin before the year is author name, and the match is year in brackets
  select(Culture, variable, Author = before, year = matched) %>%
  mutate(year = as.numeric(gsub('[^0-9]', '', year))) %>%
  # filter(Culture == 'aia' )  %>% # & substring(variable, 1,3) == 'v29'
  # split multiple authors
  mutate(Author = strsplit(Author, split = ' & ')) %>%
  # convert a multi-Author line to multiple lines
  unnest(Author) %>%
  # filter numbers (some errors occur)
  filter(grepl('[a-zA-Z]', Author)) %>%
#   # break multi-Culture lines to separate lines
#   mutate(Culture = strsplit(Culture, split = '; ')) %>%
#   # convert a multi-Author line to multiple lines
#   unnest(Culture) %>%
  tbl_df() ->
source_freq


# Top N contributors -----------------------------------------------------

N <- 20

source_freq %>%
  count(Author, sort = TRUE) %>%
  top_n(N) %>%
  ggplot(aes(x = reorder(Author, desc(n)), y = n)) + 
    ggtitle('Top 20 contributors') +
    geom_bar(stat = 'identity', fill = 'blue', alpha = 0.75) +
    #scale_x_discrete(limits = Author) + 
    #ylab('Citations') + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Heatmap: source X culture -----------------------------------------------

source_freq %>%
  count(Culture, Author) %>%
  semi_join({source_freq %>% count(Author, sort = TRUE) %>% top_n(N)}, c('Author' = 'Author')) %>%
  #filter(n > 40) %>%
  ggplot(aes(x = Culture, y = Author)) + 
    ggtitle(paste0('Heat-map: top ', N, ' contributors vs. Culture Culture')) +
    geom_raster(aes(fill = n)) + 
    scale_fill_gradientn(colours=rainbow(4)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Cummulative contribution over time (by culture) -------------------------

source_freq %>%
  group_by(year) %>%
  summarise(source_count = n()) %>%
  arrange(year) %>%
  mutate(source_cumsum = cumsum(source_count)) -> 
source_freq_year

source_freq %>%
  mutate(decade = 10*floor(year/10)) %>%
  group_by(decade) %>%
  summarise(source_count = n()) %>%
  arrange(decade) %>%
  mutate(source_cumsum = cumsum(source_count)) -> 
source_freq_decade

ggplot() +
  geom_line(mapping = aes(x=year,   y = source_cumsum), data = source_freq_year, color = 'black', lwd = 1) +
  geom_bar( mapping = aes(x=decade, y = source_cumsum), data = source_freq_decade, stat = 'identity', fill = 'blue', alpha = 0.5) +
  ggtitle('Source accumulation over time')


# World cloud for Culture_Notes -------------------------------------------

#install.packages('wordcloud')
require(wordcloud)

data("stop_words")

d %>% 
  select(Culture, Culture_Notes) %>%
  unnest_tokens(word, Culture_Notes, token = 'words') %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  top_n(50) %>%
  do(wordcloud(.$word, .$n, colors = blues9[5:9]), .)

