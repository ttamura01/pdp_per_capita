setwd("/Users/takayukitamura/Documents/R_Computing/us_pop_gdp")
library(tidyverse)
library(patchwork)
library(ggtext)
library(glue)
library(scales)

## download data from csv file
per_capita <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1318&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=A939RC0Q052SBEA&scale=left&cosd=1947-01-01&coed=2024-04-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-07-25&revision_date=2024-07-25&nd=1947-01-01") %>% 
  rename(date = DATE, gdp_per_capita = A939RC0Q052SBEA) 
tail(per_capita)
## familiarize the dataset
str(per_capita)
summary(per_capita)

per_capita %>% 
  ggplot(aes(x = gdp_per_capita)) +
  geom_histogram()

per_capita %>% 
  ggplot(aes(x = date, y = gdp_per_capita)) +
  geom_line()


## extract the initial and latest data
# initial data
initial_data <- per_capita %>% 
  slice_min(date)

initial_date <- initial_data$date
initial_gdp_per_capita <- round(initial_data$gdp_per_capita)
initial_gdp_per_capita_label <- format(initial_gdp_per_capita, big.mark =",")


initial_year <- year(initial_date)
initial_month <- month(initial_date)
initial_day <- day(initial_date)

# latest data
latest_per_capita_data <- per_capita %>% 
  slice_max(date)

latest_date <- latest_per_capita_data $date
latest_gdp_per_capita <- latest_per_capita_data $gdp_per_capita
gdp_per_capita_label <- format(round(latest_gdp_per_capita), big.mark = ",")

# find gdp_per_capita growth over the interval 
multiple <- latest_gdp_per_capita/initial_gdp_per_capita
gdp_per_capita_multiple <- round(multiple)

# find interval years 
years <- interval(initial_date, latest_date)/years(1)

# annualized growth rate
# (gdp_multiple)^(1/years) = (x + 1)
annual_gdp_per_capita_growth_rate <-  round(((gdp_per_capita_multiple)^(1/years) - 1)*100, 2)


## line plot of quaterly data/gdp
per_capita %>% 
  ggplot(aes(x = date, y = gdp_per_capita)) +
  geom_line() +
  geom_text(data=latest_per_capita_data, aes(x = date, y = gdp_per_capita, label = gdp_per_capita_label, vjust = -0.3), color = "blue") +
  scale_y_continuous(
    limits = c(0, 90000),
    breaks = seq(0, 90000, 20000),
    labels = label_comma(accuracy = 0.1)) +
  labs(title = glue("US GDP_Per_Capita grew {gdp_per_capita_multiple}x to ${gdp_per_capita_label} past for {years} years from ${initial_gdp_per_capita_label} in {initial_year} (= annual growth rate at {annual_gdp_per_capita_growth_rate}%)"),
       x = NULL,
       y = "GDP Per Capita (in $)") +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(),
    text = element_text(face = "bold"), 
    legend.position = "none"
  )

## transform quaterly gdp_per_capita to annualized gdp_per_capita
annual_gdp_pc <- per_capita %>% 
  drop_na() %>% 
  separate(date, sep = "-", into = c("year","month","day")) %>% 
  group_by(year) %>% 
  mutate("per_capita" = sum(gdp_per_capita)/4,
  ) %>% 
  mutate("per_capita" = round(gdp_per_capita)) %>% 
  slice_tail() %>% 
  # filter(year != "2024") %>% 
  select(year, per_capita) %>% 
  ungroup()
# 
sapply(annual_gdp_pc, class)
# 
# annual_gdp$year <- as.Date(annual_gdp$year, format = "%Y")

## extract the initial and latest data
# initial data
initial_data <- annual_gdp_pc %>% 
  slice_min(year)

initial_year <- initial_data$year
initial_per_capita <- initial_data$per_capita

# initial_date <- initial_data$date
# initial_gdp <- round(initial_data$gdp)

# latest data
latest_data <- annual_gdp_pc %>% 
  slice_max(year)

latest_year <- latest_data$year
latest_per_capita <- latest_data$per_capita
gdp_percapita_label <- comma(latest_per_capita)

latest_data$year <- as.Date(latest_data$year, "%Y")

latest_year <- latest_data$year


# find gdp growth over the interval 
multiple <- latest_per_capita/initial_per_capita
annual_gdp_percapita_multiple <- round(multiple)

# find interval years 
# years <- as.numeric(latest_year) - as.numeric(initial_year)
years_annual <- interval(as.Date(initial_year, "%Y"), as.Date(latest_year, "%Y")) / years(1)

# annualized growth rate
# (gdp_multiple)^(1/years) = (x + 1)
annual_gdp_percapita_growth_rate <-  round(((annual_gdp_percapita_multiple)^(1/years) - 1)*100, 2)

annual_gdp_pc$year <- as.Date(annual_gdp_pc$year, format = "%Y")
latest_data$year <- as.Date(latest_data$year, "%Y")

annual_gdp_pc %>% 
  ggplot(aes(x = year, y = per_capita)) +
  geom_line() +
  geom_text(data=latest_data, aes(x = year, y = per_capita, label =gdp_percapita_label, vjust = -0.5), color = "blue") +
  scale_y_continuous(
    limits = c(0, 90000),
    breaks = seq(0, 80000, 20000),
    labels = label_comma(accuracy = 0.1)) +
  labs(title = glue("US GDP PerCapita grew {annual_gdp_percapita_multiple}x to ${gdp_percapita_label} past for {years_annual} years from ${initial_per_capita} in {initial_year} (= annual growth rate at {annual_gdp_percapita_growth_rate }%)"),
       x = NULL,
       y = "GDP PerCapita (in $)") +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(),
    text = element_text(face = "bold"),
    legend.position = "none"
  )

ggsave("/Users/takayukitamura/Documents/R_Computing/us_pop_gdp/figures/US_GDP_percapita_grwoth.png", width = 6, height = 4)
