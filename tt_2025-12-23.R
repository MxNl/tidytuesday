library(readr)
library(forcats)
library(purrr)
library(ggplot2)
library(stringr)
library(vctrs)
library(tidyr)
library(dplyr)


# Data Import ------------------------------------------------------------

endangered_status <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-23/endangered_status.csv')
families <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-23/families.csv')
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-23/languages.csv')

endangered_status
languages
families

data_languages <- 
  languages |> 
  left_join(families, by = join_by(family_id == id)) |> 
  left_join(endangered_status, by = join_by(id))

# data_languages |> 
#   pull(countries) |> 
#   str_c(sep = ";") |> 
#   str_split(pattern = ";") |> 
#   unlist() |> 
#   na.omit() |> 
#   unique()


# Countrie Codes
# rnaturalearth::ne_countries(returnclass = "sf") |> 
#   select(name, postal) |> 
#   as_tibble() |> 
#   distinct(postal) |> 
#   pull(postal)

country_codes <- 
  countrycode::codelist |> 
  select(id_country = iso2c, id_country_3c = iso3c, name_country = iso.name.en) |> 
  distinct(across(everything()))

data_languages <- 
  data_languages |> 
  separate_rows(countries) |> 
  left_join(country_codes, by = join_by(countries == id_country)) |> 
  relocate(id, name, id_country = countries, id_country_3c, name_country)


# Preliminary Analysis ---------------------------------------------------

data_plot1 <- data_languages |>
  drop_na(status_label) |> 
  filter(status_label == "not endangered") |> 
  summarise(
    n_languages = n(),
    .by = c("id_country", "name_country")
  ) |> 
  arrange(desc(n_languages))


data_plot1 |> 
  slice_max(n = 20, order_by = n_languages) |> 
  mutate(id_country = str_to_lower(id_country)) |> 
  ggplot(aes(n_languages, fct_rev(fct_inorder(name_country)))) +
  geom_col(aes(fill = n_languages)) +
  ggflags::geom_flag(aes(x = -50, country = id_country), size = 4) +
  scale_x_continuous(position = "top") +
  scale_fill_viridis_c(option = "turbo", direction = -1, guide = "none") +
  labs(
    x = "Number of spoken languages",
    y = "Country"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x.top = element_text(margin = margin(b = 15))
  )

data_plot1 |> 
  filter(name_country == "Germany")


data_languages |> 
  distinct(family)

data_languages |> 
  filter(name_country == "Germany") |> 
  relocate(id, name, status_code, status_label) |> 
  filter(status_code == 1)
