
# Paquetes ----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(scales)


# Objetos utilitarios -----------------------------------------------------
colores <- lst(
  tableau = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", "#EDC948", 
              "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC"),
  blue = "#0070C0",
  gray = "#7E8083",
  green = "#007033",
  red = "#C00000",
  paleta = c(blue, gray, green, red)
)


# La data -----------------------------------------------------------------

# EOE data 

#list.files('../procesamiento_eoe_eem/data/eoe/historicos')
series_eoe <- readxl::read_excel("data/serie_eoe.xlsx") %>% 
  mutate(periodo = ymd(paste(year, bcdata::crear_mes(mes), "01")))

eoe_desagregada <- read_rds('data/eoe_desagregada.rds')
eoe_data <- read_rds('data/eoe_data.rds')

# Stringency Index
gri <- readxl::read_excel(here("data", 'owid-covid-data.xlsx'))

gri_month <- gri %>% 
  filter(location %in% c("Dominican Republic")) %>% 
  mutate(date = ymd(date), dia = day(date)) %>%
  #filter(dia %in% 0:10) %>% 
  group_by(date = floor_date(date, unit = "month"), location) %>%
  summarise(ri = mean(stringency_index, na.rm = TRUE))

# Data internacional
internacionales <- lst(
  
  mexico  = read_excel('data/series_internacionales.xlsx', 'mexico') %>% 
    fill(year, .direction = 'down') %>% 
    mutate(date = seq(ymd("2004-01-01"), length.out = nrow(.), by = "month"),
           year = parse_number(year),
           mes = month(date)) %>% 
    select(date, year, mes, sit_econ, sit_econ_exp),
  
  colombia = read_excel('data/series_internacionales.xlsx', 'colombia') %>% 
    mutate(
      date = seq(ymd("2018-06-01"), length.out = nrow(.), by = "month"),
      year = year(date), mes = month(date)
    ) %>% 
    select(date, year, mes, sit_econ, sit_econ_exp),
  
  dominicana = series_eoe %>% 
    mutate(mes = month(periodo)) %>% 
    select(date = periodo, year, mes, sit_econ, sit_econ_exp)
  
) %>% bind_rows(.id = 'location') %>% 
  mutate(
    location = recode(location, 'dominicana' = 'Dominican Republic'),
    location = str_to_title(location)
  )

internacionales <- gri %>% 
  filter(location %in% c("Dominican Republic", 'Colombia', 'Mexico')) %>% 
  mutate(date = ymd(date), dia = day(date)) %>%
  #filter(dia %in% 0:10) %>% 
  group_by(date = floor_date(date, unit = "month"), location) %>%
  summarise(ri = mean(stringency_index, na.rm = TRUE)) %>% 
  mutate(
    date = date - months(1),
    year = year(date), mes = month(date)
  )  %>% 
  left_join(internacionales) %>% 
  select(location, date, year, mes, sit_econ, sit_econ_exp, ri)


# Visualizaciones ---------------------------------------------------------

negativo_gsi <- eoe_desagregada %>% 
  select(date, negativo, variable, variable_key) %>% 
  filter(variable_key %in% c("sit_econ", "sit_econ_exp")) %>% 
  bind_rows(
    gri_month %>% 
      mutate(variable = 'Stringency Index', variable_key = 'stringency',
             date = date - months(1)) %>% 
      rename(negativo = ri) %>% 
      select(-location)
  ) 


# Evolución stringency index y situación económica
(plt_gsi_sit_econ <-  negativo_gsi %>% 
  filter(variable_key %in% c('sit_econ', 'stringency'), date > '2020-01-01') %>% 
  ggplot(aes(x = date, y = negativo, color = variable)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_point(color = 'white') +
  labs(x = NULL, y = NULL, color = NULL) +
  scale_y_continuous(breaks = pretty_breaks(5))+
  scale_color_manual(values = c(colores$blue, colores$green)) +
  theme_light() +
  theme(
    legend.position = 'bottom',
    panel.grid = element_blank(),
    text = element_text(size = 14)
  ))

# Evolución stringency index y expectativas de la situación económica
(plt_gsi_sit_econ_exp <-  negativo_gsi %>% 
    filter(variable_key %in% c('sit_econ_exp', 'stringency'), date > '2020-01-01') %>% 
    ggplot(aes(x = date, y = negativo, color = variable)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    geom_point(color = 'white') +
    labs(x = NULL, y = NULL, color = NULL) +
    scale_y_continuous(breaks = pretty_breaks(5))+
    scale_color_manual(values = c(colores$red, colores$green)) +
    theme_light() +
    theme(
      legend.position = 'bottom',
      panel.grid = element_blank(),
      text = element_text(size = 14)
    ))

# Scatter Stringency Index vs Situación económica (colombia RD)
(plt_gsi_sit_econ_colombia_rd <- internacionales %>% 
  filter(!location == 'Mexico', year == 2020) %>% 
  ggplot(aes(x = ri, y = sit_econ, color = location)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_smooth(method = 'lm', se = FALSE) +
  scale_color_manual(values = c(colores$blue, colores$green)) +
  theme_light() +
  theme(legend.position = 'bottom',
        panel.grid = element_blank()) +
  labs(x = 'Stringency Index', y = 'Situación económica', color = NULL))

# Scatter Stringency Index vs expectativas Situación económica (colombia RD)
(plt_gsi_sit_econ_exp_colombia_rd <- internacionales %>% 
    filter(!location == 'Mexico', year == 2020) %>% 
    ggplot(aes(x = ri, y = sit_econ_exp, color = location)) +
    geom_point(size = 3, alpha = 0.6) +
    geom_smooth(method = 'lm', se = FALSE) +
    scale_color_manual(values = c(colores$blue, colores$green)) +
    theme_light() +
    theme(legend.position = 'bottom',
          panel.grid = element_blank()) +
    labs(x = 'Stringency Index', y = 'Expectativas situación económica', color = NULL))


# Correlaciones -----------------------------------------------------------

gri_month %>% 
  mutate(date = date - months(1)) %>% 
  left_join(
    eoe_data %>% 
      select(-year, -mes),
    by = c('date' = 'periodo')
  ) %>% select(-location) %>% 
  ungroup() %>% 
  summarise(across(c(sit_econ:ice), ~cor(., ri))) %>% 
  pivot_longer(
    everything(),
    names_to = 'variable_key',
    values_to = 'correlacion') %>% 
  arrange(correlacion) %>% 
  left_join(
    count(eoe_desagregada, variable, variable_key) %>% 
      select(-n)
  ) %>% 
  mutate(
    variable = case_when(variable_key == "ice" ~ "Índice de Clima Empresarial",
                         variable_key == "ici" ~ "Índice de Confianza Industrial",
                         TRUE ~ variable)
  ) %>% 
  select(Variable = variable, Correlación = correlacion) %>% 
  formattable( alig = c('l', 'r'),
               #list(`Correlación` = color_bar("#FA614B"))
               list(area(col = 2) ~ color_tile("#71CA97", "#DeF7E9"))
  )
               


# Guardando gráficos ------------------------------------------------------

ggsave(
  filename = "graficos/stringency_sit_econ.png",
  plot = plt_gsi_sit_econ, dpi = 350,
  width = 6, height = 3.5
)

ggsave(
  filename = "graficos/stringency_exp_sit_econ.png",
  plot = plt_gsi_sit_econ_exp, dpi = 350,
  width = 6, height = 3.5
)

ggsave(
  filename = "graficos/stringency_sit_econ_colombia_rd.png",
  plot = plt_gsi_sit_econ_colombia_rd, dpi = 350,
  width = 7, height = 4.5
)

ggsave(
  filename = "graficos/stringency_sit_econ_exp_colombia_rd.png",
  plot = plt_gsi_sit_econ_exp_colombia_rd, dpi = 350,
  width = 7, height = 4.5
)
