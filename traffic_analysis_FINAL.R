library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(purrr)
library(writexl)
library(leaflet)
library(ggrepel)
library(sf)

setwd("C://Users//user//Documents//big data final")

a1 <- read.csv("df_traffic_temp_rain.csv")

df <- a1 %>%
  mutate(
    rain_category = case_when(
      rainfall < 1  ~ "None",
      rainfall < 30 ~ "Light",
      rainfall < 60 ~ "Moderate",
      TRUE          ~ "Heavy"
    ),
    rain_category = factor(rain_category, levels = c("None", "Light", "Moderate", "Heavy"))
  )

accident_counts <- df %>%
  filter(party_seq == 1) %>%
  group_by(date, gender, rain_category) %>%
  summarise(n_accidents = n(), .groups = "drop")

model <- glm(n_accidents ~ rain_category + gender + rain_category:gender,
             data = accident_counts,
             family = poisson)

summary(model)

# Regression Coefficient Plot

model_results <- as.data.frame(summary(model)$coefficients) %>%
  mutate(Variable = rownames(.)) %>%
  rename(Estimate = Estimate, Std_Error = `Std. Error`, P_Value = `Pr(>|z|)`) %>%
  filter(Variable != "(Intercept)") %>%
  mutate(
    Lower_CI = Estimate - 1.96 * Std_Error,
    Upper_CI = Estimate + 1.96 * Std_Error
  )

p_coef <- ggplot(model_results, aes(x = reorder(Variable, Estimate), y = Estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "skyblue4", linewidth = 0.8) +
  geom_point(color = "firebrick", size = 3) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Coefficient Plot of Traffic Accident Poisson Regression Model",
    subtitle = "Points represent Estimates; Lines represent 95% Confidence Intervals",
    x = "Variables / Interaction Terms",
    y = "Log-Odds Estimate"
  )

ggsave("coefficient_plot.png", plot = p_coef, width = 8, height = 5, dpi = 300)

# Map 4: Traffic Accident Fatality Rates

df_mortality <- df %>%
  filter(party_seq == 1) %>%
  filter(longitude > 119 & longitude < 123 & latitude > 21 & latitude < 26) %>%
  mutate(
    grid_x = floor(longitude / 0.05) * 0.05,
    grid_y = floor(latitude / 0.05) * 0.05,
    is_fatal = if_else(deaths > 0, 1, 0)
  )

grid_mortality <- df_mortality %>%
  group_by(grid_x, grid_y) %>%
  summarise(
    total_accidents = n(),
    fatal_accidents = sum(is_fatal),
    .groups = "drop"
  ) %>%
  filter(total_accidents >= 10) %>%
  mutate(fatal_rate = fatal_accidents / total_accidents)

p_fatal_rate <- ggplot(data = grid_mortality) +
  geom_tile(aes(x = grid_x + 0.025, y = grid_y + 0.025, fill = fatal_rate)) +
  scale_fill_viridis_c(option = "inferno", labels = scales::percent) +
  coord_quickmap() +
  theme_minimal() +
  labs(
    title = "Grid-Based Traffic Accident Fatality Rates in Taiwan (2022-2024)",
    subtitle = "Grids with at least 10 accidents; Color represents (Fatal Accidents / Total Accidents)",
    x = "Longitude",
    y = "Latitude",
    fill = "Fatality Rate"
  )

ggsave("map_traffic_fatality_rate.png", plot = p_fatal_rate, width = 8, height = 10, dpi = 300)


# Map 5: Male At-Fault Accident Proportions

df_male_rate <- df %>%
  filter(party_seq == 1) %>%
  filter(longitude > 119 & longitude < 123 & latitude > 21 & latitude < 26) %>%
  mutate(
    grid_x = floor(longitude / 0.05) * 0.05,
    grid_y = floor(latitude / 0.05) * 0.05,
    is_male = if_else(gender == "M", 1, 0)
  )

grid_male_rate <- df_male_rate %>%
  group_by(grid_x, grid_y) %>%
  summarise(
    total_accidents = n(),
    male_accidents = sum(is_male),
    .groups = "drop"
  ) %>%
  filter(total_accidents >= 10) %>%
  mutate(male_rate = male_accidents / total_accidents)

p_male_rate <- ggplot(data = grid_male_rate) +
  geom_tile(aes(x = grid_x + 0.025, y = grid_y + 0.025, fill = male_rate)) +
  scale_fill_viridis_c(option = "mako", labels = scales::percent) +
  coord_quickmap() +
  theme_minimal() +
  labs(
    title = "Grid-Based Male At-Fault Accident Proportions in Taiwan (2022-2024)",
    subtitle = "Grids with at least 10 accidents; Color represents (Male At-Fault / Total At-Fault)",
    x = "Longitude",
    y = "Latitude",
    fill = "Male Proportion"
  )

ggsave("map_male_at_fault_rate.png", plot = p_male_rate, width = 8, height = 10, dpi = 300)


###

library(sf)
twn_sf <- st_read("鄉(鎮、市、區)界線1140318/TOWN_MOI_1140318.shp")
names(twn_sf)



typology <- read.csv("C:/Users/user/Downloads/academia_sinica_typology.csv") %>%
  mutate(urban_class = case_when(
    urban_level <= 2 ~ "都市",
    urban_level <= 5 ~ "半都市",
    TRUE             ~ "鄉村"
  ))

twn_sf <- st_read("鄉(鎮、市、區)界線1140318/TOWN_MOI_1140318.shp") %>%
  st_transform(crs = 4326)

typology <- read.csv("academia_sinica_typology.csv") %>%
  mutate(urban_class = case_when(
    urban_level <= 2 ~ "都市",
    urban_level <= 5 ~ "半都市",
    TRUE             ~ "鄉村"
  ))

twn_sf <- twn_sf %>%
  left_join(typology, by = "TOWNCODE")

# ── 2. 車禍點對應到鄉鎮 ─────────────────────────────────
df_sf <- df %>%
  filter(party_seq == 1,
         longitude > 119, longitude < 123,
         latitude > 21, latitude < 26) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

df_urban <- st_join(df_sf, twn_sf[, c("TOWNCODE", "urban_class")],
                    join = st_within) %>%
  st_drop_geometry()

# ── 3. 長條圖：哪種地區車禍最多 ─────────────────────────
df_urban %>%
  filter(!is.na(urban_class)) %>%
  count(urban_class) %>%
  ggplot(aes(x = reorder(urban_class, -n), y = n, fill = urban_class)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = scales::comma(n)), vjust = -0.5) +
  scale_fill_manual(values = c("都市" = "#E74C3C", "半都市" = "#F39C12", "鄉村" = "#27AE60")) +
  theme_minimal() +
  labs(title = "各都市化程度車禍件數", x = "都市化程度", y = "車禍件數", fill = "")
















typology

typology <- data.frame(
  TOWNCODE = c(
    "63000010","63000020","63000030","63000040","63000050","63000060","63000070",
    "63000080","63000090","63000100","63000110","63000120",
    "65000010","65000020","65000030","65000040","65000050","65000140",
    "65000060","65000100","65000110","65000130",
    "65000070","65000080","65000160","65000170","65000180",
    "65000090","65000120","65000150","65000230",
    "65000190","65000200","65000210","65000220","65000240","65000250","65000260","65000270","65000280","65000290",
    "68000010","68000020","68000030","68000040","68000050","68000060","68000070","68000080",
    "68000090","68000170","68000180","68000190","68000270","68000280",
    "68000130","68000140","68000150","68000160","68000230","68000250","68000260",
    "68000100","68000110","68000120","68000200","68000210","68000220","68000240",
    "68000290",
    "66000010","66000020","66000030","66000040","66000050","66000060","66000070","66000080","66000110",
    "66000100","66000130","66000160","66000170","66000180","66000190","66000200","66000210","66000280",
    "66000120","66000140","66000150","66000190","66000230","66000240","66000250","66000260","66000290","66000300",
    "66000220","66000270","66000310","66000320","66000330","66000340","66000350","66000360","66000370",
    "67000010","67000020","67000030","67000040","67000050",
    "67000060","67000070","67000250",
    "67000080","67000090","67000140","67000180","67000190","67000330","67000360",
    "67000110","67000170","67000180","67000200","67000210","67000220","67000230","67000270","67000300","67000310","67000320","67000370",
    "67000100","67000120","67000130","67000160","67000240","67000260","67000280","67000290","67000340","67000350",
    "68010010","68010070","68010080",
    "68010020","68010090","68010130",
    "68010030","68010040","68010100","68010120",
    "68010050","68010060","68010110",
    "10004010",
    "10004080","10004100",
    "10004020","10004030","10004070",
    "10004040","10004050","10004060","10004090","10004110","10004120","10004130",
    "10005090","10005010","10005020",
    "10005070","10005080","10005110","10005120","10005140","10005150","10005160","10005170","10005180",
    "10005030","10005040","10005050","10005060",
    "10007010","10007020","10007030","10007040","10007060","10007070","10007080","10007090","10007100","10007110","10007120","10007130","10007140","10007150","10007160",
    "10007050",
    "10007170","10007180","10007190","10007200","10007210","10007220","10007230","10007250",
    "10007240","10007260",
    "10008030","10008080","10008090",
    "10008010","10008070","10008100","10008110","10008120",
    "10008020","10008040","10008060","10008130",
    "10009010",
    "10009030","10009040","10009050","10009060","10009070","10009080","10009090","10009100","10009110","10009120","10009140","10009150","10009160","10009170","10009180",
    "10009020","10009130",
    "10009190","10009200",
    "10010010","10010050",
    "10010020","10010030","10010040","10010060","10010070","10010080","10010090","10010100","10010110","10010120","10010130","10010140","10010150",
    "10010160","10010170","10010180",
    "10011010",
    "10011090","10011120","10011130","10011140","10011220","10011250",
    "10011050","10011060","10011070","10011080","10011100","10011110","10011170","10011180","10011190","10011200","10011210","10011230","10011240","10011260","10011320",
    "10011020","10011030","10011040","10011150","10011160","10011270","10011280","10011290","10011300","10011310","10011330",
    "10013010","10014010","10014040",
    "10014020","10016060",
    "10016010",
    "10016030","10016040","10016050","10016070","10016090","10016100","10016110",
    "10016020","10016080","10016120",
    "10013050",
    "10013060","10013070","10013080","10013090","10013100","10013110","10013120","10013130","10013140","10013150","10013160","10013020","10013030","10013040",
    "10014050","10014060","10014070","10014080","10014090","10014100","10014110","10014120",
    "10015010",
    "10015020","10015030","10015040","10015050","10015060",
    "10017040","10017050","10017060",
    "10017010","10017020","10017030","10017070",
    "10020010","10020020",
    "10020030",
    "10018010","10018020"
  ),
  urban_level = c(
    1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,
    2,2,2,2,
    3,3,3,3,3,
    4,4,4,4,
    5,5,5,5,5,5,5,5,5,5,
    1,1,1,1,1,1,1,1,
    2,2,2,2,2,2,
    3,3,3,3,3,3,3,
    4,4,4,4,4,4,4,
    7,
    1,1,1,1,1,1,1,1,1,
    2,3,3,3,3,3,3,3,4,
    4,4,4,4,4,4,4,4,6,6,
    7,7,7,7,7,7,7,7,7,
    1,1,1,1,1,
    2,2,4,
    4,4,4,4,4,4,4,
    6,6,6,6,6,6,6,6,6,6,6,6,
    7,7,7,7,7,7,7,7,7,7,
    2,2,2,
    3,3,3,
    4,4,4,4,
    6,7,7,
    2,
    4,6,
    4,4,4,
    6,6,6,6,6,6,6,
    4,4,4,
    6,6,6,6,6,6,6,6,6,
    7,7,7,7,
    4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
    5,
    6,6,6,6,6,6,6,6,
    6,6,
    4,6,6,
    4,6,6,6,6,
    7,7,7,7,
    4,
    5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
    5,5,
    7,7,
    4,4,
    6,6,6,6,6,6,6,6,6,6,6,6,6,
    7,7,7,
    4,
    4,4,4,4,4,5,
    6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
    7,7,7,7,7,7,7,7,7,7,7,
    4,4,4,
    4,4,
    4,
    6,6,6,6,6,6,6,
    6,7,7,
    6,
    7,7,7,7,7,7,7,7,7,7,7,7,7,7,
    7,7,7,7,7,7,7,7,
    4,
    7,7,7,7,7,
    2,2,2,
    2,3,3,4,
    2,2,
    4,
    4,4
  )
) %>%
  mutate(urban_class = case_when(
    urban_level <= 2 ~ "都市",
    urban_level <= 5 ~ "半都市",
    TRUE             ~ "鄉村"
  ))


# ── 2. 車禍點對應到鄉鎮 ─────────────────────────────────
df_sf <- df %>%
  filter(party_seq == 1,
         longitude > 119, longitude < 123,
         latitude > 21, latitude < 26) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

twn_sf <- twn_sf %>%
  left_join(typology, by = "TOWNCODE")

df_urban <- st_join(df_sf, twn_sf[, c("TOWNCODE", "urban_class")],
                    join = st_within) %>%
  st_drop_geometry()

# ── 3. 長條圖 ────────────────────────────────────────────
df_urban %>%
  filter(!is.na(urban_class)) %>%
  count(urban_class) %>%
  ggplot(aes(x = reorder(urban_class, -n), y = n, fill = urban_class)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = scales::comma(n)), vjust = -0.5) +
  scale_fill_manual(values = c("都市" = "#E74C3C", "半都市" = "#F39C12", "鄉村" = "#27AE60")) +
  theme_minimal() +
  labs(title = "各都市化程度車禍件數", x = "都市化程度", y = "車禍件數", fill = "")

ggsave("urban_accident_count.png", width = 6, height = 5, dpi = 300)


# 車禍件數
df_urban %>%
  filter(!is.na(urban_class)) %>%
  count(urban_class) %>%
  ggplot(aes(x = reorder(urban_class, -n), y = n, fill = urban_class)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = scales::comma(n)), vjust = -0.5) +
  scale_fill_manual(values = c("都市" = "#E74C3C", "半都市" = "#F39C12", "鄉村" = "#27AE60"),
                    labels = c("都市" = "Urban", "半都市" = "Suburban", "鄉村" = "Rural")) +
  scale_x_discrete(labels = c("都市" = "Urban", "半都市" = "Suburban", "鄉村" = "Rural")) +
  theme_minimal() +
  labs(title = "Traffic Accident Counts by Urbanization Level",
       x = "Urbanization Level", y = "Number of Accidents", fill = "")

ggsave("urban_accident_count.png", width = 6, height = 5, dpi = 300)

# 死亡率
df_urban %>%
  filter(!is.na(urban_class)) %>%
  group_by(urban_class) %>%
  summarise(fatal_rate = mean(deaths > 0, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(urban_class, -fatal_rate), y = fatal_rate, fill = urban_class)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = scales::percent(fatal_rate, accuracy = 0.1)), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("都市" = "#E74C3C", "半都市" = "#F39C12", "鄉村" = "#27AE60"),
                    labels = c("都市" = "Urban", "半都市" = "Suburban", "鄉村" = "Rural")) +
  scale_x_discrete(labels = c("都市" = "Urban", "半都市" = "Suburban", "鄉村" = "Rural")) +
  theme_minimal() +
  labs(title = "Traffic Accident Fatality Rate by Urbanization Level",
       x = "Urbanization Level", y = "Fatality Rate", fill = "")

ggsave("urban_fatal_rate.png", width = 6, height = 5, dpi = 300)



# 補充修正對照表
typology_fix <- data.frame(
  TOWNCODE = c(
    # 宜蘭縣（正確代碼是 10002xxx）
    "10002010","10002020","10002030","10002040","10002050",
    "10002060","10002070","10002080","10002090","10002100","10002110","10002120",
    # 基隆市（正確代碼是 10017xxx）
    "10017010","10017020","10017030","10017040","10017050","10017060","10017070",
    # 新竹市
    "10020010","10020020","10020030",
    # 嘉義市
    "10018010","10018020",
    # 高雄市漏掉的
    "64000010","64000020","64000030","64000040","64000050","64000060","64000070",
    "64000080","64000090","64000100","64000110","64000120","64000130","64000140",
    "64000150","64000160","64000170","64000180","64000190","64000200","64000210",
    "64000220","64000230","64000240","64000250","64000260","64000270","64000280",
    "64000290","64000300","64000310","64000320","64000330","64000340","64000350",
    "64000360","64000370",
    # 屏東縣漏掉的
    "10013210"
  ),
  urban_level = c(
    # 宜蘭
    4,6,5,6,6,6,6,7,5,5,5,7,
    # 基隆
    2,3,4,2,2,2,4,
    # 新竹市
    2,2,4,
    # 嘉義市
    4,4,
    # 高雄市（64開頭，同66開頭內容）
    1,1,1,1,1,1,1,1,4,2,2,4,3,4,
    4,3,3,4,4,5,7,6,4,6,6,6,6,5,
    6,6,7,7,7,7,7,7,7,
    # 屏東佳冬
    6
  )
) %>%
  mutate(urban_class = case_when(
    urban_level <= 2 ~ "Urban",
    urban_level <= 5 ~ "Suburban",
    TRUE             ~ "Rural"
  ))

# 合併補充資料
typology_full <- typology %>%
  mutate(urban_class = case_when(
    urban_level <= 2 ~ "Urban",
    urban_level <= 5 ~ "Suburban",
    TRUE             ~ "Rural"
  )) %>%
  bind_rows(typology_fix)

# 重新 join
twn_sf <- st_read("鄉(鎮、市、區)界線1140318/TOWN_MOI_1140318.shp") %>%
  st_transform(crs = 4326) %>%
  left_join(typology_full, by = "TOWNCODE")

# 確認還剩幾個 NA（金門、連江離島正常會有）
twn_sf %>% filter(is.na(urban_class)) %>% select(TOWNCODE, COUNTYNAME, TOWNNAME)

##
missing_towns <- twn_sf %>%
  st_drop_geometry() %>%
  filter(is.na(urban_class)) %>%
  select(TOWNCODE, COUNTYNAME, TOWNNAME)

print(as.data.frame(missing_towns), max = 999)