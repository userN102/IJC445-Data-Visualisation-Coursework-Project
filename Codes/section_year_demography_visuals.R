
library(tidyverse)
library(ggrepel)
library(scales)
library(dplyr)
library(ggplot2)



# Generate Figures 1–5 for the composite visualisation
# Input: section_year_business_demography.csv (section × year totals for the five selected industries)
# Notes:
#   - Rates are computed using active enterprises as the denominator
#   - Where averages are used, they summarise 2019–2023 to support cross-industry comparison


# Ensure output directory for visuals exists
visuals_dir <- "./Visuals"
if (!dir.exists(visuals_dir)) {
  dir.create(visuals_dir, recursive = TRUE)
}

business_demography_data <- read_csv(
  "./Dataset/Final_Master_Datasets/section_year_business_demography.csv"
)

# Figure 1: Entry and exit over time (counts)
# Shows how enterprise births and deaths evolved over time within each industry section.
# How did patterns of enterprise entry and exit evolve across key UK industries between 2019 and 2023 ?

viz_long <- business_demography_data %>%
  select(
    year,
    section_name,
    births_of_new_enterprises,
    deaths_of_new_enterprises
  ) %>%
  pivot_longer(
    cols = c(births_of_new_enterprises, deaths_of_new_enterprises),
    names_to = "event_type",
    values_to = "count"
  ) %>%
  mutate(
    # Wrap long industry names for facet strips so it prevents truncation/overlap
    section_name = str_wrap(section_name, width = 28),
    
    event_type = factor(
      event_type,
      levels = c("births_of_new_enterprises", "deaths_of_new_enterprises"),
      labels = c("Births of New Enterprises", "Deaths of New Enterprises")
    )
  )

plot_v1 <- ggplot(viz_long, aes(x = year, y = count, colour = event_type, group = event_type)) +
  geom_line(linewidth = 0.9, lineend = "round") +
  geom_point(size = 1.6) +
  facet_wrap(~ section_name, scales = "free_y") +
  scale_colour_manual(
    values = c(
      "Births of New Enterprises" = "#0072B2",
      "Deaths of New Enterprises" = "#7F7F7F" 
    )
  ) +
  scale_x_continuous(breaks = 2019:2023) +
  labs(
    title = "How did patterns of enterprise entry and exit evolve across key UK industries between 2019 and 2023?",
    subtitle = "Panels use independent y-scales to highlight within-industry trends rather than absolute differences across industries.",
    caption = "Source: UK Office for National Statistics (ONS), Business Demography.",
    x = "Year",
    y = "Number of enterprises",
    colour = "Enterprise event"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 12
    ),
    plot.subtitle = element_text(
      size = 10,          
      colour = "grey35" 
    ),
    plot.caption = element_text(
      size = 9,
      colour = "grey40",
      hjust = 1
    ),
    strip.text = element_text(face = "bold", size = 10),
    panel.spacing = unit(0.9, "lines"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey88", linewidth = 0.35),
    legend.position = "right"
  )

ggsave(
  filename = file.path(visuals_dir, "visual_1_entry_exit_over_time.png"),
  plot = plot_v1,
  width = 10,
  height = 6,
  dpi = 300
)



# Figure 2: Churn and net change (rates + cumulative outcome)
# Compares churn intensity using average entry/exit rates and link this to cumulative net change.
# - Entry/exit are rates (births/active, deaths/active)
# - Rates are averaged across 2019–2023 for cross-industry comparison
# - Net change is summed across 2019–2023 to capture cumulative expansion or contraction
# Which industries exhibited high levels of enterprise churn, and how does this relate to their overall net change between 2019 and 2023?

viz_churn <- business_demography_data %>%
  filter(active_enterprises > 0) %>% 
  mutate(
    birth_rate = births_of_new_enterprises / active_enterprises,
    death_rate = deaths_of_new_enterprises / active_enterprises,
    net_change = births_of_new_enterprises - deaths_of_new_enterprises
  ) %>%
  group_by(section_name) %>%
  summarise(
    birth_rate_avg = mean(birth_rate, na.rm = TRUE),
    death_rate_avg = mean(death_rate, na.rm = TRUE),
    net_change_total = sum(net_change, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    net_direction = if_else(net_change_total >= 0, "Net growth", "Net decline"),
    net_magnitude = abs(net_change_total)
  )


# Label the most informative points such as top magnitude changes 
top_labels <- viz_churn %>%
  slice_max(net_magnitude, n = 4) %>%
  bind_rows(viz_churn %>% dplyr::filter(section_name == "Manufacturing")) %>%
  distinct(section_name, .keep_all = TRUE)


plot_v2 <- ggplot(
  viz_churn,
  aes(
    x = birth_rate_avg,
    y = death_rate_avg,
    size = net_magnitude,
    colour = net_direction
  )
) +
  # Parity line is births = deaths (zero net change benchmark)
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
  annotate(
    "text",
    x = 0.112,
    y = 0.118,
    label = "Births = Deaths",
    size = 3,
    colour = "grey40"
  ) +
  geom_point(alpha = 0.85) +
  ggrepel::geom_text_repel(
    aes(label = section_name),
    colour = "grey10",
    size = 3.2,
    box.padding = 0.8,
    point.padding = 0.7,
    force = 2,
    max.overlaps = Inf,
    min.segment.length = 0,
    segment.colour = "grey65",
    direction = "x" 
  ) +
  
  ggrepel::geom_text_repel(
    data = top_labels,
    aes(
      label = paste0(
        ifelse(net_change_total >= 0, "+", "−"),
        format(abs(round(net_change_total, 0)), big.mark = ",")
      )
    ),
    colour = "grey10",
    size = 3.0,
    fontface = "bold",
    box.padding = 0.4,
    point.padding = 0.5,
    force = 3,
    max.overlaps = Inf,
    min.segment.length = 0,
    segment.colour = "grey55",
    nudge_y = -0.004,  
    direction = "y"
  ) +
  scale_colour_manual(
    values = c(
      "Net growth"  = "#0072B2",
      "Net decline" = "#E69F00"
    )
  ) +
  scale_size_area(max_size = 12, name = "|Net change| (total)") +
  guides(
    colour = guide_legend(override.aes = list(size = 4)),
    size   = guide_legend(override.aes = list(alpha = 1))
  ) +
  labs(
    title = "Which industries exhibited high levels of enterprise churn,\nand how does this relate to their overall net change between 2019 and 2023?",
    subtitle = paste(
      "Position shows average entry and exit rates. Colour indicates direction of net change.",
      "Bubble size reflects magnitude of net change.",
      "Rates are averaged across 2019–2023. Net change is summed across 2019–2023.",
      sep = "\n"
    ),
    x = "Average birth rate (births / active enterprises)",
    y = "Average death rate (deaths / active enterprises)",
    colour = "Net outcome",
    caption = "Source: UK Office for National Statistics (ONS), Business Demography."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold",size = 12),
    plot.subtitle = element_text(size = 10, colour = "grey40"),
    plot.caption = element_text(size = 9, colour = "grey40",hjust=1),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )


ggsave(
  filename = file.path(visuals_dir, "visual_2_churn_and_net_change.png"),
  plot = plot_v2,
  width = 10,
  height = 6,
  dpi = 300
)


# Figure 3: Entry intensity (births per 1,000 active enterprises)
# Compares entry intensity after normalising births by the size of the industry (active enterprises).
# Which industries are more entrepreneurship-intensive relative to their size?

viz_entry_intensity <- business_demography_data %>%
  filter(active_enterprises > 0) %>% 
  group_by(section_name, year) %>%
  summarise(
    births = sum(births_of_new_enterprises, na.rm = TRUE),
    active = sum(active_enterprises, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(rate_per_1000 = (births / active) * 1000) %>%
  group_by(section_name) %>%
  summarise(
    mean_rate = mean(rate_per_1000, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_rate) %>%
  mutate(
    section_name = factor(section_name, levels = section_name),
    is_top = row_number() == n()
    )

plot_v3 <- ggplot(viz_entry_intensity, aes(x = section_name, y = mean_rate)) +
  geom_col(aes(fill = is_top), show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "grey35", "FALSE" = "grey65")) +
  coord_flip() +
  geom_text(
    aes(label = sprintf("%.1f", mean_rate)),
    hjust = -0.15,
    colour = "grey15",
    size = 3
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.20))) +  
  labs(
    title = "Which industries are more entrepreneurship-intensive relative to their size?",
    subtitle = "Entry intensity = enterprise births per 1,000 active enterprises. Bars show the 2019–2023 mean.",
    x = NULL,
    y = "Births per 1,000 active enterprises",
    caption = "Source: UK Office for National Statistics (ONS), Business Demography."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, colour = "grey40"),
    plot.caption = element_text(size = 9, colour = "grey40", hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.35)
  )

ggsave(
  filename = file.path(visuals_dir, "visual_3_entry_intensity.png"),
  plot = plot_v3,
  width = 10,
  height = 6,
  dpi = 300
)


# Figure 4: Entry intensity vs one-year survival (median quadrants)
# Compares entry intensity to short-term survival and classify industries into relative profiles.
# Quadrants are defined using median splits (relative thresholds, not absolute benchmarks).

summary(business_demography_data$survival_rate)

# Does high enterprise entry coincide with higher short-term survival across industries?

viz_resilience <- business_demography_data %>%
  filter(active_enterprises > 0) %>%
  mutate(birth_rate = births_of_new_enterprises / active_enterprises) %>%
  group_by(section_name) %>%
  summarise(
    birth_rate_avg = mean(birth_rate, na.rm = TRUE),
    survival_rate_avg = mean(survival_rate, na.rm = TRUE),
    .groups = "drop"
  )

x_cut <- median(viz_resilience$birth_rate_avg, na.rm = TRUE)
y_cut <- median(viz_resilience$survival_rate_avg, na.rm = TRUE)

viz_resilience <- viz_resilience %>%
  mutate(
    quadrant = case_when(
      birth_rate_avg >= x_cut & survival_rate_avg >= y_cut ~ "High entry / High survival",
      birth_rate_avg <  x_cut & survival_rate_avg >= y_cut ~ "Low entry / High survival",
      birth_rate_avg >= x_cut & survival_rate_avg <  y_cut ~ "High entry / Low survival",
      TRUE                                                ~ "Low entry / Low survival"
    )
  )

plot_v4 <- ggplot(viz_resilience, aes(birth_rate_avg, survival_rate_avg, colour = quadrant)) +
  geom_point(size = 3) +
  geom_vline(xintercept = x_cut, linetype = "dashed", colour = "grey40") +
  geom_hline(yintercept = y_cut, linetype = "dashed", colour = "grey40") +
  ggrepel::geom_label_repel(
    aes(label = section_name),
    size = 3,
    fill = "white",
    alpha = 0.85,
    label.size = 0,
    seed = 1
  ) +
  scale_colour_manual(
    values = c(
      "High entry / High survival" = "#009E73",  
      "High entry / Low survival"  = "#E69F00",  
      "Low entry / High survival"  = "#56B4E9",  
      "Low entry / Low survival"   = "#999999"  
    )
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Does high enterprise entry coincide with higher short-term survival across industries?",
    subtitle = "Entry intensity vs one-year survival (2019–2023 mean).\nQuadrants are defined using median birth and survival rates and indicate relative industry profiles rather than absolute performance thresholds.",
    x = "Average birth rate (births / active enterprises)",
    y = "Average one-year survival rate",
    colour = "Resilience profile",
    caption = "Source: UK Office for National Statistics (ONS), Business Demography."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold",size = 12),
    plot.subtitle = element_text(size = 10, colour = "grey40"),
    plot.caption = element_text(size = 9, colour = "grey40",hjust=1),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(visuals_dir, "visual_4_entry_vs_survival.png"),
  plot = plot_v4,
  width = 10,
  height = 6,
  dpi = 300
)



# Figure 5: High-growth enterprises per 1,000 active enterprises (mean + min–max)
# Compares the intensity of high-growth enterprises after normalising by industry size.
# Min–max bars show annual variation across 2019–2023 (not a confidence interval).
# Which industries generate more high-growth enterprises relative to size?


viz_quality <- business_demography_data %>%
  filter(active_enterprises > 0) %>%                       
  group_by(section_name, year) %>%                         
  summarise(
    high_growth = sum(high_growth_enterprises, na.rm = TRUE),
    active      = sum(active_enterprises, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(high_growth_per_1000 = (high_growth / active) * 1000) %>%
  group_by(section_name) %>%
  summarise(
    high_growth_mean = mean(high_growth_per_1000, na.rm = TRUE),
    high_growth_min  = min(high_growth_per_1000, na.rm = TRUE),
    high_growth_max  = max(high_growth_per_1000, na.rm = TRUE),
    .groups = "drop"
  )


plot_v5 <- ggplot(
  viz_quality,
  aes(
    x = high_growth_mean,
    y = reorder(section_name, high_growth_mean)
  )
) +
  geom_errorbar(
    aes(xmin = high_growth_min, xmax = high_growth_max),
    orientation = "y",
    height = 0.2,
    colour = "grey60",
    linewidth = 0.6
  ) +
  geom_point(size = 3, colour = "black") +
  geom_text(
    aes(label = round(high_growth_mean, 1)),
    nudge_y = -0.20,   
    size = 3,
    colour = "black"
  ) +
  labs(
    title = "Which industries generate more high-growth enterprises relative to size?",
    subtitle = "Dots show the 2019–2023 mean. Horizontal lines indicate annual min–max variation (not a confidence interval).\nHigh-growth enterprises are those classified by ONS as experiencing rapid employment growth.",
    x = "High-growth enterprises per 1,000 active enterprises",
    y = NULL,
    caption = "Source: UK Office for National Statistics (ONS), Business Demography."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold",size=12),
    plot.subtitle = element_text(size = 10, colour = "grey40"),
    plot.caption = element_text(size = 9, colour = "grey40", hjust=1),
    panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.4),
    panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank()
  )


ggsave(
  filename = file.path(visuals_dir, "visual_5_high_growth_intensity.png"),
  plot = plot_v5,
  width = 10,
  height = 6,
  dpi = 300
)


