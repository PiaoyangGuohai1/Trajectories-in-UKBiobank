rm(list = ls())
library(data.table)
library(table1)
library(ggplot2)
library(ggthemr)
library(paletteer)
library(patchwork)
library(dplyr)
library(tidyr)
library(gridExtra)


data_filtered <- fread("result/data/01_personal_info/UKB_ICD10_data_long_filtered.csv")
min(data_filtered$age_at_diagnosis)
setDT(data_filtered)

if(T){
  
  
  abbr <- fread("result/data/01_personal_info/ICD10_abbreviation_name.csv") %>%
    select(abbreviation, disease_name) %>%
    rename(disease = abbreviation)
  
  disease_counts <- data_filtered %>%
    group_by(disease, disease_category) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(count)) %>%
    left_join(abbr, by = "disease")
  
  library(tidyr)
  
  disease_counts <- data_filtered %>%
    group_by(disease, disease_category) %>%
    summarise(
      total  = n(),
      Female = sum(Sex == "Female"),
      Male   = sum(Sex == "Male"),
      .groups = "drop"
    ) %>%
    left_join(abbr, by = "disease") %>%
    arrange(desc(total))
  
  fwrite(disease_counts, "result/data/01_personal_info/supplement_table_1.csv")
}


if(T){
  table(data_filtered$disease_category)
  table(data_filtered$diagnosis_source2)
  
  data_filtered[diagnosis_source2 == "Hospital admissions data", 
                diagnosis_source2 := "Hospital admissions"]
  
  
  data_filtered[disease_category == "E: Metabolic", 
                disease_category := "Chapter IV (Endocrine, Nutritional, and Metabolic Diseases)"]
  
  data_filtered[disease_category == "I: Circulatory", 
                disease_category := "Chapter IX (Diseases of the Circulatory System)"]
  
  data_filtered[disease_category == "N: Genitourinary", 
                disease_category := "Chapter XIV (Diseases of the Genitourinary System)"]
  
  data_filtered[, diagnosis_year := year(diagnosis_date)]
  
  head(data_filtered)
}

ggthemr_reset()
ggthemr("flat")

my_d_palette<- paletteer_d("ggsci::nrc_npg")[c(1,3,4)]


if(F){
  library(ggplot2)
  library(patchwork)
  
  common_theme <- theme(
    text            = element_text(size = 14),
    axis.title      = element_text(size = 14),
    axis.text       = element_text(size = 12),
    strip.text      = element_text(size = 14),
    plot.title      = element_text(hjust = 0),
    legend.box      = "horizontal",
    legend.title    = element_text(size = 16),
    legend.text     = element_text(size = 12)
  )
  
  p1 <- ggplot(data_filtered, aes(x = age_at_diagnosis, fill = disease_category)) +
    stat_density(aes(y = after_stat(count)),
                 geom     = "area",
                 position = "stack",
                 alpha    = 0.7) +
    labs(title = "Age Distribution of Diagnoses",
         x     = "Age (years)",
         y     = "Diagnoses Count",
         fill  = "ICD-10 chapters") +
    facet_grid(Sex ~ diagnosis_source2) +
    scale_fill_manual(values = my_d_palette) +
    common_theme +
    theme(legend.position = "none")
  
  p2 <- ggplot(data_filtered, aes(x = diagnosis_year, fill = disease_category)) +
    stat_density(aes(y = after_stat(count)),
                 geom     = "area",
                 position = "stack",
                 alpha    = 0.7) +
    labs(title = "Temporal Distribution of Diagnoses",
         x     = "Year of Diagnosis",
         y     = "Diagnoses Count",
         fill  = "ICD-10 chapters") +
    facet_grid(Sex ~ diagnosis_source2) +
    scale_fill_manual(
      values = my_d_palette,
      guide = guide_legend(
        title.position = "left",
        title.hjust    = 0,
        direction      = "vertical"
      )
    ) +
    common_theme +
    theme(
      strip.text.x     = element_blank(),
      legend.position  = "bottom"
    )
  
  p1 / p2
  
  ggsave(filename = "result/plot/figure_3_Diagnosis_density.pdf", height = 10, width = 12)
}



disease_counts <- aggregate(disease ~ disease_category, data = data_filtered, FUN = function(x) length(unique(x)))
disease_counts
sum(disease_counts$disease)
colnames(disease_counts) <- c("disease_category", "unique_disease_count")

data_filtered[, .SD[1], by = eid][, .N, by = Sex]

prop.table(table(data_filtered$diagnosis_source2)) * 100

prop.table(table(data_filtered$disease_category)) * 100

disease_counts <- data_filtered %>%
  group_by(disease, disease_category, Sex) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count)) 

disease_icd10 <- read.csv("result/data/01_personal_info/ICD10_abbreviation_name.csv") %>%
  select(abbreviation, disease_name)

disease_counts_wide <- disease_counts %>%
  pivot_wider(
    names_from = Sex,
    values_from = count,
    values_fill = list(count = 0)
  )%>%
  mutate(
    total_count = Male + Female,
    gender_diff_percent = ((Male - Female) / total_count) * 100
  ) %>%
  left_join(disease_icd10, by = c("disease" = "abbreviation"))

top50_data <- data_filtered %>%
  filter(disease %in% disease_counts$disease) %>%
  group_by(disease, disease_category, Sex) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(count = ifelse(Sex == "Female", -count, count))

age_data <- data_filtered %>%
  filter(disease %in% disease_counts$disease) %>%
  group_by(disease, Sex) %>%
  summarise(
    mean_age = mean(age_at_diagnosis, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = Sex, values_from = mean_age, names_prefix = "Mean_Age_")

p_values <- data_filtered %>%
  filter(disease %in% disease_counts$disease) %>%
  group_by(disease) %>%
  filter(n_distinct(Sex) == 2) %>%
  summarise(
    p_value = tryCatch({
      wilcox.test(age_at_diagnosis ~ Sex)$p.value
    }, error = function(e) { NA }),
    .groups = 'drop'
  )

age_pvalues <- age_data %>%
  left_join(p_values, by = "disease") %>%
  mutate(
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      !is.na(p_value) ~ "ns",
      TRUE            ~ ""
    )
  )

table_data <- age_pvalues %>%
  mutate(
    Disease = disease,
    `Mean Age (Male)` = round(Mean_Age_Male, 2),
    `Mean Age (Female)` = round(Mean_Age_Female, 2),
    `p-value` = round(p_value, 2),
    `Significance` = significance
  ) %>%
  mutate(Disease = factor(Disease, levels = unique(disease_counts$disease))) %>%
  arrange(Disease)

table_grob <- table_data %>%
  select(`Mean Age (Male)`, `Mean Age (Female)`, Significance)  %>%
  tableGrob(table_data, rows = NULL, theme = ttheme_minimal(base_size = 8))

count_plot <- ggplot(top50_data, aes(x = reorder(disease, abs(count)), y = count, fill = disease_category)) +
  geom_bar(stat = "identity", position = "identity") +
  labs(
    title = "Top 50 Diseases by Count (Split by Sex)",
    x = "Disease",
    y = "Count (Positive: Male, Negative: Female)",
    fill = "Disease Category"
  ) +
  coord_flip() +
  theme_bw() +
  scale_fill_brewer(palette = "Set2", name = "Disease Category") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")

if(T){
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(purrr)
  
  category = "N: Genitourinary"
  
  
  disease_counts <- data_filtered %>%
    filter(disease_category == category) %>%
    group_by(disease, disease_category) %>%
    summarise(total_count = n(), .groups = 'drop') %>%
    arrange(desc(total_count)) %>%
    filter(total_count > 100)
  length(disease_counts$disease)
  
  
  top_data <- data_filtered %>%
    filter(disease %in% disease_counts$disease)
  
  total_counts <- top_data %>%
    group_by(disease, Sex) %>%
    summarise(N = n(), .groups = 'drop')
  
  p_values <- top_data %>%
    group_by(disease) %>%
    filter(n_distinct(Sex) == 2) %>%
    summarise(
      p_value = tryCatch({
        wilcox.test(age_at_diagnosis ~ Sex)$p.value
      }, error = function(e) { NA }),
      .groups = 'drop'
    )
  
  annotations <- total_counts %>%
    pivot_wider(names_from = Sex, values_from = N, values_fill = list(N = 0)) %>%
    mutate(
      label_male = paste0("N(Male)=", Male),
      label_female = paste0("N(Female)=", Female)
    ) %>%
    select(disease, label_male, label_female) %>%
    left_join(p_values, by = "disease") %>%
    mutate(
      significance = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01  ~ "**",
        p_value < 0.05  ~ "*",
        !is.na(p_value) ~ "ns",
        TRUE            ~ ""
      )
    )
  
  p <- ggplot(top_data, aes(x = age_at_diagnosis, color = Sex, fill = Sex)) +
    geom_density(
      data = top_data %>% group_by(disease, Sex) %>% filter(n() >= 2),
      alpha = 0.4
    ) +
    facet_wrap(~ disease, ncol = 5, scales = "free") +
    labs(
      title = "Top 50 Diseases Age Density Distribution by Sex",
      x = "Age at Diagnosis",
      y = "Density",
      color = "Sex",
      fill = "Sex"
    ) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    theme_bw() +
    theme(
      strip.text = element_text(size = 6, face = "bold"),
      axis.text = element_text(size = 6),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    ) +
    geom_text(
      data = annotations,
      aes(x = -Inf, y = Inf, label = label_male),
      hjust = -0.1,
      vjust = 4.5,
      size = 3,
      fontface = "bold",
      color = "blue",
      inherit.aes = FALSE
    ) +
    geom_text(
      data = annotations,
      aes(x = -Inf, y = Inf, label = label_female),
      hjust = -0.1,
      vjust = 3,
      size = 3,
      fontface = "bold",
      color = "red",
      inherit.aes = FALSE
    ) +
    geom_text(
      data = annotations,
      aes(x = -Inf, y = Inf, label = ifelse(!is.na(p_value), paste0("p=", signif(p_value, 3)), "p=NA")),
      hjust = -0.1,
      vjust = 1.5,
      size = 3,
      fontface = "bold",
      color = "black",
      inherit.aes = FALSE
    ) +
    coord_cartesian(clip = "off")
  
  p
  
  ggsave(paste0("result/plot/DensityPlot_", gsub(": ", "_", category), "_Disease_Age_Distribution_by_Sex.pdf"), plot = p, height = 30, width = 15)
}




date_ranges <- data_filtered %>%
  group_by(diagnosis_source2) %>%
  summarise(min_date = min(diagnosis_date),
            max_date = max(diagnosis_date))





data_wide <- data_filtered[, .(
  Birth_year = first(Birth_year),
  Sex = first(Sex),
  
  total_diagnoses = .N,
  
  E_Metabolic = sum(disease_category == "E: Metabolic"),
  N_Genitourinary = sum(disease_category == "N: Genitourinary"),
  I_Circulatory = sum(disease_category == "I: Circulatory"),
  
  mean_age = mean(age_at_diagnosis),
  
  E_metabolic_mean = mean(age_at_diagnosis[disease_category == "E: Metabolic"]),
  N_genitourinary_mean = mean(age_at_diagnosis[disease_category == "N: Genitourinary"]),
  I_circulatory_mean = mean(age_at_diagnosis[disease_category == "I: Circulatory"]),
  
  Death = sum(diagnosis_source2 == "Death register"),
  Hospital = sum(diagnosis_source2 == "Hospital admissions data"),
  Primary = sum(diagnosis_source2 == "Primary care"),
  Self = sum(diagnosis_source2 == "Self-report"),
  
  Death_mean = mean(age_at_diagnosis[diagnosis_source2 == "Death register"]),
  Hospital_mean = mean(age_at_diagnosis[diagnosis_source2 == "Hospital admissions data"]),
  Primary_mean = mean(age_at_diagnosis[diagnosis_source2 == "Primary care"]),
  Self_mean = mean(age_at_diagnosis[diagnosis_source2 == "Self-report"])
  
), by = eid]

cols <- c("E_metabolic_mean", "N_genitourinary_mean", "I_circulatory_mean",
          "Death_mean","Hospital_mean","Primary_mean","Self_mean")
data_wide[, (cols) := lapply(.SD, function(x) ifelse(is.nan(x), NA, x)), .SDcols = cols]

data_wide$Sex <- factor(data_wide$Sex, levels = c("Male", "Female"))

p_values <- data_wide[, lapply(.SD, function(x) {
  if (is.numeric(x)) {
    test <- wilcox.test(x ~ Sex, data = data_wide)
    return(test$p.value)
  }
  return(NA)
}), .SDcols = c("total_diagnoses", "E_Metabolic", "N_Genitourinary", "I_Circulatory", 
                "mean_age", "E_metabolic_mean", "N_genitourinary_mean", "I_circulatory_mean",
                "Death", "Hospital", "Primary", "Self",
                "Death_mean","Hospital_mean","Primary_mean","Self_mean")]


table1(~ Birth_year + total_diagnoses + 
         E_Metabolic + N_Genitourinary + I_Circulatory +
         mean_age + E_metabolic_mean + N_genitourinary_mean + I_circulatory_mean +
         Death + Hospital + Primary + Self +
         Death_mean + Hospital_mean + Primary_mean + Self_mean| Sex, 
       data = data_wide,
       overall = "Total Population",
       pvalues = p_values,
       caption = "Patient Baseline Characteristics with P-values for Sex Differences")



rm(p_values, cols, data_wide)
