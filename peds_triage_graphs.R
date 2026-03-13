# Libraries ------------

library(dplyr)
library(ggplot2)
library(yingtools2)
library(tidyverse)
library(ggpubr)

raw_data = read_csv("/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Dataframes/model_results.csv")
performances_df = raw_data %>% 
  mutate(model = factor(model, levels = c("xgboost_class",
                                          "xgboost_regress",
                                          "tf_idf",
                                          "bio_clinical_BERT",
                                          "mm_late_nodropout",
                                          "mm_late_10drop",
                                          "mm_late_20drop",
                                          "mm_late_30drop",
                                          "mm_late_40drop",
                                          "mm_late_50drop",
                                          "mm_late_60drop"),
                        labels = c("XGBoost Class",
                                   "XGBoost Regress",
                                   "TF-IDF",
                                   "BioClinicalBERT",
                                   "Multimodal",
                                   "10% Dropout",
                                   "20% Dropout",
                                   "30% Dropout",
                                   "40% Dropout",
                                   "50% Dropout",
                                   "60% Dropout")),
         model_type = factor(model_type, levels = c("tabular", "text", "multimodal"),
                             labels = c("Tabular Only", "Text Only", "Multimodal")),
         cohort = factor(cohort, levels = c("adult", "peds"),
                                 labels = c("Adult Cohort", "Pediatric Cohort")))

# Model Cleaned Tables --------------
adults_df = raw_data %>%
  filter(cohort == "adult") %>%
  mutate(model = factor(model, levels = c("xgboost_class",
                                          "xgboost_regress",
                                          "tf_idf",
                                          "bio_clinical_BERT",
                                          "mm_late_nodropout",
                                          "mm_late_10drop",
                                          "mm_late_20drop",
                                          "mm_late_30drop",
                                          "mm_late_40drop",
                                          "mm_late_50drop",
                                          "mm_late_60drop"),
                        labels = c("XGBoost Class",
                                   "XGBoost Regress",
                                   "TF-IDF",
                                   "BioClinicalBERT",
                                   "Multimodal",
                                   "10% Dropout",
                                   "20% Dropout",
                                   "30% Dropout",
                                   "40% Dropout",
                                   "50% Dropout",
                                   "60% Dropout"))) %>%
  select(model, metric, value) %>%
  pivot_wider(names_from = metric,values_from = value)

write_csv(adults_df, '/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Dataframes/model_overview_adult.csv')

peds_df = raw_data %>%
  filter(cohort == "peds") %>%
  mutate(model = factor(model, levels = c("xgboost_class",
                                          "xgboost_regress",
                                          "tf_idf",
                                          "bio_clinical_BERT",
                                          "mm_late_nodropout",
                                          "mm_late_10drop",
                                          "mm_late_20drop",
                                          "mm_late_30drop",
                                          "mm_late_40drop",
                                          "mm_late_50drop",
                                          "mm_late_60drop"),
                        labels = c("XGBoost Class",
                                   "XGBoost Regress",
                                   "TF-IDF",
                                   "BioClinicalBERT",
                                   "Multimodal",
                                   "10% Dropout",
                                   "20% Dropout",
                                   "30% Dropout",
                                   "40% Dropout",
                                   "50% Dropout",
                                   "60% Dropout"))) %>%
  select(model, metric, value) %>%
  pivot_wider(names_from = metric,values_from = value) %>%
  select(model, kappa, everything())
peds_df %>% dt

write_csv(peds_df, '/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Dataframes/model_overview_peds.csv')


# Model Accuracies --------------------
accuracy_df = performances_df %>%
  filter(metric == "accuracy") 

overview_accuracy = ggplot(accuracy_df, aes(x = model, y = value, fill = model_type)) +
  geom_col(color = "black", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", value)),
            vjust = -0.9, size = 2.0) +
  scale_fill_manual(values = c("Multimodal" = "forestgreen", 
                               "Text Only" = "indianred3", 
                               "Tabular Only" = "steelblue")) +
  ylim(c(0,1)) +
  facet_wrap( ~ cohort, ncol = 1) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 70, vjust = 0.5)) +
  labs(title = "Model Accuracies",x = "Model", y = 'Accuracy', fill = "Model Type")

pdf("/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Graphs/overview_acc.pdf", width = 8.5, height = 6)
overview_accuracy
dev.off()

balacc_df = performances_df %>%
  filter(metric == "balanced_accuracy") 

overview_balacc = ggplot(balacc_df, aes(x = model, y = value, fill = model_type)) +
  geom_col(color = "black", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", value)),
            vjust = -0.9, size = 2.0) +
  scale_fill_manual(values = c("Multimodal" = "forestgreen", 
                               "Text Only" = "indianred3", 
                               "Tabular Only" = "steelblue")) +
  ylim(c(0,1)) +
  facet_wrap( ~ cohort, ncol = 1) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 70, vjust = 0.5)) +
  labs(title = "Model Balanced Accuracies", x = "Model", y = 'Accuracy', fill = "Model Type")

pdf("/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Graphs/overview_balacc.pdf", width = 8.5, height = 6)
overview_balacc
dev.off()

macrof1_df = performances_df %>%
  filter(metric == "macroF1") 

overview_macrof1 = ggplot(macrof1_df, aes(x = model, y = value, fill = model_type)) +
  geom_col(color = "black", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", value)),
            vjust = -0.9, size = 2.0) +
  scale_fill_manual(values = c("Multimodal" = "forestgreen", 
                               "Text Only" = "indianred3", 
                               "Tabular Only" = "steelblue")) +
  ylim(c(0,1)) +
  facet_wrap( ~ cohort, ncol = 1) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 70, vjust = 0.5)) +
  labs(title = "Model Average MacroF1", x = "Model", y = 'Accuracy', fill = "Model Type")

pdf("/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Graphs/overview_macroF1.pdf", width = 8.5, height = 6)
overview_macrof1
dev.off()

qwk_df = performances_df %>%
  filter(metric == "kappa") 

overview_qwk = ggplot(qwk_df, aes(x = model, y = value, fill = model_type)) +
  geom_col(color = "black", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", value)),
            vjust = -0.9, size = 2.0) +
  scale_fill_manual(values = c("Multimodal" = "forestgreen", 
                               "Text Only" = "indianred3", 
                               "Tabular Only" = "steelblue")) +
  ylim(c(0,1)) +
  facet_wrap( ~ cohort, ncol = 1) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 40, vjust = 0.8)) +
  labs(title = "Comparative Performance of Triage Models", 
       x = "Model", y = 'Quadratic Weighted Kappa', fill = "Model Type")

pdf("/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Graphs/overview_qwk.pdf", width = 8.5, height = 5)
overview_qwk
dev.off()

# Dropout Figures -----------------------------
dropout_df = raw_data %>%
  filter(model_type == "multimodal", cohort == "peds") %>%
  filter(metric %in% c("accuracy", "balanced_accuracy", "macroF1", "kappa")) %>%
  mutate(model = factor(model, levels = c("mm_late_nodropout",
                                          "mm_late_10drop",
                                          "mm_late_20drop",
                                          "mm_late_30drop",
                                          "mm_late_40drop",
                                          "mm_late_50drop",
                                          "mm_late_60drop"),
                              labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%")),
    metric = factor(metric, levels = c("accuracy", "balanced_accuracy", "macroF1", "kappa"),
                            labels = c("Accuracy", "Balanced Accuracy", "Macro F1", "Cohen's Kappa")))

dropout = ggplot(dropout_df, aes(x = model, y = value, group = 1)) + 
  geom_line(color = "plum4", linewidth = 1) +
  geom_point(color = "plum4", size = 3) +
  geom_text(aes(label = sprintf("%.3f", value)),
            vjust = -0.9, size = 3.5) +
  facet_wrap(~ metric, ncol = 2, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "Effect of Modality Dropout on Pediatric Triage Predictions",
       # subtitle = "Late-fusion multimodal model evaluated on held-out pediatric cohort",
       x = "Dropout Rate During Training",
       y = "Score") +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 11),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold"),
        panel.grid.minor = element_blank())

pdf("/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Graphs/dropout.pdf", width = 10, height = 8)
dropout
dev.off()

dropout_adult_df = raw_data %>%
  filter(model_type == "multimodal", cohort == "adult") %>%
  filter(metric %in% c("accuracy", "balanced_accuracy", "macroF1", "kappa")) %>%
  mutate(model = factor(model, levels = c("mm_late_nodropout",
                                          "mm_late_10drop",
                                          "mm_late_20drop",
                                          "mm_late_30drop",
                                          "mm_late_40drop",
                                          "mm_late_50drop",
                                          "mm_late_60drop"),
                        labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%")),
         metric = factor(metric, levels = c("accuracy", "balanced_accuracy", "macroF1", "kappa"),
                         labels = c("Accuracy", "Balanced Accuracy", "Macro F1", "Cohen's Kappa")))

dropout_adult = ggplot(dropout_adult_df, aes(x = model, y = value, group = 1)) + 
  geom_line(color = "gray2", linewidth = 1) +
  geom_point(color = "gray2", size = 3) +
  geom_text(aes(label = sprintf("%.3f", value)),
            vjust = -0.9, size = 3.5) +
  facet_wrap(~ metric, ncol = 2, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "Effect of Modality Dropout on Adult Triage Predictions",
       # subtitle = "Late-fusion multimodal model evaluated on held-out pediatric cohort",
       x = "Dropout Rate During Training",
       y = "Score") +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 11),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold"),
        panel.grid.minor = element_blank())

pdf("/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Graphs/dropout.pdf", width = 10, height = 8)
dropout_adult
dev.off()

# Dropout Together ---------------------------

dropout_peds_tgt = ggplot(dropout_df, aes(x = model, y = value, group = 1)) + 
  geom_line(color = "plum4", linewidth = 1) +
  geom_point(color = "plum4", size = 3) +
  geom_text(aes(label = sprintf("%.3f", value)),
            vjust = -0.9, size = 3.5) +
  facet_wrap(~ metric, ncol = 2, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "Pediatric Cohort",
       # subtitle = "Late-fusion multimodal model evaluated on held-out pediatric cohort",
       x = "Dropout Rate During Training",
       y = "Score") +
  theme_bw(base_size = 13) +
  theme(# plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 11),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold"),
        panel.grid.minor = element_blank())

dropout_adult_tgt = ggplot(dropout_adult_df, aes(x = model, y = value, group = 1)) + 
  geom_line(color = "gray2", linewidth = 1) +
  geom_point(color = "gray2", size = 3) +
  geom_text(aes(label = sprintf("%.3f", value)),
            vjust = -0.9, size = 3.5) +
  facet_wrap(~ metric, ncol = 2, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "Adult Cohort",
       # subtitle = "Late-fusion multimodal model evaluated on held-out pediatric cohort",
       x = "Dropout Rate During Training",
       y = "Score") +
  theme_bw(base_size = 13) +
  theme(# plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 11),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold"),
        panel.grid.minor = element_blank())

dropout_together = ggarrange(dropout_adult_tgt, dropout_peds_tgt, nrow=1) %>%
  annotate_figure(top = text_grob("Effect of Modality Dropout on Triage Prediction", face = "bold", size = 20))

pdf("/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Graphs/dropout_together.pdf", width = 15, height = 7)
dropout_together
dev.off()


dropout_together_vert = ggarrange(dropout_adult_tgt, dropout_peds_tgt, nrow=2) %>%
  annotate_figure(top = text_grob("Effect of Modality Dropout on Triage Prediction", face = "bold", size = 20))

pdf("/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Graphs/dropout_together_vert.pdf", width = 9, height = 11)
dropout_together_vert
dev.off()

# Age Stratification Error Analysis -------------

# data "import"
age_strat_results = data.frame(
  age_group = rep(c("Infants (0-1 yrs) \n n=1181", 
                    "Toddlers (2-5 yrs) \n n=1344", 
                    "School Age (6-12 yrs) \n n=1572", 
                    "Adolescents (13-17 yrs) \n n=1484"), each = 3),
  modality = rep(c("Both Intact", "Text Only", "Tabular Only"), 4),
  accuracy = c(0.546, 0.450, 0.533,  # infants
               0.569, 0.459, 0.554,  # toddlers
               0.569, 0.508, 0.518,  # school Age
               0.594, 0.563, 0.477)  # adolescents
)

age_strat_results = age_strat_results %>%
  mutate(age_group = factor(age_group, levels = c("Infants (0-1 yrs) \n n=1181", 
                                                  "Toddlers (2-5 yrs) \n n=1344", 
                                                  "School Age (6-12 yrs) \n n=1572", 
                                                  "Adolescents (13-17 yrs) \n n=1484")),
         modality = factor(modality, levels = c("Both Intact", "Text Only", "Tabular Only")))

# graph
age_strat = ggplot(age_strat_results, aes(x = modality, y = accuracy, fill = modality)) +
  geom_col(color = "black", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", accuracy)), 
            vjust = -0.5, size = 4, fontface = "plain") +
  facet_wrap(~ age_group, ncol = 4) +
  scale_fill_manual(values = c("Both Intact" = "forestgreen", 
                               "Text Only" = "indianred3", 
                               "Tabular Only" = "steelblue")) +
  scale_y_continuous(limits = c(0, 0.7), breaks = seq(0, 0.7, 0.1)) +
  labs(title = "Age-Stratified Error Analysis",
       # subtitle = "With 5% Modality Dropout",
       # caption = "Figure X. Age-stratified accuracy across input modalities. Model performance is shown for three modality conditions (both modalities intact, text-only, and tabular-only) across pediatric age groups. Results are reported from the multimodal model trained with 5% modality dropout, which randomly removed one modality during training to improve robustness to missing inputs.",
       x = NULL,
       y = "Prediction Accuracy") +
  # theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
        plot.subtitle = element_text(color = "gray30", margin = margin(b = 15)),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 12),
        panel.grid.major.x = element_blank(),
        legend.position = "right",
        legend.title = element_blank())

age_strat

pdf("/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Graphs/age_strat.pdf", width = 10, height = 4)
age_strat
dev.off()

# Heatmap ------------

heat_df = data.frame(
  model = rep(c("XGBoost (Tabular)", "ClinicalBERT (Text)", "Late Fusion", "Late Fusion with Dropout"), each = 5),
  acuity = rep(c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5"), times = 4),
  f1_Score = c(
    # XGBoost
    0.28, 0.25, 0.54, 0.64, 0.22,
    # ClinicalBERT
    0.11, 0.29, 0.55, 0.47, 0.00,
    # Late Fusion
    0.17, 0.20, 0.56, 0.64, 0.01,
    # Late Fusion with Dropout
    0.14, 0.27, 0.53, 0.67, 0.00
  )
)

heat_df$acuity = factor(heat_df$acuity, levels = c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5"))
heat_df$model = factor(heat_df$model, levels = c("XGBoost (Tabular)", "ClinicalBERT (Text)", "Late Fusion", "Late Fusion with Dropout"))

heat = ggplot(heat_df, aes(x = acuity, y = model, fill = f1_Score)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient(low = "#fee0d2", high = "#de2d26", name = "F1-Score") + # Red gradient
  geom_text(aes(label = sprintf("%.2f", f1_Score)), color = "black", size = 4) + # Add numbers
  theme_minimal() +
  labs(
    title = "Model Performance on Pediatric Data by Acuity Level",
    subtitle = "Comparing F1-Scores across single and multimodal architectures",
    x = "Triage Acuity Level",
    y = "Model Architecture"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face="bold"),
    axis.text.y = element_text(face="bold"),
    panel.grid = element_blank())

pdf("/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Graphs/heat.pdf", width = 7, height = 6)
heat
dev.off()

# Drop --------------

drop_df = raw_data %>%
  filter(metric == "kappa") %>%
  filter(model %in% c("xgboost_class", "bio_clinical_BERT", "mm_late_nodropout", "mm_late_40drop")) %>%
  mutate(
    Model = case_when(
      model == "xgboost_class" ~ "Tabular Only",
      model == "bio_clinical_BERT" ~ "Text Only",
      model == "mm_late_nodropout" ~ "Multimodal",
      model == "mm_late_40drop" ~ "MM with Dropout"),
    Model = factor(Model, levels = c("Tabular Only", "Text Only", "Multimodal", "MM with Dropout")),
    Cohort = case_when(
      cohort == "adult" ~ "Adult",
      cohort == "peds" ~ "Pediatric")) %>%
  select(Model, Cohort, value)

drop_df_wide = drop_df %>%
  pivot_wider(names_from = Cohort, values_from = value) %>%
  mutate(
    Gap = Adult - Pediatric,
    Label_Y = Adult + 0.08  
  )

drop_df_long = drop_df %>%
  mutate(Cohort = factor(Cohort, levels = c("Adult", "Pediatric")))


drop_plot = ggplot() +
  geom_bar(data = drop_df_long, aes(x = Model, y = value, fill = Cohort),
           stat = "identity", position = position_dodge(width = 0.8), 
           width = 0.7, color = "black") +
  geom_text(data = drop_df_long, aes(x = Model, y = value, label = value, group = Cohort),
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3.5) +
  # add the horizontal "bridge" line 
  geom_segment(data = drop_df_wide, aes(x = as.numeric(Model) - 0.2, xend = as.numeric(Model) + 0.2,
                                   y = Label_Y - 0.02, yend = Label_Y - 0.02),
               color = "darkred", linewidth = 0.8) +
  geom_text(data = drop_df_wide, aes(x = Model, y = Label_Y, label = paste0("-", round(Gap, 3))),
            fontface = "plain", color = "darkred", size = 4) +
  scale_fill_manual(values = c("Adult" = "gray60", "Pediatric" = "plum4")) + 
  scale_y_continuous(limits = c(0, 0.8)) + 
  labs(
    title = "Generalization Gap: Adult vs. Pediatric Cohorts",
    y = "Quadratic Weighted Kappa",
    x = "Model Architecture",
    fill = "Test Cohort"
  ) +
  theme_minimal() +
  theme(legend.position = "top",
    text = element_text(size = 12),
    panel.grid.major.x = element_blank(), # Remove vertical grid lines
    plot.title = element_text(face = "bold", hjust = 0.5))

drop_plot
pdf("/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Graphs/drop.pdf", width = 6, height = 7)
drop_plot
dev.off()

# Asymmetric ------------


file_path = "/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Dataframes/peds_asymmetric.csv"
df = read.csv(file_path)

df$Drop_Text = as.factor(df$Drop_Text)
df$Drop_Tab = as.factor(df$Drop_Tab)

heat_qwk = ggplot(df, aes(x = Drop_Text, y = Drop_Tab, fill = QWK_Both)) +
  geom_tile(color = "white", size = 0.5) +  # Creates the grid with white borders
  geom_text(aes(label = sprintf("%.3f", QWK_Both)), size = 3.5, color = "black") + 
  scale_fill_gradient2(low = "#ef8a62", mid = "#f7f7f7", high = "#67a9cf", 
                       midpoint = 0.345, name = "QWK Score") + 
  labs(
    # title = "Asymmetric Modality Dropout: Pediatric Cohort",
    # subtitle = "Evaluating text vs. tabular reliance via QWK",
    x = "Text Dropout Rate",
    y = "Tabular Dropout Rate") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    panel.grid = element_blank())

heat_acc = ggplot(df, aes(x = Drop_Text, y = Drop_Tab, fill = Acc_Both)) +
  geom_tile(color = "white", size = 0.5) +  # Creates the grid with white borders
  geom_text(aes(label = sprintf("%.3f", Acc_Both)), size = 3.5, color = "black") + 
  scale_fill_gradient2(low = "#ef8a62", mid = "#f7f7f7", high = "#67a9cf", 
                       midpoint = 0.5625, name = "Accuracy") + 
  labs(
    # title = "Asymmetric Modality Dropout: Pediatric Cohort",
    # subtitle = "Evaluating text vs. tabular reliance via Accuracy",
    x = "Text Dropout Rate",
    y = "Tabular Dropout Rate") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    panel.grid = element_blank())
heat_acc

heat_asymmetric = ggarrange(heat_acc, heat_qwk) %>%
  annotate_figure(top = text_grob("Asymmetric Modality Dropout: Pediatric Cohort", face = "bold", size = 15))
heat_asymmetric

pdf("/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Graphs/asymmetric.pdf", width = 10, height = 3)
heat_asymmetric
dev.off()
