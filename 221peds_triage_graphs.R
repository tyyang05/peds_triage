# Libraries ------------
rm(list = ls())


library(dplyr)
library(ggplot2)
library(yingtools2)
library(tidyverse)
library(ggpubr)

raw_data = read_csv('/Users/Tyler/Documents/Stanford Classes/CS221/Final/Dataframes/model_results.csv')
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
                                          "mm_late_60drop",
                                          "mm_early_nodropout",
                                          "mm_early_10drop",
                                          "mm_early_20drop",
                                          "mm_early_30drop",
                                          "mm_early_40drop"),
                        labels = c("XGBoost Class",
                                   "XGBoost Regress",
                                   "TF-IDF",
                                   "BioClinicalBERT",
                                   "MM Late-Fusion",
                                   "Late+10% Dropout",
                                   "Late+20% Dropout",
                                   "Late+30% Dropout",
                                   "Late+40% Dropout",
                                   "Late+50% Dropout",
                                   "Late+60% Dropout",
                                   "MM Early-Fusion",
                                   "Early+10% Dropout",
                                   "Early+20% Dropout",
                                   "Early+30% Dropout",
                                   "Early+40% Dropout")),
         model_type = factor(model_type, levels = c("tabular", "text", "multimodal_late", "multimodal_early"),
                             labels = c("Tabular Only", "Text Only", "Multimodal (Late Fusion)", "Multimodal (Early Fusion)")),
         cohort = factor(cohort, levels = c("adult", "peds"),
                         labels = c("Adult Cohort", "Pediatric Cohort")))

# Model Accuracies --------------------
accuracy_df = performances_df %>%
  filter(metric == "accuracy") 

overview_accuracy = ggplot(accuracy_df, aes(x = model, y = value, fill = model_type)) +
  geom_col(color = "black", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", value)),
            vjust = -0.9, size = 2.0) +
  scale_fill_manual(values = c("Multimodal (Late Fusion)" = "forestgreen", 
                               "Multimodal (Early Fusion)" = "plum4", 
                               "Text Only" = "indianred3", 
                               "Tabular Only" = "steelblue")) +
  ylim(c(0,1)) +
  facet_wrap( ~ cohort, ncol = 1) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 70, vjust = 0.5)) +
  labs(title = "Model Accuracies",x = "Model", y = 'Accuracy', fill = "Model Type")

pdf("/Users/Tyler/Documents/Stanford Classes/CS221/Final/Graphs/overview_acc.pdf", width = 9.5, height = 6)
overview_accuracy
dev.off()

balacc_df = performances_df %>%
  filter(metric == "balanced_accuracy") 

overview_balacc = ggplot(balacc_df, aes(x = model, y = value, fill = model_type)) +
  geom_col(color = "black", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", value)),
            vjust = -0.9, size = 2.0) +
  scale_fill_manual(values = c("Multimodal (Late Fusion)" = "forestgreen", 
                               "Multimodal (Early Fusion)" = "plum4", 
                               "Text Only" = "indianred3", 
                               "Tabular Only" = "steelblue")) +
  ylim(c(0,1)) +
  facet_wrap( ~ cohort, ncol = 1) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 70, vjust = 0.5)) +
  labs(title = "Model Balanced Accuracies", x = "Model", y = 'Accuracy', fill = "Model Type")

pdf("/Users/Tyler/Documents/Stanford Classes/CS221/Final/Graphs/overview_balacc.pdf", width = 8.5, height = 6)
overview_balacc
dev.off()

macrof1_df = performances_df %>%
  filter(metric == "macroF1") 

overview_macrof1 = ggplot(macrof1_df, aes(x = model, y = value, fill = model_type)) +
  geom_col(color = "black", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", value)),
            vjust = -0.9, size = 2.0) +
  scale_fill_manual(values = c("Multimodal (Late Fusion)" = "forestgreen", 
                               "Multimodal (Early Fusion)" = "plum4", 
                               "Text Only" = "indianred3", 
                               "Tabular Only" = "steelblue")) +
  ylim(c(0,1)) +
  facet_wrap( ~ cohort, ncol = 1) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 70, vjust = 0.5)) +
  labs(title = "Model Average MacroF1", x = "Model", y = 'Accuracy', fill = "Model Type")

pdf("/Users/Tyler/Documents/Stanford Classes/CS221/Final/Graphs/overview_macroF1.pdf", width = 8.5, height = 6)
overview_macrof1
dev.off()

qwk_df = performances_df %>%
  filter(metric == "kappa") 

overview_qwk = ggplot(qwk_df, aes(x = model, y = value, fill = model_type)) +
  geom_col(color = "black", alpha = 0.85, width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", value)),
            vjust = -0.9, size = 2.0) +
  scale_fill_manual(values = c("Multimodal (Late Fusion)" = "forestgreen", 
                               "Multimodal (Early Fusion)" = "plum4", 
                               "Text Only" = "indianred3", 
                               "Tabular Only" = "steelblue")) +
  ylim(c(0,1)) +
  facet_wrap( ~ cohort, ncol = 1) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 40, vjust = 0.8)) +
  labs(title = "Comparative Performance of Triage Models", 
       x = "Model", y = 'Quadratic Weighted Kappa', fill = "Model Type")

pdf("/Users/Tyler/Documents/Stanford Classes/CS221/Final/Graphs/overview_qwk.pdf", width = 8.5, height = 5)
overview_qwk
dev.off()

# Dropout Figures -----------------------------
dropout_df = raw_data %>%
  filter(model_type %in% c("multimodal_late", "multimodal_early"),
    cohort == "peds",
    metric %in% c("accuracy", "balanced_accuracy", "macroF1", "kappa")) %>%
  mutate(dropout_rate = case_when(
      str_detect(model, "nodropout") ~ "0%",
      str_detect(model, "10drop") ~ "10%",
      str_detect(model, "20drop") ~ "20%",
      str_detect(model, "30drop") ~ "30%",
      str_detect(model, "40drop") ~ "40%",
      TRUE ~ NA_character_), dropout_rate = factor(dropout_rate, levels = c("0%", "10%", "20%", "30%", "40%")),
    fusion_type = factor(
      model_type,
      levels = c("multimodal_early", "multimodal_late"),
      labels = c("Early Fusion", "Late Fusion")),
    metric = factor(metric,
      levels = c("accuracy", "balanced_accuracy", "macroF1", "kappa"),
      labels = c("Accuracy", "Balanced Accuracy", "Macro F1", "Cohen's Kappa"))) %>%
  filter(!is.na(dropout_rate))

dropout_adult_df = raw_data %>%
  filter(model_type %in% c("multimodal_late", "multimodal_early"),
         cohort == "adult",
         metric %in% c("accuracy", "balanced_accuracy", "macroF1", "kappa")) %>%
  mutate(dropout_rate = case_when(
    str_detect(model, "nodropout") ~ "0%",
    str_detect(model, "10drop") ~ "10%",
    str_detect(model, "20drop") ~ "20%",
    str_detect(model, "30drop") ~ "30%",
    str_detect(model, "40drop") ~ "40%",
    TRUE ~ NA_character_), dropout_rate = factor(dropout_rate, levels = c("0%", "10%", "20%", "30%", "40%")),
    fusion_type = factor(
      model_type,
      levels = c("multimodal_early", "multimodal_late"),
      labels = c("Early Fusion", "Late Fusion")),
    metric = factor(metric,
                    levels = c("accuracy", "balanced_accuracy", "macroF1", "kappa"),
                    labels = c("Accuracy", "Balanced Accuracy", "Macro F1", "Cohen's Kappa"))) %>%
  filter(!is.na(dropout_rate))

pdf("/Users/Tyler/Documents/Stanford Classes/CS229/CS229Final/Graphs/dropout.pdf", width = 10, height = 8)
dropout_adult
dev.off()

# Dropout Together ---------------------------

dropout_peds_tgt = ggplot(dropout_df, aes(x = dropout_rate, y = value, color = fusion_type, group = fusion_type)) + 
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.3f", value)), vjust = -0.9, size = 3.3, show.legend = FALSE) +
  facet_wrap(~ metric, ncol = 2, scales = "free_y") +
  scale_color_manual(
    values = c("Late Fusion" = "forestgreen",
               "Early Fusion" = "plum4")) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.18))) +
  labs(title = "Pediatric Cohort",
       x = "Dropout Rate During Training",
       y = "Score",
       color = "Model") +
  theme_bw(base_size = 13) +
  theme(# plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    panel.grid.minor = element_blank())

dropout_adult_tgt = ggplot(dropout_adult_df, aes(x = dropout_rate, y = value, color = fusion_type, group = fusion_type)) + 
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.3f", value)), vjust = -0.9, size = 3.3, show.legend = FALSE) +
  facet_wrap(~ metric, ncol = 2, scales = "free_y") +
  scale_color_manual(
    values = c("Late Fusion" = "forestgreen",
               "Early Fusion" = "plum4")) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.18))) +
  labs(title = "Adult Cohort",
       x = "Dropout Rate During Training",
       y = "Score",
       color = "Model") +
  theme_bw(base_size = 13) +
  theme(# plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    panel.grid.minor = element_blank())

dropout_together = ggarrange(dropout_adult_tgt, dropout_peds_tgt, nrow=1) %>%
  annotate_figure(top = text_grob("Effect of Modality Dropout on Triage Prediction", face = "bold", size = 20))

pdf("/Users/Tyler/Documents/Stanford Classes/CS221/Final/Graphs/dropout_together.pdf", width = 15, height = 7)
dropout_together
dev.off()


dropout_together_vert = ggarrange(dropout_adult_tgt, dropout_peds_tgt, nrow=2) %>%
  annotate_figure(top = text_grob("Effect of Modality Dropout on Triage Prediction", face = "bold", size = 20))

pdf("/Users/Tyler/Documents/Stanford Classes/CS221/Final/Graphs/dropout_together_vert.pdf", width = 9, height = 11)
dropout_together_vert
dev.off()


