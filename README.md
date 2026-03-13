# peds_triage
**Improving Pediatric Emergency Triage with Modality Dropout in Multimodal EHR Models**

Emergency department (ED) triage is critical to patient care, yet current practices are highly variable. Triage decisions typically rely on a combination of quantitative vital signs and qualitative nurse-reported notes, with significant subjectivity leading to some inter-provider inconsistency.

While machine learning models have been developed to predict Emergency Severity Index (ESI) levels from multimodal EHR data, many suffer from modality collapse, over-weighting structured vitals while underutilizing informative clinical text. This limitation is especially problematic in pediatrics, where normal vital ranges vary with age and development, and the most important information is often conveyed through the lower-weighted caregiver and nurse descriptions.

To address this gap, we propose a multimodal learning framework that explicitly balances structured and unstructured EHR data to improve pediatric triage. Our goal is to develop a model that jointly leverages vital signs and clinical text, using modality dropout to ensure that neither modality dominates, in order to more accurately assign patients to triage categories and ultimately improve patient outcomes.

This repository has three main files: \\
`triage_tabular_baseline.ipynb` — single modality tabular-only models. 

`triage_text_baseline.ipynb` - single modality text-only models. 

`mm_latefusion_moddrop.ipynb` - late fusion multimodal model.
