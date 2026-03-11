from transformers import AutoModel, AutoTokenizer
import pandas as pd
import numpy as np
import torch
from sklearn.preprocessing import StandardScaler
from sklearn.neural_network import MLPClassifier
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report, cohen_kappa_score

def tabular_feature(data):
    #deal with NaN
    data = data.fillna(data.mean())
    scaler = StandardScaler()
    return scaler.fit_transform(data)

def text_feature(data):
    model_name = "emilyalsentzer/Bio_ClinicalBERT"
    tokenizer = AutoTokenizer.from_pretrained(model_name)
    model = AutoModel.from_pretrained(model_name)
    #account for the NaN values and convert the text to strings
    data = [str(text) for text in data]
    full_list = []
    #process it in 20 smaller batches in order to improve memory usage
    for i in range(0,len(data), 20):
        batch = data[i: i+20]
        #tokenize the strings and add padding/truncate to ensure they are the same length
        text_input = tokenizer(batch, return_tensors="pt", padding=True, truncation=True, max_length=512)
        # we do not need the gradient here since we are just extracting features
        with torch.no_grad():
            outputs = model(**text_input)
        #we only need the CLS token which is at index 0 of the last layer
        CLS_token = outputs.last_hidden_state[:,0,:]
        full_list.append(CLS_token)
    return np.vstack(full_list)

def modality_drop(text_data, tabular_data, drop_modality, drop_rate):
    text_modified = text_data.copy()
    tabular_modified = tabular_data.copy()
    total_rows = text_data.shape[0]
    #randomly select which rows to drop such that the probability of being dropped equals the drop_rate
    dropped_rows = np.random.rand(total_rows) < drop_rate
    if drop_modality == 'text':
        text_modified[dropped_rows] = 0
    if drop_modality == 'tabular':
        tabular_modified[dropped_rows] = 0
    return np.hstack((text_modified, tabular_modified))


def fusion_model_reg(data):
    data['y'] = data['acuity'] - 1
    y_vals = data['y'].values
    x_train, x_test, y_train, y_test = train_test_split(data, y_vals, test_size=0.2, random_state=0,
                                                        stratify=y_vals)
    x_train_tabular = x_train.drop(columns=['chiefcomplaint','race','acuity', 'y'])
    x_test_tabular = x_test.drop(columns=['chiefcomplaint','race','acuity', 'y'])
    x_train_text = x_train['chiefcomplaint']
    x_test_text = x_test['chiefcomplaint']
    train_tabular = tabular_feature(x_train_tabular)
    test_tabular = tabular_feature(x_test_tabular)
    train_text = text_feature(x_train_text)
    test_text = text_feature(x_test_text)

    # bring the two  modalities together
    combined_train = np.hstack((train_text, train_tabular))
    combined_test = np.hstack((test_text, test_tabular))
    # shift the labels for the triage level down to be zero-indexed

    fusion_model = MLPClassifier(hidden_layer_sizes=(256, 256), max_iter=5000)
    fusion_model.fit(combined_train, y_train)
    predictions = fusion_model.predict(combined_test)
    print(classification_report(y_test, predictions))

def fusion_model_dropout(data, modality, drop_rate):
    data['y'] = data['acuity'] - 1
    y_vals = data['y'].values
    x_train, x_test, y_train, y_test = train_test_split(data, y_vals, test_size=0.2, random_state=0,
                                                            stratify=y_vals)
    x_train_tabular = x_train.drop(columns=['chiefcomplaint', 'race', 'acuity', 'y'])
    x_test_tabular = x_test.drop(columns=['chiefcomplaint', 'race', 'acuity', 'y'])
    x_train_text = x_train['chiefcomplaint']
    x_test_text = x_test['chiefcomplaint']
    train_tabular = tabular_feature(x_train_tabular)
    test_tabular = tabular_feature(x_test_tabular)
    train_text = text_feature(x_train_text)
    test_text = text_feature(x_test_text)

    #apply modality dropout while training to get more robust model
    dropped_train = modality_drop(train_text, train_tabular, modality, drop_rate)
    combined_test = np.hstack((test_text, test_tabular))

    fusion_model = MLPClassifier(hidden_layer_sizes=(256, 256), max_iter=5000)
    fusion_model.fit(dropped_train, y_train)
    predictions = fusion_model.predict(combined_test)
    print(f"Model Dropout: {modality}, {drop_rate}")
    print(classification_report(y_test, predictions))

def fusion_model_dropout_ped(adult_data, ped_data, modality, drop_rate):
    adult_data['y'] = adult_data['acuity'] - 1
    ped_data['y'] = ped_data['acuity'] - 1

    #keep the entire adult data intact for training and test on pediatric
    x_train_tabular = adult_data.drop(columns=['chiefcomplaint', 'race', 'acuity', 'y'])
    x_train_text = adult_data['chiefcomplaint']
    y_train = adult_data['y'].values
    x_test_tabular = ped_data.drop(columns=['chiefcomplaint', 'race', 'acuity', 'y'])
    x_test_text = ped_data['chiefcomplaint']
    y_test = ped_data['y'].values

    train_tabular = tabular_feature(x_train_tabular)
    test_tabular = tabular_feature(x_test_tabular)
    train_text = text_feature(x_train_text)
    test_text = text_feature(x_test_text)

    #apply modality dropout while training to get more robust model
    dropped_train = modality_drop(train_text, train_tabular, modality, drop_rate)
    combined_test = np.hstack((test_text, test_tabular))

    fusion_model = MLPClassifier(hidden_layer_sizes=(256, 256), max_iter=5000)
    fusion_model.fit(dropped_train, y_train)
    predictions = fusion_model.predict(combined_test)
    print(f"Model Dropout: {modality}, {drop_rate}")
    print(classification_report(y_test, predictions))

if __name__ == '__main__':
    adult_data = pd.read_csv("tyrm-alladults.csv")
    ped_data = pd.read_csv("tyrm-pedsNHAMCS.csv")
    fusion_model_reg(adult_data)
    drop_rates = [0.2, 0.4, 0.6, 0.8]
    for drop_rate in drop_rates:
        fusion_model_dropout(adult_data, 'text', drop_rate)
        fusion_model_dropout(adult_data, 'tabular', drop_rate)
        fusion_model_dropout_ped(adult_data, ped_data, 'text', drop_rate)
        fusion_model_dropout_ped(adult_data, ped_data, 'tabular', drop_rate)


