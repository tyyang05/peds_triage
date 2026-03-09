from transformers import AutoModel, AutoTokenizer
import pandas as pd
import numpy as np
import torch
from sklearn.preprocessing import StandardScaler
from sklearn.neural_network import MLPClassifier
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report, cohen_kappa_score

def tabular_feature(data):
    scaler = StandardScaler()
    return scaler.fit_transform(data)

def text_feature(data):
    model_name = "emilyalsentzer/Bio_ClinicalBERT"
    tokenizer = AutoTokenizer.from_pretrained(model_name)
    model = AutoModel.from_pretrained(model_name)
    #account for the NaN values and convert the text to strings
    data = [str(text) for text in data]
    #tokenize the strings and add padding/truncate to ensure they are the same length
    text_input = tokenizer(data, return_tensors="pt", padding=True, truncation=True, max_length=512)
    # we do not need the gradient here since we are just extracting features
    with torch.no_grad():
        outputs = model(**text_input)
    #we only need the CLS token which is at index 0 of the last layer
    CLS_token = outputs.last_hidden_state[:,0,:]
    return CLS_token.numpy()

def fusion_model(data):
    data['y'] = data['acuity'] - 1
    y_vals = data['y'].values
    x_train, x_test, y_train, y_test = train_test_split(data, y_vals, test_size=0.2, random_state=0,
                                                        stratify=y_vals)
    x_train_tabular = x_train.drop(columns=['chiefcomplaint','race','acuity'])
    x_test_tabular = x_test.drop(columns=['chiefcomplaint','race','acuity'])
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

if __name__ == '__main__':
    processed_data = pd.read_csv("tyrm-alladults.csv")
    fusion_model(processed_data)




