import pandas as pd
import numpy as np
import random

random.seed(1209)

def split_dataset(ds_length, perc_train=0.8, perc_test=0.2):
    test_size = int(ds_length * perc_test)
    train_size = int(ds_length * perc_train)

    test_list = ['test'] * test_size
    train_list = ['train'] * train_size

    split = []

    split.extend(test_list)
    split.extend(train_list)

    if (len(split) < ds_length):
        extra = ['train']*(ds_length - len(split))
        split.extend(extra)

    random.shuffle(split)
    return split

def create_train_test_datasets():
	file_names = ['../data/raw/datacontinuousstudents.csv', '../data/raw/datacountstudents.csv', '../data/raw/databinarystudents.csv']

	test_file_name_template = '../data/test/{}_test.csv'
	train_file_name_template = '../data/train/{}_train.csv'

	for file in file_names:

		file_name = file.split('/')[3]
		data = pd.read_csv(file)

		split = split_dataset(data.shape[0])
		data['split'] = split

		test_data = data[data['split'] == 'test']
		train_data = data[data['split'] == 'train']

		test_data.drop('split', 1).to_csv(test_file_name_template.format(file_name.split('.')[0]), index=False)
		train_data.drop('split', 1).to_csv(train_file_name_template.format(file_name.split('.')[0]), index=False)

if __name__ == '__main__':
	create_train_test_datasets()