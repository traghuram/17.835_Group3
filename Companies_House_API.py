# -*- coding: utf-8 -*-

import requests
import os
import pandas as pd
import ijson
import itertools
import random

os.chdir(r'C:\Users\Taran\Documents\Personal\Courses\MIT Final pset')


filename = 'persons-with-significant-control-snapshot-2020-04-14.txt'     
  
basic_filename = 'BasicCompanyDataAsOneFile-2020-04-01.csv'

### Method 1
     
n = sum(1 for line in open(basic_filename))-1  # Calculate number of rows in file
s = n//10  # sample size of 10%
skip = sorted(random.sample(range(1, n+1), n-s))  # n+1 to compensate for header 
df = pd.read_csv(basic_filename, skiprows=skip)



##### Method 2

# define the number of lines to read
number_of_lines = 5

with open(filename, 'r') as input_file:
    lines_cache = itertools.islice(input_file, number_of_lines)
   
    for current_line in lines_cache:
        print (current_line)



#### Method 3
        
with open(filename, 'r') as f:
    objects = ijson.items(f, 'company_number')
    columns = list(objects)



#### Company data product


def get_API():
    r = requests.get('https://api.companieshouse.gov.uk/company/00002065', auth=('xxxxxxxxxxxxxxxxxxxxxxxxxxx', ''))
    print(r.text)
    
    
def load_data():
    df = pd.read_json('persons-with-significant-control-snapshot-2020-04-14.txt')
    return df