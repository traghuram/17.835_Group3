# -*- coding: utf-8 -*-
"""
Created on Sun Apr 19 17:51:06 2020

@author: Taran
"""

import requests
import os
import pandas as pd
import json


file_path = os.chdir(r'C:\Users\Taran\Documents\Personal\Courses\MIT Final pset')


'''
import ijson
import itertools
import random
'''


'''
filename = 'UK_ocds_data.json'     
  
basic_filename = 'BasicCompanyDataAsOneFile-2020-04-01.csv'


with open('UK_ocds_data.json', 'r') as myfile:
    data=myfile.read()

data = json.load(filename)
'''

### Requests from Company House

url = 'https://api.companieshouse.gov.uk/company/08209948'
key = 'ezzyL-AgcrjuATWDriXmVdZkKzr7fYgmDDSbJjrn'
response = requests.get(url, auth=(key,''))
response.status_code

' Create a loop that pulls in records for companies that were not in basic file

list_needed = pd.read_csv('unmerged_PSC_list.csv', dtype = 'str') #[list of companies that weren't in merge file
json_list = []

last = 0
new = 12000

for idx, val in enumerate(list_needed['x'][last:new]):
    
    url = 'https://api.companieshouse.gov.uk/company/' + val
    response = requests.get(url, auth=(key,''))
    if (response.status_code != 200):
        print(idx)
        break
    json_list.append(response.json())


    
        
    


