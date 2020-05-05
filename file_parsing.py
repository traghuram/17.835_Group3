# -*- coding: utf-8 -*-
"""
Created on Sun Apr 19 17:51:06 2020

@author: Taran
"""

import requests
import os
import pandas as pd
import ijson
import json
import itertools
import random

file_path = os.chdir(r'C:\Users\Taran\Documents\Personal\Courses\MIT Final pset')


filename = 'UK_ocds_data.json'     
  
basic_filename = 'BasicCompanyDataAsOneFile-2020-04-01.csv'


with open('UK_ocds_data.json', 'r') as myfile:
    data=myfile.read()

data = json.load(filename)