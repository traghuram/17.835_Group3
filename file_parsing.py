# -*- coding: utf-8 -*-
"""
Created on Sun Apr 19 17:51:06 2020

@author: Taran
"""

import requests
import os
import pandas as pd
import ijson
import itertools
import random

os.chdir(r'C:\Users\Taran\Documents\Personal\Courses\MIT Final pset')


filename = 'persons-with-significant-control-snapshot-2020-04-14.txt'     
  
basic_filename = 'BasicCompanyDataAsOneFile-2020-04-01.csv'
