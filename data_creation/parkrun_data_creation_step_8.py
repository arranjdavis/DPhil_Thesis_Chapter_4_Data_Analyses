#!/usr/bin/env python3
# -*- coding: utf-8 -*-

### DESCRIPTION ###

#This script orders the runs by date, parkrunid, and then time

"""

@authors: Arran Davis and Hristo Hristov

"""

import os
import pandas as pd

os.chdir('/Users/arrandavis/Desktop/University of Oxford/DPHIL/Study 2/Parkrun.1/Data.Merge1')

#load the data set
dat = pd.read_csv('full_data_set_ordered.csv')

#see the first and last 50 lines of the data set
dat.head(50)
dat.tail(50)

#order the data set first by date, then by parkrunid, and finally by time
dat_sorted = dat.sort_values(by = ['date', 'parkrunid', 'time']).reset_index()

#drop the index column from the data set
dat_sorted = dat_sorted.drop(['index'], axis=1)

#output the data set (without the index)
dat_sorted.to_csv('full_data_set_ordered_by_date.csv', index = False)
