# -*- coding: cp1252 -*-
#
# Created by Pádraig Mac Carron
#
################################
#Import Libraries
from __future__ import print_function
import networkx as nx
import numpy as np
import time
################################

### THIS SCRIPT IS WRITTEN IN PYTHON 3 ###

#this script will create a .csv file with rows of parkrun event pairs (first column: Run A, Second Column: Run B) and all the parkrunners who ran at both runs (varying number of rows)
#the next row will match Run A with another run (first column: Run A, Second Column: Run C), and this pattern continues until all parkrun event pairs have been exhausted 

### NOTES ###

#the number of columns per row will vary - the more runners who ran at both parkruns being compaired, the more rows




### FIX LINES 95, 102, 154



################################################################################################################################################################################################################################

t0 = time.time()

data_file = 'full_data_set_ordered_by_date_friends_runs.csv'

#this creates an empty dicitonary, list, and set
run_dict, run_list, run_set = {}, [], set()

#this is the first parkrun event in the data set
run0 = 'bansteadwoods0001'

#this is the first parkrun in the data set
location0 = 'bansteadwoods'

#this is the first date in the data set
date0 = '2007-06-16'

print(location0)

with open(data_file) as f:
    
    for line in f:
        
        l = line.strip().split(',')

        #this skips the headers
        if l[0] == 'Country':
            continue
        
        #this is the parkrun event
        run = l[1]
        
        #this is the date of the parkrun event
        date = l[3]
        
        #this is the location of the parkrun event
        location = ''.join([i for i in run if not i.isdigit()])

        #this will print the old data every time a new date is reached (to track progress of the script in the dataset)
        if date != date0:
            print(date)
            date0 = date

        #when it arrives at a new parkrun event
        if run != run0:

            #add the parkrun event to the list of parkrun events
            run_list.append(run0)

            #add to the dictionary the parkrun event as a key and all the runners at the event as values
            run_dict[run0] = run_set

            #this empties the parkrunner set for the new parkrun event
            run_set = set()

            #this updates the current parkrun event
            run0 = run
            
        #this adds the parkrunner, l[2] is runners' parkrun ID, to the set of runners for the current parkrun event (it won't add duplicates)
        #this needs to be below the if statement on line 69, otherwise the first runner from the next parkrun event will appear 
        #in the set from the previous parkrun event
        run_set.add(l[2])

#this adds the last parkrun event to the list of parkrun events
run_list.append(run)

#this adds the set of parkrunners at the current parkrun event to the dictionary containg all the parkrunners that attended each parkrun event
run_dict[run] = run_set

################################################################################################################################################################################################################################

#the next step is to see how many parkrunners have attended multiple (three or more) parkruns together

#this creates a new .csv file with everyone in it who has run at two or more locations together
output = open('partners_at_multiple_locations_full.csv','w')

#this writes headers for the new .csv
output.write('run1,run2,partners\r\n')

#this loop goes through every parkrun event ('run_i') and compares it to all the other parkrun events (run_j), trying to
#find instances where two parkrunners have ran at both 'run_i' and 'run_j', if parkrunners have run at the same parkrun event at two or more
#locations they are added to the output file

#this is the first location in the dateset (used to track the progess of the script; see below)
first_loc = 'bansteadwoods'

#enumerate returns all the objects (run_i) in a list, along with their indices (i), so goes through all the parkrun events in the list
for i,run_i in enumerate(run_list):

    #this returns the location of the current parkrun event
    loc_i = ''.join(s for s in run_i if s.isdigit() == False)
    
    #track progress
    print('###########run_i:', run_i)

    #this also goes through the list of all parkrun events and if the location of the parkrun event is different than the location of
    #'run_i' then it compares the lists of runners who attended 'run_i' and 'run_j', if it is more than two (i.e., if two or more
    #parkrunners ran together at two different parkrun locations), then it adds the parkrun events they ran together at and their
    #parkrun ID's to the output .csv (the 'i+1' means that it will only compare locations that come after the location of 'run_i')
    for j in range(i+1,len(run_list)):

        #this is the current parkrun event
        run_j = run_list[j]

        #gets the location of the current parkrun event
        loc_j = ''.join(s for s in run_j if s.isdigit() == False)

        #if the parkrun event location is not the same location as that of 'run_i'
        if loc_i != loc_j:

            #intersect gets a set of all the runners who ran at both 'run_i' and 'run_j'
            intersect = run_dict[run_i].intersection(run_dict[run_j])

            #if two or more parkrunners ran at both 'run_i' and 'run_j'
            if len(intersect) >= 2:

                #add the information for 'run_i' and 'run_j' and all the parkrunners who ran at both events
                output.write(run_i +','+run_j+','+','.join(e for e in intersect)+'\r\n')

output.close()

t1 = time.time()
print('\ntime =', t1-t0)

#the next step is to run through the 'partners_at_multiple_locations.csv' output file and find all the parkrunners who ran together
#at at least three different locations (see next script)