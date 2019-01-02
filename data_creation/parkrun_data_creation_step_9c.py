#!/usr/bin/env python3
# -*- coding: utf-8 -*-

### DESCRIPTION ###

#This script creates the FPRS variable and the previous runs variable

#The cut-off for 'finishing partners' is set to: 00:02:00
"""

@authors: Arran Davis and and Pádraig Mac Carron

"""

import networkx as nx
from datetime import datetime
import time 

#begin timer
t0 = time.time()

#os.chdir('/Users/arrandavis/Desktop/University of Oxford/DPHIL/Study 2/Parkrun.1/Data.Merge1')

data = open('full_data_set_ordered_by_date.csv', 'r')

#link function that will add parkrunner pairs to a networkx graph
def link(ath1, ath2):
    if G.has_edge(ath1, ath2):
        G.get_edge_data(ath1, ath2)['weight'] += 1
        G.get_edge_data(ath1, ath2)['date'] += [date]
    else: 
        G.add_edge(ath1, ath2, weight=1, date = [date])

#this is the cutoff point for parkrunner pairs: they must run within 15 seconds of eachother to be added to the networkx graph
tw = '00:02:00' 

#used to convert character strings to timedeltas that can be operated on
MS = '%H:%M:%S'

#converts tw to a timedelta 
tw1 = datetime.strptime(tw, MS) - datetime.strptime('00:00:00', MS)

#this is the first date in the full data set
date0 = '2007-06-16'

#creates empty networkx graph (defined above)
G = nx.Graph()
        
#reads first line of data
line = data.readline()

#creates empty networkx graph
gtot = nx.Graph()

#goes through each line in the data set
while line:
    
    #reads current line
    line = data.readline()
    
    #breaks current line into a indexable list
    l = line.strip().split(',')
    
    #this is the parkrun ID for the parkrunner in the current line
    ath1 = l[2]
    
    #this will stop the while loop at the last parkrunner in the .csv file (they will already be added to the graph as an 'ath2' (see below)
    if ath1 == '1988245' and l[1] == 'zelenograd0034':
        break
    
    #this is the time
    t1 = l[5]
    
    #this is the date
    date = l[3]
    
    #this is the event
    event = l[1]

    #if the parkrunner is not in G, add them and the date to G
    if ath1 not in G:
        
        G.add_node(ath1, date = [date])
        
    #if parkrunner is already in G, but dont have date information, add their date
    if ath1 in G and len(G.node[ath1]) == 0:
        G.node[ath1]['date'] = [date]
        
    #if they are in G, and have date information, we add date if its not already there
    elif ath1 in G and date not in G.node[ath1]['date']:
        G.node[ath1]['date'] += [date]

    if date != date0:
        print(date)
        date0 = date
    
    #defines a makrker for the next athlete (tells what byte on text)
    goto = data.tell()
    
    #reads the next line (after getting parkrunner information from the previous line)
    line = data.readline()
    
    #same comments as above (getting parkrunner information from current line)
    l = line.strip().split(',')
    ath2 = l[2]
    t2 = l[5]
    date2 = l[3]
    event2 = l[2]

    #if the parkrunner from the current line is not in G, it adds them and the date of their current run
    if ath2 not in G:
        G.add_node(ath2, date = [date2])  
        
    #if the parkrunner is already in G, but doesn't have date information, it adds the date
    if ath2 in G and len(G.node[ath2]) == 0:
        G.node[ath2]['date'] = [date2]
        
    #if the parkrunner is already in G, and has date information, it adds the current date if its not already there
    elif ath2 in G and date2 not in G.node[ath2]['date']:
        G.node[ath2]['date'] += [date2]
        
    #move to the next line (in while loop) if at the end of a particular run (otherwise it will continue looking for parkrunners to pair with 'ath1')
    if date2 != date:
        continue
    
    #puts t1 and t2 into times in the format of MS so they can be subtracted
    tdif = datetime.strptime(t2, MS) - datetime.strptime(t1, MS)
    
    #if 'tdif' is less than the criteria of tw then link the two parkrunners and go to the next line to see if tdif < tw1 is still true
    while tdif < tw1:
        
        #add both parkrunners to the networkx graph (G)
        link(ath1, ath2)

        #continues to get new ath2 (and new t2) and links with ath1 if tdif < tw1
        line = data.readline()
        
        #stop script if it comes to the last line
        if len(line) <2:
            break 
        
        #get all the information for the parkrunner in the next line
        l = line.strip().split(',')
        ath2 = l[2]
        t2 = l[5]
        date2 = l[3]
        
        #stops if at a new date (i.e., a new parkrun)
        if date2 != date:
            break
            
        #stops if at a new event (i.e., a new parkrun)
        if event2 != event:
            break
        
        #get tdif for new parkrunner then checks if tdif < tw1, stops if not (the absolute value bit is for the reason explained below)
        tdif = abs(datetime.strptime(t2, MS) - datetime.strptime(t1, MS))
    
    #go back to byte in data (where the while loop was before it read the next line)
    data.seek(goto)

data.close()
print(len(G))
print(len(G.edges()))

################################################################################################################################################################################################################################

#reopen the data set
data = open('full_data_set_ordered_by_date.csv', 'r')
output = open('full_data_set_ordered_by_date_friends_2min_runs.csv', 'w')

#this is the first date in the full data set (this will be printed when the date changes, to track the script's progress in the data set)
date0 = '2007-06-16'

#read the first line of the data set
line = data.readline()

#writes first line of data plus new headers in output
output.write(line.strip() + ',' + 'previous_runs' + ',' 'partner_relation\r\n')

#make sure all parkrunners are included in G (the networkx graph defined above)
not_included = 0

#goes through each line in the data set
while line:
    
    #reads current line
    line = data.readline()

    #breaks current line into a indexable list
    l = line.strip().split(',')
    
    #this is the parkrun ID for the parkrunner in the current line
    ath1 = l[2]
    
    #this is the time
    t1 = l[5]
    
    #this is the date
    date = l[3]
    
    #this is just to make sure that all parkrunners are in G
    if ath1 not in G:
        
        #add to the count of parkrunners not included in G
        not_included += 1
    
    #this will stop the while loop at the last parkrunner in the .csv file (thus excluding them, they are under 18)
    if ath1 == '1988245' and l[1] == 'zelenograd0034':

        break
    
    #gets a sorted list of all the dates that parkrunner ran (from networkx graph defined above)
    dateran = sorted(G.node[ath1]['date'])
    
    #gets index of the current date (first run will be 0; 0 previous runs)
    prev_runs = dateran.index(date)
    
    #adds number of previous runs to the output file, but first outputs the rest of the current line
    output.write(line.strip() + ',' + str(prev_runs))
    
    #creates a number for the amount of parkrunners the current parkrunner is linked with at the current run
    n_neighbors = 0
            
    #interates through all the parkrunners that the current parkrunner is linked with
    for neighbor in G.neighbors(ath1):
                
        #sorts all the dates that they match with a given athlete
        dates_together = sorted(G.get_edge_data(ath1, neighbor)['date'])
                
        #if they ran together on the current run
        if date in dates_together:
                    
            #looks up list of how many times they have ran togetehr and adds 1 (since index begins at 0)
            #and then adds this to however many times the current parkrunner has ran with the other parkrunners they finished with that day
            n_neighbors += dates_together.index(date) + 1
            
    #adds the 'FPRS' (partner_relation) variable to the output file; this is the amount of previous runs within tw that a parkrunner has with their finsihing
    #partner(s) on a given day     
    output.write(',' + str(n_neighbors) + '\r\n')
    
    #print when the script gets to a new date (to track progress)    
    if date != date0:
        print(date)
        date0 = date
        
data.close()
output.close()

#prints the number of parkrunners in the data set that were not in G       
print(not_included)
 
#adds up the total number of edges greater than 10 (parkrunners who have finished within tw at least 10 times)
num10 = 0
num10 += len([d['weight'] for u,v,d in G.edges(data=True) if d['weight'] >=10])

#number of parkrunner pairs that finished within tw more than 10 times
print(num10)

#number of parkrunners linked with other parkrunners in the total dataset
print(len(G))

#total number of links between parkrunners
print(len(G.edges()))

#end timer, print time
t1 = time.time()
print('\ntime =', t1 - t0)      
    
            
                    
        
    
        
    

