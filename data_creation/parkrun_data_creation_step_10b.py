#!/usr/bin/env python3
# -*- coding: utf-8 -*-

### DESCRIPTION ###

#This script creates a variable that indicates the presence of a likely running partner; someone
#who (faster) parkrunners modify their running pace to run alongside

#the cut-off for parkrunners to be considered running partners is five finishes within two seconds
#(see main text for a justification of this cut-off) 

"""

@authors: Arran Davis and Hristo Hristov

"""

from __future__ import print_function
import networkx as nx
import numpy as np
import time
import os

#gets machine time
t0 = time.time()

os.chdir('/Users/arrandavis/Desktop/University of Oxford/DPHIL/Study 2/Parkrun.1/Data.Merge1')

data = open('full_data_set_ordered_by_date_friends_1min_runs.csv')

#gets the headers from the old data file (these are not currently used)
headers = data.readline().strip().split(',')

G = nx.Graph()

ath_times, aths_event = {}, []

#this sets the date and event variable to 0 (for now)
date0, event0 = 0, 0

#go through the entire data set
for line in data:
    
    l = line.strip().split(',')
    
    #get the current parkrun event
    event = l[1]
    
    #get the date of the current parkrun
    date = l[3]

    #this big if statement connects parkrunners at the same event (run) as running partners if they finished within 2 seconds
    #this happens only once all parkrunners at the event have have been added to ath_times
    if event != event0:
    
        #looks through all the parkrunners at an event
        for i in range(len(aths_event)):

            #looks through all the next (i + 1) parkrunner
            for j in range(i+1,len(aths_event)):

                #u and v become the pair of parkrunners
                u,v = aths_event[i],aths_event[j]

                #if the times for both parkrunners in the most recent event [-1] are within two seconds 
                if ath_times[v][-1] - ath_times[u][-1] <= 2:

                    #if the two parkrunners are already linked
                    if G.has_edge(u,v):

                        #add 1 to the amount of times they have ran together
                        G.get_edge_data(u,v)['weight'] += 1

                        #add the event where they ran together
                        G.get_edge_data(u,v)['event'] += [event0]

                        #get the new average time for both runners up to that point
                        G.get_edge_data(u,v)['av_u'] += [sum(ath_times[u])/len(ath_times[u])]
                        G.get_edge_data(u,v)['av_v'] += [sum(ath_times[v])/len(ath_times[v])]

                    #if they are not connected, add a link with a weight of 1 (one time running together)
                    else:
                        G.add_edge(u,v,weight=1,event=[event0],
                                   
                                   #start the new list of averages (this will be their run time for this run since it is their first run)
                                   av_u=[sum(ath_times[u])/len(ath_times[u])],
                                   av_v=[sum(ath_times[v])/len(ath_times[v])],)
                
                #if parkrunners did not run within two seconds of eachother, move to the next line
                else:
                    break

        #empties the list once the location is finishsed
        aths_event = []

        #set 'event0' to the new event (the one that just caused the if loop to be exited)
        event0 = event
        
    #this next line gets the location rather than specific event
    loc = ''.join([i for i in event if not i.isdigit()])
        
    if date != date0:
        
        #print new date (to track progress)              
        print(date)
        date0 = date
        
    #converts the time to seconds
    seconds = sum(int(i) * 60**index for index, i in enumerate(l[5].split(":")[::-1]))

    #gets the first athlete
    ath = l[2]

    #adds athlete to the list of athletes in the event (run)
    aths_event.append(ath)

    #if the athlete is not in the run add them and their time
    if ath not in ath_times:
        ath_times[ath] = [seconds]
    else:
        ath_times[ath].append(seconds)

#this is the same loop annotated above (it captures the last event)
for i in range(len(aths_event)):
    
    for j in range(i+1,len(aths_event)):
        u,v = aths_event[i],aths_event[j]
        
        if ath_times[v][-1] - ath_times[u][-1] <= 2:
            
            if G.has_edge(u,v):
                G.get_edge_data(u,v)['weight'] += 1
                G.get_edge_data(u,v)['event'] += [event0]
                G.get_edge_data(u,v)['av_u'] += [sum(ath_times[u])/len(ath_times[u])]
                G.get_edge_data(u,v)['av_v'] += [sum(ath_times[v])/len(ath_times[v])]

            else:
                G.add_edge(u,v,weight=1,event=[event0],
                           av_u=[sum(ath_times[u])/len(ath_times[u])],
                           av_v=[sum(ath_times[v])/len(ath_times[v])],)

        else:
            break


data.close()

print(len(G))
print(len(G.edges()))

################################################################################################################################################################################################################################

#now go through the dataset again and add the networkx data to the output .csv

#creates a new networkx graph
g = nx.Graph()

#adds to the new networkx graph all athletes who have run with someone else at least 5 times
g.add_edges_from([(u,v) for u,v,d in G.edges(data=True) if d['weight']>4])

#outputs the new data now that the networkx graphs are created

#get data set
data = open('full_data_set_ordered_by_date_friends_1min_runs.csv')

#output = open('partners_aberdeen.csv','w')
output = open('full_data_set_ordered_by_date_friends_1min_runs_partners.csv','w')

#this gets the headers of the read data file
headers = data.readline()

#write the headers to the output file
output.write(headers.strip() + ',partners\n')

#creates a dictionary and list for athlete times and athletes at a given event, respectively
ath_times, aths_event = {}, []

#sets the first location and first event of that location
loc0, event0 = 0, 0

#goes through all the lines in the read dataset
for line in data:

    l = line.strip().split(',')

    #this gets the parkrun event
    event = l[1]
    
    #this gets the date of the current parkrun event
    date = l[3]

    #creates empty list for parkrunners at the end of every run, updates 'event0' to the next run
    if event != event0:
        aths_event = []
        event0 = event

    #updates loc0 to the next location
    loc = ''.join([i for i in event if not i.isdigit()])
        
    if date != date0:
        print(date)
        date0 = date
        
    #this converts the time to seconds
    seconds = sum(int(i) * 60**index for index, i in enumerate(l[5].split(":")[::-1]))
    
    #gets the parkrunner's parkrun ID
    ath = l[2]

    #adds the parkrunner to the list of parkrunners in the current run
    aths_event.append(ath)

    #adds the parkrunner to the dictionary of parkrunner times
    if ath not in ath_times:
        ath_times[ath] = [seconds]
    else:
        ath_times[ath].append(seconds)
    
    #this will output the current line in the data set being read
    output.write(line.strip())  
        
    #if parkrunner is in the networkx graph of people who have ran with a partner 5 or more times
    if ath in g:

        #creates a dictionary of parkrunners' partners
        partners = []

        #for the neighbors of the parkrunners in the graph
        for n in nx.neighbors(g,ath):

            #if parkrunner is paired to a neighbor at a given event
            if event in G.get_edge_data(ath,n)['event']:

                #add to the partners dictionary
                partners.append(n)
                
        #if the parkrunner has more than one partner for this run
        if len(partners) > 1:

            #writes to the new .csv the binary (1/0) partner variable, previous runs with partners (NA since more than one partner), number of partners
            output.write(',1\n')

        #if the parkrunner has no partners for this run
        if len(partners) == 0:

            #writes to the new .csv the binary partner variable
            output.write(',0\n')

        #if the parkrunner has one partner
        if len(partners) == 1:

            #writes to the new .csv the binary partner variable
            output.write(',1\n')
            
    #if the parkrunner is not in the networkx graph of people who have ran with a partner 5 or more times
    else:
        output.write(',0\n')
        
        
data.close()
output.close()

#adds up the total number of edges greater than 10 (parkrunner pairs who have finished within two seconds of eachother five or more times)
num10 = 0
num10 += len([d['weight'] for u,v,d in G.edges(data=True) if d['weight'] >=10])

#number of parkrunners pairs that finished within two seconds of eachother more than 10 times
print(num10)

#number of parkrunners linked with other parkrunners in the total dataset
print(len(G))

#total number of links between parkrunners
print(len(G.edges()))

#end timer, print time
t1 = time.time()
print('\ntime =', t1 - t0)      

