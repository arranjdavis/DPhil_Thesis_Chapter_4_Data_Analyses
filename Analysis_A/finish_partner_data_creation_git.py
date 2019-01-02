# -*- coding: cp1252 -*-
#
# Created by Pádraig Mac Carron and Arran Davis
#
################################

#import Libraries
from __future__ import print_function
import networkx as nx
import time
import os

################################

### THIS SCRIPT IS WRITTEN IN PYTHON 3 ###

#this script creates data on whether or not parkrunners finished within two seconds of another parkrunner they had finished within
#two seconds of at least three times (to acquire different cutoffs for finishing together, replace 3 with the desired number below)

#data includes whether or not they finished with a partner (as defined above), the number of previous runs they'd finished with that partner
#how many partners they finished within two seconds of on that day (as defined above), whether they were the faster or slower of the finishing partner pair

###NOTES###

#the networkx graph is not contigent on location

#if parkrunners' average run time up to a given run is the same: both marked as faster

#if a parkrunner has ran with another parkrunner 5 or more times they will be marked as partners on their first run together in the output file
#the "previous runs with current partner(s)" variable is 'NA' when parkrunners run alone and when parkrunners run with more than one partner

################################################################################################################################################################################################################################

#gets machine time
t0 = time.time()

os.chdir('/Users/arrandavis/Desktop/University of Oxford/DPHIL/Study 2/Parkrun.1/Data.Merge1')

################################################################################################################################################################################################################################

#opens data set described in main text
data = open('full_data_set_ordered_by_date_friends_runs_partners.csv')

################################################################################################################################################################################################################################

#this will build a networkx graph containing all parkrunner pairs who ever finish within two seconds of one another

#gets the headers from the old data file (these are not currently used)
headers = data.readline().strip().split(',')

#creates the empty networkx graph
G = nx.Graph()

#creates an empty dictionary and list to be filled with parkrunner data
ath_times, aths_event = {}, []

#set parkrun location and even to 0
date0, event0 = 0, 0

for line in data:
    
    #makes current line index-able
    l = line.strip().split(',')
    
    #this is the parkrun event
    event = l[1]
    
    #this is the date
    date = l[3]

    #this big 'if' statment connects parkrunners at the same event (run) as running partners if they finished within 2 seconds of each other
    if event != event0:
    
        #looks through all the parkrunners at an event
        for i in range(len(aths_event)):

            #looks through all the next (i + 1) parkrunners
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

                        #get the new average time for both parkrunners up to that point
                        G.get_edge_data(u,v)['av_u'] += [sum(ath_times[u])/len(ath_times[u])]
                        G.get_edge_data(u,v)['av_v'] += [sum(ath_times[v])/len(ath_times[v])]

                    #if they are not connected, add a link with a weight of 1 (one time running together)
                    else:
                        G.add_edge(u,v,weight=1,event=[event0],
                                   
                                   #start the new list of averages (this will be their run time for this run since it is their first run)
                                   av_u=[sum(ath_times[u])/len(ath_times[u])],
                                   av_v=[sum(ath_times[v])/len(ath_times[v])],)

                else:
                    break

        #empties the list once the location is finishsed
        aths_event = []

        #set 'event0' to the new event (the one that just caused the 'if' loop to be exited)
        event0 = event
        
    #this next line gets the parkrun location rather than specific event
    if date != date0:
        
        #print location to track progress
        print(date)
        
        #set loc0 to current location
        date0 = date
        
    #converts the time to seconds
    seconds = sum(int(i) * 60**index for index, i in enumerate(l[5].split(":")[::-1]))

    #gets the first parkrunner
    ath = l[2]

    #adds parkrunner to the list of parkrunners in the event (run)
    aths_event.append(ath)

    #if the parkrunner is not in the run, add them and their time
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

################################################################################################################################################################################################################################

#this will give the number of parkrunner pairs who have finished with each other more than three times (there are 1,160,283 parkrunners in this data set)
len([d['weight'] for u,v,d in G.edges(data=True) if d['weight'] >=3]) #78381

#the next step is to establish a cut-off point for the number of times parkrunner pairs must finish together to be included in the analysis
#this was chosen to be the pairs in the top 5% of times finished togther

#pair who have finshed together at least three times are in the top 5%, those that finished together at least two times are not
len([d['weight'] for u,v,d in G.edges(data=True) if d['weight'] >=2]) / len([d['weight'] for u,v,d in G.edges(data=True) if d['weight'] >=1]) #0.0638
len([d['weight'] for u,v,d in G.edges(data=True) if d['weight'] >=3]) / len([d['weight'] for u,v,d in G.edges(data=True) if d['weight'] >=1]) #0.0179

################################################################################################################################################################################################################################

#total number of unique parkrunners in full graph (outputs should match)
nx.number_of_nodes(G)
print('number of total unique parkrunners in networkx graph:', len(ath_times))

#total number of unique parkrunners in graph containing only those parkrunner pairs that have ran together at least three times

#creates a new networkx graph
g = nx.Graph()

#adds to the new networkx graph all parkrunners who have run with someone else at least three times
g.add_edges_from([(u,v) for u,v,d in G.edges(data=True) if d['weight']>=3])

#total number of unique parkrunners in reduced graph
print(nx.number_of_nodes(g))

################################################################################################################################################################################################################################

#now go through the data set again and add the networkx data to the output .csv

#outputs the new data now that the reduced networkx graph is created

#data to be read
data = open('full_data_set_ordered_by_date_friends_runs_partners.csv')

#data to be output
output = open('partners_output.csv','w')

#writes new headers
output.write('parkrunid,athnumber,date,time,gender,age,current_ave_time,current_prev_runs,'
             'partner,current_prev_partner_runs,number_of_partners,faster_or_slower\n')

#this gets the headers of the read data file
headers = data.readline().strip().split(',')

#creates a dictionary and list for parkrunner times and parkrunners at a given event, respectively
ath_times, aths_event = {}, []

#sets the first location and first event of that location
date0, event0 = 0, 0

#goes through all the lines in the read dataset
for line in data:

    l = line.strip().split(',')

    #this gets the parkrun event
    event = l[1]
    
    #this gets the date
    date = l[3]

    #creates empty list for parkrunners at the end of every run, updates 'event0' to the next run
    if event != event0:
        aths_event = []
        event0 = event

    #updates loc0 to the next location
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

    #if parkrunner is in the networkx graph of people who have ran with a partner three or more times
    if ath in g:

        #creates a dictionary of parkrunner's partners
        partners = []

        #for the neighbors of the parkrunner in the graph
        for n in nx.neighbors(g,ath):

            #if parkrunners is paired to a neighbor at a given event
            if event in G.get_edge_data(ath,n)['event']:

                #add to the partners dictionary
                partners.append(n)
                part = n
                
        #writes to the new .csv the parkrun event, the parkrunner's parkrun ID, the date, the time, the parkrunner's gender, the parkrunner's age category, 
        #the parkrunner's current average time, parkrunner's current previous runs
        output.write(l[1] + ',' + l[2] + ',' + l[3] + ',' + l[5] + ',' + l[8] + ',' + l[9] + ',' + str(sum(ath_times[ath])/len(ath_times[ath])) + ',' + str(len(ath_times[ath])))

        #if the parkrunner more than one partner for this run
        if len(partners) > 1:

            #writes to the new .csv the binary (1/0) partner variable, previous runs with partners (NA since more than one partner), number of partners
            output.write(',1,NA,' + str(len(partners)))
            
            #this will be a list filled with partner's average times to date
            partner_ave_times = []
            
            #this will get the amount of partners the parkrunner had at this particular event
            num_partners = len(partners)
            
            #go through all the parkrunners that are partners for the current parkrunner and get their current average time
            for runner in partners:
                
                #this will give the index of the current event
                run_ind = G.get_edge_data(ath,runner)['event'].index(event)
                
                #this will give the average time for the current runner (partner)
                p_ave_time = G.get_edge_data(ath, runner)['av_v'][run_ind]
                
                partner_ave_times.append(p_ave_time)
            
            #adds the current parkrunner's average time to the list containing the average times of their partners
            partner_ave_times.append(sum(ath_times[ath])/len(ath_times[ath]))
            
            #sort the times
            partner_ave_times = sorted(partner_ave_times)
            
            #if the parkrunner's average time is the fastest of all their partner's average times, write that they are the fastest
            if partner_ave_times.index(sum(ath_times[ath])/len(ath_times[ath])) == 0:
                
                #writes to the new .csv that the parkrunner is the faster of the partners
                output.write(',f')
            
            #if the parkrunner's average time is not the fastest of all their partner's average times, write that they are the slowest
            if partner_ave_times.index(sum(ath_times[ath])/len(ath_times[ath])) != 0:
            
                #writes to the new .csv that the parkrunner is the slower of the partners
                output.write(',s')
 
        #if the parkrunner has no partners during that run
        if len(partners) == 0:

            #writes to the new .csv the binary partner variable, previous runs with partners (NA since no partner), number of partners, and whether faster partner (f), slower partner (s), or no partner (0)
            output.write(',0,NA,0,0')

        #if the parkrunner has one partner
        if len(partners) == 1:

            #this gets the number of previous runs with the partner
            index = G.get_edge_data(ath,part)['event'].index(event)

            #writes to the new .csv the binary partner variable, previous runs with partners (NA since no partner), number of partners
            output.write(',1,' + str(index+1) + ',1')

            #this gets the times for the pair of parkrunners for that given event
            av_u, av_v = G.get_edge_data(ath,part)['av_u'][index], G.get_edge_data(ath,part)['av_v'][index]

            #these are the same loops as annotated above
            if av_u == sum(ath_times[ath])/len(ath_times[ath]):           
                t_dif = av_u - av_v
            if av_v == sum(ath_times[ath])/len(ath_times[ath]):
                t_dif = av_v - av_u
            if t_dif <= 0:
                output.write(',f')
            if t_dif > 0:
                output.write(',s')
                
        output.write("\n")


data.close()
output.close()
t1 = time.time()

#prints the amount of time that the script takes to run (machine time at beginning minus machine time at end)
print('\ntime =', t1-t0)

################################################################################################################################################################################################################################

#this will give the descriptive statistics for the main graph (G)

#this for loop get the number of parkrunner pairs at each weight (times finished together)
numbers = list(range(1, 150))

parkrunner_pairs = []

for m in numbers:
    
    pairs = len([d['weight'] for u,v,d in G.edges(data=True) if d['weight'] == m]) #4383865
    
    parkrunner_pairs.append(pairs)
    
    print(m)    
    
#this for loop creates a list representing the reduced graph (g) and the amount of times that each individual fisnihed with their running partner(s)
m = []
count = 0
for k in parkrunner_pairs:
    
    count += 1
    
    h = [count] * k
    
    m.extend(h)
    
len(m)
sum(m)

#max runs together
max(m) 

#mean runs together
sum(m) / len(m)

#sd runs together
import statistics
statistics.stdev(m)

#skewness of runs together
from scipy.stats import skew
skew(m)