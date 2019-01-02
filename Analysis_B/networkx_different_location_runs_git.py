################################
#Import Libraries
from __future__ import print_function
import networkx as nx
import numpy as np
import time
from datetime import datetime
import collections
from itertools import chain
################################

### THIS SCRIPT IS WRITTEN IN PYTHON 3 ###

#this script will create a networkx graph of parkrunner pairs who have attended at least 8 different locations together
#this script will output a data set including all the runs from parkrunner pairs who have attended at least 8 different
#parkrun locations together and never ran within one minute of each other

#for different cut-offs for travel partners, change the number 8 below with the desired number

### NOTES ###

#it will then exclude from the graph any pairs who have run within one minute of each other

#this means that some parkrunners could be indicated as having a 'travel_partner ' but that this travel partner will not be present
#in the final outputed data set, because this parkrunner will have run within one minute of a different partner (and thus will
#excluded from the final output for this reason). This should not lead to a large number of exclusions, as few runners will have attended
#at least 8 different locations with multiple parkrunners

################################################################################################################################################################################################################################

#begin timer (takes 192 minutes to run on MacBook - 2.6 GHz Intel Core i7; 16 GB 2133 MHz LPDDR3)
t0 = time.time()

#the next step is to run through the 'partners_at_multiple_locations_full.csv' output file and find all the parkrunners who ran together
#at at least eight different locations

#this will link two parkrunners in a networkx graph, every time they run at the same parkrun event it will add 1 to the 'weight' of their
#relationship and it will also add the location at which they matched
def link(ath1, ath2):

    #if the two parkrunners are already linked add the location(s) and the parkrun event(s) at which they matched
    #also add 1 to the weight of their relationship if the current parkrun events are not already in the network x graph
    #(if they are: skip)
    if G.has_edge(ath1, ath2):
        if location1 not in G.get_edge_data(ath1, ath2)['location']:
            G.get_edge_data(ath1, ath2)['location'] += [location1]
            G.get_edge_data(ath1, ath2)['diff_locations'] += 1
        if location2 not in G.get_edge_data(ath1, ath2)['location']:
            G.get_edge_data(ath1, ath2)['location'] += [location2]
            G.get_edge_data(ath1, ath2)['diff_locations'] += 1
        if run1 not in G.get_edge_data(ath1, ath2)['parkruns']:
            G.get_edge_data(ath1, ath2)['parkruns'] += [run1]
            G.get_edge_data(ath1, ath2)['runs_together'] += 1
        if run2 not in G.get_edge_data(ath1, ath2)['parkruns']:
            G.get_edge_data(ath1, ath2)['parkruns'] += [run2]
            G.get_edge_data(ath1, ath2)['runs_together'] += 1


    #if the two parkrunners are not already linked then add them to the networkx graph, make the weight equal 2 (they have ran together twice) and add both locations
    else: 
        G.add_edge(ath1, ath2, runs_together = 2, diff_locations = 2, location = [location1] + [location2], parkruns = [run1] + [run2])

#this creates the networkx graph
G = nx.Graph()

#this opens the output file from above with all possible pairs of parkrun events (from different locations) and the parkrunners that
#ran at both events in the pair
data = 'partners_at_multiple_locations.csv'

#this goes through the dataset and adds parkrunners who have run together at the same location to the networkx graph defined above
with open(data) as data:
    
    for line in data:

        l = line.strip().split(',')

        #this skips the headers
        if l[0] == 'run1':
            continue

        #these are the parkrun events at which the parkrunners from the current line ran together
        run1 = l[0]
        run2 = l[1]

        #these are the locations at which the parkrunners from the current line ran together
        location1 = ''.join([i for i in run1 if not i.isdigit()])
        location2 = ''.join([i for i in run2 if not i.isdigit()])

        #this gets the length of the current line (line lengths vary according to how many parkrunners ran at both 'run1' and 'run2')
        line_length = len(l)

        #this gets a list of all of the parkrunners who ran at both 'run1' and 'run2'
        parkrunners = l[2:(line_length)]

        #this adds all the parkrunners from the current line to the networkx graph

        #this goes through the list of parkrunners in the current line
        for i, ath_i in enumerate(parkrunners):

            #this will skip the last parkunner in the list 'parkrunners' (they will have already been linked to everyone in the list)
            if i == (len(parkrunners) - 1):
                continue
            
            #this gets the parkrun ID of the parkrunner to be linked with the other parkrunners in the list
            ath1 = parkrunners[i]

            #this gets the index in 'parkrunners' of aht1
            ath1_index = i

            #this goes through the list of parkrunners in the current line (starting one after the index of 'ath1')
            for j in range(ath1_index + 1,len(parkrunners)):
                           
                #this gets the parkrun ID of the parkrunner to be linked with 'ath1'
                ath2 = parkrunners[j]

                #this links 'ath1' and 'ath2' (see description of function above)
                link(ath1,ath2)

#this prints the number of pairs who have ran at 8 or more different locations (parkrunners can appear in list twice: parkrunner A and parkrunner B; parkrunner A and parkrunner C)
print('number of pairs who have ran at 8 or more different locations:', len([d['diff_locations'] for u,v,d in G.edges(data=True) if d['diff_locations'] >= 8])) #8919 (for full data set)

#this prints the total number of parkrunners who had running partners at two or more parkrun events at two or more locations
print('total number of parkrunners in networkx graph:', len(G)) #220736

###############################################################################################################################################################################################################################

count = 0
removed_pairs = 0
removed_parkrunners = 0
sect_t0 = time.time()

#now we need to remove any edges (pairs of parkrunners) from the networkx graph that appear at fewer than 8 different locations together

#go through all the pairs in the networkx graph
for u,v,d in G.edges(data=True):
    
    count += 1
    
    #this creates variables for each of the parkrunners in the pair
    parkrunner1 = u
    parkrunner2 = v
    
    #remove the pair of they have ran together at fewer than 8 different locations
    if G.get_edge_data(parkrunner1, parkrunner2)['diff_locations'] < 8:
        
        G.remove_edge(parkrunner1, parkrunner2)
        
        removed_pairs += 1
        
        #parkrunners will remain in networkx graph with 0 edges, remove all parkrunners with 0 edges
        if len(G[parkrunner1]) == 0:
            
            G.remove_node(parkrunner1)
            removed_parkrunners += 1
            
            
        if len(G[parkrunner2]) == 0:
            
            G.remove_node(parkrunner2)
            removed_parkrunners += 1
        
sect_t1 = time.time()

print('\ntime =', sect_t1-sect_t0)
print('total number of parkrunner pairs in networkx graph:', count) 
print('total number of parkrunner pairs removed from networkx graph:', removed_pairs) 
print('total number of parkrunner pairs remaining in networkx graph:', count - removed_pairs) 
print('total number of removed parkrunners:', removed_parkrunners) 

###############################################################################################################################################################################################################################

#we now have a networkx graph (Graph A) with parkrunner pairs who have ran with each other at 8 different locations or more
#now we need to go through the main dataset and add any parkrunners in Graph A to a new dictionary with their times and where they ran that time at

data = 'full_data_set_ordered_by_date_friends_runs.csv'

#this creates the dictionary that will have a list of parkrun events, run times, and dates (matched by index)for all parkrunners in the networkx graph
parkrunner_dict = collections.defaultdict(list)

with open(data) as data:
    
    for line in data:
        
        l = line.strip().split(',')
        
        #this skips the headers
        if l[0] == 'Country':
            continue
        
        #this gets the parkrunner's parkrun ID
        parkrunner = l[2]
        
        #this gets the parkrun event
        event = l[1]
        
        #this gets the date of the parkrun event
        date = l[3]
        
        #this gets the parkrunner's run time
        run_time = l[5]
        
        #first check if the current parkrunner is in the networkx graph, if they are, add them to the dictionary
        if parkrunner in G:
            
            #if they are already in the dictionary append their current run time and parkrun event
            if parkrunner in parkrunner_dict:
                parkrunner_dict[parkrunner][0].append(event)
                parkrunner_dict[parkrunner][1].append(run_time)
                parkrunner_dict[parkrunner][2].append(date)
                
            #if they are not yet in the dictionary, add them
            else:
                #put variale in brackets so that a new list can be added
                parkrunner_dict[parkrunner].append([event])
                parkrunner_dict[parkrunner].append([run_time])
                parkrunner_dict[parkrunner].append([date])

        #skip any runners who are not in the networkx graph (i.e., who have not ran at the same race with someone else at least three times)
        else:
            continue
        
data.close()

###############################################################################################################################################################################################################################

#now we need to go through the networkx graph and check whether the parkrunner pairs have ever run within one minute of each other (at the same parkrun event);
#we want to exclude these pairs from analyses
            
count = 0
sect_t0 = time.time()

#this is the time format of the data (and what the datetime function will use)
MS = '%H:%M:%S'

#this is the cutoff time used below (any pairs who run within 1 minute of each other at the same parkrun event will be removed from the dataset)
cutoff = datetime.strptime('00:01:00', MS) - datetime.strptime('00:00:00', MS)

#this will create a list to be filled with parkrunners who go to different locations and run together (i.e., finish within 1 minute of each other)
ran_together = []

#this will iterate through the networkx graph containing all parkrunners who have run with someone at two or more locations
for u,v,d in G.edges(data=True):
    
    count += 1
    
    #this creates a tuple of the current pair of parkrunners
    parkrunner1 = u
    parkrunner2 = v
    
    #this will give all the parkrun events at which u and v ran together
    parkruns = G.get_edge_data(u, v)['parkruns']
    
    #this goes through all the parkrun events that the current pair ran together at
    for i in parkruns:
        
        #this will give the index in the dictionary of the run being compared
        current_run_u = parkrunner_dict[u][0].index(i)
        
        #with the index we can look up u's run time for that parkrun event
        current_time_u = parkrunner_dict[u][1][current_run_u]
        
        #this will give the index in the dictionary of the run being compared
        current_run_v = parkrunner_dict[v][0].index(i)
        
        #with the index we can look up v's run time for that parkrun event
        current_time_v = parkrunner_dict[v][1][current_run_v]
        
        #now we need to compare the times of u and v
        
        #converts 'cvurrent_time_u' and 'current_time_v' to timedeltas (which can be operated on) and gets the difference between the two
        time_dif = datetime.strptime(current_time_u, MS) - datetime.strptime(current_time_v, MS)
        
        #compares the difference between u and v's times (using absolute values) for the current parkrun event
        if abs(time_dif) < abs(cutoff):
            
            #if they ran within one minute of each other, then add the pair to this list as a tuple
            
            #create the tuple
            pair = (parkrunner1, parkrunner2)
            
            #add to the tuple to the list (if not already there)
            if pair not in ran_together:
                
                ran_together.append(pair)
            
        else:
            continue
        
        #this will convert 'ran_together' from a list of tuples to a list of all the elements (pairs contained in the tuple)
        ran_together_list = list(chain.from_iterable(ran_together))
        
        #this will give just the unique values from the list
        unique_ran_together_list = list(set(ran_together_list))
        
sect_t1 = time.time()
print('\ntime =', sect_t1-sect_t0)
        
#this will print how many unique parkrunners are in the main networkx graph and in the list 'unique_ran_together_list' (first needs to be converted to a set)
print('number of total unique parkrunners in networkx graph:', len(set(tuple(parkrunner_dict)))) 
print('total number of parkrunner pairs in networkx graph:', len(G.edges())) 
print('number of parkrunners who ran within 60 seconds / 30 seconds of travel partner at least once:', len(unique_ran_together_list)) 
print('number of total unique parkrunners with travel partners they did not run with:', len(G) - len(unique_ran_together_list)) 
print('total number of parkrunner pairs in new networkx graph (minimum of 8 runs together at different locations):', count)

###############################################################################################################################################################################################################################

sect_t0 = time.time()

#now we need to go through the full dataset one last time, looking for people who have ran together at 8 or more different locations
#and who have not finished within one minutes of each other; skip people who are not in the networkx graph containing those
#who have ran together at 8 or more locations or who are in the list of those who finished within one minute of each other;
#for those in the networkx graph who have not run within one minutes of each other: add a '1' to a 'friend_present' when their 
#running partner was at the same event as them, and add a '0' when the running partner was not at that event
            
#this is the full data set
#data = 'australia_subset.csv'
#data =  'russia_subset.csv'
data = 'full_data_set_ordered_by_date_friends_runs.csv'

#this is the data set to be written
output = open('travel_partners_full_1min_8locations.csv', 'w')

#this will get the total number of runs where people run with a person they've been to 8 or more parkrun locations with (and don't run within 1 minute of)
partner_run_count = 0

#this will be a list of tuples containing a parkrunner and the date which they had a running partner
parkrunners_with_partners = []

#this will count the total number of runs where parkrunners run without a travel partner
solo_run_count = 0

#this will count the total number of runs skipped (those runs from people who haven't run with a partner at 8 or more locations or who 
#finished within 1 minute of their running partner)
skipped_runs = 0

#location counter (this is the first location in the data set)
location0 = 'bansteadwoods'

#this will run through all the lines in the full data set
with open(data) as data:
    
    for line in data:
        
        #reads the line
        l = line.strip().split(',')
        
        #this excludes the FPRS variable (the last column)
        headers = l[:-1]
        headers_string = ','.join(str(e) for e in headers)
        
        #this writes the headers
        if l[0] == 'Country':
            output.write(headers_string + ',' + 'location' + ',' + 'travel_partner' + ',' + 'rank' + ',' + 'ave_time\r\n')
            continue
        
        location = l[1]
        
        location = ''.join([i for i in location if not i.isdigit()])
        
        #this will print the old location every time a new location is reached (to track progress of the script in the dataset)
        if location != location0:
            
            #print(location)
            location0 = location
        
        #this gets the parkrunner's parkrun ID
        parkrunner = l[2]
        
        #this gets the current parkrun event
        current_parkrun = l[1]
        
        #this gets the date of the current parkrun
        date = l[3]
        
        #we are only interested in these people
        if parkrunner in G and parkrunner not in unique_ran_together_list:
            
            #create lists for each variable of interest
            events = parkrunner_dict[parkrunner][0]
            times = parkrunner_dict[parkrunner][1]
            dates = parkrunner_dict[parkrunner][2]

            #this will create a list (times), sorted by the the list indexed in the 'pair' function (dates), 'zip' is used to combine the lists 
            sorted_times = [times for (events,times,dates) in sorted(zip(events,times,dates),key= lambda pair: pair[2])]

            #get all the run times before the date of the current run
            #get the index of the date for the current (first need to sort the dates list)
            ind = sorted(dates).index(date)

            #then take all times before (but not including) that date in the list of tuples ('ind + 1' will include the current date)
            sorted_times_to_present = sorted_times[0:ind+1]

            #goes through all the current parkrunners' runs to date and gets the average time (in seconds)
            secs = [sum(int(i) * 60**index for index, i in enumerate(i.split(":")[::-1])) for i in sorted_times_to_present]
            parkrunner_ave_time = round(sum(secs) / len(secs), 3)
            
            #creates a list to be filled with all current parkrunner's running partners average run times
            partner_ave_times = []
    
            #now go through all the current parkrunners' running partners
            for neighbor in G.neighbors(parkrunner):
                
                #if the current neighbor ran with the current parkrunner at the current parkrun, get the current neighbor's average time to that point
                if current_parkrun in G.get_edge_data(neighbor, parkrunner)['parkruns']:
                    
                    #create lists for each variable of interest
                    events_n = parkrunner_dict[neighbor][0]
                    times_n = parkrunner_dict[neighbor][1]
                    dates_n = parkrunner_dict[neighbor][2]
        
                    #this will create a list (times), sorted by the the list indexed in the 'pair' function (dates), 'zip' is used to combine the lists 
                    sorted_times_n = [times_n for (events_n,times_n,dates_n) in sorted(zip(events_n,times_n,dates_n),key= lambda pair: pair[2])]
                    
                    #get all the run times before the date of the current run
                    #get the index of the date for the current (first need to sort the dates list)
                    ind = sorted(dates_n).index(date)
        
                    #then take all times before (but not including) that date in the list of tuples ('ind + 1' will include the current date)
                    sorted_times_to_present_n = sorted_times_n[0:ind+1]
  
                    #goes through all the current parkrunners' runs to date and gets the average time (in seconds)
                    secs_n = [sum(int(i) * 60**index for index, i in enumerate(i.split(":")[::-1])) for i in sorted_times_to_present_n]
                    neighbor_ave_time = round(sum(secs_n) / len(secs_n), 3)
                    
                    #add the neighbor's average time to date to the list of neighbor average times to date
                    partner_ave_times.append(neighbor_ave_time)
                        
            #now output the data
            #if the current parkrunner had a partner at the current run
            if len(partner_ave_times) > 0:
                
                #add the parkrunner and the date to a list
                tup = (parkrunner, date)
                parkrunners_with_partners.append(tup)
                partner_run_count += 1
    
                #create a list and add the current parkrunner's average time and their partners average time
                faster_or_slower = []
                faster_or_slower.append(parkrunner_ave_time)
                #'.extend' will append elements from the list (instead of the entire list)
                faster_or_slower.extend(i for i in partner_ave_times)
 
                #sorts the list from the fastest time to the slowest time
                faster_or_slower_sorted = sorted(faster_or_slower)
 
                #this gets the rank (in terms of average run time) for the current parkrunner
                rank = faster_or_slower_sorted.index(parkrunner_ave_time) + 1
                
                #this excludes the FPRS variable (the last column)
                short_line = line.strip().split(',')[:-1]
                short_list = ','.join(str(e) for e in short_line)
 
                #the first variable is whether they ran with a partner, the second is their rank in terms of average run time
                output.write(short_list + ',' + str(location) + ',' + '1' + ',' + str(rank) + ',' + str(parkrunner_ave_time) + '\r\n')
            
            #if current parkrunner did not have a partner at the current run    
            else:
                
                #this excludes the FPRS variable (the last column)
                short_line = line.strip().split(',')[:-1]
                short_list = ','.join(str(e) for e in short_line)
                
                output.write(short_list + ',' + str(location) + ',' + '0' + ',' + 'NA' + ',' + str(parkrunner_ave_time) + '\r\n')
                solo_run_count += 1


        #skip all parkrunners who are not in the networkx graph or who have ran withing 5 minutes of their running partner
        else:
            
            #add to counter of skipped runs
            skipped_runs += 1
            continue
            
            
data.close()
output.close()

print('runs where parkrunners are attending with a partner they have been to x or more parkrun locations with (never running within 30 seconds):', partner_run_count) 

print('solo runs:', solo_run_count) 

print('excluded runs:', skipped_runs) 

#end timer, print time
sect_t1 = time.time()
print('\ntime =', sect_t1-sect_t0)
