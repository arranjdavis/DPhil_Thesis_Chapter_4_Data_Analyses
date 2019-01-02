### DESCRIPTION ###

#This script removes all runs with the time of '00:59:59' as some parkruns labeled all runs over one hour as '00:59:59' (there are over 6000 observations 
#of '00:59:59' and less than 80 observation of '00:59:58' and '01:00:00')

#This script also gives parkrunners the correct 'total runs' (or, more specifically, the number of times a parkrunner appears in the data set)

"""

@authors: Arran Davis

"""

#this will exclude all times of '00:59:59'

data = open('finishlistcountry4.csv', 'r')

#this creates a dictionary of all athletes in the data set with values being the amount of times an athlete appears in the dataset
d = {}

#reads headers
L = data.readline()

#goes through all the lines in the data set
for line in data:
    l = line.strip().split(',')
    athlete = l[2]

    #parkrunner's run time
    run_time = l[5]

    #don't add people with this time to the dictionary (see above); they won't be counted here because they will be removed below
    if run_time == '00:59:59':
        continue

    #adds parkrunner to dicitonary if not already in it
    if athlete not in d:
        d[athlete] = 1
        
    #adds 1 to parkrunner's value in dictionary
    else:
        d[athlete] += 1

#this prints the number of parkrunners in the dateset (excluding those that only had run time observations of '00:59:59')
print(len(d))

data.close()

#parkrun mislabeled some runners, giving them the incorrect number of total runs; the script below
#gives them the correct number of total runs (more specifically, the amount of times they appear in the dataset)

data = open('finishlistcountry4.csv', 'r')
output = open('full_data_set_time_errors_removed.csv', 'w')

Q = data.readline()
output.write(Q)

#goes through all the lines in the data set
for line in data:
    j = line.strip().split(',')
    
    run_time = j[5]
    
    #skip runs with this time
    if run_time == '00:59:59':
        continue
    
    athlete = j[2]
    
    #join all items in index of line up to (but not including) the total runs varianble
    astring = ",".join(map(str,j[0:7]))
        
    #join all items in indexes after (but not including) the total runs variable
    bstring = ",".join(map(str,j[8:10]))
        
    #make index for parkrunners (appearances in data set) a string
    athlete_runs = str(d[athlete])
        
    #replace the total runs variable with athlete_str
    newstring = astring + ',' + athlete_runs + ',' + bstring + '\n'
    output.write(newstring)
               
data.close()
output.close()