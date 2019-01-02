### DESCRIPTION ###

#This script takes the data from the HTML files downloaded from every parkrun location and convert it to a .csv file
#These HTML files contain all the information in parkruns' online, publically available data set (as of 7 November 2015)

### NOTES ###

#This script must be in the same folder as the HTML files containing parkruns' online data set

"""

@authors: Arran Davis and Pádraig Mac Carron

"""

import time as timer
import os

#create timer
t0=timer.time()


#opens all files in directory (data)
files = []

#calling the files 'f'
for f in os.listdir('data/'):
    
    #skip irrelevant files that start with '.' (these files contain no information on parkrunners' runs)
    if f[0] != '.':
        
        #replaces .html with nothing
        files.append(f.replace('.html',''))

#creates a .csv       
output=open('finishlist.csv','w')

#this gives the column headers: parkrun ID = parkrun event; athnumber = parkrunners' parkrun ID, club = parkrunners' club (0 is no club), time = parkrunners' 5km time;
#sex&age = parkrunners' parkrun sex&age category; totalruns = total runs parkrunners has done at that particular parkun
output.write('parkrunid,athnumber,date,club,time,sex&age,totalruns\r\n')

counter=0

#this for loop goes through all the files in the directory
for event in files:
    
    #this opens the file
    data = open('data/' + event +'.html','r')

    #finds the date of the parkrun event in the file
    for line in data:
        if 'table class' in line:
            date1 = line.strip().split('</h2>')[0]
            
            #the above line makes 'date1' a string and then the below line makes date = the last 10 characters of the string (counting backwards from the end)
            date = date1[-10:]
            break

    #this closes the data file
    data.close()

    #split the table, each row is indetified by <tr>
    lines = line.split('<tr>')

    #try to go through the table and find variabels, but if this doesn't work for all variables print 'error'
    try:       
        
        #iterate through each row in the line containing 'table class'
        for line in lines:
                
            #some event locations are unknown or have corrupted html files
            if 'Unknown' in line or "Ukendt" in line or '\xd0\x9d\xd0\xb5\xd0\xb8\xd0\xb7\xd0\xb2\xd0\xb5\xd1\x81\xd1\x82\xd0\xbd\xd1\x8b\xd0\xb9' in line or 'nieznany' in line:
                continue

            #some lines do not include parkrunners' parkun IDs, these lines were skipped
            if 'athleteNumber' not in line:
                continue
            
            #get the parkrunners' parkrun ID            
            if 'athleteNumber' in line:
                
                #take relevant characters for parkrunners' parkun IDs
                athnumber = line.split('athleteNumber=')[1].split('"')[0]
                
            #this gets the parkrunners' parkun IDs 5 km time 
            if '</td><td>' in line:
                
                #current line
                L = line.split('</td><td>')
                
                #time equals that split in the second column
                time = L[2]
                
                #this will give the index from which to get the total runs variable below
                if len(L)==8:
                    
                    index=7
                    
                else:
                    index=9
            
            #this gives the parkrunners' parkun IDs and age category, which also contains their gender
            if'ageCat' in line:
                
                age = line.split('ageCat=')[1].split('"')[0]

            #this gets the total runs for each parkrunners
            if'</td><td>' in line:
                
                totalruns = line.split('</td><td>')[index].split('</td><td')[0]
            
            #this gets the parkrunners' club ID (if applicable)
            if 'clubNum' in line:
                club = line.split('clubNum=')[1].split('"')[0]
                
                #1187 is for athletes with no club
                if club == '1187':
                    output.write(event+','+athnumber+','+date+',0,'+time+','+age+','+totalruns+',''\r\n')
                    
                else:
                    #print/output into .csv athnumber, club, time, etc.- "event+str(n)+','"
                    output.write(event+','+athnumber+','+date+','+club+','+time+','+age+','+totalruns+',''\r\n')
                    
    except IndexError:
        output.write("error, error, error, error, error, error, error"',''\r\n')
 
    counter+=1

t1=timer.time()
output.close()
print t1-t0