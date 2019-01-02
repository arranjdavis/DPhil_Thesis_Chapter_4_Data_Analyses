#!/usr/bin/env python3
# -*- coding: utf-8 -*-

### DESCRIPTION ###

#This script converts parkrunids to four character strings (Oxford87 to Oxford0087, for example) and then orders that data set by parkrunid

#This script also makes Country the first column


"""

@authors: Arran Davis

"""

output = open('full_data_set_ordered.csv','w')
c = 1

#creates an empty dictionary
lines = []

#goes through the data set
with open('full_data_set_time_errors_removed.csv') as f:
    
    line = f.readline()

    #write headers to output
    output.write(line)
    
    for line in f:
        
        l = line.strip().split(',')

        #switches parkrun ID and country so that parkrun ID appears first (so that the 'lines' dictinary can be sorted)
        l[0], l[1] = l[1], l[0]
        
        #keep all parts of the location string except numbers ('aberdeen10' becomes 'aberdeen')
        loc = ''.join(i for i in l[0] if i.isdigit() == False)

        #keeps all the parts of the location string except letters ('aberdeen10' becomes '10')
        n = ''.join(i for i in l[0] if i.isdigit())

        #adds zeros to 'n' so that it is 4 characters long ('10' becomes '0010'; 4 - 2 = 2 zeros)
        zeros = ''.join('0' for _ in range(4-len(n)))

        #now adds the four-characeter 'n' string to the name of the location
        l[0] = loc + zeros + n

        #adds the line (l) to the dictionary
        lines.append(l)

        #counts how many lines in total
        c += 1

    #sort the dictionary alphabetically (and then numerically)
    lines.sort()

    #creates an empty dictionary
    race = []

    l_0 = 'aberdeen1'

    #intereates through the 'lines' dictionary (which is now full of lines)
    for l in lines:

        #swithes parkrun ID and country back to original positions (with country first)
        l[0], l[1] = l[1], l[0]

        #switches run time and country so that run time appears first (so that 'race' dictionary can be sorted)
        l[0], l[5] = l[5], l[0]

        #if a new location is reached
        if l[1] != l_0:

            #print the new location
            printloc1 = ''.join(i for i in l[1] if i.isdigit() == False)
            printloc2 = ''.join(i for i in l_0 if i.isdigit() == False)

            if printloc1 != printloc2:
                print(printloc1)

            #sort the race dictionary by run times
            race.sort()

            #new location becomes '1_0'
            l_0 = l[1]

            #iterates through the 'race' dictionary
            for r in race:

                #swithes runtime and country back to original positions (with country first)
                r[0], r[5] = r[5],r[0]

                #create a comma-seperated string of the line (it was a list above)
                s = ''.join(i+',' for i in r)

                #outputs the new 's' string minus the last character and gives a new ending
                output.write(s[:-1] + "\r\n")

            #creates an empty dicitonary
            race = []

        #adds the line to the 'race' dictionary               
        race.append(l)

#sort the dictionary alphabetically (and then numerically)
race.sort()

#this is the same as above, but is for the last line (which would otherwise be skipped)
for r in race:
    r[0], r[5] = r[5],r[0]
    s = ''.join(i+',' for i in r)
    output.write(s[:-1] + "\r\n")

#print the total number of lines
print(c)
output.close()