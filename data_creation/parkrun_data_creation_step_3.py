### DESCRIPTION ###

#This script converts dates to a format that python can read

"""

@authors: Arran Davis

"""

from datetime import datetime

g = open('finishlistcountry.csv', 'r')
output = open('finishlistcountry2.csv', 'w')

#reads the first line (the header) and skips the next
L = g.readline()

#writes the header in .csv output
output.write(L)

#goes through all the lines, reads the date, and converts it into a format Python can read
for line in g:
    
    M = line.strip().split(',')
    date = M[3]
    
    #this is the old date format
    date_object = datetime.strptime(date, '%d/%m/%Y')
    
    #converts date from datetime.datetime to something that can be in a string
    date_object.strftime('%Y-%m-%d')
    
    #makes date a variable
    newdate = date_object.strftime('%Y-%m-%d')
    
    M[3] = newdate
    M[8] = '\n'
    
    #add new date format to line
    Mstring = ",".join(map(str, M))
    output.write(Mstring)
    
g.close()
output.close()