### DESCRIPTION ###

#This script adds the country data to each parkrun (country data was taken from the URLs of parkruns online data sets).

### NOTES ###

#This script needs "parkrunlocations.csv"

#This script originally produces dupliicates.
#So, an additional script is run in order to remove these duplicates (which also existed in the original data set finishlist.csv)
#See the end of the script for duplicate information

"""

@authors: Arran Davis and Pádraig Mac Carron

"""

#open data set
f = open('parkrunlocations.csv', 'rU')

#get headers (i.e., countries)
headers = f.readline().strip().split(',')

#{} creates dictionary, 
countries = {}
countryindex = {}

#adds countries to countryindex 
for H in headers:
    countryindex[headers.index(H)] = H

#this for loop will add countries and locations to the countries dictionary   
for line in f:
    
    #line containing locations
    L = line.strip().split(',')
    
    #this for loop goes through the current line and adds the locations to the appropriate country in the dictionary
    for i in range(len(L)):
        
        #just saying only do this if the length of L is greater than 1
        if len(L[i]) > 1:
            
            #add the countries to the countries dictionary
            countries[L[i].lower()] = countryindex[i]

#close f
f.close()

#open file to match locations to countries
g = open('finishlist.csv', 'rU')

#new output file
output = open('finishlistcountry_duplicate.csv', 'w')

#makes L equal the headers in 'finishlist.csv'
L = g.readline()

#writes the first line of g (the headers) in the new file, plus the country column to be added
output.write('Country,' + L)

#goes through all the lines in the data set and adds country information
for line in g:
    
    #strips all the lines in g to get the elements
    M = line.strip().split(',')
    
    #for the elements in the dictionary countries
    for location in countries:
        
        #if a parkrun location in the dictionary countries is in the line
        if location in M[0]:
            
            #write in the new file the country in the countries dictionary that was in index 0 of whatever line
            output.write(countries[location]+','+line)

g.close()
output.close()


#for some reason the above prints 300k duplicate lines, so this will remove them
inFile = open('finishlistcountry_duplicate.csv', 'r')
outFile = open('finishlistcountry.csv', 'w')

seen = set()

for line in inFile:

    Q = line.strip().split(',')
    
    #this makes A the index of the parkrunid and the athleteid (these should not be repeated)
    A = Q[1:3]
    
    #converts A back to a string
    B = ",".join(map(str, A))
    
    #adds B to set, and then writes line
    if B not in seen:
        seen.add(B)
        outFile.write(line)
    
    #if B is already in the set, then the line is not added to the output    
    else:
        continue

outFile.close()

inFile.close()

#this is now fixed:
#finishlist.csv = 9820072
#finishlistcountry_duplicate.csv = 10123203
#finishlistcountry.csv = 9820030 (so finishlist.csv had 42 duplicates as well)

    
    




    
