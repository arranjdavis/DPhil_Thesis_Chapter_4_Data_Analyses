### DESCRIPTION ###

#This script creates seperate age and gender variables from the combined age and gender data in parkrun's online data sets.

"""

@authors: Arran Davis

"""

data = open('finishlistcountry2.csv','r')
output = open('finishlistcountry3.csv', 'w')

#reads first line of data (the headers)
line = data.readline()

#writes first line of data plus new headers in output
output.write(line.strip() + ',gender,age\r\n')

#go through all the lines in the data set
for line in data:

    #strips the comma seperated lines into list 
    l = line.strip().split(',')
    
    #this takes the index of the list that gives an parkrunner's age and sex
    g = l[6]
    
    #returns index 1; either M or W for the parkrunner's sex
    gender = g[1]
    
    #returns from index 2 to the end of the string; a parkrunner's age category
    age = g[2:]
    
    #writes the existing line - line.strip() removes any new line command at the end of the line) - plus the sex and age variables
    output.write(line.strip() + str(gender) + ',' + str(age) + '\r\n')

data.close()
output.close()

