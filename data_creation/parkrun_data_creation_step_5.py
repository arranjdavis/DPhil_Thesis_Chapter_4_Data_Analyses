### DESCRIPTION ###

#This script converts parkrunners' run times to a consistent format that can be read by python (and R)

"""

@authors: Arran Davis

"""

data = open('finishlistcountry3.csv', 'r')
output = open('finishlistcountry4.csv', 'w')

headers = data.readline()

#writes first line of data plus new headers in output
output.write(headers)

#this for loop goes through all the lines in the data set
for line in data:

    l = line.strip().split(',')
    
    #this will get parkrunners' 5 km run time for that event
    time = l[5]
    
    #most times are 5 characters long (29:45)
    if len(time) == 5: 
           newtime = "00:" + time
           
           #this replaces the old time variable with the new time variable and outputs the rest of the line
           output.write(line.replace(time,newtime))
           
    #some runs lasted over an hour and are thus seven characters long (1:04:45)
    else:
           newtime = "0" + time
           output.write(line.replace(time,newtime))

data.close()
output.close()


