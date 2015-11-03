import urllib2
from StringIO import StringIO
import gzip
import xml.etree.ElementTree as etree
import os

## get a function to read data from website by api
def read_data(url):
    request = urllib2.Request(url)
    request.add_header('Accept-encoding', 'gzip')
    response = urllib2.urlopen(request)
    if response.info().get('Content-Encoding') == 'gzip':
        buf = StringIO( response.read())
        f = gzip.GzipFile(fileobj=buf)
        data = f.read()
    return data
    
def write_file(filename,newdata):
    if os.path.exists(filename) != True:
        tmp = open(filename,'a')
        tmp.write(newdata)
        tmp.close()
    xmlD = etree.parse(filename)
    root = xmlD.getroot()
    print(root)
    
### get query data of Location info from Trulia website
def real_state(library,function,state): 
     
    rs_url='http://api.trulia.com/webservices.php?apikey='+loc_api
    final_url=rs_url+"&library="+library+"&function="+function+"&state="+state
    print(final_url)
    rs_data=read_data(final_url)
    ### save data into xml file
    data_file = library+function+state+'.xml'
    write_file(data_file,rs_data)

### make a funciton to get three form s of data from websiite according to certain states name
def state_real(statename):
    real_state('LocationInfo','getCitiesInState',statename)
    real_state('LocationInfo','getZipCodesInState',statename)

### the satte list is which I get form the canndidate form etc.
allstate=['MA','NY','OH','VA','NC','MI','GA','MD','IL','DC','IN','NJ','FL','CA','MN','WI','WV','PA','CT','TX','MO','TN','MT','NH','AR','AZ','IA','KY','LA','WA','NE','SC','OR']
for item in allstate:
    print item
    state_real(item)
## after this part ,I get getCitiesInState and getZipcodeInState fifles of all states
## here I just use MA as an example
state_real('MA')

### get states info xml file

url_state='http://api.trulia.com/webservices.php?library=LocationInfo&function=getStates&apikey='+loc_api
states=read_data(url_state)
state_file = 'states.xml'
write_file(state_file,states)
### use of online transform tool, I transform xml file to csv fiels
