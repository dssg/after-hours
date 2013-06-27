import urllib2
import re
from BeautifulSoup import BeautifulSoup

# Our url
address ='http://thehappinessshow.com/PositiveAdjectives.htm'

# Read in the response to a HTTP request
response = urllib2.urlopen(address).read()

# Make a soup object of the website
soup = BeautifulSoup(response)

# Create an output text file
output = open('adjectives.txt','w')

## We want the text that looks like this
line = soup.fetch('td', {'valign':'top','rowspan':'25'})

word_list = []
for words in line:
  word_rows = str(words)
  # remove the non-informative strings from the word_rows string
  word_rows = word_rows.replace('&nbsp;','')
  word_rows = re.sub(r'<.*?>','',word_rows)
  # split word_list by the blank spaces
  word_list = word_rows.split()
  # list of words we don't want
  inappropro = ['sexy','kissable','godly','reposed','cute','lovely','beautiful','sensual','faithful','alluring','good looking','gorgeous','tender','chaste','desirable','fertile','consumate','attractive']
  # write each word into a text file delimited by a newline
  for word in word_list:
    if word not in inappropro:
      output.write(word.title() + '\n')
output.close()


