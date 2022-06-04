###############################################################################
#First, import the needed packages:
from bs4 import BeautifulSoup
from time import sleep
import csv
import pandas as pd
import random
import requests
from numpy import inf
import re

from datetime import datetime

#prepare selenium and chrome driver:
from selenium import webdriver
from selenium.webdriver.chrome.options import Options

chrome_options = Options()
chrome_options.add_argument('--no-sandbox')
chrome_options.add_argument('--headless')
chrome_options.add_argument("--disable-dev-shm-usage")
chrome_options.add_argument("--window-size=1980,1030")
chrome_options.add_argument("start-maximised")
driver = webdriver.Chrome(options=chrome_options)
driver.get('https://www.google.com/')



#Secondly, start with importing the users
User_list = pd.read_csv('all_users.csv', encoding='latin1', sep=';')


if 'Nr Ratings' not in User_list: #For your first run you want to add the extra columns to the dataset. 
    User_list['Details'] = 'NA'
    User_list['Activity'] = 'NA'
    User_list['Nr Ratings']='NA'
    User_list['Avg Rating']='NA'
    User_list['Nr Reviews']= 'NA'
    User_list['Nr Books Read'] = 'NA'
    User_list['Nr Friends'] = 'NA'




###############################################################################

#start the session 
with requests.Session() as s:
    res = s.get('https://www.goodreads.com/user/sign_in?source=home')
    signin = BeautifulSoup(res._content, 'html.parser')


###############################################################################

#Make a function that visits the url and scrapes the data, and stores this in the User_list
def User_info_scraper(User_URL, User_list, count):  

    res=s.get(User_URL)
    sleep(5)
    soup=BeautifulSoup(res.text, "html.parser")
    
    Relevant_info=soup.find_all('div', class_="infoBoxRowItem")
    
    Details = Relevant_info[0].text.replace('\n','')
    #print(Details)
    
    
    Tag_info = soup.find_all('div', class_="infoBoxRowTitle")
    counter=0
    for info in Tag_info:
        if info.text == "Activity":
            Activity = Relevant_info[counter].text.replace('\n','')
            break
        else:
            counter+=1    
    #print(Activity)
    
    Nr_ratings=(soup.find('div', class_="profilePageUserStatsInfo")).find('a').text.replace('\n','')
    #print(Nr_ratings)
    Avg_rating=(soup.find('div', class_="profilePageUserStatsInfo")).find_all('a')[1].text.replace('\n','')
    #print(Avg_rating)
    Nr_reviews=(soup.find('div', class_="profilePageUserStatsInfo")).find_all('a')[2].text.replace('\n','')
    #print(Nr_reviews)
    Nr_books_read=soup.find('a', class_="actionLinkLite userShowPageShelfListItem").text.replace('\n','')
    #print(Nr_books_read)
    
    try:
        Brownbackground=soup.find_all('h2', class_ = "brownBackground")
        for line in Brownbackground:
            if 'Friends (' in line.find('a').text.replace('\n',''):
                #print(line.find('a').text.replace('\n',''))
                Nr_friends = line.find('a').text.replace('\n','')
                break
            else:
                continue
    except:
        print('We dont have friends')
    #Nr_friends = soup.find_all('h2', class_ = "brownBackground")[8].find('a').text.replace('\n','')
    
    User_list.at[count,'Details'] = Details
    User_list.at[count,'Activity'] = Activity
    User_list.at[count,'Nr Ratings']= Nr_ratings
    User_list.at[count,'Avg Rating']= Avg_rating
    User_list.at[count,'Nr Reviews']= Nr_reviews
    User_list.at[count,'Nr Books Read'] =  Nr_books_read
    try:
        User_list.at[count,'Nr Friends'] =Nr_friends
    except:
        print('We dont have nr of friends')
    
    return User_list
###############################################################################
count=0
errorcounter=0
for User in range(len(User_list)):
    
    User_URL=User_list.iloc[count]['User Url']
    print(str(count)+'--> Scraping: '+ User_URL)
    
    
    #User_list=User_info_scraper(User_URL, User_list, count)
    
    try:
        User_list=User_info_scraper(User_URL, User_list, count)
        print('SUCCESFULL')
        errorcounter=0
    except:
        try:
            print('RETRY')
            sleep(5)
            User_list=User_info_scraper(User_URL, User_list, count)
        except:
            print('We did not succeed ' +str(errorcounter)+ ' times in a row')
            errorcounter+=1

             
            try:
                res=s.get(User_URL)
                sleep(5)
                soup=BeautifulSoup(res.text, "html.parser")
                Private_profile_check = soup.find('div', class_="mediumText").text
                if "profile data is set to private." in Private_profile_check:
                    print('This is private')
                    User_list.at[count, 'Details'] = "PRIVATE"
                
                else:
                    Author_profile_check = soup.find_all('div', class_= "authorFollowButtonContainer")
                    try:
                        print(Author_profile_check[0].text)
                        User_list.at[count, 'Details'] = "AUTHOR"
                    except:
                        print('no author')
            except:
                print('Likely an error')
    count+=1
    
    if (count/232).is_integer():
        currenttime=((datetime.now()).strftime('%d-%m %H.%M'))
        User_list.to_csv("Goodreads_user_info "+ currenttime + ".csv", index=False, sep=';') #Finally save your data

User_list.to_csv("user_info_complete.csv", index=False, sep=';')