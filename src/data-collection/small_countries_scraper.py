###############################################################################
#First, import the needed packages:
from bs4 import BeautifulSoup
from time import sleep
import csv
import pandas as pd
import random
import requests
from numpy import inf

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


#Give the url's of the goodreads homepage and the link to the members page of the group we want to scrape:
goodreads_url= "https://www.goodreads.com" 
url_addition= "&utf8=%E2%9C%93"



#Give the base url of the shelves and the shelf identifier:
url_review = "https://www.goodreads.com/review/list/" 
shelf = "?shelf=read"



#Define the following:
shelf_id= 'https://www.goodreads.com/review/list/316864-an?shelf=read'  
user_page = "/user/show/"
    
    
    
#Indicate the maximum number of group pages from which you want to scrape users if you want to limit running time, otherwise set to float(inf).
max_nr_pages=float(inf)
#max_nr_pages=100


###############################################################################

#Create a function that cuts only the date from the date joined variable:
def date_cutter(date_joined):
    nr_characters = len(date_joined)
    
    for cindex in range(0,nr_characters):
        character=date_joined[cindex]
        if character == 's': 
            begin_at= cindex+6
            stop_at=cindex+18
            break
        else:
            continue
    return(begin_at, stop_at)
            

#Create a function to scrape the urls to the individual users homepages and some basic information of the users:
def extratact_user_urls(group_page_url, country):
    '''
    With the variable 'group_page_url' (i.e. a link to one of the pages with group members), 
    this function scrapes the url's of the individual homepages of the members that are on this group page,
    and it scrapes some basic information of the user (such as their name and the number of books they read).
    '''
    
    
    members_info=[] #first, create an empty list in which we later store all url's to the indivudual bookshelves
    
    for page_url in group_page_url: #loop through all page urls from the list of 'group_page_url'
        #res=requests.get(page_url)
        res=s.get(page_url)
        sleep(1)
        soup=BeautifulSoup(res.text, "html.parser")
        #Check if no loading error occured:
        try: 
            Check_error = soup.find('h1').get_text()
            if Check_error == 'page unavailable':
                print('We have a loading error')
                while Check_error == 'page unavailable':
                    sleep(3)
                    res=s.get(page_url)
                    soup=BeautifulSoup(res.text, "html.parser")
                    Check_error = soup.find('h1').get_text()
                    print('We no longer have a loading error')
            else:
                sleep(4)
        except:
             print('no error')
        
        
        individual_info = soup.find_all(attrs={"class":"elementList"}) #the individual_info list contains the info of all users shown on the page
    
        for info in individual_info: #loop through the info belonging to all users shown on the page url, by going trough the idivudal_info list
                user_url = goodreads_url + info.find(class_="meta").find("a").attrs["href"]
                user_name = info.find(class_="userName").attrs["title"]
                books_read = info.find(class_="meta").find("a").text
                #user_details = info.find(class_="userData").find("a")
                nr_comments = info.find_all('div')[1].get_text()[8:15]
                date_joined = info.find_all('div')[1].get_text()[22:65]
                start_stop=date_cutter(date_joined)
                date_joined=date_joined[start_stop[0]:start_stop[1]]
                
                #store the individual user data scraped above in a dictionary:
                individual_user_data = {"User Name": user_name,"User Url": user_url, "Books read": books_read, "Date joined": date_joined, "Nr Comments": nr_comments, "Country": country}               
                #check if we really scraped a new user, and if so, add it to the individual_info list:
                #(Note: this check is necessary because there is a (very small) chance that a user occures on two different group pages.
                #This happens for example if someone joined or leaved the Netherlands-Flanders-group while we where scraping it).
                if individual_user_data not in members_info:
                    members_info.append(individual_user_data)
        
                else:
                    continue   
        sleep(4) #wait for two seconds to prevent being blocked from the goodreads site for going to fast to different pages
    return members_info

#Create a function that checks if there still is a button to the next page, i.e. if we can still continue to scrape a next page of group members.
def check_next_page(page_url):
    '''Given the input 'page_url', this function checks if there is a button to access a following page'''
    res = s.get(page_url)
    soup = BeautifulSoup(res.text, "html.parser")
    
    
    #Check if no loading error occured:
    try: 
        Check_error = soup.find('h1').get_text()
        if Check_error == 'page unavailable':
            print('We have a button loading error')
            while Check_error == 'page unavailable':
                sleep(3)
                res=s.get(page_url)
                soup=BeautifulSoup(res.text, "html.parser")
                Check_error = soup.find('h1').get_text()
            print('We no longer have a button error')
        else:
            sleep(2)
    except:
        print('no button error 2')
    
    
    
    try:
        next_btn = soup.find(attrs={"class":"leftContainer"}).find(attrs={"rel":"next"}).text
        return next_btn #when there is a next page button, the number of the page to which it refers will be returned

    except:
        return "None" #when there is no next page butten, "None" will be returned (which, indicates that there is no next button)


def check_nr_members(page_url):
    '''this function returns the number of members that result from this specific search'''
    res = s.get(page_url)
    soup = BeautifulSoup(res.text, "html.parser")
    
    #nr_members = soup.find_all('span')[9].get_text()[-7:]
    nr_members = soup.find_all('span')[58].get_text()[-7:]
    #driver.save_screenshot('nrmembers_'+str(nr_members)+'.png')
    return(nr_members)




#Create a function that makes a list with information of all individual group members that we can (and want to) scrape.
#note: the limit is the maximum number of pages you want to scrape (this is useful to make scraping faster).
def extract_all_users(users, Complete_url, country, limit):
    '''
    With the variable 'url_members_group' (i.e. the base url of the different pages with group members), 
    this function 'visits' and scrapes each page with group members from page 1 to either the 'limit' or the actual last page.
    Here it holds that the last scraped page = min('limit','actual last page').
    '''
    
    
    page_url=Complete_url +'1'  #Create the link to the first page of group members
    nr_members = check_nr_members(page_url) #Store how many members belong to this search
    print("This search gives " + str(nr_members) + " members")
    
    
    count=1 #set a counter to check if we have not yet reached the 'limit'
    
    
    while page_url: #loop trough all pages we can (and want to) scrape.   
        print('Currently scraping page ' + str(count) + ', and length of members equals: ' + str(len(users)))
        
        #start scraping users from the group members page and store them in a list, trough which we will then loop:
        users_dummy = extratact_user_urls([page_url], country)
                        
        
        print(str(len(users_dummy))+' users were found')
        for user in users_dummy:
            if user not in users:
                users.append(user)
            else:
                continue
        if check_next_page(page_url) != "None" and count<limit:  #Check if we still can (and want to) scrape a next group member page
            page_url= Complete_url + check_next_page(page_url)  #create the page_url of the next page
            count+=1
        else:
            break
    print('In total, we have scraped users from ' + str(count) + ' pages')
    return users, nr_members


###############################################################################

#Sign in to the website:
payload = {
    'user[email]': 'm.r.j.a.verweij@tilburguniversity.edu',
    'user[password]': 'nieuwwachtwoord1G',
    'utf8':'✓', 
    }

with requests.Session() as s:
    res = s.get('https://www.goodreads.com/user/sign_in?source=home')
    signin = BeautifulSoup(res._content, 'html.parser')
    
    payload['authenticity_token'] = signin.find('input', attrs={'name':"authenticity_token", 'type':'hidden'})['value']
    payload['n'] = signin.find('input', attrs={'name':"n", 'type':"hidden"})['value']
    res = s.post('https://www.goodreads.com/user/sign_in?source=home', data=payload)
    

###############################################################################




#letter_pages = [68, 41, 46]

#letters = ['aa', 'ad', 'ae', 'ah', 'ai', 'al', 'am', 'an', 'ao', 'ar', 'as', 'da', 'dd', 'de', 'dh', 'di', 'dl', 'dm', 'dn', 'do', 'dr', 'ds', 'ea', 'ed', 'ee', 'eh', 'ei', 'el', 'em', 'en', 'eo', 'er', 'es', 'ha', 'hd', 'he', 'hh', 'hi', 'hl', 'hm', 'hn', 'ho', 'hr', 'hs', 'ia', 'id', 'ie', 'ih', 'ii', 'il', 'im', 'in', 'io', 'ir', 'is', 'la', 'ld', 'le', 'lh', 'li', 'll', 'lm', 'ln', 'lo', 'lr', 'ls', 'ma', 'md', 'me', 'mh', 'mi', 'ml', 'mm', 'mn', 'mo', 'mr', 'ms', 'na', 'nd', 'ne', 'nh', 'ni', 'nl', 'nm', 'nn', 'no', 'nr', 'ns', 'oa', 'od', 'oe', 'oh', 'oi', 'ol', 'om', 'on', 'oo', 'or', 'os', 'ra', 'rd', 're', 'rh', 'ri', 'rl', 'rm', 'rn', 'ro', 'rr', 'rs', 'sa', 'sd', 'se', 'sh', 'si', 'sl', 'sm', 'sn', 'so', 'sr', 'ss']

#users=[] #create an empty list in which we will later store the different users
users = []

country_index= 0

countries = ['Pakistan', 'Poland', 'Turkey', 'Finland', 'Sweden', 'Norway', 'UAE', 'Latvija', 'Netherlands', 'Bangladesh', 'Venezuela',  'Russia', 'Singapore', 'Estonia', 'Nepal', 'Austria', 'Panama', 'Bulgaria']
Country_identifiers = ['84613-pakistani-readers','3460-polska-poland', '25661-t-rk-kitap-kurtlar','7397-suomi-finland', '360-sweden', '5966-norway', '2233-uae-good-readers', '45352-latvija', '223-netherlands-flanders-group', '2079-bangladesh', '3511-venezuela-lee', '5951-russia', '41267-goodreads-singapore', '45927-eesti-estonia', '28718-goodreads-nepal', '38632-sterreich-austria', '43465-panama-goodreads', '11582-bulgaria'] 
Country_nr_members = [3000, 3000, 2846, 2643, 2487, 2337, 1510, 1488, 1370, 1262, 638, 742, 1880, 975, 765, 238, 360, 928]




for country in countries:
    print('We will now scrape the country ' + country + ', whith nr pages: ' )
    
    Country_url = Country_identifiers[country_index]
    Country_nr = Country_nr_members[country_index]
    
    Complete_url = 'https://www.goodreads.com/group/' + Country_url + '/members?page='
    
    limit = 100
    country_index +=1
    
    nr_members_before = len(users)
    enough = False
    
    while not enough:
        user_scraper_test = extract_all_users(users, Complete_url, country, limit)
        if len(users)>nr_members_before+Country_nr*0.90:
            enough = True
        else:
            print('We have not scraped enough users and have to re-do this country')
        
    user_list_test = user_scraper_test[0]
    nr_members = user_scraper_test[1][:len(user_scraper_test[1])-1]
    
    
    
    #Store all users from this search in a csv:
    df_users = pd.DataFrame(user_list_test)
    df_users.to_csv("GR_search_"+country+"_" +"_"+ nr_members +".csv", index=False)

