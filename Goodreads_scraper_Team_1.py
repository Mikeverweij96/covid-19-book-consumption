#!/usr/bin/env python
# coding: utf-8

# In[1]:


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
url_members_group= "https://www.goodreads.com/group/223-netherlands-flanders-group/members?page="

#Give the base url of the shelves and the shelf identifier:
url_review = "https://www.goodreads.com/review/list/" 
shelf = "?shelf=read"

#Define the year's of interest, i.e. we want to scrape books read in the following years:
dates= ["2019", "2020","2021"]

#Define the following:
shelf_id= 'https://www.goodreads.com/review/list/316864-an?shelf=read'  
user_page = "/user/show/"
    
    
    
#Indicate the maximum number of group pages from which you want to scrape users if you want to limit running time, otherwise set to float(inf).
max_nr_pages=float(inf)
#max_nr_pages=2


# In[2]:


#Create a function to scrape the urls to the individual users homepages and some basic information of the users:
def extratact_user_urls(group_page_url):
    '''
    With the variable 'group_page_url' (i.e. a link to one of the pages with group members), 
    this function scrapes the url's of the individual homepages of the members that are on this group page,
    and it scrapes some basic information of the user (such as their name and the number of books they read).
    '''
    
    
    members_info=[] #first, create an empty list in which we later store all url's to the indivudual bookshelves
    
    for page_url in group_page_url: #loop through all page urls from the list of 'group_page_url'
        res=requests.get(page_url)
        soup=BeautifulSoup(res.text, "html.parser")
        individual_info = soup.find_all(attrs={"class":"elementList"}) #the individual_info list contains the info of all users shown on the page
    
        for info in individual_info: #loop through the info belonging to all users shown on the page url, by going trough the idivudal_info list
                user_url = goodreads_url + info.find(class_="meta").find("a").attrs["href"]
                user_name = info.find(class_="userName").attrs["title"]
                books_read = info.find(class_="meta").find("a").text
                user_details = info.find(class_="userData").find("a")
                
                #store the individual user data scraped above in a dictionary:
                individual_user_data = {"User Name": user_name,"User Url": user_url, "User enrollment": user_details, "Books read": books_read}
                
                #check if we really scraped a new user, and if so, add it to the individual_info list:
                #(Note: this check is necessary because there is a (very small) chance that a user occures on two different group pages.
                #This happens for example if someone joined or leaved the Netherlands-Flanders-group while we where scraping it).
                if individual_user_data not in members_info:
                    members_info.append(individual_user_data)
        
                else:
                    continue   
        sleep(1) #wait for one second to prevent being blocked from the goodreads site for going to fast to different pages
    return members_info


# In[3]:


#Create a function that checks if there still is a button to the next page, i.e. if we can still continue to scrape a next page of group members.
def check_next_page(page_url):
    '''Given the input 'page_url', this function checks if there is a button to access a following page'''
    res = requests.get(page_url)
    soup = BeautifulSoup(res.text, "html.parser")
    try:
        next_btn = soup.find(attrs={"class":"leftContainer"}).find(attrs={"rel":"next"}).text
        return next_btn #when there is a next page button, the number of the page to which it refers will be returned

    except:
        return "None" #when there is no next page butten, "None" will be returned (which, indicates that there is no next button)


# In[4]:


#Create a function that makes a list with information of all individual group members that we can (and want to) scrape.
#note: the limit is the maximum number of pages you want to scrape (this is useful to make scraping faster).
def extract_all_users(url_members_group,limit):
    '''
    With the variable 'url_members_group' (i.e. the base url of the different pages with group members), 
    this function 'visits' and scrapes each page with group members from page 1 to either the 'limit' or the actual last page.
    Here it holds that the last scraped page = min('limit','actual last page').
    '''
    
    
    page_url=url_members_group +'1' #Create the link to the first page of group members
    
    
    users=[] #create an empty list in which we will later store the different users
    count=1 #set a counter to check if we have not yet reached the 'limit'
    
    while page_url: #loop trough all pages we can (and want to) scrape.   
        
        #start scraping users from the group members page and store them in a list, trough which we will then loop:
        for user in extratact_user_urls([page_url]):
            users.append(user)
        if check_next_page(page_url) != "None" and count<limit:  #Check if we still can (and want to) scrape a next group member page
            page_url= url_members_group + check_next_page(page_url) #create the page_url of the next page
            count+=1
        else:
            break
    print('In total, we have scraped users from ' + str(count) + ' pages')
    return users


# In[5]:


#Create a function that extracts the url to the homepage of each individual user from the list of all users:
def extract_url_user_page(user_info_list):
    '''This function extracts from the user_info_list only the url to the user's home page''' 
    user_url_list=[] #create an empty list in which we will later stor the url's to all members homepages

    for i in range(0, len(user_info_list)): #loop trough all users in the user_info_list
        individual_user= user_info_list[i] 
        for j in individual_user:
            if j == "User Url": #extract only the user url
                if individual_user[j] not in user_url_list: #check if we are really scraping a new user (just to be sure)
                    user_url_list.append(individual_user[j]) #append the link to the users homepage to the user_url_list
                    
    return user_url_list


# In[6]:


#Create a function that generates the link to all users bookshelves:
def extract_user_shelf(my_user_url_list):
    '''This function creates from the list of urls to user's homepages a link to their bookshelves and stores it in another list''' 
    user_shelf_list = []
    
    for element in my_user_url_list:
        for letter in range(len(element)):
            user_id=element[36:]
            shelf_id= url_review + user_id + shelf
            
            if shelf_id not in user_shelf_list:
                user_shelf_list.append(shelf_id)
            else:
                continue
    return user_shelf_list


# In[7]:


#Create a function that extracts the user id from the shelf url's
#(this function is needed as user id's vary in length):
def extract_user_id(shelf_url):
    '''This function extracts from the 'shelf_url' the user id'''
    user_id=shelf_url[38:50] #Each user id starts at index 38 of the shelf url, and always stops before index 50.
    digit=True
    count = 0
    while digit: #while we are still looking at a digit, we know that we are still looking at a part of the user id. 
        try:
            int(user_id[count]) #Check if we are looking at a digit
            count+=1
        except:
            digit = False #if we are not looking at a digit anymore, we want to stop the loop
    user_id = user_id[0:count] 
    return user_id
                      


# In[8]:


#Create a function that is able to scroll the book shelf page as long as we load books of our interest:
def scroller():
    '''This function keeps scrolling on the current book shelf url until it does not load any new books that are of our interest'''
    #Since loding the shelf typically takes 0.5 seconds, we wait 1 second to be sure the shelf is fully loaded, before we start scrolling:
    sleep(1) 
    lenOfPage = driver.execute_script("window.scrollTo(0, document.body.scrollHeight);var lenOfPage=document.body.scrollHeight;return lenOfPage;")
    
    load=True #this variable indicates if we still want to load more books (and hence, scroll further)
    last_books=[] 
    count=0
    double=0
    
    #The following while loop will keep scrolling the book shelve as long as we still find new books from the years of interest:
    while load:
        load_more = False
        res = driver.page_source.encode('utf-8')
        soup = BeautifulSoup(res, "html.parser")
        books=soup.find_all(class_="bookalike")
        
        #The following try will break the while loop if there are no books to scrape from this user:
        try:
            Last_book=books[-1]
        except:
            break 
            
        #Below code will check the last book that was loaded on the book shelf:    
        date_added_last_book =(Last_book.find(class_= "field date_added").find("span").text).replace("\n", "").replace("  ", "")
        book_title_last_book = (Last_book.find(class_="field title").find("a").text).replace("\n", "").replace("  ", "")
        last_books.append(book_title_last_book) #to check if we are not at the end of the shelf
        for year in dates: #to check if the last book that was loaded belongs to the years of interest
            if year in date_added_last_book:
                load_more = True
                break
            else:
                load_more = False
        if load_more == False: #This makes sure the while loop will be stopped if we do not want to load more books.
            load= False 
            
        #Since the last loaded book could also be the end of the users book shelf, we want to check if we are still loading new books. 
        #However, sometimes, we have not loaded a new book because the site was not loading fast enough after scrolling.
        #Therefore, we do the following: if we have the same last loaded book for the first time, we give it another time to load (this time 3 times as long).
        #Then, if we have the same last element for the second time, we assume we are at the end of the users book shelf and stop scrolling.
        else:
            if count>0: #we can not compare the last book to the previous last loaded book if we are still in our first scroll, therefore this line.
                if last_books[count]==last_books[count-1] and double==1:
                    load=False
                elif last_books[count]==last_books[count-1] and double==0:
                    sleep(3)
                    driver.execute_script("window.scrollTo(0, document.body.scrollHeight);var lenOfPage=document.body.scrollHeight;return lenOfPage;")
                    sleep(3)
                    double=1
                else:
                    double=0
                    
            count+=1
            
            #below line will actually scroll the page:
            driver.execute_script("window.scrollTo(0, document.body.scrollHeight);var lenOfPage=document.body.scrollHeight;return lenOfPage;")
            sleep(1) #give the driver 1 second to load the books after we have scrolled. 
    return None 


# The next function will iterate over the user shelfs of all users to scrape all books they read between 2019 and 2021. However, the function will not scrape:
# 1. users that are private (example: https://www.goodreads.com/review/list/10760281-natalie-verheyden?shelf=read)
# 2. users that have 0 books read (example: https://www.goodreads.com/review/list/6820404-jos-van-rosmalen?shelf=read)
# 3. users who only read books before 2019 (example: https://www.goodreads.com/review/list/24091570-yne?shelf=read   and https://www.goodreads.com/review/list/8485636-joeri-dassen?shelf=read)
# 
# When users read books on the research period (i.e. 2019 to 2021), the code iterates over those books and stores the following information in a datasheet:
# 1. The user id of the reader
# 2. The title of the book
# 3. The author of the book
# 4. Url linking to the book
# 5. The date they read the book
# 6. The date they added the book to their shelf
# 7. The average rating the book received from all users
# 8. The rating that the user gave to the book

# In[9]:


#Create a function that extracts the read books from a list of different shelf url's:
def extract_books_info(my_shelf_url_list):
    '''
    This function will go through the my_shelf_url_list (i.e. a list with all urls to users their book shelfs),
    and it will scrape all books read in 2019, 2020 and 2021 from these shelfs
    '''
    book_list = [] #create an empty list in which we will later store all information of the read books. 
    nr_users_scraped=0
    total_nr_users_to_scrape=len(my_shelf_url_list)
    Fractiles=[0,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95]
    
    for shelf_url in my_shelf_url_list: #We will loop trough all shelves that are present in the my_shelf_url_list
        
        driver.get(shelf_url) #open the shelf in the driver
        
        scroller() #start scrolling until we do not load anymore books that are of our interest
        
        res = driver.page_source.encode('utf-8')
        soup = BeautifulSoup(res, "html.parser")
        books=soup.find_all(class_="bookalike")
        
        #Below code prints an indication every time you have scFraped another 5% of users:
        nr_users_scraped+=1
        if (round(nr_users_scraped/total_nr_users_to_scrape,2) in Fractiles):
            print("You have now scraped "+ str((round(nr_users_scraped/total_nr_users_to_scrape,2))*100) +"% of bookshelves")
            Fractiles.remove(round(nr_users_scraped/total_nr_users_to_scrape,2))
            
        #Below code stores the information of each read book read in the period of interest in a dictionary:
        for book in books: #iterate over all books
            date_added =(book.find(class_= "field date_added").find("span").text).replace("\n", "").replace("  ", "")
            for year in dates:
                if year in date_added: #if the book was added in the years of interest, we will scrape the books information
                    reader_id = extract_user_id(shelf_url)
                    book_title= (book.find(class_="field title").find("a").text).replace("\n", "").replace("  ", "")
                    try: #As there is a small amount of books for which no author is defined, we have to add this 'try
                        author_name = book.find(class_='field author').find("a").text
                    except:
                        author_name = "NOT AVAILABLE"
                    book_url = goodreads_url + book.find(class_="field title").find("a").attrs["href"]
                    average_rating=(book.find(class_= "field avg_rating").find("div").text).replace("\n", "").replace("  ", "")


                    for rating in book:
                        if book.find(class_="stars").find("title") is None:
                            user_rating=0
                        else:
                            user_rating = book.find(class_="stars").find("title").text

                    for starting_date in book:
                        if book.find(class_="field date_read").find("span").text == "not set":
                            date_read=0
                        else:
                            date_read= book.find(class_="field date_read").find("span").text
                            
                    #store the scraped book information in a dictionary:
                    book_data= {"reader id": reader_id,"book title": book_title, "author_name": author_name,"book url": book_url,
                                        "date read": date_read, "date added": date_added,
                                        "average rating": average_rating, "user rating":user_rating}
                    if book_data not in book_list: #check if we have really scraped new information (just to be sure)
                        book_list.append(book_data)
                        
        sleep(2) #everytime we visit a new shelf, we bring the driver to sleep for 2 seconds, to prevent the goodreads server from blocking us
    
    #finally, we store all scraped books in a csv file:
    df_books = pd.DataFrame(book_list)
    df_books.to_csv("final_books_info.csv", index=False)
    print("Now you are done scraping!")
    return(book_list)


# In[10]:


#Store all users from the group member pages we can and want to scrape in a list:
user_info_list= extract_all_users(url_members_group,max_nr_pages)


#Extract the url's to the users homepages from the user_info_list and store them in the my_user_url_list:
my_user_url_list=extract_url_user_page(user_info_list)
print("In total, we have ", str(len(my_user_url_list)), "users that we want to scrape")


#Create a list of url's to all user's bookshelves:
my_shelf_url_list= extract_user_shelf(my_user_url_list)

#Start scraping books from the shelves:
my_book_info_list = extract_books_info(my_shelf_url_list)


# In[11]:


#stop the driver:
driver.quit()

