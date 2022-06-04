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


goodreads_url= "https://www.goodreads.com"

#Secondly, start with importing the users
User_list = pd.read_csv('user_info_complete.csv', encoding='latin1', sep=';')


if 'Nr_Books_scraped' not in User_list: #For your first run you want to add the extra columns to the dataset. 
    User_list['Nr_Books_scraped'] = 'NA'
    User_list['Enough_scraped'] = 'NA'



###############################################################################
#start the session 
with requests.Session() as s:
    res = s.get('https://www.goodreads.com/user/sign_in?source=home')
    signin = BeautifulSoup(res._content, 'html.parser')

###############################################################################

#crate a function to scrape the book shelfs
def Bookshelf_scraper(shelf_URL, User_URL):

    book_list=[] #empty list to store this users books in.
    
    #below code chunck is used to check if there still is a next page with books, and stops the loop otherwise.
    page_exists = True 
    page_counter = 1
    while page_exists:
        
        shelf_URL_complete = shelf_URL + '?page='+ str(page_counter) +'&per_page=100&shelf=read&utf8=✓'
        res=s.get(shelf_URL_complete)
        sleep(5)
        soup=BeautifulSoup(res.text, "html.parser")
        
        print('now scraping page number: ' + str(page_counter) +', we have scraped thus far ' + str(len(book_list)))
        
        try:
            button=soup.find(class_="next_page disabled").text
            print('We found:' + button)
            page_exists = False
            print('The last page is page: '+ str(page_counter))
            print('no next page')
        except:
            try:
                no_content = soup.find(class_="greyText nocontent stacked").text
                print(no_content)
                page_exists = False
                break
            except: 
                page_exists = True
                page_counter +=1
        
        
        if (page_counter/10).is_integer() and len(book_list)/20 < page_counter/2:
            print('We are not adding enough, we hav only added: ' + str(len(book_list)) + ', while we are on page ' + str(page_counter))
            break
        
        books=soup.find_all(class_="bookalike")
        reader_id = User_URL[35:]
        
        for book in books: #iterate over all books
            
            book_title= (book.find(class_="field title").find("a").text).replace("\n", "").replace("  ", "")    
            
            date_added =(book.find(class_= "field date_added").find("span").text).replace("\n", "").replace("  ", "")
     
        
            try: #As there is a small amount of books for which no author is defined, we have to add this 'try', the same holds for isbn, number of pages, etc.
                author_name = (book.find(class_='field author').find("a").text).replace("\n", "")
            except:
                author_name = "NA"
            
            try:
                isbn13 = (book.find(class_='field isbn13').find('div').text).replace("\n", "").replace("  ", "")
            except:
                isbn13 = "NA"
            
            try:
                num_pages = (book.find(class_='field num_pages').find('nobr').text).replace("\n", "").replace("  ", "")
            except:
                num_pages = "NA"
            
            try:
                avg_rating = (book.find(class_="field avg_rating").find('div').text).replace("\n", "").replace("  ", "")
            except:
                avg_rating = "NA"
            
            try:
                num_ratings = (book.find(class_="field num_ratings").find('div').text).replace("\n", "").replace("  ", "")
            except:
                num_ratings = "NA"
            
            try:
                date_pub = (book.find(class_="field date_pub").find('div').text).replace("\n", "")
            except:
                date_pub = "NA"
                
            try:
                rating = (book.find(class_='field rating').find('span').attrs["title"]).replace("\n", "")
            except:
                rating = "NA"
            
            try:
                read_count = (book.find(class_="field read_count").find('div').text).replace("\n", "").replace("  ", "")
            except:
                read_count = "NA"
            
            try:
                date_started = (book.find(class_="field date_started").find(class_="date_started_value").text).replace("\n", "")
            except:
                date_started = "NA"
                
            try:
                date_read = (book.find(class_= "field date_read").find(class_='date_read_value').text).replace("\n", "")
            except:
                date_read = "NA"
            
            #every book has a book url, so no try needed here.
            book_url = book.find(class_="field title").find("a").attrs["href"]
    
    
                
        #store the scraped book information in a dictionary:
            book_data= {"reader id": reader_id,"book title": book_title, "author_name": author_name,"book url": book_url, "date_started": date_started,
                                "date read": date_read, "date added": date_added, "user rating":rating, "isbn13": isbn13, "num_pages": num_pages,
                                "avg_rating": avg_rating, "num_ratings": num_ratings, "date_pub": date_pub, "read_count": read_count}
        
            if book_data not in book_list: #check if we have really scraped new information (just to be sure)
                book_list.append(book_data)

    
    Book_shelf=book_list
    
    return(Book_shelf)



#initiate the scraper
User_URL=User_list.iloc[0]['User.Url']
Bookshelf_URL='https://www.goodreads.com/review/list'+User_URL[35:]
print(str(0)+'--> Scraping: '+ Bookshelf_URL)
All_books=pd.DataFrame(Bookshelf_scraper(Bookshelf_URL, User_URL))
User_list.at[0, 'Nr_Books_scraped'] = len(All_books)
Nr_books_user=User_list.iloc[0]['Nr.Books.Read']
if len(All_books)/Nr_books_user < 0.80:
    All_books=pd.DataFrame(Bookshelf_scraper(Bookshelf_URL, User_URL))
    User_list.at[0, 'Enough_scraped'] = len(All_books)/Nr_books_user
else:
    User_list.at[0, 'Enough_scraped'] = len(All_books)/Nr_books_user


count=1 #start at one, since we have already scraped user 0 with the initialisation here before. 
for User in range(len(User_list)):
    
    User_URL=User_list.iloc[count]['User.Url']
    
    Bookshelf_URL='https://www.goodreads.com/review/list'+User_URL[35:]
    
    Nr_books_user=User_list.iloc[count]['Nr.Books.Read']
    
    print(str(count)+'--> Scraping: '+ Bookshelf_URL + 'With a total number of books equal to: ' + str(Nr_books_user))
    
    try:
        Readers_books =pd.DataFrame(Bookshelf_scraper(Bookshelf_URL, User_URL))
    except:
        print("this user is probably deleted")
    
    try:
        User_list.at[count, 'Nr_Books_scraped'] = len(Readers_books)
    except:
        User_list.at[count, 'Nr_Books_scraped'] = "Probably delted"
    
    
    try:
        if len(Readers_books)/Nr_books_user < 0.85:
    
            print("we have to retry, we only scraped: " + str(len(Readers_books)))
            Readers_books =pd.DataFrame(Bookshelf_scraper(Bookshelf_URL, User_URL))
            User_list.at[count, 'Nr_Books_scraped'] = len(Readers_books)
            User_list.at[count, 'Enough_scraped'] = len(Readers_books)/Nr_books_user      
        else:
            User_list.at[count, 'Enough_scraped'] = len(Readers_books)/Nr_books_user
    except:
        print("This user is prably deleted")
        User_list.at[count, 'Enough_scraped'] = 'Probably delted'


    try:
        All_books = All_books.append(Readers_books)
    except: 
        print("This user is probably deleted")
 
    if (count/500).is_integer():
        currenttime=((datetime.now()).strftime('%d-%m %H.%M'))
        All_books.to_csv("book list at "+ currenttime + ".csv", index=False, sep=';')
        User_list.to_csv("user info at " + currenttime + ".csv", index = False, sep = ';')
 
    
 
    count+=1
    if count == len(User_list):
        break
    
    
User_list.to_csv("all_users.csv", index = False, sep = ';')
All_books.to_csv("all_books.csv", index=False, sep=';')