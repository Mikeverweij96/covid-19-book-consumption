#Fill in below two variables to refer to the group going to be scraped:
url_members_group= "https://www.goodreads.com/group/52853-romania/members?commit=Search+Members&page="
country_name="Romania"

#Fill in the letters that you want to scrape:
letters = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '.']
#letters = ['aa', 'ba', 'ca', 'da', 'ea', 'fa', 'ga', 'ha', 'ia', 'ja', 'ka', 'la', 'ma', 'na', 'oa', 'pa', 'qa', 'ra', 'sa', 'ta', 'ua', 'va', 'wa', 'xa', 'ya', 'za', '.a', 'ab', 'bb', 'cb', 'db', 'eb', 'fb', 'gb', 'hb', 'ib', 'jb', 'kb', 'lb', 'mb', 'nb', 'ob', 'pb', 'qb', 'rb', 'sb', 'tb', 'ub', 'vb', 'wb', 'xb', 'yb', 'zb', '.b', 'ac', 'bc', 'cc', 'dc', 'ec', 'fc', 'gc', 'hc', 'ic', 'jc', 'kc', 'lc', 'mc', 'nc', 'oc', 'pc', 'qc', 'rc', 'sc', 'tc', 'uc', 'vc', 'wc', 'xc', 'yc', 'zc', '.c', 'ad', 'bd', 'cd', 'dd', 'ed', 'fd', 'gd', 'hd', 'id', 'jd', 'kd', 'ld', 'md', 'nd', 'od', 'pd', 'qd', 'rd', 'sd', 'td', 'ud', 'vd', 'wd', 'xd', 'yd', 'zd', '.d', 'ae', 'be', 'ce', 'de', 'ee', 'fe', 'ge', 'he', 'ie', 'je', 'ke', 'le', 'me', 'ne', 'oe', 'pe', 'qe', 're', 'se', 'te', 'ue', 've', 'we', 'xe', 'ye', 'ze', '.e', 'af', 'bf', 'cf', 'df', 'ef', 'ff', 'gf', 'hf', 'if', 'jf', 'kf', 'lf', 'mf', 'nf', 'of', 'pf', 'qf', 'rf', 'sf', 'tf', 'uf', 'vf', 'wf', 'xf', 'yf', 'zf', '.f', 'ag', 'bg', 'cg', 'dg', 'eg', 'fg', 'gg', 'hg', 'ig', 'jg', 'kg', 'lg', 'mg', 'ng', 'og', 'pg', 'qg', 'rg', 'sg', 'tg', 'ug', 'vg', 'wg', 'xg', 'yg', 'zg', '.g', 'ah', 'bh', 'ch', 'dh', 'eh', 'fh', 'gh', 'hh', 'ih', 'jh', 'kh', 'lh', 'mh', 'nh', 'oh', 'ph', 'qh', 'rh', 'sh', 'th', 'uh', 'vh', 'wh', 'xh', 'yh', 'zh', '.h', 'ai', 'bi', 'ci', 'di', 'ei', 'fi', 'gi', 'hi', 'ii', 'ji', 'ki', 'li', 'mi', 'ni', 'oi', 'pi', 'qi', 'ri', 'si', 'ti', 'ui', 'vi', 'wi', 'xi', 'yi', 'zi', '.i', 'aj', 'bj', 'cj', 'dj', 'ej', 'fj', 'gj', 'hj', 'ij', 'jj', 'kj', 'lj', 'mj', 'nj', 'oj', 'pj', 'qj', 'rj', 'sj', 'tj', 'uj', 'vj', 'wj', 'xj', 'yj', 'zj', '.j', 'ak', 'bk', 'ck', 'dk', 'ek', 'fk', 'gk', 'hk', 'ik', 'jk', 'kk', 'lk', 'mk', 'nk', 'ok', 'pk', 'qk', 'rk', 'sk', 'tk', 'uk', 'vk', 'wk', 'xk', 'yk', 'zk', '.k', 'al', 'bl', 'cl', 'dl', 'el', 'fl', 'gl', 'hl', 'il', 'jl', 'kl', 'll', 'ml', 'nl', 'ol', 'pl', 'ql', 'rl', 'sl', 'tl', 'ul', 'vl', 'wl', 'xl', 'yl', 'zl', '.l', 'am', 'bm', 'cm', 'dm', 'em', 'fm', 'gm', 'hm', 'im', 'jm', 'km', 'lm', 'mm', 'nm', 'om', 'pm', 'qm', 'rm', 'sm', 'tm', 'um', 'vm', 'wm', 'xm', 'ym', 'zm', '.m', 'an', 'bn', 'cn', 'dn', 'en', 'fn', 'gn', 'hn', 'in', 'jn', 'kn', 'ln', 'mn', 'nn', 'on', 'pn', 'qn', 'rn', 'sn', 'tn', 'un', 'vn', 'wn', 'xn', 'yn', 'zn', '.n', 'ao', 'bo', 'co', 'do', 'eo', 'fo', 'go', 'ho', 'io', 'jo', 'ko', 'lo', 'mo', 'no', 'oo', 'po', 'qo', 'ro', 'so', 'to', 'uo', 'vo', 'wo', 'xo', 'yo', 'zo', '.o', 'ap', 'bp', 'cp', 'dp', 'ep', 'fp', 'gp', 'hp', 'ip', 'jp', 'kp', 'lp', 'mp', 'np', 'op', 'pp', 'qp', 'rp', 'sp', 'tp', 'up', 'vp', 'wp', 'xp', 'yp', 'zp', '.p', 'aq', 'bq', 'cq', 'dq', 'eq', 'fq', 'gq', 'hq', 'iq', 'jq', 'kq', 'lq', 'mq', 'nq', 'oq', 'pq', 'qq', 'rq', 'sq', 'tq', 'uq', 'vq', 'wq', 'xq', 'yq', 'zq', '.q', 'ar', 'br', 'cr', 'dr', 'er', 'fr', 'gr', 'hr', 'ir', 'jr', 'kr', 'lr', 'mr', 'nr', 'or', 'pr', 'qr', 'rr', 'sr', 'tr', 'ur', 'vr', 'wr', 'xr', 'yr', 'zr', '.r', 'as', 'bs', 'cs', 'ds', 'es', 'fs', 'gs', 'hs', 'is', 'js', 'ks', 'ls', 'ms', 'ns', 'os', 'ps', 'qs', 'rs', 'ss', 'ts', 'us', 'vs', 'ws', 'xs', 'ys', 'zs', '.s', 'at', 'bt', 'ct', 'dt', 'et', 'ft', 'gt', 'ht', 'it', 'jt', 'kt', 'lt', 'mt', 'nt', 'ot', 'pt', 'qt', 'rt', 'st', 'tt', 'ut', 'vt', 'wt', 'xt', 'yt', 'zt', '.t', 'au', 'bu', 'cu', 'du', 'eu', 'fu', 'gu', 'hu', 'iu', 'ju', 'ku', 'lu', 'mu', 'nu', 'ou', 'pu', 'qu', 'ru', 'su', 'tu', 'uu', 'vu', 'wu', 'xu', 'yu', 'zu', '.u', 'av', 'bv', 'cv', 'dv', 'ev', 'fv', 'gv', 'hv', 'iv', 'jv', 'kv', 'lv', 'mv', 'nv', 'ov', 'pv', 'qv', 'rv', 'sv', 'tv', 'uv', 'vv', 'wv', 'xv', 'yv', 'zv', '.v', 'aw', 'bw', 'cw', 'dw', 'ew', 'fw', 'gw', 'hw', 'iw', 'jw', 'kw', 'lw', 'mw', 'nw', 'ow', 'pw', 'qw', 'rw', 'sw', 'tw', 'uw', 'vw', 'ww', 'xw', 'yw', 'zw', '.w', 'ax', 'bx', 'cx', 'dx', 'ex', 'fx', 'gx', 'hx', 'ix', 'jx', 'kx', 'lx', 'mx', 'nx', 'ox', 'px', 'qx', 'rx', 'sx', 'tx', 'ux', 'vx', 'wx', 'xx', 'yx', 'zx', '.x', 'ay', 'by', 'cy', 'dy', 'ey', 'fy', 'gy', 'hy', 'iy', 'jy', 'ky', 'ly', 'my', 'ny', 'oy', 'py', 'qy', 'ry', 'sy', 'ty', 'uy', 'vy', 'wy', 'xy', 'yy', 'zy', '.y', 'az', 'bz', 'cz', 'dz', 'ez', 'fz', 'gz', 'hz', 'iz', 'jz', 'kz', 'lz', 'mz', 'nz', 'oz', 'pz', 'qz', 'rz', 'sz', 'tz', 'uz', 'vz', 'wz', 'xz', 'yz', 'zz', '.z', 'a.', 'b.', 'c.', 'd.', 'e.', 'f.', 'g.', 'h.', 'i.', 'j.', 'k.', 'l.', 'm.', 'n.', 'o.', 'p.', 'q.', 'r.', 's.', 't.', 'u.', 'v.', 'w.', 'x.', 'y.', 'z.', '..'] 

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
#max_nr_pages=float(inf)
max_nr_pages=100


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
def extratact_user_urls(group_page_url):
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
                sleep(0.1)
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
                individual_user_data = {"User Name": user_name,"User Url": user_url, "Books read": books_read, "Date joined": date_joined, "Nr Comments": nr_comments, "Country": country_name}               
                #check if we really scraped a new user, and if so, add it to the individual_info list:
                #(Note: this check is necessary because there is a (very small) chance that a user occures on two different group pages.
                #This happens for example if someone joined or leaved the Netherlands-Flanders-group while we where scraping it).
                if individual_user_data not in members_info:
                    members_info.append(individual_user_data)
        
                else:
                    continue   
        sleep(2) #wait for two seconds to prevent being blocked from the goodreads site for going to fast to different pages
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
            sleep(0.1)
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


#Create a function that adds the search term to the last part of the url:
def create_url_addition(url_addition, search_term):
       
    url_addition_final= '&q='+ search_term + url_addition
    
    return url_addition_final



#Create a function that makes a list with information of all individual group members that we can (and want to) scrape.
#note: the limit is the maximum number of pages you want to scrape (this is useful to make scraping faster).
def extract_all_users(users, url_members_group, url_addition_final, limit):
    '''
    With the variable 'url_members_group' (i.e. the base url of the different pages with group members), 
    this function 'visits' and scrapes each page with group members from page 1 to either the 'limit' or the actual last page.
    Here it holds that the last scraped page = min('limit','actual last page').
    '''
    
    
    page_url=url_members_group +'1' + url_addition_final #Create the link to the first page of group members
    nr_members = check_nr_members(page_url) #Store how many members belong to this search
    print("This search gives " + str(nr_members) + " members")
    
    
    count=1 #set a counter to check if we have not yet reached the 'limit'
    
    
    while page_url: #loop trough all pages we can (and want to) scrape.   
        print('Currently scraping page ' + str(count) + ', and length of members equals: ' + str(len(users)))
        
        #start scraping users from the group members page and store them in a list, trough which we will then loop:
        users_dummy = extratact_user_urls([page_url])
                        
        
        print(str(len(users_dummy))+' users were found')
        for user in users_dummy:
            if user not in users:
                users.append(user)
            else:
                continue
        if check_next_page(page_url) != "None" and count<limit:  #Check if we still can (and want to) scrape a next group member page
            page_url= url_members_group + check_next_page(page_url) + url_addition_final #create the page_url of the next page
            count+=1
        else:
            break
    print('In total, we have scraped users from ' + str(count) + ' pages')
    return users, nr_members


###############################################################################

#start the session 
with requests.Session() as s:
    res = s.get('https://www.goodreads.com/user/sign_in?source=home')
    signin = BeautifulSoup(res._content, 'html.parser')

###############################################################################




users=[] #create an empty list in which we will later store the different users


letter_index= 0

for letter in letters:
    print('We will now scrape the letter(s) ' + letter + ', whith nr pages: ' )
    search_term = letter
    url_addition_final = create_url_addition(url_addition, search_term)
    #limit = letter_pages[letter_index]
    limit = 100
    letter_index +=1
    
    user_scraper_test = extract_all_users(users, url_members_group, url_addition_final, limit)
    user_list_test = user_scraper_test[0]
    nr_members = user_scraper_test[1][:len(user_scraper_test[1])-1]

    #Store all users from this search in a csv:
    df_users = pd.DataFrame(user_list_test)
    df_users.to_csv("GR_search_"+country_name+"_" + search_term +"_"+ nr_members +".csv", index=False)

