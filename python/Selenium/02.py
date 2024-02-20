from selenium import webdriver 
from selenium.webdriver.common.keys import Keys # this gives access to some Keys commands (enter, escape, etc)
import time

chromedriverfile = '/home/dancab/bin/chromedriver'
driver = webdriver.Chrome(chromedriverfile)

driver.get('http://campus.fi.mdp.edu.ar/')

# the browser is in the given website, now we can locate elements from its html code
# to locate an element, go for: id, name, class (in that order)

# i'm going to locate the search bar
searchbar = driver.find_element_by_id('search-1')
searchbar.send_keys('Física')
searchbar.send_keys(Keys.RETURN)

time.sleep(5) # 5 seconds to see the browser

driver.quit()