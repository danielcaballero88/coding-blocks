from selenium import webdriver

chromedriverfile = '/home/dancab/bin/chromedriver'
driver = webdriver.Chrome(chromedriverfile)

driver.get('http://campus.fi.mdp.edu.ar/')
print(driver.title)
# driver.close() # closes the browser tab
driver.quit() # closes the entire browser