""" En este ejemplo entro en el campus
hago una busqueda y recojo los titulos de los resultados"""


from selenium.common.exceptions import NoSuchElementException as NoSuch 
from selenium import webdriver 
from selenium.webdriver.common.keys import Keys # this gives access to some Keys commands (enter, escape, etc)
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
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

# NOTA: en este punto el browser (driver) se encuentra en la pagina con los resultados de busqueda

# print(driver.page_source) # print the source! no es muy util de esta forma

try:
    # este metodo va a fallar porque no estoy dando tiempo al browser
    # para que cargue la data necesaria
    container = driver.find_element_by_id('page-content')
    print(container.text)
except NoSuch:
    print("Fallo en obtener elemento")
    print(NoSuch)

try:
    # This waits up to 10 seconds before throwing a TimeoutException
    # unless it finds the element to return within 10 seconds.
    print('finding page-content element')
    container = WebDriverWait(driver, 10).until(
        EC.presence_of_element_located((By.ID, 'page-content')))
    print(container)
    #
    # print('finding region-main element within page-content element')
    main = container.find_element_by_id('region-main')
    # print('main: ', main)
    #
    main2 = main.find_element_by_tag_name('div')
    # print('main2: ', main2)
    main3 = main2.find_element_by_tag_name('div')
    # print('main3: ', main3)
    #
    # print('finding coursebox clearfix elements within region-main element')
    # al haber un espacio en el class name real solamente pongo una palabra para que funque
    courses = main3.find_elements_by_class_name('coursebox') 
    #
    for i, course in enumerate(courses): 
        coursename = course.find_element_by_class_name('coursename')
        print(i, coursename.text)
    #
except Exception as err:
    print("Elemento no encontrado!")
    print(err)
finally:
    driver.quit()