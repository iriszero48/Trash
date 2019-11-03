from selenium import webdriver
from selenium.webdriver.chrome.options import Options
UserAgent='Mozilla/5.0(Android;Linuxarmv7l;rv:5.0)Gecko/Firefox/5.0fennec/5.0'
chrome_options = Options()
chrome_options.add_argument("--user-agent="+UserAgent)
driver=webdriver.Chrome(executable_path ='chromedriver.exe',chrome_options=chrome_options)
driver.get("http://192.168.240.3")
