import requests
import json
import psycopg2

# print('-----GET_REQUESTS-----')
# URL = 'https://deep-sea.ru/rest/users'
# r = requests.get(URL)
# if r.status_code == 200:
#     print(URL, r.status_code)
# else:
#     print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

# URL = 'https://deep-sea.ru/rest/timeControl?user=19'
# r = requests.get(URL)
# if r.status_code == 200:
#     print(URL, r.status_code)
# else:
#     print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")


# URL = 'https://deep-sea.ru/rest/issues?user=spiridovich'
# r = requests.get(URL)
# if r.status_code == 200:
#     print(URL, r.status_code)
# else:
#     print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

# URL = 'https://deep-sea.ru/rest/issuesViewed?user=spiridovich'
# r = requests.get(URL)
# if r.status_code == 200:
#     print(URL, r.status_code)
# else:
#     print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")


# URL = 'https://deep-sea.ru/sections'
# r = requests.get(URL)
# if r.status_code == 200:
#     print(URL, r.status_code)
# else:
#     print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

# URL = 'https://deep-sea.ru/rest/nestingFiles'
# r = requests.get(URL)
# if r.status_code == 200:
#     print(URL, r.status_code)
# else:
#     print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")


# URL = 'https://deep-sea.ru/rest/materialNodes?project=200101'
# r = requests.get(URL)
# if r.status_code == 200:
#     print(URL, r.status_code)
# else:
#     print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

# URL = 'https://deep-sea.ru/rest/materials?project=200101'
# r = requests.get(URL)
# if r.status_code == 200:
#     print(URL, r.status_code)
# else:
#     print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

# URL = 'https://deep-sea.ru/rest-spec/materialsSummary?projects=N002&kinds=pipe,hull,device'
# r = requests.get(URL)
# if r.status_code == 200:
#     print(URL, r.status_code)
# else:
#     print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

# URL = 'https://deep-sea.ru/rest-spec/materialPurchases?project=N002'
# r = requests.get(URL)
# if r.status_code == 200:
#     print(URL, r.status_code)
# else:
#     print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

# URL = 'https://deep-sea.ru/doclist'
# r = requests.get(URL)
# if r.status_code == 200:
#     print(URL, r.status_code)
# else:
#     print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

# URL = 'https://deep-sea.ru/rest-spec/trayBundles?project=P701'
# r = requests.get(URL)
# if r.status_code == 200:
#     print(URL, r.status_code)
# else:
#     print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")


# URL = 'https://deep-sea.ru/rest-spec/elecCables?project=P701&bundle=170701-871-2001&magistral=1'
# r = requests.get(URL)
# if r.status_code == 200:
#     print(URL, r.status_code)
# else:
#     print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

# URL = 'https://deep-sea.ru/rest/projectNames'
# r = requests.get(URL)
# if r.status_code == 200:
#     print(URL, r.status_code)
# else:
#     print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

# URL = 'https://deep-sea.ru/billing?foranProject=N002'
# r = requests.get(URL)
# if r.status_code == 200:
#     print(URL, r.status_code)
# else:
#     print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

# URL = 'https://deep-sea.ru/pipe-billing?foranProject=N002'
# r = requests.get(URL)
# if r.status_code == 200:
#     print(URL, r.status_code)
# else:
#     print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

# URL = 'https://deep-sea.ru/weight?project=N004'
# r = requests.get(URL)
# if r.status_code == 200:
#     print(URL, r.status_code)
# else:
#     print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

# URL = 'https://deep-sea.ru/rest-spec/bsDesignNodes?project=N004'
# r = requests.get(URL)
# if r.status_code == 200:
#     print(URL, r.status_code)
# else:
#     print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

print('-----POST_REQUESTS-----')

# START PROJECT + EDIT USERS PROJECT
URL_1 = 'https://deep-sea.ru/rest/startProject'
r = requests.post(URL_1, 
json={
    "id": 0,
    "name": "1",
    "foran": "1",
    "rkd": "1",
    "pdsp": "1",
    "factory": "1",
    "managers": "spiridovich",
    "status": "1"
})

connect = psycopg2.connect(dbname='deepsea', user='deepsea', password='Ship1234', host='192.168.1.26')
cursor = connect.cursor()

id = r.json()
req = "delete from issue_projects where id = " + id
sql = connect.cursor()
sql.execute(req)
connect.commit()
connect.close()

URL_2 = 'https://deep-sea.ru/rest/editUsersProject?idProject=' + id
r = requests.post (URL_2, 
json=[
    156,
    159
    ])
