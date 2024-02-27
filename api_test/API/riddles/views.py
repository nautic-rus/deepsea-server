from django.shortcuts import render
import requests
import json
import psycopg2


print('-----GET_REQUESTS-----')
URL = 'https://deep-sea.ru/rest/users'
r = requests.get(URL)
if r.status_code == 200:
    print(URL, r.status_code)
else:
    print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

URL = 'https://deep-sea.ru/rest/timeControl?user=19'
r = requests.get(URL)
if r.status_code == 200:
    print(URL, r.status_code)
else:
    print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")


URL = 'https://deep-sea.ru/rest/issues?user=spiridovich'
r = requests.get(URL)
if r.status_code == 200:
    print(URL, r.status_code)
else:
    print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

URL = 'https://deep-sea.ru/rest/issuesViewed?user=spiridovich'
r = requests.get(URL)
if r.status_code == 200:
    print(URL, r.status_code)
else:
    print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")


URL = 'https://deep-sea.ru/sections'
r = requests.get(URL)
if r.status_code == 200:
    print(URL, r.status_code)
else:
    print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

URL = 'https://deep-sea.ru/rest/nestingFiles'
r = requests.get(URL)
if r.status_code == 200:
    print(URL, r.status_code)
else:
    print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")


URL = 'https://deep-sea.ru/rest/materialNodes?project=200101'
r = requests.get(URL)
if r.status_code == 200:
    print(URL, r.status_code)
else:
    print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

URL = 'https://deep-sea.ru/rest/materials?project=200101'
r = requests.get(URL)
if r.status_code == 200:
    print(URL, r.status_code)
else:
    print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

URL = 'https://deep-sea.ru/rest-spec/materialsSummary?projects=N002&kinds=pipe,hull,device'
r = requests.get(URL)
if r.status_code == 200:
    print(URL, r.status_code)
else:
    print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

URL = 'https://deep-sea.ru/rest-spec/materialPurchases?project=N002'
r = requests.get(URL)
if r.status_code == 200:
    print(URL, r.status_code)
else:
    print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

URL = 'https://deep-sea.ru/doclist'
r = requests.get(URL)
if r.status_code == 200:
    print(URL, r.status_code)
else:
    print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

URL = 'https://deep-sea.ru/rest-spec/trayBundles?project=P701'
r = requests.get(URL)
if r.status_code == 200:
    print(URL, r.status_code)
else:
    print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")


URL = 'https://deep-sea.ru/rest-spec/elecCables?project=P701&bundle=170701-871-2001&magistral=1'
r = requests.get(URL)
if r.status_code == 200:
    print(URL, r.status_code)
else:
    print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

URL = 'https://deep-sea.ru/rest/projectNames'
r = requests.get(URL)
if r.status_code == 200:
    print(URL, r.status_code)
else:
    print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

URL = 'https://deep-sea.ru/billing?foranProject=N002'
r = requests.get(URL)
if r.status_code == 200:
    print(URL, r.status_code)
else:
    print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

URL = 'https://deep-sea.ru/pipe-billing?foranProject=N002'
r = requests.get(URL)
if r.status_code == 200:
    print(URL, r.status_code)
else:
    print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

URL = 'https://deep-sea.ru/weight?project=N004'
r = requests.get(URL)
if r.status_code == 200:
    print(URL, r.status_code)
else:
    print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

URL = 'https://deep-sea.ru/rest-spec/bsDesignNodes?project=N004'
r = requests.get(URL)
if r.status_code == 200:
    print(URL, r.status_code)
else:
    print(URL,r.status_code, "ACHTUNG!ACHTUNG!ACHTUNG!")

print('-----POST_REQUESTS-----')

print ("#1 --- START PROJECT + EDIT USERS PROJECT")
URL_1 = 'https://deep-sea.ru/rest/startProject'
r1 = requests.post(URL_1, 
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

print( "Create project with id=", r1.json())

id = r1.json()
URL_2 = 'https://deep-sea.ru/rest/editUsersProject?idProject=' + id
r2 = requests.post (URL_2, 
json=[
    156,
    159
    ]
)
print("Edit user for project id = ", id)

connect = psycopg2.connect(dbname='deepsea', user='deepsea', password='Ship1234', host='192.168.1.26')
cursor = connect.cursor()

id = r1.json()
req = "delete from issue_projects where id = " + id
sql = connect.cursor()
sql.execute(req)
connect.commit()
connect.close()
print("Project id = ", id, "delete")

print ("#2 --- NEW USER")
URL_1 = 'https://deep-sea.ru/rest/startUser'
r1 = requests.post(URL_1, 
json={
  "id": 0,
  "login": "CvYlzXjrv9",
  "password": "CvYlzXjrv9",
  "name": "CvYlzXjrv9",
  "surname": "CvYlzXjrv9",
  "profession": "-",
  "department": "Hull",
  "id_department": 1,
  "birthday": "12/18/2023",
  "email": "",
  "phone": "",
  "tcid": 0,
  "avatar": "assets/employees/account-user.png",
  "avatar_full": "assets/employees/account-user.png",
  "rocket_login": "",
  "gender": "male",
  "visibility": "ck",
  "visible_projects": [
    "-"
  ],
  "visible_pages": [
    "-"
  ],
  "shared_access": [],
  "groups": [],
  "groupNames": "",
  "permissions": [],
  "token": "",
  "removed": 0
})

print( "Create new user - ", r1.json())

connect = psycopg2.connect(dbname='deepsea', user='deepsea', password='Ship1234', host='192.168.1.26')

req = "select * from users where login = 'CvYlzXjrv9'"
sql = connect.cursor()
sql.execute(req)
records = sql.fetchall()
recordsKortege = records[0]
recordsId = str(recordsKortege[23])
print("Detected user id = " + recordsId)
connect.commit()
connect.close()

connect = psycopg2.connect(dbname='deepsea', user='deepsea', password='Ship1234', host='192.168.1.26')
cursor = connect.cursor()

req = "delete from users where id = " + recordsId
sql = connect.cursor()
sql.execute(req)
connect.commit()
connect.close()
print("User ID = ",recordsId, "delete")
