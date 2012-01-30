import pymongo

connection = pymongo.Connection('localhost', 27017)
drake_db = connection.drake