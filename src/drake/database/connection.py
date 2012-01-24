import pymongo

connection = pymongo.Connection('localhost', 27017)

class ActorDatabase(object):
    def __init__(self):
        self.db = connection.actors
    
    def read_attribute(self):
        pass
    
    def save_actor(self):
        pass
    
    def save_inventory(self):
        pass
    
actor_db = ActorDatabase()
    