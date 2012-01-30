from connection import drake_db

def to_doc(player):
    doc = {
        '_id': player.id,
        'short_description': player.short_description,
        'current_room': player.room.room_path,
        'inventory': []    
    }
    return doc

class PlayerDatabase(object):
    def __init__(self):
        self.db = drake_db.players
        
    def get_player_info(self, player_id):
        player = self.db.find_one({'_id': player_id})
        return player
    
    def player_exists(self, player_id):
        player = self.db.find({'_id': player_id})
        return player.count()
    
    def read_attribute(self, player, attribute):
        player = self.db.find_one({'_id': player.id})
        return player[attribute]
    
    def save_player(self, player):
        doc = to_doc(player)
        #the True option here makes this an "upsert"
        self.db.update({'_id': player.id}, doc, True)
    
player_db = PlayerDatabase()
    