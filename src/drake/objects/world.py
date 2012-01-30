import os, re
import room

import sys
sys.path.append('C:\\Drake\src')

from drake.mobiles.orcs import warrior

def split_path(path):
    path = re.split(r'(?:\\)|(?:/)', path)
    return [part for part in path if part]

def extract_zone_path(root_path, this_directory, dirname):
    #slice away a substring equal in length to the root directory,
    #this gives us the actual zone path
    no_root_path = this_directory[len(root_path):]
    if no_root_path:
        path = split_path(no_root_path)
        path.append(dirname)
        return path
    else:
        return [dirname]

def extract_zone_path_from_room(root_path, room_path):
    no_root_path = room_path[len(root_path):]
    path = split_path(no_root_path)
    return path[:-1]

class Zone(object):
    def __init__(self, name):
        self.id = name
        self.rooms = {}
        self.sub_zones = {}
    
    def __repr__(self):
        return '<Zone "%s">' % self.id
    
    def add_room(self, room):
        self.rooms[room.id] = room
    
    def add_subzone(self, zone):
        self.sub_zones[zone.id] = zone
        
    def get_room(self, room_id):
        return self.rooms[room_id]
    
    def get_zone(self, zone_id):
        return self.sub_zones[zone_id]
        
    def remove_room(self, room):
        del self.rooms[room.id]
    
    def remove_zone(self, zone):
        del self.sub_zones[zone.id]

class World(object):
    def __init__(self, data_path):
        self.data_path = data_path
        self.zones = Zone('World')
        self.init_rooms()
        self.initial_room = 'Sephix/Outside/Market/Central Square'
    
    def create_zone(self, zone_path):
        if len(zone_path) == 1:
            zone_id = zone_path[0]
            zone = Zone(zone_id)
            self.zones.add_subzone(zone)
        else:
            zone_id = zone_path[-1]
            zone = Zone(zone_id)
            superzone = self.query_zone(zone_path[:-1])
            superzone.add_subzone(zone)
        
    def init_rooms(self):
        rooms = []
        for dirpath, dirnames, filenames in os.walk(self.data_path):
            for dirname in dirnames:
                zone_path = extract_zone_path(self.data_path, dirpath, dirname)
                self.create_zone(zone_path)
            for name in filenames:
                room_path = os.path.join(dirpath, name)
                this_zone = extract_zone_path_from_room(self.data_path, 
                                                        room_path)
                room_id = re.sub('[.].+$', '', name)
                fh = open(room_path, 'r')
                room_string = fh.read()
                path = '%s/%s' % ('/'.join(this_zone), room_id)
                rm = room.Room(self, room_id, path, room_string)
                self.insert_room(this_zone, rm)
                rooms.append(rm)
                fh.close()                
                
        for rm in rooms:
            rm.parse()
            
        #for testing
        
        rm = self.query_room('Test/Arena/Foyer')
        mob = warrior.OrcWarrior('orc-64', rm)
            
    def insert_room(self, zone_path, room):
        zone = self.query_zone(zone_path)
        zone.add_room(room)
            
    def query_room(self, query):
        if hasattr(query, 'split'):
            room_path = query.split('/')
        else:
            room_path = query
    
        zone = self.query_zone(room_path[:-1])
        room = zone.rooms.get(room_path[-1])
        if room:
            return room
        else:
            raise KeyError('No room found')

    def query_zone(self, query):
        current_zone = self.zones
        while query:    
            next_zone_id = query.pop(0)
            current_zone = current_zone.get_zone(next_zone_id)
        return current_zone