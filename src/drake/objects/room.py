import stackless
import re

class Exits(object):
    def __init__(self):
        self.routes = {
                'north': [None, 'closed'],
                'south': [None, 'closed'],
                'east': [None, 'closed'],
                'west': [None, 'closed'],
                'northwest': [None, 'closed'],
                'southeast': [None, 'closed'],
                'southwest': [None, 'closed'],
                'northeast': [None, 'closed'],
                'up': [None, 'closed'],
                'down': [None, 'closed'],
                'in': [None, 'closed'],
                'out': [None, 'closed'],  
            }
        self.special_exits = {}
        
    def __getitem__(self, key):
        if key not in self.routes:
            return self.special_exits.get(key, [None])[0]
        return self.routes.get(key, [None])[0]
    
    def _set_exit_status(self, key, status):
        if key not in self.routes:
            ex = self.special_exits.get(key)
        else:
            ex = self.routes.get(key)
        
        if ex:
            ex[1] = status
            self.special_exits[key] = ex
    
    def close(self, key):
        self._set_exit_status(key, 'close')
        
    def has_attr(self, key, attr):
        if key not in self.routes:
            ex = self.special_exits.get(key)
        else:
            ex = self.routes.get(key)
        
        return attr in ex        

    def is_open(self, key):
        if key not in self.routes:
            ex = self.special_exits.get(key)
        else:
            ex = self.routes.get(key)
        if ex:
            return ex[1] == 'open'
        return False
    
    def open(self, key):
        self._set_exit_status(key, 'open')
    
    def set_exit(self, direction, destination, *attrs):
        l = [destination]
        l.extend(attrs)
        if direction in self.routes:
            self.routes[direction] = l
        else:
            self.special_exits[direction] = l
    
    def to_string(self):
        valid_exits = []
        for k in self.routes:
            if self.routes[k][0]:
                valid_exits.append(k)
        valid_exits.sort()
        return ', '.join(valid_exits)

class Room(object):
    def __init__(self, world, room_id, room_path, room_string):
        self.id = room_id
        self.room_string = room_string
        self.room_path = room_path
        self.world = world
        self.exits = Exits()
        self.actors = set()
        #features are items in the room that can be looked at/interacted with
        self.features = {}
    
    def __hash__(self):
        return hash(self.id)
    
    def __eq__(self, other):
        if not isinstance(other, Room):
            raise ValueError('Room objects can only be compared to each other')
        return self.id == other.id
    
    def _parse_exits(self, value):
        exits = value.split('\n')
        for e in exits:
            if not e:
                continue
            pieces = e.split('-')
            direction = pieces[0].lower()
            attrs = pieces[1].split('%')
            destination = self.world.query_room(attrs[0])
            
            if len(attrs) > 1:
                attrs = attrs[1].split(',')
                self.exits.set_exit(direction, destination, *attrs)
            else:
                self.exits.set_exit(direction, destination, 'open')
    
    def _parse_features(self, value):
        features = value.split('%')
        for f in features:
            if not f:
                continue
            pieces = f.split('-')
            self.features[pieces[0]] = pieces[1]

    def add_actor(self, actor):
        self.actors.add(actor)
        
    def broadcast(self, message, from_actor):
        to_remove = []
        #all iterations over a hashed collection like sets and dictionaries
        #need to be atomic
        t = stackless.getcurrent()
        atomic = t.set_atomic(True)
        for actor in self.actors:
            if hasattr(actor, 'alive') and not actor.alive:
                to_remove.append(actor)
                continue
            if actor != from_actor:
                actor.send_message(from_actor, message)
        #handle removal from the room within the room itself, this prevents
        #strange concurrency issues from the server trying to do it
        for remove in to_remove:
            self.actors.remove(remove)
        t.set_atomic(atomic)
    
    def parse(self):
        parts = re.split(r'==|==\n', self.room_string)
        for part in parts:
            if not part:
                continue
            name, value = part.split(':\n')
            canon_name = name.lower().strip('\n')
            if canon_name == 'exits':
                self._parse_exits(value)
            elif canon_name == 'features':
                self._parse_features(value)
            else:
                setattr(self, canon_name, value.strip('\n'))
        del self.room_string
                
    def query_feature(self, feature):
        return self.features.get(feature)
                    
    def query_target(self, target):
        for actor in self.actors:
            if actor.id == target or target in actor.short_description:
                return actor
                    
    def remove_actor(self, actor):
        t = stackless.getcurrent()
        atomic = t.set_atomic(True)
        try:
            self.actors.remove(actor)
        except KeyError:
            pass
        t.set_atomic(atomic)
                    
    def to_string(self, actor):
        other_actors = ', '.join([a.short_description for a in self.actors if 
                                  a != actor])
        string = '[%s]\n\n%s\n\nExits: %s' % (self.title, self.description,
                                        self.exits.to_string())
        if other_actors:
            string += '\nAlso here: %s\n' % other_actors
        return string
            
        
           
           