import stackless

class Exits(object):
    def __init__(self):
        self.routes = {
                'north': None,
                'south': None,
                'east': None,
                'west': None,
                'up': None,
                'down': None             
            }
        
    def __setitem__(self, key, value):
        if key not in self.routes:
            raise ValueError('Exit direction %s not supported.' % key)
        self.routes[key] = value
    
    def __getitem__(self, key):
        return self.routes.get(key)
    
    def to_string(self):
        valid_exits = []
        for k in self.routes:
            if self.routes[k]:
                valid_exits.append(k)
        return ', '.join(valid_exits)

class Room(object):
    def __init__(self, world, room_id, room_string):
        self.id = room_id
        self.room_string = room_string
        self.world = world
        self.exits = Exits()
        self.actors = set()
    
    def __hash__(self):
        return hash(self.id)
    
    def __eq__(self, other):
        if not isinstance(other, Room):
            raise ValueError('Room objects can only be compared to each other')
        return self.id == other.id
        
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
        for t in to_remove:
            self.actors.remove(t)
        t.set_atomic(atomic)
    
    def parse(self):
        parts = self.room_string.split('==\n')
        for part in parts:
            if not part:
                continue
            name, value = part.split(':\n')
            if name != 'Exits':
                setattr(self, name.lower(), value.strip('\n'))
            else:
                exits = value.split('\n')
                for e in exits:
                    if not e:
                        continue
                    pieces = e.split('-')
                    direction = pieces[0].lower()
                    try:
                        destination = self.world.query_room(pieces[1])
                        self.exits[direction] = destination
                    except:
                        pass
    
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
            
        
           
           