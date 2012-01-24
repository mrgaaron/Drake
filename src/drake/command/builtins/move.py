import sys
sys.path.append('C:\\Drake\\src')

from drake.command import Command
import stackless

_CONVERSIONS = {
    's': 'south',
    'n': 'north',
    'e': 'east',
    'w': 'west',
    'ne': 'northeast',
    'se': 'southeast',
    'sw': 'southwest',
    'nw': 'northwest',
    'up': 'up',
    'in': 'in'
    }

class MoveCommand(Command):
    __commands__ = ['n', 'north', 'east', 'e', 'w', 'west', 's', 'south', 'up',
                    'down', 'nw', 'ne', 'se', 'sw', 'in', 'out']
    
    def __call__(self, rest, actor):
        current_room = actor.room
        direction = rest[0]

        if len(direction) <= 2:
            direction = _CONVERSIONS[direction]        
        
        destination = current_room.exits[direction]
        
        if destination:
            current_room.remove_actor(actor)
            current_room.broadcast('%s just went %s.' % (actor.action_description,
                                                        direction),
                                   actor)
            destination.broadcast('%s just arrived.' % actor.action_description,
                                  actor)
            destination.add_actor(actor)
            actor.send_message(actor, destination.to_string(actor))
            actor.room = destination
        else:
            actor.send_message(actor, 'You cannot go there.')

class GoCommand(Command):
    __commands__ = ['go']
    
    def __call__(self, rest, actor):
        current_room = actor.room
        if len(rest) == 1:
            actor.send_message(actor, 'Go where?')
            return
        
        direction = rest[1]

        destination = current_room.exits[direction]
        
        if destination:
            current_room.remove_actor(actor)
            current_room.broadcast('%s just went into the %s.' % 
                                   (actor.action_description, direction), actor)
            destination.broadcast('%s just arrived.' % actor.action_description,
                                  actor)
            destination.add_actor(actor)
            actor.send_message(actor, destination.to_string(actor))
            actor.room = destination
        else:
            actor.send_message(actor, 'You cannot go there.')

class GoToCommand(Command):
    __commands__ = ['goto']
    
    def __call__(self, rest, actor):
        current_room = actor.room
        if len(rest) == 1:
            actor.send_message(actor, 'Goto where?')
            return
        path = ' '.join(rest[1:])
        
        try:
            destination = actor.room.world.query_room(path)
            current_room.remove_actor(actor)
            actor.send_message(actor, destination.to_string(actor))
            destination.add_actor(actor)
            actor.room = destination
        except KeyError:
            actor.send_message(actor, '%s is an invalid room path.' % rest[1])
        
        
