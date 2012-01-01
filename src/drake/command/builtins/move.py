import sys
sys.path.append('C:\\Drake\\src')

from drake.command import Command
import stackless

_CONVERSIONS = {
    's': 'south',
    'n': 'north',
    'e': 'east',
    'w': 'west'
    }

class MoveCommand(Command):
    __commands__ = ['n', 'north', 'east', 'e', 'w', 'west', 's', 'south', 'up',
                    'down']
    
    def __call__(self, rest, actor):
        current_room = actor.room
        direction = rest[0]

        if len(direction) == 1:
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
