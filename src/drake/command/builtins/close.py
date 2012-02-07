from drake.command import Command

#in general you don't want the player opening and closing directions
FORBIDDEN = set(['n', 'north', 'south', 's', 'e', 'east', 'west', 'w', 'nw',
                 'northwest', 'southwest', 'sw', 'southeast', 'se', 'u', 'up',
                 'd', 'down'])

class OpenCommand(Command):
    __commands__ = ['open']
    
    def __call__(self, commands, actor):    
        direction = ' '.join(commands[1:])
        if len(commands) < 2:
            actor.send_message(actor, 'What did you want to open?')
            return
        
        current_room = actor.room
        if direction in FORBIDDEN:
            actor.send_message(actor, 'You can\'t open that direction.')
            return
           
        if current_room.exits.is_open(direction):
            actor.send_message(actor, 'The %s is already open.' % direction)
            return
            
        if not current_room.exits[direction]:
            actor.send_message(actor, 'There is no %s to open.' % direction)
            return
        
        current_room.exits.open(direction)
        actor.send_message(actor, 'You open the %s.' % direction)
    
class CloseCommand(Command):
    __commands__ = ['close']
    
    def __call__(self, commands, actor):    
        direction = ' '.join(commands[1:])
        if len(commands) < 2:
            actor.send_message(actor, 'What did you want to close?')
            return
        
        current_room = actor.room
        if direction in FORBIDDEN:
            actor.send_message(actor, 'You can\'t close that direction.')
            return
            
        if not current_room.exits.is_open(direction):
            actor.send_message(actor, 'The %s is already closed.' % direction)
            return
        
        if not current_room.exits[direction]:
            actor.send_message(actor, 'There is no %s to close.' % direction)
            return
        
        current_room.exits.close(direction)
        actor.send_message(actor, 'You close the %s.' % direction)