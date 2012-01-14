import actor
import stackless

import sys

sys.path.append('C:\\Drake\\src')

from drake.command.registry import CommandRegistry
import actor

class Mobile(actor.Actor):
    short_description = None
    action_description = None
    
    def __init__(self, actor_id, room):
        super(Mobile, self).__init__(actor_id, room)
        room.add_actor(self)
        self.is_ai = True
        self.attackers = []
        
    def turn_aggressive(self):
        raise NotImplementedError
    
    def respond(self, from_actor, msg):
        if not from_actor == self:
            cmd = 'say I will do nothing for you.'
            stackless.tasklet(
                    CommandRegistry.dispatch)(cmd, self)
            stackless.schedule()
        