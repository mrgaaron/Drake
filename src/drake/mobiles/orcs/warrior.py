import sys
import stackless

sys.path.append('C:\\Drake\\src')

from drake.objects.actors.mobile import *
from attributes import *

LONG_DESCRIPTION = \
'\n'.join(['The orc warrior levels a malevolent grin at you as he runs a wicked blade',
            'down the palm of his hand.  He looks ready to embed the knife in your',
            'chest at the slightest provocation.'])
    
class OrcWarrior(Mobile):
    short_description = 'an orc warrior'
    action_description = 'An orc warrior'
    long_description = LONG_DESCRIPTION
    
    def __init__(self, actor_id, room):        super(OrcWarrior, self).__init__(actor_id, room)
        self.avatar = create_body(self)
    
    def respond(self, from_actor, msg):
        if from_actor != self:
            if 'arrived' in msg:
                cmd = 'emote spits in your face.'
                self.execute_command(cmd)
            elif 'just went' in msg:
                target = msg.split()[0]
                cmd = 'emote laughs at %s as he leaves.' % target
                self.execute_command(cmd)
            elif 'is looking' in msg:
                cmd = 'say I don\'t like it when people look at me, fool!'
                self.execute_command(cmd)
                cmd = 'emote suddenly sticks a blade in your chest!'
                self.execute_command(cmd)
                
    def turn_aggressive(self, from_actor):
        cmd = 'say So you want to attack me eh?'
        CommandRegistry.dispatch(cmd, self)
        if from_actor in self.attackers:
            return
        stackless.schedule()
        
        self.attackers.append(from_actor)
        while self.attackers:
            stackless.schedule()
            for i, attacker in enumerate(self.attackers):
                if self.room.query_target(from_actor.id):
                    cmd = 'kill %s' % attacker.id
                    self.execute_command(cmd)
                    stackless.schedule()
                else:
                    self.attackers.pop(i)
                stackless.schedule()
                
