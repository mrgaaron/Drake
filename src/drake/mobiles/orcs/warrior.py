import sys
import stackless

sys.path.append('C:\\Drake\\src')

from drake.objects.actors.mobile import *

class OrcWarrior(Mobile):
    short_description = 'an orc warrior'
    action_description = 'An orc warrior'
    
    def respond(self, from_actor, msg):
        if from_actor != self:
            if 'arrived' in msg:
                cmd = 'emote spits in your face.'
                stackless.tasklet(
                        CommandRegistry.dispatch)(cmd, self)
                stackless.schedule()
                
            elif 'just went' in msg:
                target = msg.split()[0]
                cmd = 'emote laughs at %s as he leaves.' % target
                stackless.tasklet(
                        CommandRegistry.dispatch)(cmd, self)
                stackless.schedule()
            
                