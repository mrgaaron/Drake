import sys
sys.path.append('C:\\Drake\\src')

from drake.command import Command

class HealthCommand(Command):
    __commands__ = ['health']
    
    def __call__(self, commands, actor):    
        actor.send_message(actor, 
                           'You have %d health remaining' % actor.avatar.body.hp)