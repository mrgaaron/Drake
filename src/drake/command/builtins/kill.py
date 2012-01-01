import sys
sys.path.append('C:\\Drake\\src')

from drake.command import Command

class KillCommand(Command):
    __commands__ = ['kill']
    
    def __call__(self, commands, actor):
        actor.apply_round_time(4)
        actor.send_message(actor, 'You kill everything. (roundtime: 4 secs)')
