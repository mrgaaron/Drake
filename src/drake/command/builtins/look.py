import sys
sys.path.append('C:\\Drake\\src')

from drake.command import Command

class LookCommand(Command):
    __commands__ = ['look']
    
    def __call__(self, commands, actor):
        current_room = actor.room
        actor.send_message(actor, current_room.to_string(actor))
