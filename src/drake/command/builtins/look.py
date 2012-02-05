import sys
sys.path.append('C:\\Drake\\src')

from drake.command import Command

class LookCommand(Command):
    __commands__ = ['look']
    
    def __call__(self, commands, actor):
        current_room = actor.room
        if len(commands) == 1:
            actor.send_message(actor, current_room.to_string(actor))
        else:
            target = ' '.join(commands[1:])
            obj = current_room.query_target(target)
            if obj:
                actor.send_message(actor, obj.long_description)
                obj.send_message(actor, 
                                 '%s is looking at you' % actor.action_description)
            else:
                feature = current_room.query_feature(target)
                if feature:
                    actor.send_message(actor, feature)
                else:
                    actor.send_message(actor, 'There is no %s here.' % target)