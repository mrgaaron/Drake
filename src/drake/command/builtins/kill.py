import sys
sys.path.append('C:\\Drake\\src')

from drake.command import Command

class KillCommand(Command):
    __commands__ = ['kill']
    
    def __call__(self, commands, actor):
        if len(commands) < 2:
            actor.send_message(actor, 'Who are you talking about?')
            return
        
        target = commands[1]
        mob = actor.room.query_target(target)
        if mob:
            actor.apply_round_time(4)
            actor.send_message(actor, 'You slice at %s. (roundtime: 4 secs)' % mob.short_description)
            if mob.is_ai:
                mob.turn_aggressive(actor)
            mob.send_message(actor, '%s slices at you.' % actor.action_description)
            if mob.is_ai:
                mob.avatar.damage(0,0,0)
            else:
                mob.avatar.body.damage(0,0,0)
            
        else:
            actor.send_message(actor, 'Who are you talking about?')