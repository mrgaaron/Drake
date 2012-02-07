from drake.command import Command

class EmoteCommand(Command):
    __commands__ = ['emote']
    
    def __call__(self, commands, actor):
        emote_string = ' '.join(commands[1:])
        if not emote_string:
            actor.send_message('You can\'t emote nothing!')
        else:
            actor.room.broadcast('%s %s' % (actor.action_description, emote_string), actor)
            actor.send_message(actor, 'You %s' % emote_string)