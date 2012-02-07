from drake.command import Command

class SayCommand(Command):
    __commands__ = ['say']
    
    def __call__(self, rest, actor):
        words = ' '.join(rest[1:])
        actor.send_message(actor, 'You say "%s"' % (words))
        actor.room.broadcast('%s says "%s"' % (actor.action_description, words), 
                               actor)
