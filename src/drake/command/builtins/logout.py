from drake.command import Command

class LogoutCommand(Command):
    __commands__ = ['logout']
    
    def __call__(self, rest, actor):    
        actor.close_connection()