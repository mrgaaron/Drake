import stackless

class Registry(object):
    def __init__(self):
        self.registry = {}
    
    def dispatch(self, command_string, actor):
        words = command_string.split()
        if not words:
            return
        if words[0] not in self.registry:
            actor.send_message(actor, 'I\'m not sure what you mean.')
        else:
            stackless.tasklet(self.registry[words[0]])(words, actor)
        stackless.schedule()
    
    def register(self, keyword, command):
        self.registry[keyword] = command

CommandRegistry = Registry()

ns = __import__('builtins', globals(), locals(), '*')

for item in dir(ns):
    obj = getattr(ns, item)
    if hasattr(obj, '__commands__'):
        for key in obj.__commands__:
            CommandRegistry.register(key, obj())