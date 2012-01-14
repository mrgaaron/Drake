import sys, time, threading
import stackless

sys.path.append('C:\\Drake\\src')

from drake.command.registry import CommandRegistry
import actor
from drake.mobiles.humans.basic import Human

class Player(actor.Actor):
    def __init__(self, connection, room, dead_channel):
        super(Player, self).__init__(None, room)
        self.dead_channel = dead_channel
        self.connection = connection
        self.alive = True
        stackless.tasklet(self.handle_login)()
        self.last_contact = time.time()
        self.warned = False
        self.cleaned = False
        self.avatar = Human(self)
        self.is_ai = False
        
    def __repr__(self):
        return '<Player "%s">' % self.id
    
    def close_connection(self):
        self.alive = False
        self.dead_channel.send('go')
        
    def handle_login(self):
        self.send_message(None, 'What is your name?')
        self.id = self.connection.recv(1024)
        self.short_description = self.id
        self.action_description = self.id
        self.long_description = '%s is a game player.' % self.id
        self.room.add_actor(self)
        stackless.tasklet(self.read_command)()
        self.send_message(None, self.room.to_string(self))
        self.room.broadcast('%s just entered.' % self.id,
                                    self)
        stackless.schedule()
    
    def send_message(self, from_actor, message):
        try:
            self.connection.send(message + '\n')
        except Exception, e:
            print '%s: %s' % (self.id, e)
            self.close_connection()
    
    def read_command(self):
        while True:
            try:
                command = self.connection.recv(1024)
                self.last_contact = time.time()
            except Exception, e:
                print '%s: %s' % (self.id, e)
                break
            self.execute_command(command)
            #stackless.tasklet(CommandRegistry.dispatch)(command, self)
            stackless.schedule()
        self.close_connection()