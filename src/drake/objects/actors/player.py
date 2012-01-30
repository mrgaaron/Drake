import sys, time, threading
import stackless

sys.path.append('C:\\Drake\\src')

from drake.command.registry import CommandRegistry
import actor
from drake.mobiles.humans.basic import Human
from drake.database.player import player_db

class Player(actor.Actor):
    def __init__(self, connection, room, dead_channel):
        super(Player, self).__init__(None, room)
        self.dead_channel = dead_channel
        self.connection = connection
        self.alive = True
        self.last_contact = time.time()
        self.warned = False
        self.cleaned = False
        self.avatar = Human(self)
        self.is_ai = False
        
    def __repr__(self):
        return '<Player "%s">' % self.id
    
    def close_connection(self):
        self.alive = False
        print 'Closing connection for %s' % (self.id)
        self.save()
        self.dead_channel.send('go')
        
    def save(self):
        player_db.save_player(self)
    
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