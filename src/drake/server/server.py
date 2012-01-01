import sys, time, threading
import stackless

sys.path.append('C:\\Drake\\src')

import ssocket
ssocket.install()
import socket

from drake.objects import world
from drake.objects.actors import player
from drake.command.registry import CommandRegistry

idle_channel = stackless.channel()
dead_channel = stackless.channel()

class ServerTaskScheduler(threading.Thread):
    def run(self):
        while True:
            time.sleep(30)
            idle_channel.send('go')
            time.sleep(30)
            dead_channel.send('go')

class Server(object):
    def __init__(self, host, port):
        self.host = host
        self.port = int(port)
        stackless.tasklet(self.run)()
        stackless.tasklet(self.close_dead_connections)()
        stackless.tasklet(self.close_idle_connections)()
        self.players = []
        self.last_sweep = time.time()
        self.scheduler = ServerTaskScheduler()
        self.scheduler.setDaemon(True)
        print 'Initializing game world...'
        self.world = world.World('C:\\Drake\\Game Data')
    
    def close_dead_connections(self):
        while True:
            dead_channel.receive()
            print 'Sweeping dead connections...'
            for i, p in enumerate(self.players):
                if not p.alive:
                    print 'Closing dead connection'
                    p.connection.close()
                    pl = self.players.pop(i)
    
    def close_idle_connections(self):
        while True:
            idle_channel.receive()
            print 'Sweeping idle connections...'
            for p in self.players:
                now = time.time()
                if now - p.last_contact > 300 and p.warned:
                    p.send_message(None, 'You were warned.  Bye.')
                    p.close_connection()
                elif now - p.last_contact > 240:
                    p.warned = True
                    p.send_message(None, 
                                   'Warning: you have been idle too long. Respond or be disconnected.')
            stackless.schedule()
    
    def run(self):
        print 'Initializing server...'
        server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server_socket.bind((self.host, self.port))
        print 'Server listening on %d and task scheduler started.' % self.port
        server_socket.listen(5)
        self.scheduler.start()
        
        while True:
            client_socket, client_address = server_socket.accept()
            print 'Accepting connection from %s:%d' % client_address
            client_socket.send('Welcome to Drake!\n\n')
            initial_room = self.world.query_room(self.world.initial_room)
            self.players.append(player.Player(client_socket, initial_room,
                                              dead_channel))
            stackless.schedule()
            
def initialize():
    host, port = sys.argv[1:]
    server = Server(host, port)
    
    while True:
        stackless.run()        
        
initialize()