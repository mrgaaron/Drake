import sys, time, threading
import stackless

sys.path.append('C:\\Drake\\src')

import ssocket
#ssocket.install()
import socket

from drake.objects import world
from drake.objects.actors import player
from drake.database.player import player_db
from drake.command.registry import CommandRegistry

idle_channel = stackless.channel()
dead_channel = stackless.channel()

def handle_login(server, connection, tries=1):
    if tries > 3:
        connection.close()
        
    try:
        connection.send('What is your name?')
        player_id = connection.recv(1024)[:-1]
    except Exception, e:
        print e
        connection.close()
        dead_channel.send('go')
        return
    
    exists = server.already_connected(player_id)
    if exists:
        print 'Cleaning up existing connection for player'
        exists[0].close_connection()
        stackless.schedule()

    initial_room = server.world.query_room(server.world.initial_room)
    new_player = player.Player(connection, initial_room,
                                dead_channel)
    
    if player_db.player_exists(player_id):
        player_info = player_db.get_player_info(player_id)
        new_player.id = player_id
        new_player.short_description = player_info['short_description']
        new_player.action_description = player_id
        new_player.long_description = '%s is a game player.' % new_player.id
        new_player.room = server.world.query_room(
            player_info['current_room'])
        new_player.room.add_actor(new_player)
        stackless.tasklet(new_player.read_command)()
        new_player.send_message(None, new_player.room.to_string(new_player))
        new_player.room.broadcast('%s just entered.' % new_player.id,
                                new_player)
        stackless.schedule()
        server.players.append(new_player)

    else:
        try:
            connection.send(None, 'I\'m sorry, I don\'t recognize you.')
        except:
            connection.close()
            dead_channel.send('go')
            return

        next_try = tries+1
        handle_login(tries=next_try)


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
        
    def already_connected(self, player_id):
        def filt(x):
            return x.id == player_id
        
        return filter(filt, self.players) 
    
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
        server_socket = ssocket._socketobject_new(socket.AF_INET, socket.SOCK_STREAM)
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server_socket.bind((self.host, self.port))
        print 'Server listening on %d and task scheduler started.' % self.port
        server_socket.listen(5)
        self.scheduler.start()
        
        while True:
            client_socket, client_address = server_socket.accept()
            print 'Accepting connection from %s:%d' % client_address
            client_socket.send('Welcome to Drake!\n\n')
            handle_login(self, client_socket)
            stackless.schedule()
            
def initialize():
    host, port = sys.argv[1:]
    server = Server(host, port)
    
    while True:
        stackless.run()        
        
initialize()