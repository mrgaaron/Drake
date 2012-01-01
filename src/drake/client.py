import stackless
import sys, signal
from server import ssocket
ssocket.install()
import socket

import threading

channel = stackless.channel()

class UserInput(threading.Thread):
    def run(self):
        while True:
            try:
                command = raw_input()
            except:
                raise ValueError
            channel.send(command)

class Client(object):
    def __init__(self, host, port):
        self.host = host
        self.port = port
        self.user_input = UserInput()
        self.user_input.setDaemon = True
        
        stackless.tasklet(self.run)()
    
    def run(self):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.socket.connect((self.host, self.port))
        
        stackless.tasklet(self.send_command)()
        stackless.tasklet(self.receive_messages)()
        self.user_input.start()
    
    def send_command(self):
        while True:
            try:
                command = channel.receive()
                self.socket.send(command)
                if command == 'logout':
                    self.socket.close()
            except:
                self.socket.close()
                print 'Connection lost... exiting.'
                sys.exit(0)
            stackless.schedule()
        
    def receive_messages(self):
        while True:
            try:
                message = self.socket.recv(1024)
                print message
            except:
                self.socket.close()
                print 'Connection lost... exiting.'
                sys.exit(0)
            stackless.schedule()
    
c = Client('127.0.0.1', 5555)

stackless.run()