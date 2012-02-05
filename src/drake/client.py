import stackless
import sys, signal, curses, traceback
import curses.textpad
from server import ssocket
ssocket.install()
import socket

import threading

channel = stackless.channel()

OUTPUT_SCREEN_HEIGHT = 22

def handle_quit(sock):
     curses.echo()
     curses.nocbreak()
     curses.endwin()
     sock.close()
     sys.exit(0)

class UserInput(threading.Thread):
    def __init__(self, screen, sock):
        self.screen = screen
        self.socket = sock
        threading.Thread.__init__(self)
        
    def run(self):
        while True:
            try:
                command = self.screen.input_textpad.edit()
                if 'quit' in command:
                    handle_quit(self.socket)
                self.screen.input_window.clear()
                self.screen.input_window.refresh()
            except Exception, e:
                print e
                raise ValueError
            channel.send(command)
            
class Screen(object):
    def __init__(self):
        self.screen = curses.initscr()
        curses.start_color()
        curses.cbreak()
        self.output_window = curses.newwin(OUTPUT_SCREEN_HEIGHT, 80, 0, 0)
        for i in xrange(1, curses.COLORS):
            curses.init_pair(i, i, curses.COLOR_BLACK)
        self.output_window.scrollok(1)
        self.output_window.idlok(1)
        self.input_window = curses.newwin(1, 80, 24, 0)
        self.input_textpad = curses.textpad.Textbox(self.input_window)
        curses.noecho()
        curses.cbreak()
        self.screen.keypad(1)
        self.screen.hline(23, 0, curses.ACS_HLINE, 80)
        self.screen.refresh()
    
    def put_server_message(self, msg):
        lines = msg.split('\n')
        for line in lines:
            if line.startswith('['):
                self.output_window.addstr(line, curses.color_pair(12))
            elif line.startswith('Exits'):
                self.output_window.addstr(line, curses.color_pair(11))
            elif line.startswith('Also here'):
                self.output_window.addstr(line, curses.color_pair(14))
            else:
                self.output_window.addstr(line)
            self.output_window.addstr('\n')
        self.output_window.refresh()

class Client(object):
    def __init__(self, host, port):
        self.host = host
        self.port = port
        self.screen = Screen()
        stackless.tasklet(self.run)()
    
    def handle_exception(self, exception):
        self.screen.screen.keypad(0)
        curses.echo()
        curses.nocbreak()
        curses.endwin()
        self.socket.close()
        sys.exit(0)
    
    def run(self):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.socket.connect((self.host, self.port))
        
        stackless.tasklet(self.send_command)()
        stackless.tasklet(self.receive_messages)()
        
        self.user_input = UserInput(self.screen, self.socket)
        self.user_input.setDaemon = True
        
        self.user_input.start()
    
    def send_command(self):
        while True:
            try:
                command = channel.receive()
                self.socket.send(command)
                if command == 'logout':
                    self.socket.close()
            except Exception, e:
                self.handle_exception(e)
            stackless.schedule()
        
    def receive_messages(self):
        while True:
            try:
                message = self.socket.recv(1024)
                self.screen.put_server_message(message)
            except Exception, e:
                self.handle_exception(e)
            stackless.schedule()
    
c = Client('127.0.0.1', 5555)

stackless.run()