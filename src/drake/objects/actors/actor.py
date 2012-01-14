import stackless
import sys, time, threading

sys.path.append('C:\\drake\\src')
from drake.command.registry import CommandRegistry

def calculate_round_time(round_time):
    if not round_time:
        return 0
    
    delay_began = round_time[0]
    total_delay = round_time[1]
    now = time.time()
    delay_elapsed = now - delay_began
    
    if delay_elapsed > total_delay:
        return 0
    return total_delay - delay_elapsed

class Actor(object):
    def __init__(self, actor_id, room):
        self.id = actor_id
        self.room = room
        self.last_message = None   
        self.round_time = 0
        self.command_channel = stackless.channel()
        self.queued_commands = 0
        
    def __hash__(self):
        return hash(self.id)
    
    def __eq__(self, other):
        return self.id == other.id
    
    def apply_round_time(self, round_time):
        self.round_time = (time.time(), round_time)
        
    def _execute(self, command):
        self.round_time = 0
        stackless.tasklet(CommandRegistry.dispatch)(command, self)
        self.queued_commands = 0
        
    def execute_command(self, command):
        delay = calculate_round_time(self.round_time)
        if delay:
            #the reason for this is the %d directive rounds floating point
            #numbers and it will report to the player they have 0 seconds
            #of delay left when it is below 0.5, which would be frustrating
            if delay < 1:
                to_report = 1
            else:
                to_report = delay
            self.send_message(self,
                'You must wait %d seconds before acting again.' % to_report)
            stackless.schedule()
        else:
            self._execute(command)
            self.round_time = 0
    
    def respond(self, from_actor, msg):
        pass
    
    def send_message(self, from_actor, msg):
        self.last_message = msg
        stackless.tasklet(self.respond)(from_actor, msg)
    
    def set_room(self, room_id):
        new_room = self.room.world.query_room(room_id)
        self.room.remove_actor(self)
        self.room = new_room
        self.room.add_actor(self)