import stackless

class Body(object):
    def __init__(self, actor, body_attributes):
        for k in body_attributes:
            setattr(self, k, body_attributes[k])            
        
        self.actor = actor
    
    def damage(self, area, damage_type, severity):
        self.hp -= 4
        if self.hp <= 0:
            self.die()
    
    def die(self):
        self.actor.send_message(self.actor, 'You have just died!')
        stackless.schedule()
        self.actor.room.broadcast(
            '%s falls to the ground, dead!' % self.actor.action_description,
            self.actor)
        self.actor.set_room('Test/Death/Death Room')