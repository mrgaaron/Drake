import stackless
from organ import Heart

class Body(object):
    def __init__(self, actor, body_attributes):
        for k in body_attributes:
            setattr(self, k, body_attributes[k])            
        
        self.actor = actor
        self._init_organs()
        
    def _init_heart(self):
        self.heart = Heart(self.heart['rate'], self.heart['stroke_volume'])
        
    def _init_organs(self):
        self._init_heart()
    
    def damage(self, area, damage_type, severity):
        self.hp -= 4
        if self.hp <= 0:
            self.die()
    
    def die(self):
        self.actor.send_message(self.actor, 'You have just died!')
        self.actor.set_room('Test/Death/Death Room')
        stackless.schedule()
        self.actor.room.broadcast(
            '%s falls to the ground, dead!' % self.actor.action_description,
            self.actor)