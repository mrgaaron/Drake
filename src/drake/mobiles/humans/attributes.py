from drake.subsystems.health.limb import Limb
from drake.subsystems.health.body import Body

CHEST_REGION = None

BASIC_BODY_ATTRIBUTES = {
    'hp': 12,
    'blood': 8, #pints
    'heart': {
        'rate': 75,
        'stroke_volume': 70
     }   

}

def create_body(actor):
    return Body(actor, BASIC_BODY_ATTRIBUTES)