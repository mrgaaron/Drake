from drake.subsystems.health.body import Body

CHEST_REGION = None

BASIC_BODY_ATTRIBUTES = {
    'hp': 3,
    'blood': 12, #pints
    'heart': {
        'rate': 75,
        'stroke_volume': 100     
     }   
}

def create_body(actor):
    return Body(actor, BASIC_BODY_ATTRIBUTES)