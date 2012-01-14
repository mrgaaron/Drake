import sys

sys.path.append('C:\Drake\src')

from drake.subsystems.health.limb import Limb
from drake.subsystems.health.body import Body

CHEST_REGION = None

BASIC_BODY_ATTRIBUTES = {
    'hp': 12,
    'blood': 8 #pints   
}

def create_body(actor):
    return Body(actor, BASIC_BODY_ATTRIBUTES)