from attributes import create_body

class Human(object):
    def __init__(self, actor):
        self.body = create_body(actor)