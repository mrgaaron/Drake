class Heart(object):
    def __init__(self, rate, stroke_volume):
        #rate should be in beats per minute
        self.heart_rate = float(rate)
        #stroke volume is # of millileters pumped per beat
        self.stroke_volume = stroke_volume        
        
    def change_heart_rate(self, rate):
        self.heart_rate = float(rate)
        
    def get_cardiac_output(self):
        return self.heart_rate * self.stroke_volume