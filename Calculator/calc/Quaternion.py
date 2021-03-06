from calc.MathEntity import MathEntity

class Quaternion(MathEntity):
    def __init__(self, real, imag0, imag1, imag2):
        self.real = real
        self.imag0 = imag0
        self.imag1 = imag1
        self.imag2 = imag2
