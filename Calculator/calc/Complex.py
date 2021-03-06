from math import sqrt
from calc.MathEntity import MathEntity

class Complex(MathEntity):
    def __init__(self, real, imag0):
        self.real = real
        self.imag0 = imag0

    def __abs__(self):
        return sqrt(self.real * self.real + self.imag0 * self.imag0)

    def conjugate(self):
        return Complex(self.real, -self.imag0)

    def inverse(self):
        return 1.0 / self

    def normalize(self):
        return self / abs(self)
