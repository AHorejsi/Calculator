from math import sqrt
from calc.MathEntity import MathEntity

class Quaternion(MathEntity):
    def __init__(self, real, imag0, imag1, imag2):
        self.real = real
        self.imag0 = imag0
        self.imag1 = imag1
        self.imag2 = imag2

    def __abs__(self):
        return sqrt(self.real * self.real + self.imag0 * self.imag0 +
                    self.imag1 * self.imag1 + self.imag2 * self.imag2)

    def conjugate(self):
        return Quaternion(self.real, -self.imag0, -self.imag1, -self.imag2)

    def inverse(self):
        return self.conjugate() / (self.real * self.real + self.imag0 * self.imag0 +
                                   self.imag1 * self.imag1 + self.imag2 * self.imag2)

    def normalize(self):
        return self / abs(self)

    def __repr__(self):
        return str(self.real) + " + " + str(self.imag0) + "i + " + str(self.imag1) + "j + " + str(self.imag2) + "k"
