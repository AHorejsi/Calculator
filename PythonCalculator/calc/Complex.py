from math import sqrt, atan2, pi, nan
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

    def arg(self):
        if 0.0 == self.real:
            if self.imag0 > 0:
                return pi / 2
            elif self.imag0 < 0:
                return -pi / 2
            else:
                return nan
        else:
            return atan2(self.imag0, self.real)

    def __repr__(self):
        return "(" + str(self.real) + "," + str(self.imag0) + ")"
