from math import sqrt
from calc.MathEntity import MathEntity

class Quaternion(MathEntity):
    def __init__(self, real, imag0, imag1, imag2):
        self.__real = real
        self.__imag0 = imag0
        self.__imag1 = imag1
        self.__imag2 = imag2

    @property
    def real(self):
        return self.__real

    @property
    def imag0(self):
        return self.__imag0

    @property
    def imag1(self):
        return self.__imag1

    @property
    def imag2(self):
        return self.__imag2

    def __abs__(self):
        return sqrt(self.__real * self.__real + self.__imag0 * self.__imag0 +
                    self.__imag1 * self.__imag1 + self.__imag2 * self.__imag2)

    def conjugate(self):
        return Quaternion(self.__real, -self.__imag0, -self.imag1, -self.__imag2)

    def inverse(self):
        return self.conjugate() / (self.__real * self.__real + self.__imag0 * self.__imag0 +
                                   self.__imag1 * self.__imag1 + self.__imag2 * self.__imag2)

    def normalize(self):
        return self / abs(self)

    def __repr__(self):
        return "(" + str(self.__real) + "," + str(self.__imag0) + "," + str(self.__imag1) + "," + str(self.__imag2) + ")"
