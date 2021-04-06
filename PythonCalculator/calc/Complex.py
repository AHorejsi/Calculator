from math import sqrt, atan2, pi, nan
from calc.MathEntity import MathEntity

class Complex(MathEntity):
    def __init__(self, real, imag0):
        self.__real = real
        self.__imag0 = imag0

    @property
    def real(self):
        return self.__real

    @property
    def imag0(self):
        return self.__imag0

    def __abs__(self):
        return sqrt(self.__real * self.__real + self.__imag0 * self.__imag0)

    def conjugate(self):
        return Complex(self.__real, -self.__imag0)

    def inverse(self):
        return 1.0 / self

    def normalize(self):
        return self / abs(self)

    def arg(self):
        from calc._EqualityMediator import _EqualityMediator

        if _EqualityMediator.instance().equals(0.0, self.__real):
            if self.__imag0 > 0:
                return pi / 2
            elif self.__imag0 < 0:
                return -pi / 2
            else:
                return nan
        else:
            return atan2(self.__imag0, self.__real)

    def __repr__(self):
        return "(" + str(self.__real) + "," + str(self.__imag0) + ")"
