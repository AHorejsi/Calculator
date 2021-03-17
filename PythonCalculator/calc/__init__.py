from calc.MathEntity import MathEntity
from calc.Complex import Complex
from calc.Quaternion import Quaternion
from calc.Vector import Vector
from calc.NumberList import NumberList
from calc.Matrix import Matrix
from calc.Function import log, exp, sqrt, sin, cos, tan, sinh, cosh, tanh, asin, acos, atan, asinh, acosh, atanh
from calc.MathConstant import imag0, imag1, imag2

if __name__ == "__main__":
    import math
    import cmath

    a = -1.0
    b = 2.0
    c = -1.0
    d = 2.0

    x = Complex(a, b)
    y = complex(c, d)

    print(2.3 ** x)
    print(2.3 ** y)
