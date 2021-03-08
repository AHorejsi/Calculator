from calc.MathEntity import MathEntity
from calc.Complex import Complex
from calc.Quaternion import Quaternion
from calc.Vector import Vector
from calc.NumberList import NumberList
from calc.Matrix import Matrix
from calc.Function import log, exp
from calc.MathConstant import imag0, imag1, imag2

if __name__ == "__main__":
    import math

    x = NumberList([1.0, 2.0, 3.0, 4.0, 5.0, 6.0])
    print(x)
    print(x.mean())
