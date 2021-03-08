from math import nan
from itertools import zip_longest
from calc.Complex import Complex
from calc.Quaternion import Quaternion
from calc.Vector import Vector
from calc.NumberList import NumberList
from calc.Matrix import Matrix

def _real_div_complex(left, right):
    return _complex_div_complex(Complex(left, 0), right)

def _complex_div_complex(left, right):
    conjugateOfRight = right.conjugate()
    numerator = left * conjugateOfRight
    denominator = right * conjugateOfRight

    return numerator / denominator.real

def _real_div_quaternion(left, right):
    denominator = right.real * right.real + right.imag0 * right.imag0 + \
                  right.imag1 * right.imag1 + right.imag2 * right.imag2

    Quaternion((left * right.real) / denominator,
               (-left * right.imag0) / denominator,
               (-left * right.imag1) / denominator,
               (-left * right.imag2) / denominator)

def _complex_div_quaternion(left, right):
    denominator = abs(right)

    realResult = left.real * right.real + left.imag0 * right.imag0
    imag0Result = left.imag0 * right.real - left.real * right.imag0
    imag1Result = -left.real * right.imag1 - left.imag0 * right.imag2
    imag2Result = left.imag0 * right.imag1 - left.real * right.imag2

    return Quaternion(realResult / denominator,
                      imag0Result / denominator,
                      imag1Result / denominator,
                      imag2Result / denominator)

def _quaternion_div_complex(left, right):
    conjugateOfRight = right.conjugate()
    numerator = left * conjugateOfRight
    denominator = right * conjugateOfRight

    return numerator / denominator.real

def _quaternion_div_quaternion(left, right):
    denominator = abs(right)

    realOfResult = left.real * right.real + left.imag0 * right.imag0 + \
                   left.imag1 * right.imag1 + left.imag2 * right.imag2
    imagOfResult = left.real * right.imag0 - left.imag0 * right.real - \
                   left.imag1 * right.imag2 + left.imag2 * right.imag1
    imag1OfResult = left.real * right.imag1 + left.imag0 * right.imag2 - \
                    left.imag1 * right.real - left.imag2 * right.imag0
    imag2OfResult = left.real * right.imag2 - left.imag0 * right.imag1 + \
                    left.imag1 * right.imag0 - left.imag2 * right.real

    return Quaternion(realOfResult / denominator,
                      imagOfResult / denominator,
                      imag1OfResult / denominator,
                      imag2OfResult / denominator)

_DIVISION_MEDIATOR = {
    (float, Complex): _real_div_complex,
    (float, Quaternion): _real_div_quaternion,
    (float, NumberList): lambda left, right: NumberList([left / value for value in right]),
    (Complex, float): lambda left, right: Complex(left.real / right, left.imag0 / right),
    (Complex, Complex): _complex_div_complex,
    (Complex, Quaternion): _complex_div_quaternion,
    (Complex, NumberList): lambda left, right: NumberList([left / value for value in right]),
    (Quaternion, float): lambda left, right: Quaternion(left.real / right, left.imag0 / right,
                                                        left.imag1 / right, left.imag2 / right),
    (Quaternion, Complex): _quaternion_div_complex,
    (Quaternion, Quaternion): _quaternion_div_quaternion,
    (Quaternion, NumberList): lambda left, right: NumberList([left / value for value in right]),
    (Vector, float): lambda left, right: Vector([value / right for value in left]),
    (Vector, Complex): lambda left, right: Vector([value / right for value in left]),
    (Vector, Quaternion): lambda left, right: Vector([value / right for value in left]),
    (NumberList, float): lambda left, right: NumberList([value / right for value in left]),
    (NumberList, Complex): lambda left, right: NumberList([value / right for value in left]),
    (NumberList, Quaternion): lambda left, right: NumberList([value / right for value in left]),
    (NumberList, NumberList): lambda left, right: NumberList([leftValue / rightValue
                                                              for (leftValue, rightValue)
                                                              in zip_longest(left, right, 0.0)]),
    (Matrix, float): lambda left, right: Matrix([value / right for value in left], left.rows, left.cols),
    (Matrix, Complex): lambda left, right: Matrix([value / right for value in left], left.rows, left.cols),
    (Matrix, Quaternion): lambda left, right: Matrix([value / right for value in left], left.rows, left.cols),
    (Matrix, Matrix): lambda left, right: left * right.inverse()
}

def do_divide(left, right):
    types = (type(left), type(right))
    impl = _DIVISION_MEDIATOR.get(types)

    if impl is not None:
        return impl(left, right)
    else:
        return nan
