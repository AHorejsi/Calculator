from math import nan
from itertools import zip_longest
from calc.Complex import Complex
from calc.Quaternion import Quaternion
from calc.Vector import Vector
from calc.NumberList import NumberList
from calc.Matrix import Matrix

def _vector_minus_vector(left, right):
    if len(left) != len(right):
        return nan

    new = []

    for (leftValue, rightValue) in zip(left, right):
        new.append(leftValue - rightValue)

    return Vector(new)

def _matrix_minus_matrix(left, right):
    if left.rows != right.rows or left.cols != right.cols:
        return nan

    new = []

    for (leftValue, rightValue) in zip(left, right):
        new.append(leftValue - rightValue)

    return Matrix(new, left.rows, left.cols)

_SUBTRACTION_OPERATIONS = {
    (float, Complex): lambda left, right: Complex(left - right.real, -right.imag0),
    (float, Quaternion): lambda left, right: Quaternion(left - right.real, -right.imag0, -right.imag1, -right.imag2),
    (float, NumberList): lambda left, right: NumberList([left - value for value in right]),
    (Complex, float): lambda left, right: Complex(left.real - right, left.imag0),
    (Complex, Complex): lambda left, right: Complex(left.real - right.real, left.imag0 - right.imag0),
    (Complex, Quaternion): lambda left, right: Quaternion(left.real - right.real, left.imag0 - right.imag0,
                                                          -right.imag1, -right.imag2),
    (Complex, NumberList): lambda left, right: NumberList([left - value for value in right]),
    (Quaternion, float): lambda left, right: Quaternion(left.real - right, left.imag0, left.imag1, left.imag2),
    (Quaternion, Complex): lambda left, right: Quaternion(left.real - right.real, left.imag0 - right.imag0,
                                                          left.imag1, left.imag2),
    (Quaternion, Quaternion): lambda left, right: Quaternion(left.real - right.real, left.imag0 - right.imag0,
                                                             left.imag1 - right.imag1, left.imag2 - right.imag2),
    (Quaternion, NumberList): lambda left, right: NumberList([left - value for value in right]),
    (NumberList, float): lambda left, right: NumberList([value - right for value in left]),
    (NumberList, Complex): lambda left, right: NumberList([value - right for value in left]),
    (NumberList, Quaternion): lambda left, right: NumberList([value - right for value in left]),
    (NumberList, NumberList): lambda left, right: NumberList([leftValue - rightValue
                                                              for (leftValue, rightValue)
                                                              in zip_longest(left, right, 0.0)]),
    (Vector, Vector): _vector_minus_vector,
    (Matrix, Matrix): _matrix_minus_matrix
}

def do_subtract(left, right):
    types = (type(left), type(right))
    impl = _SUBTRACTION_OPERATIONS.get(types)

    if impl is not None:
        return impl(left, right)
    else:
        return nan
