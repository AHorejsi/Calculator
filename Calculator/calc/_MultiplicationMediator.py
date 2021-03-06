from math import nan
from itertools import zip_longest
from calc.Complex import Complex
from calc.Quaternion import Quaternion
from calc.Vector import Vector
from calc.NumberList import NumberList
from calc.Matrix import Matrix

def _vector_mult_matrix(left, right):
    if len(left) != right.cols:
        return nan

    new = []

    for colIndex in range(right.cols):
        value = 0.0

        for rowIndex in range(right.rows):
            value += right[(rowIndex, colIndex)] * left[rowIndex]

        new.append(value)

    return Matrix(new, 1, right.cols)

def _matrix_mult_vector(left, right):
    if left.rows != len(right):
        return nan

    new = []

    for rowIndex in range(left.rows):
        value = 0.0

        for colIndex in range(left.cols):
            value += left[(rowIndex, colIndex)] * right[colIndex]

        new.append(value)

    return Matrix(new, left.rowLength, 1)

def _matrix_mult_matrix(left, right):
    if left.cols != right.rows:
        return nan

    new = []

    for rowIndex in range(left.rows):
        for colIndex in range(right.columns):
            value = 0.0

            for index in range(left.rows):
                value += left[(rowIndex, index)] * right[(index, colIndex)]

            new.append(value)

    return Matrix(new, left.rows, right.columns)

_MULTIPLICATION_OPERATIONS = {
    (float, Complex): lambda left, right: Complex(left * right.real, left * right.imag0),
    (float, Quaternion): lambda left, right: Quaternion(left * right.real, left * right.imag0,
                                                        left * right.imag0, left * right.imag2),
    (float, Vector): lambda left, right: Vector([left * value for value in right]),
    (float, NumberList): lambda left, right: NumberList([left * value for value in right]),
    (float, Matrix): lambda left, right: Matrix([left * value for value in right], right.rows, right.cols),
    (Complex, float): lambda left, right: Complex(left.real * right, left.imag0 * right),
    (Complex, Complex): lambda left, right: Complex(left.real * right.real - left.imag0 * right.imag0,
                                                    left.real  * right.imag0 + left.imag0 * right.real),
    (Complex, Quaternion): lambda left, right: Quaternion(left.real * right.real - left.imag0 * right.imag0,
                                                          left.real * right.imag0 + right.real * left.imag0,
                                                          left.real * right.imag1 - left.imag0 * right.imag2,
                                                          left.real * right.imag2 + left.imag0 * right.imag1),
    (Complex, Vector): lambda left, right: Vector([left * value for value in right]),
    (Complex, NumberList): lambda left, right: NumberList([left * value for value in right]),
    (Complex, Matrix): lambda left, right: Matrix([left * value for value in right], right.rows, right.cols),
    (Quaternion, float): lambda left, right: Quaternion(left.real * right, left.imag0 * right,
                                                        left.imag1 * right, left.imag2 * right),
    (Quaternion, Complex): lambda left, right: Quaternion(left.real * right.real - left.imag0 * right.imag0,
                                                          left.real * right.imag0 + left.imag0 * right.real,
                                                          left.imag1 * right.real + left.imag2 * right.imag,
                                                          -left.imag1 * right.imag0 + left.imag2 * right.real),
    (Quaternion, Quaternion): lambda left, right: Quaternion(left.real * right.real - left.imag0 * right.imag0 -
                                                             left.imag1 * right.imag1 - left.imag2 * right.imag2,
                                                             left.real * right.imag0 + left.imag0 * right.real -
                                                             left.imag1 * right.imag2 + left.imag2 * right.imag1,
                                                             left.real * right.imag1 + left.imag0 * right.imag2 +
                                                             left.imag1 * right.real - left.imag2 * right.imag0,
                                                             left.real * right.imag2 - left.imag0 * right.imag1 +
                                                             left.imag1 * right.imag0 + left.imag2 * right.real),
    (Quaternion, Vector): lambda left, right: Vector([left * value for value in right]),
    (Quaternion, NumberList): lambda left, right: NumberList([left * value for value in right]),
    (Quaternion, Matrix): lambda left, right: Matrix([left * value for value in right], right.rows, right.cols),
    (Vector, float): lambda left, right: Vector([value * right for value in left]),
    (Vector, Complex): lambda left, right: Vector([value * right for value in left]),
    (Vector, Quaternion): lambda left, right: Vector([value * right for value in left]),
    (Vector, Matrix): _vector_mult_matrix,
    (NumberList, float): lambda left, right: NumberList([value * right for value in left]),
    (NumberList, Complex): lambda left, right: NumberList([value * right for value in left]),
    (NumberList, Quaternion): lambda left, right: NumberList([value * right for value in left]),
    (NumberList, NumberList): lambda left, right: NumberList([leftValue * rightValue
                                                              for (leftValue, rightValue)
                                                              in zip_longest(left, right, 0)]),
    (Matrix, float): lambda left, right: Matrix([value * right for value in left], left.rows, left.cols),
    (Matrix, Complex): lambda left, right: Matrix([value * right for value in left], left.rows, left.cols),
    (Matrix, Quaternion): lambda left, right: Matrix([value * right for value in left], left.rows, left.cols),
    (Matrix, Vector): _matrix_mult_vector,
    (Matrix, Matrix): _matrix_mult_matrix
}

def _do_multiply(left, right):
    types = (type(left), type(right))
    impl = _MULTIPLICATION_OPERATIONS.get(types)

    if impl is not None:
        return impl(left, right)
    else:
        return nan