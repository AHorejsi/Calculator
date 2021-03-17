from calc.Complex import Complex
from calc.Quaternion import Quaternion
from calc.NumberList import NumberList
from calc.Vector import Vector
from calc.Matrix import Matrix

def _real_eq_real(left, right, diff):
    return left - right + diff <= diff + diff

def _real_eq_complex(left, right, diff):
    return _complex_eq_real(right, left, diff)

def _complex_eq_real(left, right, diff):
    return _complex_eq_complex(left, Complex(right, 0.0), diff)

def _real_eq_quaternion(left, right, diff):
    return _quaternion_eq_real(right, left, diff)

def _quaternion_eq_real(left, right, diff):
    return _quaternion_eq_complex(left, Complex(right, 0.0), diff)

def _complex_eq_complex(left, right, diff):
    return _complex_eq_quaternion(left, Quaternion(right.real, right.imag0, 0.0, 0.0), diff)

def _complex_eq_quaternion(left, right, diff):
    return _quaternion_eq_complex(right, left, diff)

def _quaternion_eq_complex(left, right, diff):
    return _quaternion_eq_quaternion(left, Quaternion(right.real, right.imag0, 0.0, 0.0), diff)

def _quaternion_eq_quaternion(left, right, diff):
    realDiff = left.real - right.real
    imag0Diff = left.imag0 - right.imag0
    imag1Diff = left.imag1 - right.imag1
    imag2Diff = left.imag2 - right.imag2

    return realDiff + diff <= diff + diff and imag0Diff + diff <= diff + diff and \
           imag1Diff + diff <= diff + diff and imag2Diff + diff <= diff + diff

def _number_list_eq_number_list(left, right, diff):
    if len(left) != len(right):
        return False

    for (leftValue, rightValue) in zip(left, right):
        if not do_equals(leftValue, rightValue, diff):
            return False

    return True

def _vector_eq_vector(left, right, diff):
    if len(left) != len(right):
        return False

    for (leftValue, rightValue) in zip(left, right):
        if not do_equals(leftValue, rightValue, diff):
            return False

    return True

def _matrix_eq_matrix(left, right, diff):
    if left.rows != right.rows or left.cols != right.cols:
        return False

    for (leftValue, rightValue) in zip(left, right):
        if not do_equals(leftValue, rightValue, diff):
            return False

    return True

_EQUALITY_OPERATIONS = {
    (float, float, float): _real_eq_real,
    (float, Complex, float): _real_eq_complex,
    (float, Quaternion, float): _real_eq_quaternion,
    (Complex, float, float): _complex_eq_real,
    (Complex, Complex, float): _complex_eq_complex,
    (Complex, Quaternion, float): _complex_eq_quaternion,
    (Quaternion, float, float): _quaternion_eq_real,
    (Quaternion, Complex, float): _quaternion_eq_complex,
    (Quaternion, Quaternion, float): _quaternion_eq_quaternion,
    (NumberList, NumberList, float): _number_list_eq_number_list,
    (Vector, Vector, float): _vector_eq_vector,
    (Matrix, Matrix, float): _matrix_eq_matrix
}

def equals(left, right, diff):
    types = (type(left), type(right), type(diff))
    impl = _EQUALITY_OPERATIONS.get(types)

    if impl is not None:
        return impl(left, right, diff)
    else:
        return False