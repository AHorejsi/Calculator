from math import nan, sin, cos, log as math_log, exp as math_exp
from itertools import zip_longest
from calc.Complex import Complex
from calc.Quaternion import Quaternion
from calc.NumberList import NumberList
from calc.Function import exp, log

def _real_pow_complex(left, right):
    return _complex_pow_complex(Complex(left, 0.0), right)

def _complex_pow_real(left, right):
    return _complex_pow_complex(left, Complex(right, 0.0))

def _complex_pow_complex(left, right):
    leftIsZero = (0.0 == left.real) and (0.0 == left.imag0)
    rightIsZero = (0.0 == right.real) and (0.0 == left.imag0)

    if leftIsZero and rightIsZero:
        return nan
    elif leftIsZero:
        return Complex(0.0, 0.0)
    elif rightIsZero:
        return Complex(1.0, 0.0)
    else:
        a = left.real * left.real + left.imag0 * left.imag0
        b = a ** (right.real / 2.0)
        c = left.arg()
        d = right.real * c + 0.5 * right.imag0 * math_log(a)

        return b * math_exp(-right.imag0 * c) * Complex(cos(d), sin(d))

def _general_exponent(left, right):
    return exp(log(left) * right)

_EXPONENTIATION_OPERATIONS = {
    (float, Complex): _real_pow_complex,
    (float, Quaternion): _general_exponent,
    (float, NumberList): lambda left, right: NumberList([left ** value for value in right]),
    (Complex, float): _complex_pow_real,
    (Complex, Complex): _complex_pow_complex,
    (Complex, Quaternion): _general_exponent,
    (Complex, NumberList): lambda left, right: NumberList([left ** value for value in right]),
    (Quaternion, float): _general_exponent,
    (Quaternion, Complex): _general_exponent,
    (Quaternion, Quaternion): _general_exponent,
    (Quaternion, NumberList): lambda left, right: NumberList([left ** value for value in right]),
    (NumberList, float): lambda left, right: NumberList([value ** right for value in left]),
    (NumberList, Complex): lambda left, right: NumberList([value ** right for value in left]),
    (NumberList, Quaternion): lambda left, right: NumberList([value ** right for value in left]),
    (NumberList, NumberList): lambda left, right: NumberList([leftValue ** rightValue
                                                              for (leftValue, rightValue)
                                                              in zip_longest(left, right, 0.0)])
}

def do_exponent(left, right):
    types = (type(left), type(right))
    impl = _EXPONENTIATION_OPERATIONS.get(types)

    if impl is not None:
        return impl(left, right)
    else:
        return nan
