from math import nan, pi, e, log as math_log, exp as math_exp, sin as math_sin, cos as math_cos, acos as math_acos
from calc.Complex import Complex
from calc.Quaternion import Quaternion
from calc.NumberList import NumberList

def _log_real(value):
    if value > 0.0:
        return math_log(value)
    else:
        return math_log(-value) - Complex(0.0, pi)

def _log_complex(value):
    argValue = value.arg()

    if argValue is nan:
        return nan
    else:
        return math_log(abs(value)) + Complex(0.0, argValue)

def _log_quaternion(value):
    vectorPart = Quaternion(0.0, value.imag0, value.imag1, value.imag2)
    absValue = abs(value)

    return math_log(absValue) + vectorPart.normalize() * math_acos(value.real / absValue)

def log(value):
    if isinstance(value, float):
        return _log_real(value)
    elif isinstance(value, Complex):
        return _log_complex(value)
    elif isinstance(value, Quaternion):
        return _log_quaternion(value)
    elif isinstance(value, NumberList):
        return NumberList([log(val) for val in value])
    else:
        return nan

def log10(value):
    return log(value) / math_log(10.0)

def _exp_quaternion(value):
    vectorPart = Quaternion(0.0, value.imag0, value.imag1, value.imag2)
    vectorPartAbs = abs(vectorPart)

    return math_exp(value.real) * (math_cos(vectorPartAbs) + vectorPart.normalize() * math_sin(vectorPartAbs))

def exp(value):
    if isinstance(value, float):
        return math_exp(value)
    elif isinstance(value, Complex):
        return e ** value
    elif isinstance(value, Quaternion):
        return _exp_quaternion(value)
    elif isinstance(value, NumberList):
        return NumberList([exp(val) for val in value])
    else:
        return nan
