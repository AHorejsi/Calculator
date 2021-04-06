from math import nan, pi, e, log as math_log, exp as math_exp, sqrt as math_sqrt, sin as math_sin, cos as math_cos, \
                sinh as math_sinh, cosh as math_cosh, asin as math_asin, acos as math_acos, atan as math_atan, \
                asinh as math_asinh, acosh as math_acosh, atanh as math_atanh
from calc.Complex import Complex
from calc.Quaternion import Quaternion
from calc.MathConstant import imag0

def _log_real(value):
    if value > 0.0:
        return math_log(value)
    elif value < 0.0:
        return Complex(math_log(-value), -pi)
    else:
        return nan

def _log_complex(value):
    argValue = value.arg()

    if argValue is nan:
        return nan
    else:
        return Complex(math_log(abs(value)), argValue)

def _log_quaternion(value):
    if 0.0 == value.imag0 and 0.0 == value.imag1 and 0.0 == value.imag2:
        if 0.0 == value.real:
            return nan
        else:
            return Quaternion(_log_real(value.real), 0.0, 0.0, 0.0)
    else:
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
    else:
        return nan

def log10(value):
    return log(value) / math_log(10.0)

def _exp_complex(value):
    return e ** value

def _exp_quaternion(value):
    vectorPart = Quaternion(0.0, value.imag0, value.imag1, value.imag2)
    vectorPartAbs = abs(vectorPart)

    return math_exp(value.real) * (math_cos(vectorPartAbs) + vectorPart.normalize() * math_sin(vectorPartAbs))

def exp(value):
    if isinstance(value, float):
        return math_exp(value)
    elif isinstance(value, Complex):
        return _exp_complex(value)
    elif isinstance(value, Quaternion):
        return _exp_quaternion(value)
    else:
        return nan

def _sqrt_complex(value):
    return value ** 0.5

def _sqrt_quaternion(value):
    return value ** 0.5

def sqrt(value):
    if isinstance(value, float):
        return math_sqrt(value)
    elif isinstance(value, Complex):
        return _sqrt_complex(value)
    elif isinstance(value, Quaternion):
        return _sqrt_quaternion(value)
    else:
        return nan

def sin(value):
    if isinstance(value, float):
        return math_sin(value)
    elif isinstance(value, Complex):
        return (exp(imag0 * value) - exp(-imag0 * value)) / Complex(0, 2)
    else:
        return nan

def cos(value):
    if isinstance(value, float):
        return math_cos(value)
    elif isinstance(value, Complex):
        return (exp(imag0 * value) + exp(-imag0 * value)) / 2
    else:
        return nan

def tan(value):
    return sin(value) / cos(value)

def sinh(value):
    if isinstance(value, float):
        return math_sinh(value)
    elif isinstance(value, Complex):
        return (exp(value) - exp(-value)) / 2
    else:
        return nan

def cosh(value):
    if isinstance(value, float):
        return math_cosh(value)
    elif isinstance(value, Complex):
        return (exp(value) + exp(-value)) / 2
    else:
        return nan

def tanh(value):
    return sinh(value) / cosh(value)

def asin(value):
    if isinstance(value, float):
        return math_asin(value)
    elif isinstance(value, Complex):
        return -imag0 * _log_complex(imag0 * value + _sqrt_complex(1 - value * value))
    else:
        return nan

def acos(value):
    if isinstance(value, float):
        return math_acos(value)
    elif isinstance(value, Complex):
        return pi / 2 + imag0 * _log_complex(imag0 * value + _sqrt_complex(1 - value * value))
    else:
        return nan

def atan(value):
    if isinstance(value, float):
        return math_atan(value)
    elif isinstance(value, Complex):
        return 0.5 * imag0 * (_log_complex(1 - imag0 * value) - _log_complex(1 + imag0 * value))
    else:
        return nan

def asinh(value):
    if isinstance(value, float):
        return math_asinh(value)
    elif isinstance(value, Complex):
        return _log_complex(value + _sqrt_complex(1 + value * value))
    else:
        return nan

def acosh(value):
    if isinstance(value, float):
        return math_acosh(value)
    elif isinstance(value, Complex):
        return _log_complex(value + _sqrt_complex(value + 1) * _sqrt_complex(value - 1))
    else:
        return nan

def atanh(value):
    if isinstance(value, float):
        return math_atanh(value)
    elif isinstance(value, Complex):
        return (_log_complex(1 + value) - _log_complex(1 - value)) / 2
    else:
        return nan
