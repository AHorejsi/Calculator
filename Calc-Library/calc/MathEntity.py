class MathEntity:
    def __add__(self, other):
        from calc._AdditionMediator import _do_add
        return _do_add(self, other)

    def __radd__(self, other):
        from calc._AdditionMediator import _do_add
        return _do_add(other, self)

    def __sub__(self, other):
        from calc._SubtractionMediator import _do_subtract
        return _do_subtract(self, other)

    def __rsub__(self, other):
        from calc._SubtractionMediator import _do_subtract
        return _do_subtract(other, self)

    def __mul__(self, other):
        from calc._MultiplicationMediator import _do_multiply
        return _do_multiply(self, other)

    def __rmul__(self, other):
        from calc._MultiplicationMediator import _do_multiply
        return _do_multiply(other, self)

    def __truediv__(self, other):
        from calc._DivisionMediator import _do_divide
        return _do_divide(self, other)

    def __rtruediv__(self, other):
        from calc._DivisionMediator import _do_divide
        return _do_divide(other, self)

    def __pow__(self, power, modulo=None):
        from calc._ExponentiationMediator import _do_exponent
        return _do_exponent(self, power)

    def __rpow__(self, other):
        from calc._ExponentiationMediator import _do_exponent
        return _do_exponent(other, self)

    def __neg__(self):
        return self * -1

    def __eq__(self, other):
        from calc._EqualityMediator import _do_equals
        return _do_equals(self, other)

    def __ne__(self, other):
        return not (self == other)

    def __repr__(self):
        return str(self)
