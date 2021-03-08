class MathEntity:
    def __add__(self, other):
        from calc._AdditionMediator import do_add
        return do_add(self, other)

    def __iadd__(self, other):
        return self + other

    def __radd__(self, other):
        from calc._AdditionMediator import do_add
        return do_add(other, self)

    def __sub__(self, other):
        from calc._SubtractionMediator import do_subtract
        return do_subtract(self, other)

    def __isub__(self, other):
        return self - other

    def __rsub__(self, other):
        from calc._SubtractionMediator import do_subtract
        return do_subtract(other, self)

    def __mul__(self, other):
        from calc._MultiplicationMediator import do_multiply
        return do_multiply(self, other)

    def __imul__(self, other):
        return self * other

    def __rmul__(self, other):
        from calc._MultiplicationMediator import do_multiply
        return do_multiply(other, self)

    def __truediv__(self, other):
        from calc._DivisionMediator import do_divide
        return do_divide(self, other)

    def __itruediv__(self, other):
        return self / other

    def __rtruediv__(self, other):
        from calc._DivisionMediator import do_divide
        return do_divide(other, self)

    def __pow__(self, other, modulo=None):
        from calc._ExponentiationMediator import do_exponent
        return do_exponent(self, other)

    def __ipow__(self, other):
        return self ** other

    def __rpow__(self, other):
        from calc._ExponentiationMediator import do_exponent
        return do_exponent(other, self)

    def __neg__(self):
        return self * -1
