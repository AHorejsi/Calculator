from calc.Complex import Complex
from calc.Quaternion import Quaternion
from calc.NumberList import NumberList
from calc.Vector import Vector
from calc.Matrix import Matrix

class _EqualityMediator:
    __instance = None

    def __init__(self):
        raise RuntimeError("Use _EqualityMediator.instance()")

    @classmethod
    def instance(cls):
        if cls.__instance is None:
            cls.__instance = cls.__new__(cls)
            cls.__instance.__equalityOperations = {
                (float, float): cls.__instance.__real_eq_real,
                (float, Complex): cls.__instance.__real_eq_complex,
                (float, Quaternion): cls.__instance.__real_eq_quaternion,
                (Complex, float): cls.__instance.__complex_eq_real,
                (Complex, Complex): cls.__instance.__complex_eq_complex,
                (Complex, Quaternion): cls.__instance.__complex_eq_quaternion,
                (Quaternion, float): cls.__instance.__quaternion_eq_real,
                (Quaternion, Complex): cls.__instance.__quaternion_eq_complex,
                (Quaternion, Quaternion): cls.__instance.__quaternion_eq_quaternion,
                (NumberList, NumberList): cls.__instance.__number_list_eq_number_list,
                (Vector, Vector): cls.__instance.__vector_eq_vector,
                (Matrix, Matrix): cls.__instance.__matrix_eq_matrix
            }
            cls.__instance.__range = 0.001

        return cls.__instance

    def set_range(self, newRange):
        self.__range = newRange

    def equals(self, left, right):
        types = (type(left), type(right))
        impl = self.__equalityOperations.get(types)

        if impl is not None:
            return impl(left, right)
        else:
            return False

    def __real_eq_real(self, left, right):
        return left - right + self.__range <= self.__range + self.__range

    def __real_eq_complex(self, left, right):
        return self.__complex_eq_real(right, left)

    def __complex_eq_real(self, left, right):
        return self.__complex_eq_complex(left, Complex(right, 0.0))

    def __real_eq_quaternion(self, left, right):
        return self.__quaternion_eq_real(right, left)

    def __quaternion_eq_real(self, left, right):
        return self.__quaternion_eq_complex(left, Complex(right, 0.0))

    def __complex_eq_complex(self, left, right):
        return self.__complex_eq_quaternion(left, Quaternion(right.real, right.imag0, 0.0, 0.0))

    def __complex_eq_quaternion(self, left, right):
        return self.__quaternion_eq_complex(right, left)

    def __quaternion_eq_complex(self, left, right):
        return self.__quaternion_eq_quaternion(left, Quaternion(right.real, right.imag0, 0.0, 0.0))

    def __quaternion_eq_quaternion(self, left, right):
        realDiff = left.real - right.real
        imag0Diff = left.imag0 - right.imag0
        imag1Diff = left.imag1 - right.imag1
        imag2Diff = left.imag2 - right.imag2

        return realDiff + self.__range <= self.__range + self.__range and imag0Diff + self.__range <= self.__range + self.__range and \
               imag1Diff + self.__range <= self.__range + self.__range and imag2Diff + self.__range <= self.__range + self.__range


    def __number_list_eq_number_list(self, left, right):
        if len(left) != len(right):
            return False

        for (leftValue, rightValue) in zip(left, right):
            if not self.equals(leftValue, rightValue):
                return False

        return True


    def __vector_eq_vector(self, left, right):
        if len(left) != len(right):
            return False

        for (leftValue, rightValue) in zip(left, right):
            if not self.equals(leftValue, rightValue):
                return False

        return True


    def __matrix_eq_matrix(self, left, right):
        if left.rows != right.rows or left.cols != right.cols:
            return False

        for (leftValue, rightValue) in zip(left, right):
            if not self.equals(leftValue, rightValue):
                return False

        return True