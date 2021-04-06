from calc.Function import sqrt, acos
from calc.MathEntity import MathEntity

class Vector(MathEntity):
    def __init__(self, position):
        self.__position = position

    def __len__(self):
        return len(self.__position)

    def __getitem__(self, index):
        return self.__position[index]

    def __abs__(self):
        result = 0.0

        for value in self.__position:
            result += value * value

        return sqrt(result)

    def dot(self, other):
        if len(self) != len(other):
            raise ArithmeticError("Vectors must be of equal dimensions to have a dot product")

        dotProduct = 0.0

        for (leftValue, rightValue) in zip(self, other):
            dotProduct += leftValue * rightValue

        return dotProduct

    def cross(self, other):
        if 3 == len(self) and len(self) == len(other):
            return None
        elif 7 == len(self) and len(self) == len(other):
            return None
        else:
            raise ArithmeticError("Both vectors must either have exactly 3 or 7 dimensions to have a cross product")

    def normalize(self):
        return self / abs(self)

    def angle(self, other):
        return acos((self.dot(other)) / (abs(self) * abs(other)))

    def __iter__(self):
        return iter(self.__position)

    def __repr__(self):
        string = "<"

        for index in range(len(self)):
            string += repr(self[index])

            if len(self) - 1 == index:
                string += ">"
            else:
                string += ","

        return string
