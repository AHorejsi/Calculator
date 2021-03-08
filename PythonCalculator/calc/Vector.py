from math import sqrt
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

    def __iter__(self):
        return iter(self.__position)

    def __repr__(self):
        string = "<"
        index = 0

        while index < len(self):
            string += repr(self[index])

            if len(self) - 1 == index:
                string += ">"
            else:
                string += ","

            index += 1

        return string
