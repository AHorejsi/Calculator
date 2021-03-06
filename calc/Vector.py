from calc.MathEntity import MathEntity

class Vector(MathEntity):
    def __init__(self, position):
        self.__position = position

    def __len__(self):
        return len(self.__position)

    def __getitem__(self, index):
        return self.__position[index]

    def __iter__(self):
        return iter(self.__position)
