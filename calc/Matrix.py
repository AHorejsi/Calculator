from calc.MathEntity import MathEntity

class Matrix(MathEntity):
    def __init__(self, table, rows, cols):
        self.__table = table
        self.__rows = rows
        self.__cols = cols

    @property
    def rows(self):
        return self.__rows

    @property
    def cols(self):
        return self.__cols

    def __getitem__(self, indices):
        return self.__table[indices[0] * self.__cols + indices[1]]

    def __iter__(self):
        return iter(self.__table)
