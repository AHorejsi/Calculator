from math import nan
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

    def conjugate(self):
        new = []

        for value in self:
            conjugate = getattr(value, "conjugate", None)

            if conjugate is not None:
                new.append(conjugate())
            else:
                new.append(value)

        return Matrix(new, self.__rows, self.__cols)

    def determinant(self):
        if not self.__rows == self.__cols:
            return nan

        if 1 == self.__rows:
            return self.__table[0]

        return Matrix.__determinant(self.__table, self.__rows)

    @staticmethod
    def __determinant(table, size):
        if 2 == size:
            return table[0] * table[3] - table[2] * table[1]
        else:
            det = 0.0

            # Move through the first row of this matrix
            # Ignore values in the current column
            for column in range(size):
                subtable = []

                for rowIndex in range(1, size):  # Start with 1 to ignore first row
                    for columnIndex in range(size):

                        # Skip the value that is in the column being ignored
                        if columnIndex != column:
                            subtable.append(table[rowIndex * size + columnIndex])

                det += ((-1.0) ** column) * table[column] * Matrix.__determinant(subtable, size - 1)

            return det

    @staticmethod
    def identity(size):
        new = []

        for i in range(size):
            for j in range(size):
                if i == j:
                    new.append(1.0)
                else:
                    new.append(0.0)

        return Matrix(new, size, size)


    def transpose(self):
        new = []

        for colIndex in range(self.__cols):
            for rowIndex in range(self.__rows):
                new.append(self[(rowIndex, colIndex)])

        return Matrix(new, self.__cols, self.__rows)

    def __iter__(self):
        return iter(self.__table)

    def __repr__(self):
        strRep = "|"
        col = 1

        for value in self:
            strRep += repr(value)

            if 0 == col % self.__cols:
                strRep += "|"
            else:
                strRep += ","

            col += 1

        return strRep

