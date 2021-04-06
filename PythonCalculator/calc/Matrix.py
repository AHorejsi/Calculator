from copy import copy
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

    def __setitem__(self, indices, value):
        self.__table[indices[0] *  self.__cols + indices[1]] = value

    def determinant(self):
        if self.__rows != self.__cols:
            raise ArithmeticError("Matrix must be square to have a determinant")

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

                for rowIndex in range(1, size): # Start with 1 to ignore first row
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

    @staticmethod
    def zero(rows, cols):
        new = []

        for i in range(rows):
            for j in range(cols):
                new.append(0.0)

    def inverse(self):
        if self.__rows != self.__cols:
            raise ArithmeticError("Matrix must be a square matrix to have an inverse")

        tempMatrix = Matrix(copy(self.__table), self.__rows, self.__cols)
        newMatrix = Matrix.identity(self.__rows)

        for i in range(tempMatrix.__rows):
            divisor = tempMatrix[(i, i)]

            for j in range(tempMatrix.__cols):
                tempMatrix[(i, j)] /= divisor
                newMatrix[(i, j)] /= divisor

            for j in range(tempMatrix.__rows):
                coefficient = tempMatrix[(j, i)]

                for k in range(tempMatrix.__cols):
                    tempMatrix[(j, k)] -= coefficient * tempMatrix[(i, k)]
                    newMatrix[(j, k)] -= coefficient * newMatrix[(i, k)]

        return newMatrix

    def row_add(self, rowIndex1, rowIndex2):
        if rowIndex1 != rowIndex2:
            for colIndex in range(self.__cols):
                self[(rowIndex1, colIndex)] += self[(rowIndex2, colIndex)]

    def row_multiply(self, rowIndex, value):
        for colIndex in range(self.__cols):
            self[(rowIndex, colIndex)] *= value

    def row_swap(self, rowIndex1, rowIndex2):
        if rowIndex1 != rowIndex2:
            for colIndex in range(self.__cols):
                temp = self[(rowIndex1, colIndex)]
                self[(rowIndex1, colIndex)] = self[(rowIndex2, colIndex)]
                self[(rowIndex2, colIndex)] = temp

    def col_add(self, colIndex1, colIndex2):
        if colIndex1 != colIndex2:
            for rowIndex in range(self.__rows):
                self[(rowIndex, colIndex1)] += self[(rowIndex, colIndex2)]

    def col_multiply(self, colIndex, value):
        for rowIndex in range(colIndex):
            self[(rowIndex, colIndex)] *= value

    def col_swap(self, colIndex1, colIndex2):
        if colIndex1 != colIndex2:
            for rowIndex in range(self.__rows):
                temp = self[(rowIndex, colIndex1)]
                self[(rowIndex, colIndex1)] = self[(rowIndex, colIndex2)]
                self[(rowIndex, colIndex2)] = temp

    def submatrix(self, rowIndex, colIndex):
        new = []

        for i in range(self.__rows):
            for j in range(self.__cols):
                if i != rowIndex and j != colIndex:
                    new.append(self[(i, j)])

        return Matrix(new, self.__rows - 1, self.__cols - 1)

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
