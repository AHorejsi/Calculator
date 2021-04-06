from re import fullmatch, split
from itertools import chain
from calc import Complex, Quaternion, NumberList, Vector, Matrix

def parse_value(string):
    if fullmatch("\s*(([+-]?\d+(\.\d+)?)([eE][+-]?\d+)?)\s*", string) is not None:
        return float(string)
    elif fullmatch("\s*(([+-]?\d+(\.\d+)?)([eE][+-]?\d+)?\s*[\+|\-]\s*(\d+(\.\d+)?)([eE][+-]?\d+)?i)\s*", string) is not None:
        return _parse_complex(string)
    elif fullmatch("\s*(([+-]?\d+(\.\d+)?)([eE][+-]?\d+)?\s*[+-]\s*(\d+(\.\d+)?)([eE][+-]?\d+)?i"
                 "\s*[+-]\s*(\d+(\.\d+)?)([eE][+-]?\d+)?j\s*[+-]\s*(\d+(\.\d+)?)([eE][+-]?\d+)?k)\s*",
            string) is not None:
        return _parse_quaternion(string)
    elif fullmatch("\s*{(([+-]?\d+(\.\d+)?)([eE][+-]?\d+)?"
                 "([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?i)"
                 "([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?j)([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?k)))?)?"
                 "\s*,\s*)*"
                 "(([+-]?\d+(\.\d+)?)([eE][+-]?\d+)?"
                 "([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?i)"
                 "([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?j)([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?k)))?)?)?}\s*",
                 string) is not None:
        return _parse_number_list(string)
    elif fullmatch("\s*<(([+-]?\d+(\.\d+)?)([eE][+-]?\d+)?"
                 "([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?i)"
                 "([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?j)([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?k)))?)?"
                 "\s*,\s*)*"
                 "(([+-]?\d+(\.\d+)?)([eE][+-]?\d+)?"
                 "([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?i)"
                 "([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?j)([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?k)))?)?)?>\s*",
                 string) is not None:
        return _parse_vector(string)
    elif fullmatch("\s*(\["
                 "(\["
                 "(([+-]?\d+(\.\d+)?)([eE][+-]?\d+)?"
                 "([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?i)"
                 "([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?j)([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?k)))?)?"
                 "\s*,\s*)*"
                 "(([+-]?\d+(\.\d+)?)([eE][+-]?\d+)?"
                 "([+-]((\d+(\.\d+)?)([eE][\+|\-]?\d+)?i)"
                 "([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?j)([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?k)))?)?)?"
                 "\]\s*,\s*)*"
                 "(\["
                 "(([+-]?\d+(\.\d+)?)([eE][+-]?\d+)?"
                 "([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?i)"
                 "([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?j)([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?k)))?)?"
                 "\s*,\s*)*"
                 "(([+-]?\d+(\.\d+)?)([eE][+-]?\d+)?"
                 "([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?i)"
                 "([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?j)([+-]((\d+(\.\d+)?)([eE][+-]?\d+)?k)))?)?)?"
                 "\])?\])\s*", string) is not None:
        return _parse_matrix(string)
    else:
        raise ValueError("String does not represent a valid mathematical entity")

def _parse_complex(string):
    string = string.replace(" ", "")
    indexOfFirstMinus = string.find("-")

    if -1 == indexOfFirstMinus:
        pieces = split("[+i]", string)

        if string.startswith("+"):
            return Complex(float(pieces[1]), float(pieces[2]))
        else:
            return Complex(float(pieces[0]), float(pieces[1]))
    elif 0 == indexOfFirstMinus:
        indexOfSecondMinus = string.find("-", 1)

        if -1 == indexOfSecondMinus:
            pieces = split("[+i]", string)

            return Complex(float(pieces[0]), pieces[1])
        else:
            pieces = split("[-i]", string)

            return Complex(-float(pieces[1]), -float(pieces[2]))
    else:
        pieces = split("[-i]", string)

        return Complex(float(pieces[0]), -float(pieces[1]))

def _parse_quaternion(string):
    pieces = split("[ijk]", string.replace(" ", ""))
    complexPart = _parse_complex(pieces[0] + "i")
    jPart = float(pieces[1])
    kPart = float(pieces[2])

    return Quaternion(complexPart.real, complexPart.imag0, jPart, kPart)

def _parse_number_list(string):
    nums = split("\s*,\s*", string[1: len(string) - 1])
    new = []

    for numStr in nums:
        new.append(parse_value(numStr))

    return NumberList(new)

def _parse_vector(string):
    nums = split("\s*,\s*", string[1: len(string) - 1])
    new = []

    for numStr in nums:
        new.append(parse_value(numStr))

    return Vector(new)

def _parse_matrix(string):
    rows = split("\[\[|\]\]|\]\s*,\s*\[", string)
    new = []

    for row in rows:
        newRow = []
        nums = split("\s*,\s*", row)

        for num in nums:
            newRow.append(parse_value(num))

        new.append(newRow)

    return Matrix(list(chain.from_iterable(new)), len(new), len(new[0]))


def stringify(value):
    typeOfValue = type(value)

    if float is typeOfValue:
        return str(value)
    elif Complex is typeOfValue:
        pass
    elif Quaternion is typeOfValue:
        pass
    elif NumberList is typeOfValue:
        pass
    elif Vector is typeOfValue:
        pass
    elif Matrix is typeOfValue:
        pass
    else:
        raise TypeError("Value is of an invalid type")

def parse_expression(string):
    pass
