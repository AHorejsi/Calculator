from math import e, pi
from calc import imag0, imag1, imag2
from unicodedata import lookup
from json import load, dump
from parse.Parse import parse_value, stringify

class AssignmentMap:
    def __init__(self):
        self.__constMap = {
            "e": e,
            lookup("GREEK SMALL LETTER PI"): pi,
            "i": imag0,
            "j": imag1,
            "k": imag2
        }
        self.__varFile = open("../config/vars.json", "r+b")
        self.__varMap = self.__parse_var_file()

    def __parse_var_file(self):
        stringMap = load(self.__varFile)
        varMap = {}

        for (name, value) in stringMap.items():
            varMap[name] = parse_value(value)

        return varMap

    def __getitem__(self, name):
        assignment = self.__constMap.get(name)

        if assignment is not None:
            return assignment

        assignment = self.__varMap.get(name)

        if assignment is not None:
            return assignment

        return None

    def __setitem__(self, name, value):
        if name in self.__constMap:
            raise NameError("Cannot name a variable the same as a constant")

        self.__varMap[name] = value

    def close(self):
        self.__varFile.truncate(0)
        stringMap = {}

        for (name, value) in self.__varMap.items():
            stringMap[name] = stringify(value)

        dump(stringMap, self.__varFile)

        self.__varFile.close()
