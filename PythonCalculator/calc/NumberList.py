from _testcapi import INT_MAX
from copy import copy
from calc.MathEntity import MathEntity

class NumberList(MathEntity):
    def __init__(self, nums):
        self.__nums = nums

    def __len__(self):
        return len(self.__nums)

    def __getitem__(self, index):
        return self.__nums[index]

    def mean(self):
        result = 0.0

        for value in self:
            result += value

        return result / len(self)

    def __iter__(self):
        return iter(self.__nums)

    def __repr__(self):
        string = "{"
        index = 0

        while index < len(self):
            string += repr(self[index])

            if len(self) - 1 == index:
                string += "}"
            else:
                string += ","

            index += 1

        return string
