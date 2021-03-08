from copy import copy
from heapq import heapify
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

    def median(self):
        copyNums = copy(self.__nums)
        heapify(copyNums)

        if 1 == len(copyNums) % 2:
            return copyNums[len(copyNums) // 2]
        else:
            return (copyNums[len(copyNums) // 2 - 1] + copyNums[len(copyNums) // 2]) / 2

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
