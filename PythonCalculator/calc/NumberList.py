from calc.MathEntity import MathEntity
from calc.Function import sqrt

class NumberList(MathEntity):
    def __init__(self, nums):
        self.__nums = nums

    def __len__(self):
        return len(self.__nums)

    def __getitem__(self, index):
        return self.__nums[index]

    def __setitem__(self, index, value):
        self.__nums[index] = value

    def mean(self):
        return sum(self) / float(len(self))

    def median(self):
        if 0 == len(self) % 2:
            pass
        else:
            pass

    def mode(self):
        pass

    def range(self):
        return max(self) - min(self)

    def midrange(self):
        return self.range() / 2.0

    def variance(self):
        varianceValue = 0.0
        meanValue = self.mean()

        for value in self:
            varianceValue += (value - meanValue) ** 2

        return varianceValue / float(len(self) - 1)

    def std_dev(self):
        return sqrt(self.variance())

    def __iter__(self):
        return iter(self.__nums)

    def __repr__(self):
        string = "{"

        for index in range(len(self)):
            string += repr(self[index])

            if len(self) - 1 == index:
                string += "}"
            else:
                string += ","

        return string
