from calc.MathEntity import MathEntity

class NumberList(MathEntity):
    def __init__(self, nums):
        self.__nums = nums

    def __len__(self):
        return len(self.__nums)

    def __getitem__(self, index):
        return self.__nums[index]

    def __iter__(self):
        return iter(self.__nums)
