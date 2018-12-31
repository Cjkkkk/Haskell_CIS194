-- 如何对列表中0 2 4 6 8 .。 的元素进行操作

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith ($) (cycle [(*2), id])
--doubleEveryOther = zipWith (*) (cycle [1, 2])