import Text.Read (readMaybe)

safeSqrt::Double -> Maybe Double
safeSqrt x = 
    if x > 0
    then sqrt x
    else Nothing


