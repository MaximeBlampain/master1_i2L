{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module MathApi where

import Servant 

type Math42 = "quarante-deux" :> Get '[JSON] Int

-- TODO MathMul2

-- TODO MathAdd

