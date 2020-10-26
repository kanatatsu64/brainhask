module Initial (
    Initial(..)
) where

import Prelude hiding (init)

class Initial a where
    init :: a

instance Initial Int where
    init = 0

instance Initial Char where
    init = ' '

instance Initial [a] where
    init = []

instance (Initial a) => Initial (IO a) where
    init = return init