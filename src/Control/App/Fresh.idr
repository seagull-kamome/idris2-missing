||| An App that can generate fresh integer.
|||
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
|||
module Control.App.Fresh

import Control.App

export Fresh : Type -> List Type -> Type
Fresh tag = State tag Int

export fresh : (0 tag:_) -> (Fresh tag e => App e Int)
fresh tag = modify tag (\x => x + 1) >> get tag

export runFresh : tag -> (Fresh tag e => App e a) -> App e a
runFresh tag = new 0

-- vim: tw=80 sw=2 expandtab :
