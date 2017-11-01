
module Psql where

import           Prelude hiding (sum)

import           Opaleye (Column, Table(Table),
                           required, optional, (.==), (.<),
                           arrangeDeleteSql, arrangeInsertManySql,
                           arrangeUpdateSql, arrangeInsertManyReturningSql,
                           PGInt4, PGFloat8)

import           Data.Profunctor.Product (p4)
import           Data.Profunctor.Product.Default (def)
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.PGTypes as P
import qualified Opaleye.Constant as C


