module Ipe.IO where

import Control.Monad.Except
import Ipe.Types
import Ipe.Reader
import Ipe.Writer
import Control.Monad.IO.Class
import Data.Text(Text)

import Data.Vinyl.CoRec


-- type Errors es = CoRec Identity es

-- newtype ExceptE m





-- type Error = Text

-- type IpeIOM = ExceptT Error IO


-- runIpeIOM     :: IpeIOM () -> IO ()
-- runIpeIOM act = runExceptT act >>= \case
--                   Left err -> print err
--                   Right () -> pure ()


-- readIpeFile    :: FilePath -> IpeIOM (IpeFile r)
-- readIpeFile fp = undefined





-- -- runOn :: FilePath -> IO (IpeFile r -> Either Error a) -> IO a
-- -- runOn



-- -- withPage :: IpePage r -> Either Error (IpePage r) ->


-- -- withPageM         :: (IpePage r -> IO (Either Error (IpePage r)) -> IpeFile r -> IO ()
-- -- withPageM f iFile =
