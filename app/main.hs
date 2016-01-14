import Prelude              (IO)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseExtra)
import Application          (makeApplication)

main :: IO ()
main = defaultMain (fromArgs parseExtra) makeApplication
--main = defaultMain (fromArgs parseExtra) (gzip (def {gzipFiles = GzipCompress) makeApplication)
