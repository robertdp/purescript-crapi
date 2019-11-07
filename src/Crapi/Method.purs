module Crapi.Method where

import Type.Proxy (Proxy)

class IsMethod method where
  reflectMethod :: Proxy method -> String

data GET

data POST

data PUT

data PATCH

data DELETE

data OPTIONS

instance isMethodGET :: IsMethod GET where
  reflectMethod _ = "GET"

instance isMethodPOST :: IsMethod POST where
  reflectMethod _ = "POST"

instance isMethodPUT :: IsMethod PUT where
  reflectMethod _ = "PUT"

instance isMethodPATCH :: IsMethod PATCH where
  reflectMethod _ = "PATCH"

instance isMethodDELETE :: IsMethod DELETE where
  reflectMethod _ = "DELETE"

instance isMethodOPTIONS :: IsMethod OPTIONS where
  reflectMethod _ = "OPTIONS"
