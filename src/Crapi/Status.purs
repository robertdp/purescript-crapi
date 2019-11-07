module Crapi.Status where

newtype Status
  = Status { code :: Int, reasonPhrase :: String }

status :: Int -> String -> Status
status code reasonPhrase = Status { code, reasonPhrase }

statusCode :: Status -> Int
statusCode (Status s) = s.code

statusOK :: Status
statusOK = status 200 "OK"

statusCreated :: Status
statusCreated = status 201 "Created"

statusNoContent :: Status
statusNoContent = status 204 "No Content"

statusNotModified :: Status
statusNotModified = status 304 "Not Modified"

statusBadRequest :: Status
statusBadRequest = status 400 "Bad Request"

statusUnauthorized :: Status
statusUnauthorized = status 401 "Unauthorized"

statusForbidden :: Status
statusForbidden = status 403 "Forbidden"

statusNotFound :: Status
statusNotFound = status 404 "Not Found"

statusConflict :: Status
statusConflict = status 409 "Conflict"

statusInternalServerError :: Status
statusInternalServerError = status 500 "Internal Server Error"

statusMaintenanceInProgress :: Status
statusMaintenanceInProgress = status 520 "Maintenance In Progress"
