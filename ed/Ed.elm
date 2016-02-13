module Ed where
import Array exposing (Array)
import Dict exposing (Dict)

type alias File = (String, Int, Array String)
type alias History = Array String
type alias FileSystem = Dict String File
type Command = Append 
type Mode = Editing File Command | Just Command 
type alias State = (Mode, History, FileSystem)




