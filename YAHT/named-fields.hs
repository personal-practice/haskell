data Configuration =
    Configuration String  -- user name
    String -- local host
    String -- remote host
    Bool -- is guest?
    Bool -- is super user?
    String -- current directory
    String -- home directory
    Integer -- time connected
    deriving (Eq, Show)

data Configuration =
    Configuration {
            username :: String,
            localhost :: String,
            remotehost :: String,
            isguest :: Bool,
            issuperuser :: Bool,
            currentdir :: String,
            homedir :: String,
            timeconnected :: Integer
    }


changeDir :: Configuration -> String -> Configuration
changeDir cfg newDir =
    if directoryExists newDir -- make sure the directory exists
        then -- change our current directory
            cfg{currentdir = newDir}
        else
            error "directory does not exist"

postWorkingDir :: Configuration -> String
postWorkingDir cfg = currentdir cfg -- retrieve our current directory

initCFG =
    Configuration "nobody" "nowhere" "nowhere"
                            False False "/" "/" 0
initCFG’ =
    Configuration {
        username="nobody",
        localhost="nowhere",
        remotehost="nowhere",
        isguest=False,
        issuperuser=False,
        currentdir="/",
        homedir="/",
        timeconnected=0
    }
