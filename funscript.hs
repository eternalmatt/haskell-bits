decision :: (Integral a) => a -> String
decision skillz
    | has skillz = "49th Security Division"
    | otherwise  = "CPU Programming Union"
    where
          has x = x == 0
          
          
         
data Friend = { firstName :: String
              , lastName  :: String
              , email     :: String
              }
              
data Club = [(Friend)]
