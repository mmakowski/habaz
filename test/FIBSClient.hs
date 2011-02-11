import FIBSClient

test_accountCreation = 
  do conn <- connect defaultFibsHost defaultFibsPort
     readUntil "login:" conn
     send conn "guest\n"
     readUntil "!!!" conn
     send conn "bye\n"
     return $ disconnect conn


