module PPLiteral where

data PPLiteral = 
    PPString 
    { ppstrContents :: String
    , ppstrType :: StringType
    , ppstrRaw :: Bool }
    | PPDouble Double
    | PPInt Integer
    | PPChar Char
    deriving (Show, Eq)

data StringType = ORDINARY | LONG | UTF8 | UTF16 | UTF32 deriving (Show, Eq)

withStringType :: PPLiteral -> StringType -> PPLiteral
withStringType (PPString contents _ raw) t = PPString {ppstrContents=contents, ppstrType=t, ppstrRaw=raw}
withStringType _ _ = error "Called withStringType on non-string PPLiteral!"

withRaw :: PPLiteral -> Bool -> PPLiteral
withRaw (PPString contents t _) r = PPString {ppstrContents=contents, ppstrType=t, ppstrRaw=r}
withRaw _ _ = error "Called withStringType on non-string PPLiteral!"