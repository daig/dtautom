module Parse where
import Backend
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text
import Data.Void
import Data.Char

parseEnvFile :: FilePath -> IO Universe
parseEnvFile = error "TODO"

type P = Parsec Void Text

comment :: P ()
comment = L.skipLineComment "//"

lspace = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: P a -> P a
lexeme = L.lexeme lspace

symbol :: Text -> P Text
symbol = L.symbol lspace

num :: P Int
num = lexeme L.decimal

size :: P Number
size = symbol "size" >> num

insertionPoints :: P [Number]
insertionPoints = symbol "insertion_points" >> many num

cell :: P (Int,Cell)
cell = (,) <$> num <*> (Cell <$> opcodeN <*> bool <*> bool)

bool :: P Bool
bool = (False <$ symbol "F") <|> (True <$ symbol "T")

opcodeN :: P Number
opcodeN = (succ . fromEnum <$> opcode) <|> num

opcode :: P OpCode
opcode = read . unpack <$> lexeme (takeWhile1P Nothing isUpper)
