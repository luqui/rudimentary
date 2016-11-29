module Syntax where

import Control.Applicative
import Control.Monad (when)
import Data.List (intercalate)
import Data.Tuple (swap)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P

type Parser = P.Parsec String ()

tok = P.makeTokenParser $ P.LanguageDef {
        P.commentStart = "",
        P.commentEnd = "",
        P.commentLine = "",
        P.nestedComments = False,
        P.identStart = fail "no identifiers",
        P.identLetter = fail "no identifiers",
        P.opStart = fail "no operators",
        P.opLetter = fail "no operators",
        P.reservedNames = [],
        P.reservedOpNames = [],
        P.caseSensitive = True 
    }

class Pretty a where
    pretty :: a -> String


data ScaleGenus
    = Natural
    | Melodic
    | Diminished
    deriving (Eq, Ord, Read, Show, Bounded, Enum)

data Mode = Mode ScaleGenus Int  -- 1-based mode
    deriving (Eq, Ord, Show)

newtype Note = Note Int
    deriving (Eq, Ord, Show)

data Scale = Scale Note Mode
    deriving (Eq, Ord, Show)

data Degree 
    = Degree Int Int   -- degree(0-based) accidental
    | Rest
    deriving (Eq, Ord, Show)

data DegreeExp 
    = DERun [Degree]
    | DEMult DegreeExp DegreeExp
    | DEParallel DegreeExp DegreeExp
    | DEConcat DegreeExp DegreeExp
    | DEOp DegreeOperator
    deriving (Show)

data DegreeOperator
    = DOIdentity
    | DOInvert
    | DORetrograde
    deriving (Show)

data Exp = Exp Scale DegreeExp
    deriving (Show)


genusIntervals :: ScaleGenus -> [Int]
genusIntervals Natural = [2,1,2,2,2,1,2]      -- dorian, the center
genusIntervals Melodic = [2,2,1,2,1,2,2]      -- major-minor
genusIntervals Diminished = [2,1,2,1,2,1,2,1] -- 2-1 (diminished does not have a center mode!)

genusSize :: ScaleGenus -> Int
genusSize = length . genusIntervals


instance Pretty ScaleGenus where
    pretty Natural = "nat"
    pretty Melodic = "mel"
    pretty Diminished = "dim"

genusParser :: Parser ScaleGenus
genusParser = P.choice [
    Natural <$ P.symbol tok "nat",
    Melodic <$ P.symbol tok "mel",
    Diminished <$ P.symbol tok "dim" ]

instance Pretty Mode where
    pretty (Mode g n) = pretty g ++ " " ++ show n

instance Pretty Note where
    pretty = noteName

baseNoteNames :: [(String, Int)]
baseNoteNames = [("C", 0), ("D", 2), ("E", 4), ("F", 5), ("G", 7), ("A", 9), ("B", 11)]

noteName :: Note -> String
noteName (Note i) = noteMap Map.! i
    where
    noteMap = Map.fromList . map swap $ [ (name ++ "#", note+1) | (name,note) <- baseNoteNames ] ++ baseNoteNames

accidentalParser :: Parser Int
accidentalParser = P.choice [
    length <$> P.many1 (P.symbol tok "#"),
    negate . length <$> P.many1 (P.symbol tok "b"),
    return 0 ]

noteParser :: Parser Note
noteParser = do
    i <- P.choice [ i <$ (P.symbol tok (map Char.toUpper n) <|> P.symbol tok (map Char.toLower n))
                  | (n,i) <- baseNoteNames ]
    acc <- accidentalParser
    return . Note $ (i + acc) `mod` 12

instance Pretty Scale where
    pretty (Scale n m) = pretty n ++ " " ++ pretty m

scaleParser :: Parser Scale
scaleParser = do
    baseNote <- noteParser
    mode <- modeParser
    return $ Scale baseNote mode
    where
    modeParser = namedMode <|> genusMode
    namedMode = P.choice . map P.try $ [
        Mode Natural 1 <$ name "dor" "ian",
        Mode Natural 2 <$ name "phr" "ygian",
        Mode Natural 3 <$ name "lyd" "ian",
        Mode Natural 4 <$ name "mix" "olydian",
        Mode Natural 5 <$ name "aeo" "lian",
        Mode Natural 6 <$ name "loc" "rian",
        Mode Natural 7 <$ name "ion" "ian",
        Mode Natural 7 <$ name "maj" "or"
        ]
    name firstpart lastpart = P.lexeme tok (P.string firstpart <* P.option "" (P.string lastpart))
    genusMode = do
        genus <- genusParser
        mode <- fromIntegral <$> P.natural tok
        when (mode < 1 || mode > genusSize genus) $ fail "Invalid mode"
        return $ Mode genus mode

instance Pretty Degree where
    pretty (Degree n acc) = showacc ++ show (1+n)
        where
        showacc | acc < 0 = replicate (-acc) 'b'
                | acc >= 0 = replicate acc '#'


degreeParser :: Parser Degree
degreeParser = P.choice [
    flip Degree <$> accidentalParser <*> deg,
    Rest <$ P.symbol tok "~" ]
    where
    deg = do
        sign <- P.option 1 ((-1) <$ P.symbol tok "-")
        i <- (sign*) . fromIntegral <$> P.natural tok 
        when (i == 0) $ fail "degrees cannot be zero"
        return $ signum i * (abs i - 1)

degreeRunParser :: Parser [Degree]
degreeRunParser = P.choice [
    degreeParser `P.sepBy1` P.symbol tok ",",
    map (`Degree` 0) [0,1..7] <$ P.symbol tok "asc",
    map (`Degree` 0) [7,6..0] <$ P.symbol tok "desc" ]

instance Pretty DegreeExp where
    pretty (DERun ds) = intercalate "," (map pretty ds)
    pretty (DEParallel a b) = pretty a ++ " & " ++ pretty b
    pretty (DEMult a b) = pretty a ++ " " ++ pretty b
    pretty (DEConcat a b) = "(" ++ pretty a ++ " + " ++ pretty b ++ ")"
    pretty (DEOp op) = pretty op

instance Pretty DegreeOperator where
    pretty DOIdentity = "Id"
    pretty DOInvert = "Inv"
    pretty DORetrograde = "Ret"

degreeExpParser :: Parser DegreeExp
degreeExpParser = catExp
    where
    atomic = P.choice [
        P.parens tok degreeExpParser,
        DERun <$> degreeRunParser,
        DEOp <$> operator ]
    parExp = binOp (DEParallel <$ P.symbol tok "&") atomic
    opExp = binOp (pure DEMult) parExp
    catExp = binOp (DEConcat <$ P.symbol tok "+") opExp
    binOp op p = flip ($) <$> p <*> P.option id (flip <$> op <*> binOp op p)
    operator = P.choice [
        P.try (DOIdentity <$ P.symbol tok "Id"),
        P.try (DOInvert <$ P.symbol tok "Inv"),
        P.try (DORetrograde <$ P.symbol tok "Ret") ]

instance Pretty Exp where
    pretty (Exp scale de) = pretty scale ++ " " ++ pretty de

expParser :: Parser Exp
expParser = Exp <$> scaleParser <*> degreeExpParser


parse :: Parser a -> String -> Either P.ParseError a
parse parser = P.parse (parser <* P.eof) "<input>"
