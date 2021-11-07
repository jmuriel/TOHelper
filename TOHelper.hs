import Char
import System.Environment (getArgs)
import System.IO
import Text.Regex(subRegex,mkRegex)

--Main is the entry function. It is a part of the greater check system which decides what to do based on the command line option passed by the user. If no "real" option is given correctly, then the usage instructions are printed to standard output.
main :: IO ()
main = do i <- getArgs
          if i == [] then putStrLn usage else check1

check1 :: IO ()
check1 = do i<- getArgs
            if i!!0 == "-AlvarezToLatex" then hatl else check2

check2 :: IO ()
check2 = do i <- getArgs
            if i!!0 == "-AlvarezToSaxton" then ats else check3

check3  :: IO ()
check3 = do i <- getArgs
            if i!!0 == "-SaxtonToAlvarez" then sta else check4

check4 :: IO ()
check4 = do i <- getArgs
            if i!!0 == "-SaxtonToLatex" then stl else check5

check5 :: IO ()
check5 = do i <- getArgs
            if i!!0 == "-concordance" then conc else check6 

check6 :: IO ()
check6 = do i <- getArgs
            if i!!0 == "-about" then putStrLn about else putStrLn usage

about :: String
about = "tohelper, the Tohono O'odham helper. Jorge Muriel, University of Arizona, 4/2009.\n\nWelcome to the Tohono O'odham helper. This program does several things to assist the student in his translation of a Tohono O'odham text:\n\n1. Converts from the Alvarez-Hale writing system to the Saxton writing system.\n2. Converts from the Saxton writing system to the Alvarez-Hale writing system.\n3. Concatenates a rough concordance of the text to the end of the text.\n4. Produces a LaTeX template for the Alvarez-Hale or Saxton text.\n\nA unicode version of this program is in development. Until then, the following conventions are used to represent Alvarez-Hale characters in ASCII format:\n\nVowels:\n\n  long\t\t a:\n  regular\t a\n  short\t\t/a\n\nLetters with dots under them, such as s or d are reprsented as:\n\n  s\t\t/s\n  d\t\t/d\n\nThe palatalized and velar nasals are represented as:\n\n  palatalized\t/n\n  velarized\t/g\n\nA probable future feature is to convert from \"tohono helper\" Alvarez/Hale to unicode Alvarez/Hale so that things can be input in the custom system, which is very easy, yet still be output in its more elegant unicode form.\n\nThe purpose of the concordance is to aid in morphological analysis and dictionary lookup.\n"

usage :: String
usage = "\nUsage: tohelper -option input output\n\nPlease use only one option at a time. The following options are available:\n\n  -AlvarezToSaxton\t\tconverts Alvarez-Hale to Saxton\n  -SaxtonToAlvarez\t\tconverts Saxton to Alvarez-Hale\n  -AlvarezToLatex\t\tcreates a LaTeX template for Alvarez-Hale\n  -SaxtonToLatex\t\tcreates a LaTeX template for Saxton \n  -concordance  \t\tcreates a concordance of the text\n  -about          \t\tabout the program\n"

hatl :: IO ()
hatl = do i <- getArgs
          j <- readFile (i!!1)
          let y = haleAlvarezToLatex j
          let z = substitute "INSERTCODE" y latexTemplate
          writeFile (i!!2) z

stl :: IO ()
stl = do i <- getArgs
         j <- readFile (i!!1)
         let y = saxtonToLatex j
         let z = substitute "INSERTCODE" y latexTemplate
         writeFile (i!!2) z

ats :: IO ()
ats =  do i <- getArgs
          j <- readFile (i!!1)
          let y = haleAlvarezToSaxton j
          writeFile (i!!2) y

sta :: IO ()
sta =  do i <- getArgs
          j <- readFile (i!!1)
          let y = saxtonToHaleAlvarez j
          writeFile (i!!2) y

conc :: IO ()
conc = do i <- getArgs
          j <- readFile (i!!1)
          let y = j ++ "\n\n" ++ concordance j
          writeFile (i!!2) y

--creates an alphabetical all lowercase rough concordance
concordance :: String -> String
concordance = finish . go . map toLower

--the guts of the concordance. It computes the numbers and concatenates them with the item they are describing.
go :: String -> [String]
go xX =  map d (words xX) where
                          d x = x ++ ": " ++ show (j x) ++ "\n" 
                          j x = length ([js | js <- (words xX), js == x]) 

--alphabetizes the concordance and removes duplicates in one fell swoop.
clean :: [String] -> [String]
clean []     =  []
clean (x:xs) =  (clean [a|a <- xs, a < x]) ++ [x] ++ (clean [b|b <- xs, b > x])

--makes the result clean into a string.
finish :: [String] -> String
finish xs = unwords' (clean xs)

--is my custom version of unwords, because the prelude version does something I don't like with the spacing. It's a subtle difference but annoying.
unwords' :: [String] -> String
unwords' [] = []
unwords' ws = foldr1 (\w s -> w ++ s) ws

--filters out some punctuation. It could be expanded.
a :: String -> String
a xs = map b xs where b x = if x == '.' || x == ',' then ' ' else x

-- substitutes x with y in s
substitute :: String -> String -> String -> String
substitute x y s = subRegex (mkRegex x) s y

--applies a sequence of functions, each of which fixes some detail of the text
haleAlvarezToSaxton :: String -> String
haleAlvarezToSaxton = meekrules2 . meekrules21 . meekrules1 . meekrules11 . gStop . ta . e . i . o . u . aA . eE . iI . oO . uU . sa . se . si . so . sA . sE . sI . sO . c . d . dD . ddd . ddD . n . nN . g . sh . sH . oodham3 . oodham4

--The following functions have type String -> String.

--This is necessary because in Alvarez-Hale, this word is not written according to the rules of Alvarez-Hale
oodham3 = substitute "o'othham" "o'odham"
oodham4 = substitute "O'othham" "O'odham"
ta = substitute "a:" "ah"
e = substitute "e:" "eh"
i = substitute "i:" "ih"
o = substitute "o:" "oh"
u = substitute "u:" "uh"
aA = substitute "A:" "Ah"
eE = substitute "E:" "Eh"
iI = substitute "I:" "Ih"
oO = substitute "O:" "Oh"
uU = substitute "U:" "Uh"
sa = substitute "/a" "a"
se = substitute "/e" "e"
si = substitute "/i" "i"
so = substitute "/o" "o"
sA = substitute "/A" "A"
sE = substitute "/E" "E"
sI = substitute "/I" "I"
sO = substitute "/O" "O"
c = substitute "c" "ch"
d = substitute "d" "mEEKRULES1"
dD = substitute "D" "MEEKRULES1"
ddd = substitute "/d" "mEEKRULES2"
ddD = substitute "/D" "MEEKRULES2"
n =  substitute "/n" "n"
nN = substitute "/N" "N"
g =  substitute "/g" "ng"
sh = substitute "/s" "sh"
sH = substitute "/S" "Sh"
gStop = substitute " '" " "
meekrules1 = substitute "mEEKRULES1" "th"
meekrules11 = substitute "MEEKRULES1" "Th"
meekrules2 = substitute "mEEKRULES2" "d"
meekrules21 = substitute "MEEKRULES2" "D"
--dddUnicode = substitute "ḏ" "mEEKRULES2"
ddDUnicode = substitute "/D" "MEEKRULES2"
nUnicode =  substitute "ñ" "n"
nNUnicode = substitute "/N" "N"
gUnicode =  substitute "/g" "ng"
shUnicode = substitute "ṣ" "sh"
sHUnicode = substitute "/S" "Sh"
gStopUnicode = substitute " ʼ" " "

saxtonToHaleAlvarez :: String -> String
saxtonToHaleAlvarez =  oodham1 . oodham2 . addglottal .  bta . be . bi . bo . bu . baA . beE . biI . boO . buU . bsa . bse . bsi . bso . bsA . bsE . bsI . bsO . bc . bd . bdD . bddd . bddD . bn . bnN . bg . bsh . bsH . bmeekrules2 . bmeekrules21 . bmeekrules1 . bmeekrules11 
 
--The following functions have type String -> String.
oodham1 = substitute "O'o/dam" "O'odham"
oodham2 = substitute "o'o/dham" "o'odham"
bta = substitute "ah" "a:"
be = substitute "eh" "e:"
bi = substitute "ih" "i:"
bo = substitute "oh" "o:"
bu = substitute "uh" "u:"
baA = substitute "Ah" "A:"
beE = substitute "Eh" "E:"
biI = substitute "Ih" "I:"
boO = substitute "Oh" "O:"
buU = substitute "Uh" "U:"
bsa = substitute "a" "a"
bse = substitute "e" "e"
bsi = substitute "i" "i"
bso = substitute "o" "o"
bsA = substitute "A" "A"
bsE = substitute "E" "E"
bsI = substitute "I" "I"
bsO = substitute "O" "O"
bc = substitute "ch" "c"
bd = substitute "mEEKRULES1" "d"
bdD = substitute "MEEKRULES1" "D"
bddd = substitute "mEEKRULES2" "/d"
bddD = substitute "MEEKRULES2" "/D"
bn =  substitute "n" "n"
bnN = substitute "N" "N"
bg =  substitute "ng" "/g"
bsh = substitute "sh" "/s"
bsH = substitute "Sh" "/S"
bmeekrules1 = substitute "th" "mEEKRULES1"
bmeekrules11 = substitute "Th" "MEEKRULES1"
bmeekrules2 = substitute "d" "mEEKRULES2"
bmeekrules21 = substitute "D" "MEEKRULES2"
--bdddUnicode = substitute "mEEKRULES2" "ḏ"
bddDUnicode = substitute "MEEKRULES2" "/D"
bnUnicode =  substitute "n" "ñ"
bnNUnicode = substitute "N" "/N"
bgUnicode =  substitute "ng" "/g"
bshUnicode = substitute "sh" "ṣ"
bsHUnicode = substitute "Sh" "/S"
bgStopUnicode = substitute " " " ʼ"

--necessary because Alvarez/Hale writes glottal stops before all but one (sic!) vowel-initial word.
addglottal :: String -> String
addglottal  = unwords . addglottal' 

addglottal' :: String -> [String]
addglottal' x =  map addglottal'' (words x)

addglottal'' :: String -> String
addglottal'' x  = if elem (head x) vowels then "'" ++ x else x

vowels :: String
vowels = "AEIOUaeiou"

haleAlvarezToLatex :: String -> String
haleAlvarezToLatex = la . lA . le . lE . li . lI . lo . lO . ld . lD . ln . ls . lS . lg

la = substitute "/a" "\\u{a}"
lA = substitute "/A" "\\u{A}"
le = substitute "/e" "\\u{e}"
lE = substitute "/E" "\\u{E}"
li = substitute "/i" "\\u{\\i}"
lI = substitute "/I" "\\u{I}"
lo = substitute "/o" "\\u{o}"
lO = substitute "/O" "\\u{O}"
ld = substitute "/d" "\\d{d}"
lD = substitute "/D" "\\d{D}"
ln = substitute "/n" "\\~{n}"
ls = substitute "/s" "\\d{s}"
lS = substitute "/S" "\\d{S}"
lg = substitute "/g" "\\ng"

--This is reasonable because if the user didn't make a mistake about the type of file he's processing, then alvarezHaleToLatex won't break anything in the Saxton text.
saxtonToLatex :: String -> String
saxtonToLatex = haleAlvarezToLatex

latexTemplate :: String
latexTemplate = "\\documentclass[10pt]{article}\n \\usepackage{fancyhdr,geometry,tipa}\n \\pagestyle{fancy}\n \\geometry{margin=1in}\n \\chead{}\n \\lhead{}\n \\rhead{}\n \\cfoot{\\thepage}\n \\parskip = 6 pt\n \\parindent = 0 pt\n \\begin{document} \n \n \\section{}\n \nINSERTCODE\n\n\\end{document}\n"
