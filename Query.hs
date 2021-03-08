module Query where

import UserInfo
import Rating
import Movie
import Numeric
import Data.List.Split
import Data.Maybe
import Data.List
import Text.Printf
import qualified Data.Map as Map

type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry]

type ColSeparator = Char
type LnSeparator = Char

read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table col ln str = map_to_table (map (splitOn [col]) (endBy [ln] str))

map_to_table :: [[String]] -> Table
map_to_table (l:ls) = Table l ls

user_info = read_table '|' '\n' user_info_str
rating = read_table ' ' '\n' rating_str
movie = read_table '|' '\n' movie_str

get_ith_length :: Int -> [Entry] -> Int
get_ith_length i table = foldr (\x acc -> if ((length x) > acc) then (length x) else acc) 0 (get_ith_col i table)

get_ith_col :: Int -> [Entry] -> [Field]
get_ith_col i [] = []
get_ith_col i table = ((head table) !! i) : (get_ith_col i (tail table))

compute_length :: Table -> [Int]
compute_length (Table header entries) = compute_helper 0 (header : entries)

compute_helper :: Int -> [Entry] -> [Int]
compute_helper i table
    | (i + 1) < (length $ head table) = (get_ith_length i table) : (compute_helper (i + 1) table)
    | otherwise = (get_ith_length i table) : []

line_delim :: [Int] -> String
line_delim length_vector = replicate ((sum length_vector) + (length length_vector) + 1) '-' ++ "\n"

show_column :: String -> Int -> String
show_column column max_len = column ++ (replicate (max_len - (length column)) ' ') ++ "|"

show_line :: Entry -> [Int] -> String
show_line (e:es) (l:ls) = (show_column e l) ++ (show_line es ls)
show_line [] _ = "\n"
show_line _ [] = "\n"

show_entries :: [Entry] -> [Int] -> String
show_entries entries length_vector = foldr (\x acc -> "|" ++ (show_line x length_vector) ++ acc) "" entries

show_header :: TableSchema -> [Int] -> String
show_header = (\x y -> "|" ++ show_line x y)

show_table = (\(Table h e) v -> (line_delim v) ++ (show_header h v) ++ (line_delim v) ++ (show_entries e v) ++ (line_delim v))

instance Show Table where
    show = (\x -> show_table x (compute_length x))

display_table x = putStr (show_table x (compute_length x))

data FilterCondition = Lt Field Integer | Eq Field String | In Field [String] | Not FilterCondition

getFilter :: FilterCondition -> TableSchema -> (Entry -> Bool)
getFilter (Lt field val) header = lessThan (fromMaybe (-1) (elemIndex field header)) val
getFilter (Eq field str) header = equalsTo (fromMaybe (-1) (elemIndex field header)) str
getFilter (In field strings) header = isIn (fromMaybe (-1) (elemIndex field header)) strings
getFilter (Not f) header = (\entry -> not ((getFilter f header) entry))

lessThan :: Int -> Integer -> Entry -> Bool
lessThan (-1) val e = False
lessThan i val e = (read (e !! i) :: Integer) < val

equalsTo :: Int -> String -> Entry -> Bool
equalsTo (-1) str e = False
equalsTo index str e = (e !! index) == str

isIn :: Int -> [String] -> Entry -> Bool
isIn index vals e = fromMaybe (-1) (elemIndex (e !! index) vals) /= -1

data Query = Filter FilterCondition Query |
             Select [String] Query |
             SelectLimit [String] Integer Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

eval :: Query -> Table
eval (Atom x) = x
eval (Select s q) = Table s $ select_helper $ Select s (Atom $ eval q)
eval (SelectLimit s n q) = Table s $ take (fromInteger n) $ select_helper $ Select s (Atom $ eval q)
eval (Filter f (Atom (Table h e))) = Table h (filter (getFilter f h) e)
eval (Filter f q) = eval (Filter f $ Atom $ eval q)
eval (q1 :|| q2) = merge_entries (eval q1) (eval q2)
eval (Cosine q) = bad_boy_cos $ eval q

merge_entries :: Table -> Table -> Table
merge_entries (Table h1 e1) (Table h2 e2) = Table h1 (e1 ++ e2)

select_helper :: Query -> [Entry]
select_helper (Select s (Atom (Table h e)))
    | s == [] = []
    | (fromMaybe (-1) (elemIndex (head s) h)) == -1 = select_helper (Select (tail s) (Atom (Table h e)))
    | otherwise = merge_correctly (get_col (fromMaybe (-1) (elemIndex (head s) h)) e) (select_helper (Select (tail s) (Atom (Table h e))))
select_helper (Select s query) = select_helper (Select s (Atom $ eval query))

just_header :: Table -> [String]
just_header (Table h e) = h

strip_header (Table h e) = e

get_col :: Int -> [Entry] -> [Entry]
get_col i e
    | e == [] = []
    | i < length (head e) = [((head e) !! i)] : (get_col i (tail e))
    | otherwise = [[]]

merge_correctly :: [[a]] -> [[a]] -> [[a]]
merge_correctly (x:xs) (y:ys) = (x ++ y) : (merge_correctly xs ys)
merge_correctly [] x = x
merge_correctly x [] = x

get_user_zone :: String -> String
get_user_zone uid = ((strip_header $ eval $ Filter (Eq "user_id" uid) $ Atom user_info) !! 0) !! 4

same_zone :: String -> Query
same_zone = (\uid -> Atom $ eval $
                     Select ["user_id", "occupation"] $
                     Filter (Not (Eq "user_id" uid)) $
                     Filter (Eq "zone" (get_user_zone uid)) $
                     Atom user_info)

male_within_age :: Integer -> Integer -> Query
male_within_age = (\x y -> Atom $ eval $ 
                           Select ["occupation", "zone"] $
                           Filter (Eq "sex" "M") $
                           Filter (Not (Eq "age" (show x))) $
                           Filter (Not (Lt "age" x)) $
                           Filter (Lt "age" y) $
                           Atom user_info)

mixed :: [String] -> [String] -> Int -> Query
mixed = (\zone occ age -> Atom $ eval $
                          Select ["user_id"] $
                          Filter (Lt "age" (toInteger age)) $
                          Filter (In "occupation" occ) $
                          Filter (In "zone" zone) $
                          Atom user_info)

cos_header = ["user_id1", "user_id2", "sim"]

-- skz da o sa mearga numa pt rating table
bad_boy_cos :: Table -> Table
bad_boy_cos (Table h e) = Table cos_header (forEach $ pairs $ user_list $ strip_header rating)

-- primeste lista de perechi u1 u2
forEach :: [(Integer, Integer)] -> [Entry]
forEach (p:ps) = (build_entry p) : (forEach ps)
forEach [] = []

build_entry :: (Integer, Integer) -> Entry
build_entry p = [show (fst p), show (snd p), showFFloat (Just 4) (compute_cos (Map.elems $ create_map (fst p)) (Map.elems $ create_map (snd p))) ""]

create_map uid = populate_map (toMap $ build_tuples (strip_header rating)) entries where
                    entries = strip_header $ (eval $ (Filter (Eq "user_id" (show uid))) $ Atom rating)

compute_cos :: [Integer] -> [Integer] -> Float
compute_cos a b = (dot a b) / ((radical a) * (radical b))

user_list :: [Entry] -> [Integer]
user_list e = sort $ uniq $ map (\x -> read x :: Integer) (get_ith_col 0 e)

pairs xs = [(x, y) | (x:ys) <- tails (nub xs), y <- ys]

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/=x) xs)

build_tuples :: [Entry] -> [(Integer, Integer)]
build_tuples entries = foldr (\x acc -> ((read (x !! 1) :: Integer), 0) : acc) [] entries

toMap :: [(Integer, Integer)] -> Map.Map Integer Integer
toMap = Map.fromList

-- asta primeste map-ul UNUI USER si entry-urile aferente, updateaza (Film, Rating) cu rating
populate_map :: Map.Map Integer Integer -> [Entry] -> Map.Map Integer Integer
populate_map m [] = m
populate_map m (e:es) = Map.adjust (\x -> (read (e !! 2) :: Integer)) (read (e !! 1) :: Integer) (populate_map m es)

dot :: [Integer] -> [Integer] -> Float
dot a b = fromIntegral $ sum $ zipWith (*) a b

radical :: [Integer] -> Float
radical a = sqrt $ dot a a