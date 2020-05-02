import System.Random
import System.IO.Unsafe


users = ["user1","user2","user3","user4"]
items = ["item1","item2","item3","item4","item5","item6"]
purchasesHistory = [("user1",[["item1","item2","item3"],["item1","item2","item4"]]),("user2",[["item2","item5"],["item4","item5"]]),("user3",[["item3","item2"]]),("user4",[])]


empty x = (x,[])

createEmptyFreqList :: [a] -> [(a, [b])]
createEmptyFreqList a  = map empty a


-----------------------------------------------getAllUsersStats--------------------------------------------
hatElItemsElGatMa3ah y []=[]
hatElItemsElGatMa3ah y (x:xs) | elem y x== True = (filter (/=y) x) ++ (hatElItemsElGatMa3ah y xs)
hatElItemsElGatMa3ah y (x:xs) | elem y x== False = [] ++ (hatElItemsElGatMa3ah y xs)
--this takes the list of carts and an item and creats the list of other items that occure with it

countNumberOfOccurrances itemneeded l= length(filter (==itemneeded) (l))
--this counts the occurences of an item in a list

insertCountWithItem [] = []
insertCountWithItem l | l /= [] = (head l,(countNumberOfOccurrances (head l) l)) : insertCountWithItem (filter (/=(head l)) l)
--this creates a list of frequencies of each item in the list 

itemWithAllItemsThatCameWithIt y l = (y,insertCountWithItem (hatElItemsElGatMa3ah y l))
-- this creates the tupil of item and it's frequency list of other items that occure with it

appliesOnAllList [] l = []
appliesOnAllList (x:xs) l = (itemWithAllItemsThatCameWithIt x l) : appliesOnAllList xs l 
--this creates the previous function but for each item of the item list

getAllUsersStats [] = []
getAllUsersStats (x:xs) = (fst(x), appliesOnAllList items (snd(x))) : getAllUsersStats xs

-- this does the previous function for every user


----------------------------------------------purchasesIntersection----------------------------------------------------
fadygamedd [] [] = []
fadygamedd (x:xs) (y:ys)	| snd(y)==[] = fadygamedd xs ys
							| snd(x)==[] = fadygamedd xs ys
							| snd(y)/=[] && snd(y)/=[] = (fst(x),  fadygamedd2 (snd x) (snd y)): fadygamedd xs ys



fadygamedd2 [] y = y
fadygamedd2 (x:xs) y = fadygamedd2 xs (fadygamedd1 x y) 


fadygamedd1 x [] = [x]
fadygamedd1 x (y:ys)		| fst(y)== fst(x) = (fst(y), snd y + snd x) : ys
							| fst(y)/= fst(x) = y : fadygamedd1 x ys

youssefgamed _ [] = []
youssefgamed u ((a,b):xs) = (fadygamedd u b) : (youssefgamed u xs)

purchasesIntersection a b = filter (/=[]) (youssefgamed a b )

-----------------------------------------------freqListItems----------------------------------------------------------

fadygameddz2 [] y = y
fadygameddz2 (x:xs) y = fadygameddz2 xs (fadygameddz1 x y) 


fadygameddz1 x [] = [x]
fadygameddz1 x (y:ys)		| fst(y)== fst(x) = (fst(y), snd y + snd x) : ys
							| fst(y)/= fst(x) = y : fadygameddz1 x ys




safsafgamda2 y (x:xs) = safsafgamda2 (fadygamedd2 y (snd x)) xs
safsafgamda2 y [] = y


safsafgamda (x:xs:xc) = safsafgamda2 (fadygamedd2 (snd x) (snd xs)) xc

helper1 a [] = []
helper1 a ((b,c):xs)	|(a == b) = c 
						|(a /= b) = helper1 a xs
freqListItems a = safsafgamda (helper1 a (getAllUsersStats purchasesHistory))


------------------------------------------freqListCart--------------------------------------------------------------

helperkoky _ [] = []
helperkoky a ((b,c):xs) = if (a == b) then c else (helperkoky a xs)
helperkokykak _ [] = []
helperkokykak a (x:xs) =  (helperkoky x (helper1 a (getAllUsersStats purchasesHistory))) : (helperkokykak a xs)



safsafgamdaa2 y [] = y
safsafgamdaa2 y (x:xs) = safsafgamdaa2 (fadygamedd2 y x) xs

--freqListCart a l = safsafgamdaa2 (helperkatakito (helperkokykak a l))

safsafgamdaa [] = []
safsafgamdaa [x] = (fadygamedd2 x [])
safsafgamdaa (x:xs:[]) = safsafgamdaa2 (fadygamedd2 x xs) []
safsafgamdaa (x:xs:xc) = safsafgamdaa2 (fadygamedd2 x xs) xc

freqListCart a l = safsafgamdaa (helperkokykak a l)


------------------------------------------------freqListCartAndItems-------------------------------------------------

freqListCartAndItems u l = fadygamedd2 (freqListCart u l) (freqListItems u)

---------------------------------------------------------------------------------------------------------------------
recommendEmptyCart :: String -> String
recommendEmptyCart a = if (length (fokaha (freqListItems a)) /= 0) then (fokaha (freqListItems a)) !! (randomZeroToX (length (fokaha (freqListItems a)) - 1)) else " "


fokaha [] = [] 
fokaha ((a,b):xs) = (fokwa7da (a,b)) ++ (fokaha xs)

fokwa7da (a,b) = if (b /= 0) then a:(fokwa7da (a,b - 1)) else []



helpersafsaf [] = []
helpersafsaf ((a,b):xs) = b:(helpersafsaf xs)

helperwatson [] = []
helperwatson (x:xs) = (helpersafsaf x) ++ (helperwatson xs)
-- bytl3o two lists of intersection coming from purchasesIntersection 

getFirstList u [] = []
getFirstList u ((a,b):xs) = if (u==a) then b else (getFirstList u xs)

getExceptFirstList u [] = []
getExceptFirstList u ((a,b):xs) = if (u /= a ) then ((a,b):(getExceptFirstList u xs)) else (getExceptFirstList u xs)

takeUser u = purchasesIntersection (getFirstList u (getAllUsersStats purchasesHistory)) (getExceptFirstList u (getAllUsersStats purchasesHistory))


freqListUsers:: String -> [(String, Int)]
freqListUsers u = safsafgamdaa (helperwatson (takeUser u))


recommendBasedOnItemsInCart :: String -> [String] -> String
recommendBasedOnItemsInCart a l = if (length (fokaha (freqListCartAndItems a l)) /= 0) then (fokaha (freqListCartAndItems a l)) !! (randomZeroToX (length (fokaha (freqListCartAndItems a l)) - 1)) else " "

recommendBasedOnUsers :: String -> String
recommendBasedOnUsers a = if (length (fokaha (freqListUsers a)) /= 0) then (fokaha (freqListUsers a)) !! (randomZeroToX (length (fokaha (freqListUsers a)) - 1)) else " "


getRecIfEmpty a = recommendEmptyCart a
getRecNotEmpty a l = recommendBasedOnItemsInCart a l
getRecUsers a = recommendBasedOnUsers a
 


recommendhelper a l = if (length l == 0) then [(getRecIfEmpty a),(getRecUsers a)] else [(getRecNotEmpty a l),(getRecUsers a)]
final2 [] = items !! (randomZeroToX (length (items) - 1))

extracheck (x:xs) = filter (/= " " ) (x:xs)


recommend :: String -> [String] -> String
recommend a l = if ((length (extracheck(recommendhelper a l ))) == 0) 
				then (items !! (randomZeroToX (length (items) - 1))) 
				else ((recommendhelper a l) !! randomZeroToX (length ((recommendhelper a l)) - 1))



randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))