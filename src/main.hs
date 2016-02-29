{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts, Rank2Types #-}
import Haste
import Haste.DOM
import Haste.JSON
import Haste.Events
import Haste.Foreign (ffi)
import Haste.Serialize
import Haste.LocalStorage
import MakeLense
import Lens.Family2
import Lens.Family2.Stock
import Lens.Family2.Unchecked hiding (iso)
import Lens.Family2.State.Lazy
import Control.Monad.State
import Data.IORef
import Data.Maybe (isNothing)
import Data.List
import qualified Data.Map as M
import qualified Data.Tree as T
import qualified Data.Foldable as F
import Data.Functor.Identity (Identity(..))
import Text.Printf (printf)

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

instance Profunctor (->) where
  dimap ab cd bc = cd . bc . ab

type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)
type Iso' s a = Iso s s a a
type AnIso s t a b = Exchange a b a (Identity b) -> Exchange a b s (Identity t)
type AnIso' s a = AnIso s s a a

data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Profunctor (Exchange a b) where
  dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)

from :: AnIso s t a b -> Iso b a t s
from l = withIso l $ \sa bt -> iso bt sa

withIso :: AnIso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso ai k = case ai (Exchange id Identity) of
  Exchange sa bt -> k sa (runIdentity . bt)

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM mb k = mb >>= \b -> when b k

latestVersion :: [Int]
latestVersion = [1,0]

setHTML :: (IsElem e, MonadIO m) => e -> String -> m ()
setHTML e s = setProp e "innerHTML" s

addHTML :: (IsElem e, MonadIO m) => e -> String -> m ()
addHTML e s = do
  t <- getProp e "innerHTML"
  setProp e "innerHTML" $ s ++ t

appendHTML :: (IsElem e, MonadIO m) => e -> String -> m ()
appendHTML e s = do
  t <- getProp e "innerHTML"
  setProp e "innerHTML" $ t ++ s

appendHTMLif :: (IsElem e, MonadIO m) => ElemID -> e -> String -> m ()
appendHTMLif eid e s = do
  m <- elemById eid
  when (isNothing m) $ appendHTML e s

removeAttrById :: String -> String -> IO ()
removeAttrById e attr = void $ eval $ toJSString $ "$('#" ++ e ++ "').removeAttr(\"" ++ attr ++ "\")"

instance (Ord k, Serialize k, Serialize v) => Serialize (M.Map k v) where
  toJSON = toJSON . M.toList
  parseJSON j = M.fromList <$> parseJSON j

getCurrentTime :: IO String
getCurrentTime = ($ ()) $ ffi $ toJSString $ concat [
  "(function (){",
  "var time = new Date();",
  "return (time.getHours() + \"-\" + time.getMinutes() + \"-\" + time.getSeconds()); })"
  ]

-- * breaks lens laws
_MNum :: (Num a) => Lens' (Maybe a) a
_MNum = lens (maybe 0 id) (\m x -> Just x)

_digits :: Iso' [Int] Int
_digits = iso (foldl (\a b -> a * 10 + b) 0) ((0 :) . reverse . digit) where
  digit i = case i of
    0 -> []
    _ -> let (r,d) = quotRem i 10 in d : digit r

putAlert :: String -> IO ()
putAlert s = void $ do
  eid <- ("alert-" ++) <$> getCurrentTime
  withElem "alert-area" $ \e -> do
    withElemsQS e ".alert" $ \es -> do
      ms <- mapM (\e -> getProp e "outerHTML") $ take 3 es
      setHTML e $ concat ms ++ _alert eid

  setTimer (Once 5000) $ void $ do
    eval $ toJSString $ "$('#" ++ eid ++ "').alert('close');"

  where
    _alert t = concat [
      "<div id=\"" ++ t ++ "\" class=\"alert alert-dismissible fade in\" role=\"alert\">",
      "  <button type=\"button\" class=\"close\" data-dismiss=\"alert\" aria-label=\"Close\">",
      "    <span aria-hidden=\"true\">&times;</span>",
      "  </button>"] ++ s ++
      "</div>"

toolTipAttr :: String -> [Attribute]
toolTipAttr t = [
  attr "data-toggle" =: "tooltip",
  attr "data-placement" =: "bottom",
  attr "title" =: t]

(-:) :: String -> String -> String
k -: v = printf "%s = \"%s\"" k v

tag :: String -> [String] -> String -> String
tag t as x
  | as == [] = printf "<%s>%s</%s>" t x t
  | otherwise = printf "<%s %s>%s</%s>" t (unwords as) x t

appendElem :: (IsElem parent, IsElem child, MonadIO m) => parent -> m child -> m ()
appendElem p mc = appendChild p =<< mc

withElemsOf :: MonadIO m => Elem -> ElemID -> ([Elem] -> m a) -> m a
withElemsOf parent eid = withElemsQS parent ("#" ++ eid)

etag :: (MonadIO m) => String -> [Attribute] -> [m Elem] -> m Elem
etag t as m = do
  es <- sequence m
  parent <- newElem t `with` as
  setChildren parent es
  return parent

ofHTML :: (MonadIO m) => ([m Elem] -> m Elem) -> String -> m Elem
ofHTML me html = do
  e <- me []
  setHTML e html
  return e

type Unlock = UnionT '[
  "ident" :< String,
  "name" :< String,
  "description" :< String,
  "premise" :< [String],
  "cost" :< [(String, Int)]
  ]

type Building = UnionT '[
  "ident" :< String,
  "name" :< String,
  "premise" :< [String],
  "interval" :< (Double -> Double)
  ]

type Item = UnionT '[
  "ident" :< String,
  "name" :< String,
  "description" :< String
  ]

type Achievement = UnionT '[
  "name" :< String,
  "description" :< String,
  "check" :< (Game -> Bool)
  ]

type Gift = UnionT '[
  "name" :< String,
  "rarity" :< Double
  ]

type Game = UnionT '[
  "version" :< [Int],
  "items" :< M.Map String Double,
  "unlocks" :< M.Map String Bool,
  "buildings" :< M.Map String Int,
  "log" :< [String],
  "slot" :< [Int]
  ]

ident :: Has (Union xs) "ident" out => Lens' (Union xs) out; ident = lenses (Name :: Name "ident")
description :: Has (Union xs) "description" out => Lens' (Union xs) out; description = lenses (Name :: Name "description")
name :: Has (Union xs) "name" out => Lens' (Union xs) out; name = lenses (Name :: Name "name")
premise :: Has (Union xs) "premise" out => Lens' (Union xs) out; premise = lenses (Name :: Name "premise")
cost :: Has (Union xs) "cost" out => Lens' (Union xs) out; cost = lenses (Name :: Name "cost")
interval :: Has (Union xs) "interval" out => Lens' (Union xs) out; interval = lenses (Name :: Name "interval")
version :: Has (Union xs) "version" out => Lens' (Union xs) out; version = lenses (Name :: Name "version")
log :: Has (Union xs) "log" out => Lens' (Union xs) out; log = lenses (Name :: Name "log")
unlocks :: Has (Union xs) "unlocks" out => Lens' (Union xs) out; unlocks = lenses (Name :: Name "unlocks")
items :: Has (Union xs) "items" out => Lens' (Union xs) out; items = lenses (Name :: Name "items")
buildings :: Has (Union xs) "buildings" out => Lens' (Union xs) out; buildings = lenses (Name :: Name "buildings")
slot :: Has (Union xs) "slot" out => Lens' (Union xs) out; slot = lenses (Name :: Name "slot")
rarity :: Has (Union xs) "rarity" out => Lens' (Union xs) out; rarity = lenses (Name :: Name "rarity")

initialGame :: Game
initialGame =
  sinsert (Tag latestVersion) $
  sinsert (Tag (M.singleton "IQ" 100) :: "items" :< M.Map String Double) $
  sinsert (Tag M.empty :: "unlocks" :< M.Map String Bool) $
  sinsert (Tag M.empty :: "buildings" :< M.Map String Int) $
  sinsert (Tag ([] :: [String])) $
  sinsert (Tag ([0,1] :: [Int])) $
  Union HNil

makeUnlock :: String -> String -> [String] -> [(String, Int)] -> String -> Unlock
makeUnlock i n p c d =
  sinsert (Tag i :: "ident" :< String) $
  sinsert (Tag n :: "name" :< String) $
  sinsert (Tag d :: "description" :< String) $
  sinsert (Tag p :: "premise" :< [String]) $
  sinsert (Tag c) $
  Union HNil

buildTree :: [Unlock] -> T.Tree Unlock
buildTree = build . sortBy comp where
  comp :: Unlock -> Unlock -> Ordering
  comp x y
    | (x^.ident) `elem` (y^.premise) = LT
    | (y^.ident) `elem` (x^.premise) = GT
    | otherwise = EQ

  build :: [Unlock] -> T.Tree Unlock
  build xs = go u1 (T.Node r0 (fmap singleT u0)) where
    r0 = makeUnlock [] [] [] [] []
    (u0, u1) = partition (\x -> x^.premise == [] || head (x^.premise) == "root") xs
    singleT t = T.Node t []

    ins y (T.Node l1 ls)
      | head (y^.premise) == l1^.ident = T.Node l1 (ls ++ [singleT y])
      | otherwise = T.Node l1 (fmap (ins y) ls)

    go [] t = t
    go (y:ys) t = go ys (ins y t)

getBranchBy :: (a -> b -> Bool) -> a -> T.Tree b -> [b]
getBranchBy eqf a (T.Node x xs)
  | eqf a x = fmap T.rootLabel xs
  | otherwise = concat $ fmap (getBranchBy eqf a) xs

makeBuilding :: String -> String -> [String] -> (Double -> Double) -> Building
makeBuilding i n p v =
  sinsert (Tag i :: "ident" :< String) $
  sinsert (Tag n :: "name" :< String) $
  sinsert (Tag p :: "premise" :< [String]) $
  sinsert (Tag v :: "interval" :< (Double -> Double)) $
  Union HNil

makeItem :: String -> String -> String -> Item
makeItem i n d =
  sinsert (Tag i :: "ident" :< String) $
  sinsert (Tag n :: "name" :< String) $
  sinsert (Tag d :: "description" :< String) $
  Union HNil

makeAchievement :: String -> String -> (Game -> Bool) -> Achievement
makeAchievement n d c =
  sinsert (Tag n) $
  sinsert (Tag d) $
  sinsert (Tag c) $
  Union HNil

makeGift :: String -> Double -> Gift
makeGift n v =
  sinsert (Tag n) $
  sinsert (Tag v) $
  Union HNil

unlockMap :: [Unlock]
unlockMap = [
  -- makeUnlock
  --   "devil-machine" "<i class=\"fa fa-gears fa-fw\"></i>IQマシーン" ["devil-gear"] []
  --   "IQマシーンに触れた人間はIQが少し低くなります"
  ]

unlockTree :: T.Tree Unlock
unlockTree = buildTree unlockMap

buildingMap :: [Building]
buildingMap = [
  ]
  where
    itvExp base pow n = base * pow ^ (floor n)

itemMap :: [Item]
itemMap = [
  makeItem "IQ" "<i class=\"fa fa-fw fa-question-circle\"></i>IQ" "現在のあなたのIQです",
  makeItem "multiplier" "<i class=\"fa fa-fw fa-line-chart\"></i>IQ増幅器" "脳に装着することで脳が潜在能力を発揮し、IQの最大値が上昇します",
  makeItem "energy-drink" "<i class=\"fa fa-fw fa-clock-o\"></i>エナジードリンク" "摂取すると脳にエネルギーが流れだし、低下した脳の機能を高速で回復します",
  makeItem "bronze-ticket" "<span class=\"border-td border-bronze\"><i class=\"fa fa-fw fa-credit-card\"></i>ブロンズチケット</span>" "ブロンズチケットです<br />ブロンズガチャを回すのに必要です",
  makeItem "silver-ticket" "<span class=\"border-td border-silver\"><i class=\"fa fa-fw fa-credit-card\"></i>シルバーチケット</span>" "シルバーチケットです<br />シルバーガチャを回すのに必要です",
  makeItem "gold-ticket" "<span class=\"border-td border-gold\"><i class=\"fa fa-fw fa-credit-card\"></i>ゴールドチケット</span>" "ゴールドチケットです<br />ゴールドガチャを回すのに必要です"
  ]

achievementMap :: [Achievement]
achievementMap = [
  makeAchievement "<i class=\"fa fa-fw fa-question-circle\"></i>ギャンブラー" "IQが0になる" (\game -> game^.items^.at "IQ"^._MNum == 0),
  makeAchievement "<i class=\"fa fa-fw fa-question-circle\"></i>天才" "IQが200を越える" (\game -> game^.items^.at "IQ"^._MNum >= 200),
  makeAchievement "<i class=\"fa fa-fw fa-question-circle\"></i>桁違いの天才" "IQが1000を越える" (\game -> game^.items^.at "IQ"^._MNum >= 1000)
  ]

giftMap :: [Gift]
giftMap = [
  makeGift "bronze-ticket" 5,
  makeGift "silver-ticket" 1000,
  makeGift "gold-ticket" 300000,
  makeGift "multiplier" 10,
  makeGift "energy-drink" 500
  ]

probCalc :: Double -> Double -> Double
probCalc bet r
  | bet < r = 0
  | bet > 2*r = 100
  | otherwise = (100 / r^2) * (bet - r)^2

getGift :: Double -> IO [Gift]
getGift bet = flip filterM giftMap $ \g -> do
  seed <- newSeed
  let n = fst $ randomR (1,100) seed
  return $ if (n <= probCalc bet (g^.rarity)) then True else False

gainIQPS :: Game -> Double
gainIQPS game = 0.1 + 0.15 * (game ^. items ^. at "energy-drink" ^. _MNum)

maxIQ :: Game -> Double
maxIQ game = 100 + 30 * (game ^. items ^. at "multiplier" ^. _MNum)

main = do
  s <- getItem "FriendtheNet"
  ref <- case ((s :: Either String JSON) >>= fromJSON :: Either String Game) of
    Left err -> do
      print err
      newIORef initialGame
    Right x -> do
      newIORef x

  withElem "link-for-save" $ \e -> onEvent e Click $ \_ -> save ref
  withElem "link-for-reset" $ \e -> onEvent e Click $ \_ -> do
    writeIORef ref initialGame
    maininit ref

  withElem "roll-gacha" $ \e -> do
    onEvent e Click $ \_ -> do
      refStateT ref $ do
        d <- use $ slot . _digits
        iq <- use $ items . at "IQ" . _MNum
        when (iq >= fromIntegral d) $ do
          items . at "IQ" . _MNum -= fromIntegral d

          d <- use $ slot . _digits
          gs <- lift $ getGift (fromIntegral d)
          forM_ gs $ \g -> do
            items . at (g ^. name) . _MNum += 1

      maininit ref

  maininit ref
  setTimer (Repeat 100) $ mainloop ref
  setTimer (Repeat 60000) $ save ref

maininit :: IORef Game -> IO ()
maininit ref = void $ do
  displayItem
  -- displayUnlockShop
  -- displayBuilding
  -- displayUnlockTree
  displayAchievement
  displaySlot

  eval $ toJSString $ concat $ [
    "$('[data-toggle=\"tooltip\"]').tooltip({",
    "  'template': '<div class=\"tooltip\" role=\"tooltip\"><div class=\"tooltip-inner tooltip-card\"></div></div>',",
    "  'html': true",
    "});"]

  where
    -- _buttonWith t = "button" `tag` ([
    --   "type" -: "button",
    --   "class" -: "btn btn-sm btn-secondary-dark"] ++ t)
    -- _buttonTT as t = _buttonWith (toolTipAttr t ++ as)
    -- _buttonTTWith eid as = _buttonTT (["id" -: eid] ++ as)
    -- _div = "div" `tag` []

    displaySlot = do
      withElem "slot" $ \e -> do
        game <- readIORef ref
        clearChildren e

        forM_ (reverse [0..length (game^.slot) - 1]) $ \i -> do
          let cid = "caret-button-id-" ++ show i
          withElemsOf e (cid ++ "-text") $ \us -> case us of
            [] -> void $ do
              appendElem e $ "div" `etag` [attr "class" =: "slot-div"] $ [
                _p $ return $ "a" `etag` [attr "id" =: (cid ++ "-up"), attr "role" =: "button", attr "class" =: "caret-button"] `ofHTML` "<i class=\"fa fa-caret-up\"></i>",
                "p" `etag` [attr "class" =: "no-margin", attr "id" =: (cid ++ "-text")] `ofHTML` (show $ (game ^. slot) !! (length (game^.slot) - 1 - i)),
                _p $ return $ "a" `etag` [attr "id" =: (cid ++ "-down"), attr "role" =: "button", attr "class" =: "caret-button"] `ofHTML` "<i class=\"fa fa-caret-down\"></i>"
                ]

              withElem (cid ++ "-up") $ \ebtn ->
                onEvent ebtn Click $ \_ -> do
                  refStateT ref $ do
                    slot . _digits += (10 ^ i)
                    d <- use $ slot . _digits
                    iq <- use $ items . at "IQ" . _MNum
                    when (d > floor iq) $ slot . _digits .= floor iq
                  displaySlot

              withElem (cid ++ "-down") $ \ebtn ->
                onEvent ebtn Click $ \_ -> do
                  refStateT ref $ do
                    n <- use (slot . _digits)
                    when (n - (10 ^ i) >= 0) $ slot . _digits -= (10 ^ i)
                  displaySlot

            [u] -> do
              -- *this not happen*
              -- can stop generating and inserting HTML above everytime ?
              setHTML u $ show $ (game ^. slot) !! (length (game^.slot) - 1 - i)

            _ -> error $ "duplicate #" ++ cid ++ "-text"

      where
        _p :: [IO Elem] -> IO Elem
        _p = "p" `etag` [attr "class" =: "no-margin"]

    displayItem = do
      withElem "item-display-tbody" $ \e -> do
        game <- readIORef ref

        -- to preserve the order of items
        clearChildren e

        forM_ itemMap $ \itm -> do
          let tid = "item-display-" ++ (itm^.ident)
          when (game ^. items ^. at (itm^.ident) ^. _MNum > 0) $ do
            withElemsOf e tid $ \us -> case us of
              [] -> do
                appendElem e $ _trTT (itm^.description) $ [
                  _td `ofHTML` (itm^.name),
                  _tdWith [attr "class" =: "text-xs-right", attr "id" =: tid] []
                  ]
              [u] -> do
                setHTML u $ show $ floor $ game ^. items ^. at (itm^.ident) ^. _MNum
              _ -> error $ "duplicate #" ++ tid

      where
        _tdWith = etag "td"
        _td = _tdWith []
        _trTT t = "tr" `etag` (toolTipAttr t)

    displayAchievement = do
      withElem "trophy-list-div" $ \e -> do
        whenM ((== 0) . length <$> getChildren e) $ do
          forM_ achievementMap $ \ach ->
            appendElem e $
              "div" `etag` [attr "class" =: "col-lg-3 col-md-4"] $ [
                "span" `etag` ([attr "class" =: "achievement-piece"] ++ toolTipAttr (ach^.description)) `ofHTML` (ach^.name)
                ]

    --
    -- displayUnlockShop = do
    --   withElem "unlock-shop-block" $ \e -> do
    --     game <- readIORef ref
    --     setHTML e ""
    --
    --     forM_ unlockMap $ \unl -> do
    --       let uidbtn = "unlock-" ++ unl^.ident
    --       let uidspan = "unlock-span-" ++ unl^.ident
    --       when (M.notMember (unl^.ident) (game^.unlocks) || ((game^.unlocks) M.! (unl^.ident) == False)) $ do
    --         when (all (\x -> M.member x (game^.unlocks) && (game^.unlocks) M.! x) $ unl^.premise \\ ["root"]) $ do
    --           let abtn unl = "a" `tag` ["id" -: ("unlock-" ++ unl^.ident), "role" -: "button", "class" -: "btn btn-sm btn-secondary-dark disabled"] $ (unl ^. name)
    --
    --           appendHTML e $ concat [
    --             "div" `tag` [] $
    --               "div" `tag` (toolTipAttr (unl ^. description ++ "<hr />" ++ requirement unl) ++ ["id" -: uidspan, "style" -: "display: inline-block;"]) $
    --               abtn unl,
    --             let lis = concat $ fmap (\u -> "li" `tag` [] $ abtn u) (getBranchBy (\a b -> a^.ident == b^.ident) unl unlockTree) in
    --             if lis == "" then "" else "ul" `tag` ["class" -: "tree-view"] $ lis
    --             ]
    --
    --     forM_ unlockMap $ \unl -> do
    --       let uidbtn = "unlock-" ++ unl^.ident
    --       let uidspan = "unlock-span-" ++ unl^.ident
    --       when (M.notMember (unl^.ident) (game^.unlocks) || ((game^.unlocks) M.! (unl^.ident) == False)) $ do
    --         when (all (\x -> M.member x (game^.unlocks) && (game^.unlocks) M.! x) $ unl^.premise \\ ["root"]) $ do
    --           withElem uidbtn $ \ebtn -> void $ do
    --             onEvent ebtn Click $ \_ -> do
    --               refStateT ref $ do
    --                 unlocks . at (unl^.ident) .= Just True
    --                 forM_ (unl^.cost) $ \(k,v) -> do
    --                   items . at k . _MNum -= fromIntegral v
    --
    --               withElem uidspan $ \espan -> do
    --                 adb <- getAttr espan "aria-describedby"
    --                 eval $ toJSString $ "$('#" ++ adb ++ "').hide();"
    --
    --               maininit ref
    --   where
    --     requirement unl = intercalate "<br />" $ fmap (\(k,v) -> k ++ ": " ++ show v) $ unl^.cost
    --
    -- displayUnlockTree = do
    --   withElem "unlock-tree" $ \e -> do
    --     game <- readIORef ref
    --     setHTML e $ treesHTML (\unl -> _buttonTT [] (unl ^. description) (unl ^. name)) unlockTrees
    --
    --   where
    --     treesHTML :: (Unlock -> String) -> [T.Tree Unlock] -> String
    --     treesHTML f xs = "ul" `tag` ["class" -: "tree-view"] $ concat $ fmap (treeHTML f) xs
    --
    --     treeHTML :: (Unlock -> String) -> T.Tree Unlock -> String
    --     treeHTML f (T.Node x []) = "li" `tag` [] $ f x
    --     treeHTML f (T.Node x xs) = "li" `tag` [] $ concat [f x, "ul" `tag` [] $ concat $ fmap (treeHTML f) xs]
    --
    --     unlockTrees = T.subForest unlockTree
    --
    -- displayBuilding = do
    --   withElem "buildings-shop-ul" $ \e -> do
    --     game <- readIORef ref
    --     setHTML e ""
    --     let buildId s bm = "building-" ++ s ++ "-" ++ bm^.ident
    --
    --     forM_ buildingMap $ \bm -> do
    --       when (all (\x -> M.member x (game^.unlocks) && (game^.unlocks) M.! x) (bm^.premise)) $ do
    --         lbl <- labelBuilding bm
    --         appendHTML e $ _li $
    --           "div" `tag` ["class" -: "row", "style" -: "padding: 0 1.25rem;"] $ concat [
    --             "div" `tag` ["class" -: "display-none", "id" -: buildId "progress-div" bm] $
    --               "progress" `tag` ["id" -: buildId "progress" bm, "class" -: "progress progress-bar progress-bar", "value" -: "0", "max" -: "100"] $ "",
    --             "span" `tag` ["id" -: buildId "text" bm] $ concat [
    --               (bm^.name), " ",
    --               (show $ game ^. buildings ^. at (bm ^. ident) ^. _MNum)
    --               ],
    --             "div" `tag` ["class" -: "pull-sm-right"] $
    --               "button" `tag` ["id" -: buildId "btn" bm, "class" -: "btn btn-sm btn-main-dark"] $
    --                 "<i class=\"fa fa-plus-circle fa-fw\"></i>" ++ lbl
    --             ]
    --
    --     forM_ buildingMap $ \bm -> do
    --       when (all (\x -> M.member x (game^.unlocks) && (game^.unlocks) M.! x) (bm^.premise)) $ do
    --         withElem (buildId "btn" bm) $ \ebtn -> void $ do
    --           onEvent ebtn Click $ \_ -> void $ do
    --             setAttr ebtn "disabled" "disabled"
    --
    --             withElem (buildId "progress-div" bm) $ \eid -> do
    --               setClass eid "display-none" False
    --
    --             itv <- intervalBuilding bm
    --             withElem (buildId "progress" bm) $ \br -> do
    --               let w = (1+) $ floor $ 100 / (10 * itv)
    --               setAttr br "value" $ show w
    --
    --               eval $ toJSString $ concat [
    --                 "$('#" ++ buildId "progress" bm ++ "').animate({",
    --                 "value: \"100\"",
    --                 "}, " ++ show (itv * 1000) ++ ", \"linear\");"
    --                 ]
    --
    --             setTimer (Once $ floor $ 1000 * itv) $ do
    --               refStateT ref $ do
    --                 buildings . at (bm^.ident) . _MNum += 1
    --
    --               removeAttrById (buildId "btn" bm) "disabled"
    --
    --               withElem (buildId "progress-div" bm) $ \eid -> do
    --                 setClass eid "display-none" True
    --
    --               lbl <- labelBuilding bm
    --               withElem (buildId "btn" bm) $ \ebtn -> do
    --                 setHTML ebtn $ ("<i class=\"fa fa-plus-circle fa-fw\"></i>" ++) $ lbl
    --               withElem (buildId "text" bm) $ \etxt -> do
    --                 game <- readIORef ref
    --                 setHTML etxt $ concat $ [
    --                   (bm^.name), " ",
    --                   (show $ game ^. buildings ^. at (bm ^. ident) ^. _MNum)]
    --
    --     html <- getProp e "innerHTML"
    --     when (html == "") $ do
    --       setHTML e $ _li "ここに施設が追加されます"
    --
    --   where
    --     _li = "li" `tag` ["class" -: "list-group-item"]
    --     intervalBuilding bm = do
    --       game <- readIORef ref
    --       return $ bm^.interval $ fromIntegral $ game^.buildings^.at (bm^.ident)^._MNum
    --     labelBuilding bm = do
    --       game <- readIORef ref
    --       itv <- intervalBuilding bm
    --       return $ (printf "%0.2f" itv) ++ " s"
    --

mainloop :: IORef Game -> IO ()
mainloop ref = do
  refStateT ref $ do
    game <- get
    when (game ^. items ^. at "IQ" ^. _MNum < maxIQ game) $ do
      items . at "IQ" . _MNum += gainIQPS game

      when (game ^. items ^. at "IQ" ^. _MNum >= maxIQ game) $ do
        items . at "IQ" . _MNum .= maxIQ game

  game <- readIORef ref

  forM_ itemMap $ \itm -> do
    when (game ^. items ^. at (itm^.ident) ^. _MNum > 0) $ do
      let itmid = "item-display-" ++ itm^.ident
      if itm^.ident == "IQ"
        then do
          withElem itmid $ \e -> do
            setHTML e $ concat $ [
              show $ floor $ game ^. items ^. at (itm^.ident) ^. _MNum,
              " / ",
              "<small style=\"color: #aaa;\">", show $ floor $ maxIQ game, "</small>"
              ]
        else do
          withElem itmid $ \e -> do
            setHTML e $ show $ floor $ game ^. items ^. at (itm^.ident) ^. _MNum

save :: IORef Game -> IO ()
save ref = do
  setItem "FriendtheNet" =<< readIORef ref
  putAlert "セーブしました"

refStateT :: IORef s -> StateT s IO () -> IO ()
refStateT ref m = do
  writeIORef ref =<< execStateT m =<< readIORef ref
