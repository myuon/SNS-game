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


latestVersion :: [Int]
latestVersion = [1,0]

setHTML e s = setProp e "innerHTML" s
addHTML e s = do
  t <- getProp e "innerHTML"
  setProp e "innerHTML" $ s ++ t
appendHTML e s = do
  t <- getProp e "innerHTML"
  setProp e "innerHTML" $ t ++ s

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

(-:) :: String -> String -> String
k -: v = printf "%s = \"%s\"" k v

tag :: String -> [String] -> String -> String
tag t as x
  | as == [] = printf "<%s>%s</%s>" t x t
  | otherwise = printf "<%s %s>%s</%s>" t (unwords as) x t

-- * breaks lens laws
_MNum :: (Num a) => Lens' (Maybe a) a
_MNum = lens (maybe 0 id) (\m x -> Just x)

_digits :: Iso' [Int] Int
_digits = iso (foldl (\a b -> a * 10 + b) 0) (nonemp . reverse . digit) where
  digit i = case i of
    0 -> []
    _ -> let (r,d) = quotRem i 10 in d : digit r

  nonemp [] = [0]
  nonemp xs = xs

type Unlock = UnionT '[
  "ident" :< String,
  "name" :< String,
  "description" :< String,
  "premise" :< [String],
  "cost" :< [(String, Int)]
  ]

ident :: Has (Union xs) "ident" out => Lens' (Union xs) out; ident = lenses (Name :: Name "ident")
description :: Has (Union xs) "description" out => Lens' (Union xs) out; description = lenses (Name :: Name "description")
name :: Has (Union xs) "name" out => Lens' (Union xs) out; name = lenses (Name :: Name "name")
premise :: Has (Union xs) "premise" out => Lens' (Union xs) out; premise = lenses (Name :: Name "premise")
cost :: Has (Union xs) "cost" out => Lens' (Union xs) out; cost = lenses (Name :: Name "cost")

type Building = UnionT '[
  "ident" :< String,
  "name" :< String,
  "premise" :< [String],
  "interval" :< (Double -> Double)
  ]

interval :: Has (Union xs) "interval" out => Lens' (Union xs) out; interval = lenses (Name :: Name "interval")

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

type Game = UnionT '[
  "version" :< [Int],
  "items" :< M.Map String Double,
  "unlocks" :< M.Map String Bool,
  "buildings" :< M.Map String Int,
  "log" :< [String],
  "slot" :< [Int]
  ]

version :: Has (Union xs) "version" out => Lens' (Union xs) out; version = lenses (Name :: Name "version")
log :: Has (Union xs) "log" out => Lens' (Union xs) out; log = lenses (Name :: Name "log")
unlocks :: Has (Union xs) "unlocks" out => Lens' (Union xs) out; unlocks = lenses (Name :: Name "unlocks")
items :: Has (Union xs) "items" out => Lens' (Union xs) out; items = lenses (Name :: Name "items")
buildings :: Has (Union xs) "buildings" out => Lens' (Union xs) out; buildings = lenses (Name :: Name "buildings")
slot :: Has (Union xs) "slot" out => Lens' (Union xs) out; slot = lenses (Name :: Name "slot")

initialGame :: Game
initialGame =
  sinsert (Tag latestVersion) $
  sinsert (Tag M.empty :: "items" :< M.Map String Double) $
  sinsert (Tag M.empty :: "unlocks" :< M.Map String Bool) $
  sinsert (Tag M.empty :: "buildings" :< M.Map String Int) $
  sinsert (Tag ([] :: [String])) $
  sinsert (Tag ([1] :: [Int])) $
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

unlockMap :: [Unlock]
unlockMap = [
  makeUnlock
    "IQ-test" "<i class=\"fa fa-pencil fa-fw\"></i>IQテストの導入" ["root"] []
    "IQテストによってIQを向上させます",
  makeUnlock
    "IQ-test-suppliment" "<i class=\"fa fa-paperclip fa-fw\"></i>サプリメントの導入" ["IQ-test"] [("test", 5000)]
    "サプリメントを導入してIQテストの効率を上げます",
  makeUnlock
    "make-IQ-test" "<i class=\"fa fa-paperclip fa-fw\"></i>IQテストの提供" ["IQ-test"] []
    "政府へレギュレーションに違反するIQテストを提供します<br />これにより国民の平均IQが下がります",
  makeUnlock
    "training-game" "<i class=\"fa fa-gamepad fa-fw\"></i>脳トレゲームの購入" ["root"] [("IQ", 200), ("test", 5000)]
    "脳トレゲームで脳を鍛えます",
  makeUnlock
    "make-training-game" "<i class=\"fa fa-gamepad fa-fw\"></i>脳トレゲームの作成" ["training-game"] []
    "間違った方法の脳トレゲームを作成します<br />購入した人のIQを下げます",
  makeUnlock
    "IQ-rps-tournament" "<i class=\"fa fa-hand-rock-o fa-fw\"></i>IQジャンケン大会I" ["root"] [("test", 10000)]
    "定期的にIQジャンケン大会を主催し、それに参加します<br /><strong>IQジャンケン</strong>: ジャンケンをしてIQが高いほうが勝利する",
  makeUnlock
    "IQ-rps-tournament-2" "<i class=\"fa fa-hand-peace-o fa-fw\"></i>IQジャンケン大会II" ["IQ-rps-tournament"] []
    "「IQジャンケン大会では、主催者以外はグーを出せない」というルールを追加します",
  makeUnlock
    "IQ-rps-tournament-3" "<i class=\"fa fa-hand-paper-o fa-fw\"></i>IQジャンケン大会III" ["IQ-rps-tournament-2"] []
    "「IQジャンケン大会では、主催者以外はパーを出さなければならない」というルールを追加します",
  makeUnlock
    "IQ-rps-tournament-4" "<i class=\"fa fa-hand-spock-o fa-fw\"></i>IQジャンケン大会IV" ["IQ-rps-tournament-3"] []
    "主催者は一度に3人とジャンケンができるようになります",
  makeUnlock
    "IQ-rps-tournament-4" "<i class=\"fa fa-hand-lizard-o fa-fw\"></i>IQジャンケン大会V" ["IQ-rps-tournament-4"] []
    "主催者は一度に9人とジャンケンができるようになります",
  makeUnlock
    "open-lab" "<i class=\"fa fa-flask fa-fw\"></i>研究所" ["root"] [("IQ", 10000)]
    "IQ向上の研究を行います",
  makeUnlock
    "discover-IQ-gene" "<i class=\"fa fa-search fa-fw\"></i>IQ遺伝子の発見" ["open-lab"] []
    "IQを決定するIQ遺伝子を研究します",
  makeUnlock
    "IQ-gene-creature" "<i class=\"fa fa-search fa-fw\"></i>IQ遺伝子組み換え" ["discover-IQ-gene"] []
    "IQ遺伝子を組み換えられた生命体を初めて実験的に作ります",
  makeUnlock
    "bio-gorilla" "<i class=\"fa fa-bomb fa-fw\"></i>IQバイオゴリラ(IQ105)" ["IQ-gene-creature"] []
    "「ジャングル IQヒクイモノ イキノコレナイ！」<br />IQバイオゴリラが自分よりIQが低い人間を粉砕する！<br />ジャングルでは低IQは命取りなのだ！",
  makeUnlock
    "low-IQ-food" "<i class=\"fa fa-bomb fa-fw\"></i>低IQ食品" ["IQ-gene-creature"] []
    "IQを下げる効果を持つ低カロリー食品を売り出します",
  makeUnlock
    "angel-help" "<i class=\"fa fa-heart-o fa-fw\"></i>天使の導き" ["root"] []
    "天使と契約し、その導きに従います",
  makeUnlock
    "angel-gear" "<i class=\"fa fa-gear fa-fw\"></i>光のギア" ["angel-help"] []
    "眩しい光を放つギアを手に入れます<br />ギアは触れた者のIQをわずかに上昇させます",
  makeUnlock
    "angel-machine" "<i class=\"fa fa-gears fa-fw\"></i>IQマシーン" ["angel-gear"] []
    "IQマシーンに触れた人間はIQが少し高くなります",
  makeUnlock
    "devil-help" "<i class=\"fa fa-heart fa-fw\"></i>悪魔の囁き" ["root"] []
    "悪魔と契約し、その囁きに従います",
  makeUnlock
    "devil-gear" "<i class=\"fa fa-gear fa-fw\"></i>闇のギア" ["devil-help"] []
    "吸い込まれそうな闇を湛えたギアを手に入れます<br />ギアは触れた者のIQをわずかに減少させます",
  makeUnlock
    "devil-machine" "<i class=\"fa fa-gears fa-fw\"></i>IQマシーン" ["devil-gear"] []
    "IQマシーンに触れた人間はIQが少し低くなります"
  ]

unlockTree :: T.Tree Unlock
unlockTree = buildTree unlockMap

buildingMap :: [Building]
buildingMap = [
  makeBuilding "test" "テスト結果" ["IQ-test"] (itvExp 0.05 2),
  makeBuilding "score" "脳トレゲーム" ["training-game"] (itvExp 0.1 1.6),
  makeBuilding "tournament" "IQジャンケン大会" ["IQ-rps-tournament"] (itvExp 0.3 1.41),
  makeBuilding "lab" "研究所" ["open-lab"] (itvExp 0.2 1.21)
  ]
  where
    itvExp base pow n = base * pow ^ (floor n)

itemMap :: [Item]
itemMap = [
  makeItem "IQ" "<i class=\"fa fa-fw fa-question-circle\"></i>IQ" "現在のあなたのIQです",
  makeItem "test" "<i class=\"fa fa-fw fa-pencil\"></i>テスト" "IQテストによってIQが上昇します",
  makeItem "score" "<i class=\"fa fa-fw fa-gamepad\"></i>スコア" "ゲームによって稼いだスコアです",
  makeItem "tournament" "<i class=\"fa fa-fw fa-hand-rock-o\"></i>勝利数" "IQジャンケンに勝った回数です",
  makeItem "lab" "<i class=\"fa fa-fw fa-flask\"></i>研究成果" "研究所の出した成果です"
  ]

achievementMap :: [Achievement]
achievementMap = [
  makeAchievement "<i class=\"fa fa-fw fa-question-circle\"></i>普通代表" "IQが100を越える" (\game -> game^.items^.at "IQ"^._MNum >= 100),
  makeAchievement "<i class=\"fa fa-fw fa-question-circle\"></i>天才" "IQが200を越える" (\game -> game^.items^.at "IQ"^._MNum >= 200),
  makeAchievement "<i class=\"fa fa-fw fa-question-circle\"></i>桁違いの天才" "IQが1000を越える" (\game -> game^.items^.at "IQ"^._MNum >= 1000)
  ]

iqPS :: Game -> Double
iqPS game = sqrt (game ^. items ^. at "test" ^. _MNum) / 1000

buildingPS :: String -> Game -> Double
buildingPS k game = fromIntegral (game ^. buildings ^. at k ^. _MNum) / 10

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

toolTipAttr :: String -> [String]
toolTipAttr t = [
  "data-toggle" -: "tooltip",
  "data-placement" -: "bottom",
  "title" -: t]

maininit :: IORef Game -> IO ()
maininit ref = void $ do
  displayItem
  displayUnlockShop
  displayBuilding
  displayUnlockTree
  displayAchievement
  displaySlot

  eval $ toJSString $ concat $ [
    "$('[data-toggle=\"tooltip\"]').tooltip({",
    "  'template': '<div class=\"tooltip\" role=\"tooltip\"><div class=\"tooltip-inner tooltip-card\"></div></div>',",
    "  'html': true",
    "});"]

  where
    _buttonWith t = "button" `tag` ([
      "type" -: "button",
      "class" -: "btn btn-sm btn-secondary-dark"] ++ t)
    _buttonTT as t = _buttonWith (toolTipAttr t ++ as)
    _buttonTTWith eid as = _buttonTT (["id" -: eid] ++ as)
    _div = "div" `tag` []

    displaySlot = do
      withElem "slot" $ \e -> do
        game <- readIORef ref
        setHTML e ""

        forM_ [0..length (game^.slot) - 1] $ \i -> do
          let cid = "caret-button-id-" ++ show (length (game^.slot) - 1 - i)
          appendHTML e $ "div" `tag` ["class" -: "slot-div"] $ concat [
            _p $ "a" `tag` ["id" -: (cid ++ "-up"), "role" -: "button", "class" -: "caret-button"] $ "<i class=\"fa fa-caret-up\"></i>",
            _p $ show $ (game^.slot) !! i,
            _p $ "a" `tag` ["id" -: (cid ++ "-down"), "role" -: "button", "class" -: "caret-button"] $ "<i class=\"fa fa-caret-down\"></i>"
            ]

        forM_ [0..length (game^.slot) - 1] $ \i -> do
          let cid = "caret-button-id-" ++ show i

          withElem (cid ++ "-up") $ \ebtn ->
            onEvent ebtn Click $ \_ -> do
              refStateT ref $ slot . _digits += (10 ^ i)
              displaySlot

          withElem (cid ++ "-down") $ \ebtn ->
            onEvent ebtn Click $ \_ -> do
              refStateT ref $ do
                use (slot . _digits) >>= \n -> when (n - (10^i) >= 0) $ do
                  slot . _digits -= (10 ^ i)
              displaySlot

      where
        _p = "p" `tag` ["class" -: "no-margin"]

    displayItem = do
      withElem "item-display-tbody" $ \e -> do
        game <- readIORef ref
        setHTML e ""

        forM_ itemMap $ \itm -> do
          appendHTML e $ concat $ [
            _trTT (itm^.description ++ "<hr />" ++ show (buildingPS (itm^.ident) game) ++ " /s") $ concat [
              _td $ itm^.name,
              _tdWith ["class" -: "text-xs-right", "id" -: ("item-display-" ++ (itm^.ident))] ""
              ]
            ]
      where
        _tdWith = tag "td"
        _td = _tdWith []
        _trTT t = "tr" `tag` (toolTipAttr t ++ ["id" -: "id-for-trTT"])

    displayUnlockShop = do
      withElem "unlock-shop-block" $ \e -> do
        game <- readIORef ref
        setHTML e ""

        forM_ unlockMap $ \unl -> do
          let uidbtn = "unlock-" ++ unl^.ident
          let uidspan = "unlock-span-" ++ unl^.ident
          when (M.notMember (unl^.ident) (game^.unlocks) || ((game^.unlocks) M.! (unl^.ident) == False)) $ do
            when (all (\x -> M.member x (game^.unlocks) && (game^.unlocks) M.! x) $ unl^.premise \\ ["root"]) $ do
              let abtn unl = "a" `tag` ["id" -: ("unlock-" ++ unl^.ident), "role" -: "button", "class" -: "btn btn-sm btn-secondary-dark disabled"] $ (unl ^. name)

              appendHTML e $ concat [
                "div" `tag` [] $
                  "div" `tag` (toolTipAttr (unl ^. description ++ "<hr />" ++ requirement unl) ++ ["id" -: uidspan, "style" -: "display: inline-block;"]) $
                  abtn unl,
                let lis = concat $ fmap (\u -> "li" `tag` [] $ abtn u) (getBranchBy (\a b -> a^.ident == b^.ident) unl unlockTree) in
                if lis == "" then "" else "ul" `tag` ["class" -: "tree-view"] $ lis
                ]

        forM_ unlockMap $ \unl -> do
          let uidbtn = "unlock-" ++ unl^.ident
          let uidspan = "unlock-span-" ++ unl^.ident
          when (M.notMember (unl^.ident) (game^.unlocks) || ((game^.unlocks) M.! (unl^.ident) == False)) $ do
            when (all (\x -> M.member x (game^.unlocks) && (game^.unlocks) M.! x) $ unl^.premise \\ ["root"]) $ do
              withElem uidbtn $ \ebtn -> void $ do
                onEvent ebtn Click $ \_ -> do
                  refStateT ref $ do
                    unlocks . at (unl^.ident) .= Just True
                    forM_ (unl^.cost) $ \(k,v) -> do
                      items . at k . _MNum -= fromIntegral v

                  withElem uidspan $ \espan -> do
                    adb <- getAttr espan "aria-describedby"
                    eval $ toJSString $ "$('#" ++ adb ++ "').hide();"

                  maininit ref
      where
        requirement unl = intercalate "<br />" $ fmap (\(k,v) -> k ++ ": " ++ show v) $ unl^.cost

    displayUnlockTree = do
      withElem "unlock-tree" $ \e -> do
        game <- readIORef ref
        setHTML e $ treesHTML (\unl -> _buttonTT [] (unl ^. description) (unl ^. name)) unlockTrees

      where
        treesHTML :: (Unlock -> String) -> [T.Tree Unlock] -> String
        treesHTML f xs = "ul" `tag` ["class" -: "tree-view"] $ concat $ fmap (treeHTML f) xs

        treeHTML :: (Unlock -> String) -> T.Tree Unlock -> String
        treeHTML f (T.Node x []) = "li" `tag` [] $ f x
        treeHTML f (T.Node x xs) = "li" `tag` [] $ concat [f x, "ul" `tag` [] $ concat $ fmap (treeHTML f) xs]

        unlockTrees = T.subForest unlockTree

    displayBuilding = do
      withElem "buildings-shop-ul" $ \e -> do
        game <- readIORef ref
        setHTML e ""
        let buildId s bm = "building-" ++ s ++ "-" ++ bm^.ident

        forM_ buildingMap $ \bm -> do
          when (all (\x -> M.member x (game^.unlocks) && (game^.unlocks) M.! x) (bm^.premise)) $ do
            lbl <- labelBuilding bm
            appendHTML e $ _li $
              "div" `tag` ["class" -: "row", "style" -: "padding: 0 1.25rem;"] $ concat [
                "div" `tag` ["class" -: "display-none", "id" -: buildId "progress-div" bm] $
                  "progress" `tag` ["id" -: buildId "progress" bm, "class" -: "progress progress-bar progress-bar", "value" -: "0", "max" -: "100"] $ "",
                "span" `tag` ["id" -: buildId "text" bm] $ concat [
                  (bm^.name), " ",
                  (show $ game ^. buildings ^. at (bm ^. ident) ^. _MNum)
                  ],
                "div" `tag` ["class" -: "pull-sm-right"] $
                  "button" `tag` ["id" -: buildId "btn" bm, "class" -: "btn btn-sm btn-main-dark"] $
                    "<i class=\"fa fa-plus-circle fa-fw\"></i>" ++ lbl
                ]

        forM_ buildingMap $ \bm -> do
          when (all (\x -> M.member x (game^.unlocks) && (game^.unlocks) M.! x) (bm^.premise)) $ do
            withElem (buildId "btn" bm) $ \ebtn -> void $ do
              onEvent ebtn Click $ \_ -> void $ do
                setAttr ebtn "disabled" "disabled"

                withElem (buildId "progress-div" bm) $ \eid -> do
                  setClass eid "display-none" False

                itv <- intervalBuilding bm
                withElem (buildId "progress" bm) $ \br -> do
                  let w = (1+) $ floor $ 100 / (10 * itv)
                  setAttr br "value" $ show w

                  eval $ toJSString $ concat [
                    "$('#" ++ buildId "progress" bm ++ "').animate({",
                    "value: \"100\"",
                    "}, " ++ show (itv * 1000) ++ ", \"linear\");"
                    ]

                setTimer (Once $ floor $ 1000 * itv) $ do
                  refStateT ref $ do
                    buildings . at (bm^.ident) . _MNum += 1

                  removeAttrById (buildId "btn" bm) "disabled"

                  withElem (buildId "progress-div" bm) $ \eid -> do
                    setClass eid "display-none" True

                  lbl <- labelBuilding bm
                  withElem (buildId "btn" bm) $ \ebtn -> do
                    setHTML ebtn $ ("<i class=\"fa fa-plus-circle fa-fw\"></i>" ++) $ lbl
                  withElem (buildId "text" bm) $ \etxt -> do
                    game <- readIORef ref
                    setHTML etxt $ concat $ [
                      (bm^.name), " ",
                      (show $ game ^. buildings ^. at (bm ^. ident) ^. _MNum)]

        html <- getProp e "innerHTML"
        when (html == "") $ do
          setHTML e $ _li "ここに施設が追加されます"

      where
        _li = "li" `tag` ["class" -: "list-group-item"]
        intervalBuilding bm = do
          game <- readIORef ref
          return $ bm^.interval $ fromIntegral $ game^.buildings^.at (bm^.ident)^._MNum
        labelBuilding bm = do
          game <- readIORef ref
          itv <- intervalBuilding bm
          return $ (printf "%0.2f" itv) ++ " s"

    displayAchievement = do
      withElem "trophy-list-div" $ \e -> do
        forM_ achievementMap $ \ach ->
          appendHTML e $
            "div" `tag` ["class" -: "col-lg-3 col-md-4"] $
              "span" `tag` (["class" -: "achievement-piece"] ++ toolTipAttr (ach^.description)) $ ach^.name

mainloop :: IORef Game -> IO ()
mainloop ref = do
  refStateT ref $ do
    game <- get
    items . at "IQ" . _MNum += iqPS game

  refStateT ref $ do
    game <- get
    forM_ (M.keys (game ^. buildings)) $ \bl -> do
      items . at bl . _MNum += buildingPS bl game

  refStateT ref $ do
    game <- get
    forM_ unlockMap $ \unl -> do
      let uidbtn = "unlock-" ++ unl^.ident
      when (M.notMember (unl^.ident) (game^.unlocks) || ((game^.unlocks) M.! (unl^.ident) == False)) $ do
        when (all (\x -> M.member x (game^.unlocks) && (game^.unlocks) M.! x) $ unl^.premise \\ ["root"]) $ do
          when (all (\(k,v) -> game ^. items ^. at k ^. _MNum >= fromIntegral v) $ unl^.cost) $ do
            withElem uidbtn $ \e -> do
              setClass e "disabled" False

  game <- readIORef ref

  forM_ itemMap $ \itm -> do
    let itmid = "item-display-" ++ itm^.ident
    withElem itmid $ \e -> do
      setHTML e $ show $ floor $ game ^. items ^. at (itm^.ident) ^. _MNum

  withElem "id-for-trTT" $ \e -> do
    setAttr e "data-original-title" $ "フレンドが多くなると楽しいですね<hr />" ++ printf "%.3f" (iqPS game) ++ " /s"

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

  maininit ref
  setTimer (Repeat 100) $ mainloop ref
  setTimer (Repeat 60000) $ save ref

save :: IORef Game -> IO ()
save ref = do
  setItem "FriendtheNet" =<< readIORef ref
  putAlert "セーブしました"

refStateT :: IORef s -> StateT s IO () -> IO ()
refStateT ref m = do
  writeIORef ref =<< execStateT m =<< readIORef ref
