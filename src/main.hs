{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts #-}
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
import Lens.Family2.Unchecked
import Lens.Family2.State.Lazy
import Control.Monad.State
import Data.IORef
import Data.List
import qualified Data.Map as M
import qualified Data.Tree as T
import qualified Data.Foldable as F
import Text.Printf (printf)

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
  "log" :< [String]
  ]

version :: Has (Union xs) "version" out => Lens' (Union xs) out; version = lenses (Name :: Name "version")
log :: Has (Union xs) "log" out => Lens' (Union xs) out; log = lenses (Name :: Name "log")
unlocks :: Has (Union xs) "unlocks" out => Lens' (Union xs) out; unlocks = lenses (Name :: Name "unlocks")
items :: Has (Union xs) "items" out => Lens' (Union xs) out; items = lenses (Name :: Name "items")
buildings :: Has (Union xs) "buildings" out => Lens' (Union xs) out; buildings = lenses (Name :: Name "buildings")

initialGame :: Game
initialGame =
  sinsert (Tag latestVersion) $
  sinsert (Tag M.empty :: "items" :< M.Map String Double) $
  sinsert (Tag M.empty :: "unlocks" :< M.Map String Bool) $
  sinsert (Tag M.empty :: "buildings" :< M.Map String Int) $
  sinsert (Tag []) $
  Union HNil

makeUnlock :: String -> String -> [String] -> [(String, Int)] -> String -> Unlock
makeUnlock i n p c d =
  sinsert (Tag i :: "ident" :< String) $
  sinsert (Tag n :: "name" :< String) $
  sinsert (Tag d :: "description" :< String) $
  sinsert (Tag p :: "premise" :< [String]) $
  sinsert (Tag c) $
  Union HNil

unlockMap :: [Unlock]
unlockMap = [
  makeUnlock
    "install-plugin" "<i class=\"fa fa-puzzle-piece fa-fw\"></i>プラグイン導入" ["root"] [("posts", 100)]
    "プラグインを導入します<br />外部アプリケーションを利用することができるようになります" ,
  makeUnlock
    "install-client" "<i class=\"fa fa-cube fa-fw\"></i>クライアント導入" ["install-plugin"] [("plugin", 100)]
    "新しいクライアントを導入します" ,
  makeUnlock
    "multiple-accounts" "<i class=\"fa fa-users fa-fw\"></i>サブアカウントの管理" ["install-client"] [("plugin", 5000)]
    "複数のアカウントを作成し・管理します<br /> \
    \ <strong>注意*</strong> 無闇に複数のアカウントを作成することは規約違反にあたります",
  makeUnlock
    "multiple-accounts-boost" "<i class=\"fa fa-user-plus fa-fw\"></i>アカウント作成効率I" ["multiple-accounts"] []
    "サブアカウントの作成効率を上げます",
  makeUnlock
    "post-bomb" "<i class=\"fa fa-commenting fa-fw\"></i>投稿効率I" ["install-client", "programming"] [("plugin", 10000), ("code", 7000)]
    "同じ内容のメッセージを一度に大量に投稿することができます<br /> \
    \ <strong>注意*</strong> 無闇に同じ内容の投稿を繰り返すことは規約違反にあたります",
  makeUnlock
    "quote-post" "<i class=\"fa fa-quote-left fa-fw\"></i>人気ポストの引用" ["install-client"] []
    "タイムラインの人気のポストを引用し、あなたのメッセージとして投稿します<br />これにより、あなたの投稿はより面白いポストで占められます<br /> \
    \ <strong>注意*</strong> 他人のメッセージを自分のものとして投稿することは規約違反にあたります",
  makeUnlock
    "connect-SNS" "<i class=\"fa fa-share-alt fa-fw\"></i>外部アプリ共有" ["install-plugin"] []
    "メッセージを他のSNSで共有できます",
  makeUnlock
    "invitation-mail" "<i class=\"fa fa-envelope fa-fw\"></i>招待メール" ["connect-SNS"] []
    "招待メールを送ります<br />新規ユーザーが増えます",
  makeUnlock
    "make-friends-newcomer" "<i class=\"fa fa-envelope fa-fw\"></i>新規参入フレンド" ["invitation-mail"] []
    "新規ユーザーとフレンドになります",
  makeUnlock
    "post-ads" "<i class=\"fa fa-bullhorn fa-fw\"></i>広告メッセージ" ["connect-SNS"] []
    "広告リンクを投稿し、クリック数に応じて報酬がもらえる制度を利用できます<br /> \
    \ <strong>注意*</strong> ターゲットにマッチしない広告はあまり効果がありません",
  makeUnlock
    "auto-ads" "<i class=\"fa fa-bullhorn fa-fw\"></i>広告の自動投稿" ["post-ads", "programming-1"] []
    "広告を自動で投稿します",
  makeUnlock
    "programming" "<i class=\"fa fa-laptop fa-fw\"></i>プログラミングI" ["root", "install-plugin"] [("plugin", 3000)]
    "簡単なプログラムを書いて、ある程度の処理を自動化できます",
  makeUnlock
    "auto-post" "<i class=\"fa fa-gear fa-fw\"></i>自動投稿I" ["programming", "install-plugin"] []
    "定期的にメッセージを投稿できます<br /> \
    \ <strong>注意*</strong> 短時間に大量の投稿をすることは規約違反にあたります",
  makeUnlock
    "programming-2" "<i class=\"fa fa-desktop fa-fw\"></i>プログラミングII" ["programming"] []
    "より高度なプログラムによって、たくさんの処理を一度に行います",
  makeUnlock
    "auto-create-account" "<i class=\"fa fa-user-plus fa-fw\"></i>自動新規アカウント生成" ["programming-2", "multiple-accounts"] []
    "一定時間ごとに新規アカウントを作成します",
  makeUnlock
    "programming-3" "<i class=\"fa fa-desktop fa-fw\"></i>プログラミングIII" ["programming-2"] []
    "さらに高度なプログラムによって、大量の処理を効率的に自動的に行います",
  makeUnlock
    "programmer" "<i class=\"fa fa-male fa-fw\"></i>プログラマの雇用" ["programming-2", "ads-post"] []
    "プログラマを雇用して、必要な作業を任せることが出来ます",
  makeUnlock
    "kiosk-shop" "<i class=\"fa fa-shopping-cart fa-fw\"></i>コンビニの利用" ["root", "post-ads"] []
    "必要なものをコンビニで購入します",
  makeUnlock
    "coffee-break" "<i class=\"fa fa-coffee fa-fw\"></i>コーヒーブレイク" ["kiosk-shop", "post-ads"] []
    "コーヒーブレイクをとります<br />仕事の生産性を上げることで施設の生成にかかる時間を短縮します<br /> \
    \ <strong>注意*</strong> コーヒーを短時間に大量に摂取することは健康に悪影響を及ぼします",
  makeUnlock
    "energy-drink" "<i class=\"fa fa-beer fa-fw\"></i>エナジードリンク" ["coffee-break", "post-ads"] []
    "エナジードリンクを飲みます<br />仕事の生産性を上げることで施設の生成にかかる時間をより短縮します<br /> \
    \ <strong>注意*</strong> エナジードリンクを短時間に大量に摂取することは健康に悪影響を及ぼします",
  makeUnlock
    "mother-will" "<i class=\"fa fa-eye fa-fw\"></i>母なるものの意志" ["root"] []
    "アカウントの統計情報を送信します<br /> \
    \ SNS全体の統計情報の一部にアクセスできるようになります"
  ]

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

unlockTree :: T.Tree Unlock
unlockTree = buildTree unlockMap

makeBuilding :: String -> String -> [String] -> (Double -> Double) -> Building
makeBuilding i n p v =
  sinsert (Tag i :: "ident" :< String) $
  sinsert (Tag n :: "name" :< String) $
  sinsert (Tag p :: "premise" :< [String]) $
  sinsert (Tag v :: "interval" :< (Double -> Double)) $
  Union HNil

buildingMap :: [Building]
buildingMap = [
  makeBuilding "plugin" "プラグイン" ["install-plugin"] (itvExp 0.2 1.21),
  makeBuilding "share" "シェア" ["connect-SNS"] (itvExp 0.3 1.21),
  makeBuilding "code" "コード" ["programming"] (itvExp 0.5 1.21),
  makeBuilding "account" "アカウント" ["multiple-accounts"] (itvExp 2 1.21)
  ]
  where
    itvExp base pow n = base * pow ^ (floor n)

makeItem :: String -> String -> String -> Item
makeItem i n d =
  sinsert (Tag i :: "ident" :< String) $
  sinsert (Tag n :: "name" :< String) $
  sinsert (Tag d :: "description" :< String) $
  Union HNil

itemMap :: [Item]
itemMap = [
  makeItem "friends" "<i class=\"fa fa-user fa-fw\"></i>フレンド" "フレンドが多くなると楽しいですね<hr />0 /s",
  makeItem "posts" "<i class=\"fa fa-comment fa-fw\"></i>投稿" "多くの投稿は多くのフレンドを集めます",
  makeItem "plugin" "<i class=\"fa fa-puzzle-piece fa-fw\"></i>プラグイン" "外部プラグインを利用します",
  makeItem "share" "<i class=\"fa fa-share-alt fa-fw\"></i>シェア" "外部SNSへシェアした回数です",
  makeItem "code" "<i class=\"fa fa-code fa-fw\"></i>コード" "プログラム技能を使って書いたコードの量です",
  makeItem "account" "<i class=\"fa fa-envelope fa-fw\"></i>アカウント" "アカウントの数です"
  ]

makeAchievement :: String -> String -> (Game -> Bool) -> Achievement
makeAchievement n d c =
  sinsert (Tag n) $
  sinsert (Tag d) $
  sinsert (Tag c) $
  Union HNil

achievementMap :: [Achievement]
achievementMap = [
  makeAchievement "<i class=\"fa fa-fw fa-user\"></i>マイ・フレンド" "フレンドの数が1を越える" (\game -> game^.items^.at "friends"^._MNum >= 1),
  makeAchievement "<i class=\"fa fa-fw fa-user\"></i>友達の輪" "フレンドの数が100を越える" (\game -> game^.items^.at "friends"^._MNum >= 100),
  makeAchievement "<i class=\"fa fa-fw fa-user\"></i>友達マスター" "フレンドの数が10000を越える" (\game -> game^.items^.at "friends"^._MNum >= 10000)
  ]

friendsPS :: Game -> Double
friendsPS game = sqrt (game ^. items ^. at "posts" ^. _MNum) / 1000

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
    items . at "friends" . _MNum += friendsPS game

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
    setAttr e "data-original-title" $ "フレンドが多くなると楽しいですね<hr />" ++ printf "%.3f" (friendsPS game) ++ " /s"

main = do
  s <- getItem "FriendtheNet"
  ref <- case ((s :: Either String JSON) >>= fromJSON :: Either String Game) of
    Left err -> do
      print err
      newIORef initialGame
    Right x -> do
      newIORef x

  withElem "message-post" $ \e ->
    onEvent e Click $ \_ -> do
      modifyIORef ref $ items . at "posts" . _MNum +~ 1

  withElem "btn-save" $ \e ->
    onEvent e Click $ \_ -> save ref

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
