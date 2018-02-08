{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnicodeSyntax       #-}

module MainW2 where

import           Control.Arrow ((***))
-- import           Control.Lens
import           Control.Monad ((<=<), join)
import           Control.Monad.Fix
import           Control.Lens
import           Data.Default
import           Data.Semigroup                         ((<>))
import           Data.Set                               (Set)
import qualified Data.Set                               as Set
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom                             hiding (mainWidget)
import           Reflex.Dom.Core                        (mainWidget)

import           Data.Finite
import           Data.Singletons
-- import           Data.Singletons.Decide
import           Data.Singletons.TypeLits
import qualified Data.Vector.Sized                      as V

-- import GHC.TypeLits

--------------------------------------------------------------------------------

import           Reflex.Dom.HTML5.Attrs                 as A
import           Reflex.Dom.HTML5.Elements              as E
import           Reflex.Dom.HTML5.Component.TableVS
import           Reflex.Dom.HTML5.Component.TableVSHo
import           Reflex.Dom.HTML5.Elements.Tabular      as T

--------------------------------------------------------------------------------

mainW ∷ JSM ()
mainW = mainWidget examples

--------------------------------------------------------------------------------

examples ∷ MonadWidget t m => m ()
examples = do
    eH1N $ text "Welcome to reflex-dom-htmlea-vs"
    intro
    caveats
    eH1N $ text "Vector-sized input"
    exampleVS1
    exampleVS2
    exampleVS3
    exampleVS4
    exampleVS5
    exampleVS6
    exampleVS7
    exampleVS8
    exampleVS9
    exampleVS10
    exampleVS11
    exampleVS12
    exampleVS13
    exampleVS14
    -- eH1N $ text "Vector-sized input, clean these up or remove"
    -- exampleVS1b
    -- exampleVS2b
    eH1N $ text "Another vector-sized thing (WIP - or to be removed)"
    exampleVSHo1

intro :: MonadWidget t m => m ()
intro =
    eDivN $ do
        ePN $ text "Here we give examples, on how to use table-components."
        ePN $ text $
          "Component-module defines different ways to " <>
          "construct, declare, initialize and use tables. " <>
          "Vector-Sized helps to ensure that header has the correct " <>
          "number of rows."


caveats :: MonadWidget t m => m ()
caveats = do
    eH2N $ text "Caveats"
    eDivN $ do
      ePN $ text "Renamings are possible..."
      ePN $ text $ "If you want to try these on android, then vector-sized "
        <> "version (mkTableVS) cannot be used "
        <> "(singletons-package has problems with cross-compiling). "
        <> "Those parts may get separated some day to another package. "


--------------------------------------------------------------------------------

showRes :: forall t m.  (Reflex t, DomBuilder t m, PostBuild t m
                        , MonadHold t m, MonadFix m)
        => TableState t -> m ()
showRes res = do
    let eTxt = (_activeStateElem . _teMe) <$> _tsTableEvent res
        eU = fmap _activeStateElem $
            coincidence $ _teMUp <$> _tsTableEvent res -- ::Event t (ActiveState t)
        dUD = _tsDynUpLDownR res
    dElm <- holdDyn ActEnone eTxt
    dUpElm <- holdDyn ActEnone eU
    -- dElm <- holdDyn def eTxt
    -- dUpElm <- holdDyn def eU
    ePN $ do
        text "Last (any) activity is on cell "
        dynText $ (T.pack . show) <$> dElm
        text " and entered cell "
        dynText $ (T.pack . show . _activeStateElem) <$> _tsDynEnter res
    ePN $ do
        text "Mousebutton is down is "
        dynText $ (T.pack . show) <$> _tsDynDOn res
        text " and mousebutton is up is "
        dynText $ (T.pack . show) <$> _tsDynUOn res
    ePN $ do
        let dDact = _tsDynDPress res
        let dUact = _tsDynURelease res
        text "Pressed down on "
        dynText $ (T.pack . show . _activeStateElem) <$> dDact
        text "."
        text " Pressdown cell is active is "
        eBN $ dynText $ (T.pack . show) <$> join (_activeStateActive <$> dDact)
        text " and activable is "
        eBN $ dynText $ (T.pack . show) <$> join (_activeStateActivable <$> dDact)
        text ". Released up on "
        dynText $ (T.pack . show . _activeStateElem) <$> dUact
        text "."
        text " Releaseup cell is active is "
        eBN $ dynText $ (T.pack . show) <$> join (_activeStateActive <$> dUact)
        text " and activable is "
        eBN $ dynText $ (T.pack . show) <$> join (_activeStateActivable <$> dUact)
        text ". (Releaseup should be same as "
        dynText $ (T.pack . show) <$> dUpElm
        text ", except when releasing btn outside table.)"
    ePN $ do
        text "upper left / lower right dyn is on "
        dynText $ (T.pack . show .  (_activeStateElem *** _activeStateElem)) <$> dUD
    let dOut = _tsDynMOutsideBody res
        dIn  = _tsDynMInsideBody res
    dLInt :: Dynamic t Int <- count $ mouseOutEvent res
    dEInt :: Dynamic t Int <- count $ mouseInEvent res
    ePN $ do
        text "dIn = "
        dynText  $ (T.pack . show) <$> dIn
        text " and dOut = "
        dynText  $ (T.pack . show) <$> dOut
    ePN $ do
        text "Next mouseEnter events: "
        dynText $ (T.pack . show) <$> dEInt
        text " and mouseLeave events: "
        dynText $ (T.pack . show) <$> dLInt


--------------------------------------------------------------------------------


-- drawDivContent2 fat _me elm actS = do
--     -- let dA = view activeStateActive actS
--     --     dNA = not <$> view activeStateActivable actS
--     --     dACl = view activeStateActiveCl actS
--     --     dNACl = view activeStateNotActiveCl actS
--     --     dNAvCl = view activeStateNotActivableCl actS
--     let dA = _activeStateActive actS
--         dNA = not <$> _activeStateActivable actS
--         dACl = _activeStateActiveGl actS
--         dNACl = _activeStateNotActiveGl actS
--         dNAvCl = _activeStateNotActivableGl actS
--         dUse = (\ba bna acl nacl navcl ->
--             let (st,cl)
--                     | bna = (style "background-color:darkgray", navcl)
--                     | ba  = (style "color: red", acl)
--                     | otherwise = (style "color: black", nacl)
--             in setClasses cl $ style "text-align: center" <> st $ def
--                 ) <$> dA <*> dNA <*> dACl <*> dNACl <*> dNAvCl
--     (e,_) <- eDivD' dUse $
--       text $ fat elm
--     pure e


--------------------------------------------------------------------------------

-- i elems on a row, j rows.
mkVSMat :: forall r c. (KnownNat r, KnownNat c)
        => V.Vector r (V.Vector c (Int,Int))
mkVSMat = V.generate mkA4r
  where
    mkA4r ∷ KnownNat c => Int → V.Vector c (Int,Int)
    mkA4r i = V.generate (\j -> (i,j))

mkChrMat :: forall r c. (KnownNat r, KnownNat c)
         => V.Vector r (V.Vector c (Char,Char))
mkChrMat =
  V.generate
    (\i -> V.generate
      (\j ->
        ((Prelude.!!) chrs i, (Prelude.!!) chrs j) ) )
  where
    chrs = ['a'..'z'] ++ ['A'..'Z'] :: [Char]


--------------------------------------------------------------------------------

exampleVS1 :: forall t m.
  (Reflex t, DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadJSM m
    , MonadFix m , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
    => m ()
exampleVS1 = do
   let colsS1 = toSing 3
       rowsS1 = toSing 4
   case (colsS1,rowsS1) of
       (SomeSing colS, SomeSing rowS) ->
           withKnownNat colS $
               withKnownNat rowS (exampleVS1_ rowS colS)

exampleVS1_ :: forall colS rowS t m.
  (KnownNat colS, KnownNat rowS, Reflex t, DomBuilder t m, PostBuild t m
    , TriggerEvent t m, MonadJSM m, MonadFix m, MonadHold t m
    , DomBuilderSpace m ~ GhcjsDomSpace)
    => Sing rowS -> Sing colS -> m ()
exampleVS1_ rowS colS = do
    let matelms :: (KnownNat colS, KnownNat rowS)
                => V.Vector rowS (V.Vector colS (Int,Int))
        matelms = mkVSMat
        tableConf = silentPlainTableConfVS
            & set tableTableAttrVS (style "border-collapse: collapse" def)
            & set (tableTdFunsVS . tdfTdAttr)
              (const $ style "padding: 3px; background-color: lightgrey" def)
    eH2N $ text "exampleVS1"
    ePN $ text $ "Using td-attributes without events nor cell-states. Note "
        <> "that result should not change when clicking or trying to select. "
        <> "Function silentPlainTableConf can be used when only styling is "
        <> "applied. "
    res :: TableState t <- mkTableVS tableConf matelms
    showRes res

--------------------------------------------------------------------------------

exampleVS2 :: forall t m.
  (Reflex t, DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadJSM m
    , MonadFix m , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
    => m ()
exampleVS2 = do
   let colsS1 = toSing 6
       rowsS1 = toSing 4
   case (colsS1,rowsS1) of
       (SomeSing colS, SomeSing rowS) ->
           withKnownNat colS $
               withKnownNat rowS (exampleVS2_ rowS colS)

exampleVS2_ :: forall colS rowS t m.
  (KnownNat colS, KnownNat rowS, Reflex t, DomBuilder t m, PostBuild t m
    , TriggerEvent t m, MonadJSM m, MonadFix m, MonadHold t m
    , DomBuilderSpace m ~ GhcjsDomSpace)
    => Sing rowS -> Sing colS -> m ()
exampleVS2_ rowS colS = do
    let cls = fromIntegral (fromSing colS)
        matelms :: (KnownNat colS, KnownNat rowS)
                => V.Vector rowS (V.Vector colS (Int,Int))
        matelms = mkVSMat
        tConf = set cellListenerBodyVS (listenMyRow cls)
            $ set tableTableAttrVS (style "border-collapse: collapse" def)
            $ set cellDrawBodyVS drawDivContentEx def
    eH2N $ text "exampleVS2"
    ePN $ text $ "Activating the whole row. No stylings, using draw-function "
        <> "example that shows part of the internal state. "
    res :: TableState t <- mkTableVS tConf matelms
    showRes res

--------------------------------------------------------------------------------

exampleVS3 :: forall t m.
  (Reflex t, DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadJSM m
    , MonadFix m , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
    => m ()
exampleVS3 = do
   let colsS1 = toSing 6
       rowsS1 = toSing 4
   case (colsS1,rowsS1) of
       (SomeSing colS, SomeSing rowS) ->
           withKnownNat colS $
               withKnownNat rowS (exampleVS3_ rowS colS)

exampleVS3_ :: forall colS rowS t m.
  (KnownNat colS, KnownNat rowS, Reflex t, DomBuilder t m, PostBuild t m
    , TriggerEvent t m, MonadJSM m, MonadFix m, MonadHold t m
    , DomBuilderSpace m ~ GhcjsDomSpace)
    => Sing rowS -> Sing colS -> m ()
exampleVS3_ rowS colS = do
    let cls = fromIntegral (fromSing colS)
        matelms :: (KnownNat colS, KnownNat rowS)
                => V.Vector rowS (V.Vector colS (Int,Int))
        matelms = mkVSMat
        tConf = def & set cellListenerBodyVS (listenMyRow cls)
            & set cellDrawBodyVS drawDivContentS
            & set tableTableAttrVS (style "border-collapse: collapse" def)
            & set (tableTdFunsVS . tdfTrAttr) trAttrfun
            & set (tableTdFunsVS . tdfTdAttr) (const $ style "padding: 5px" def)
    eH2N $ text "exampleVS3"
    ePN $ text "Coloring the whole row based on clicks with other decorations. "
    res :: TableState t <- mkTableVS tConf matelms
    showRes res


trAttrfun :: forall t. Reflex t
          => Dynamic t (ActiveState t) → ActElem → Dynamic t ETr
    -- First param is the last row where an event occurred (with state info)
    -- and the second parameter tells, which row we are making (drawing,
    -- building).
trAttrfun dAst _ae = mkETr <$> join (_activeStateActive <$> dAst)
  where
    mkETr :: Bool -> ETr
    mkETr b =
        if b
           then style "background-color: grey" def
           else style "background-color: lightgrey" def

--------------------------------------------------------------------------------


exampleVS4 :: forall t m.
  (Reflex t, DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadJSM m
    , MonadFix m , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
    => m ()
exampleVS4 = do
   let colsS1 = toSing 6
       rowsS1 = toSing 4
   case (colsS1,rowsS1) of
       (SomeSing colS, SomeSing rowS) ->
           withKnownNat colS $
               withKnownNat rowS (exampleVS4_ rowS colS)

exampleVS4_ :: forall colS rowS t m.
  (KnownNat colS, KnownNat rowS, Reflex t, DomBuilder t m, PostBuild t m
    , TriggerEvent t m, MonadJSM m, MonadFix m, MonadHold t m
    , DomBuilderSpace m ~ GhcjsDomSpace)
    => Sing rowS -> Sing colS -> m ()
exampleVS4_ rowS colS = do
    let rws = fromIntegral (fromSing rowS)
        matelms :: (KnownNat colS, KnownNat rowS)
                => V.Vector rowS (V.Vector colS (Int,Int))
        matelms = mkVSMat
        tConf = def & set cellListenerBodyVS (listenMyCol rws)
            & set cellDrawBodyVS drawDivContentS
            -- & set cellDrawBodyVS drawDivAECntEx
            & set tableTableAttrVS (style "border-collapse: collapse" def)
            & set (tableTdFunsVS . tdfTdAttr) myTdAttrF
    eH2N $ text "exampleVS4"
    ePN $ text "Coloring the whole column based on clicks with other decorations. "
    res :: TableState t <- mkTableVS tConf matelms
    showRes res
      where
        myTdAttrF :: ActiveState t -> Dynamic t ETd
        myTdAttrF ast = defTdAttrF
                  ( ast & set activeStateActiveGl (style "color: red" def)
                  & set activeStateNotActiveGl (style "color: blue" def))


--------------------------------------------------------------------------------


exampleVS5 :: forall t m.
  (Reflex t, DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadJSM m
    , MonadFix m , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
    => m ()
exampleVS5 = do
   let colsS1 = toSing 3
       rowsS1 = toSing 6
   case (colsS1,rowsS1) of
       (SomeSing colS, SomeSing rowS) ->
           withKnownNat colS $
               withKnownNat rowS (exampleVS5_ rowS colS)

exampleVS5_ :: forall colS rowS t m.
  (KnownNat colS, KnownNat rowS, Reflex t, DomBuilder t m, PostBuild t m
    , TriggerEvent t m, MonadJSM m, MonadFix m, MonadHold t m
    , DomBuilderSpace m ~ GhcjsDomSpace)
    => Sing rowS -> Sing colS -> m ()
exampleVS5_ rowS colS = do
    let rws = fromIntegral (fromSing rowS)
        cls = fromIntegral (fromSing colS)
        matelms :: (KnownNat colS, KnownNat rowS)
                => V.Vector rowS (V.Vector colS (Int,Int))
        matelms = mkVSMat
        -- We use our own version of drawDivContent. Note that the header input
        -- is Text-type so there is no need to show it, while cell-contents are
        -- int-pairs and there is a need for showing and packing.
        hns = set thfThAttr (const $ constDyn $ style "width: 100px" def) $
            set (thfADEs . drawEl) drawDivContent  $
            set (thfADEs . actListen) (listenMyCol rws) def
        fs = CommonADEfuns
            listenHeadFootMe actMU
            -- drawDivAECntEx
            drawDivContentS
            cellEvF
            -- ownListen actMU
        fns2 = def & set tdfADEs fs & set tdfTdAttr myTdAttrF
        -- Note that the use of our own td-attribute function will possibly
        -- override the settings given in the table conf (see the not activable
        -- configuration). It is quite easy to give overlapping definitions to
        -- the same things (e.g. td-attributes of cells) and it can lead to
        -- confusing situations...
        colVc :: KnownNat colS => Maybe (V.Vector colS (Dynamic t ECol))
        colVc = V.fromList $
            (\i -> constDyn $ if even i
                   then style "background-color: lightgrey" def
                   else style "background-color: white" def
            ) <$> [1..cls]
        txtVc :: KnownNat colS => Maybe (V.Vector colS Text)
        txtVc = V.fromList $ (\i -> "Col H " <> (T.pack . show ) i) <$> [1..cls]
        colDfs :: KnownNat colS => Maybe (HeaderConfVS colS t m)
        -- colDfs = Just $ HeaderConfVS hns colVc txtVc def
        colDfs = (\cV tV -> HeaderConfVS hns cV tV def) <$> colVc <*> txtVc
        capDfs = Just (CaptionConf "Table example with td-attrs" $
            style ("background-color: black; color: white; " <>
                   "font-weight: bold") def)
        -- tableConf = TableConf fns2 capDfs colDfs Nothing def tableEventsEnLe
        tableConf = set tableTdFunsVS fns2 $ set tableCaptionVS capDfs
            $ set tableHeaderVS colDfs
            $ set tableActivityConfVS (constDyn $ Just [astNotActivable])
            def
    eH2N $ text "exampleVS5"
    ePN $ text $ "Using td-attributes with the events and cell-states. The first "
        <> "row is not activable. "
    res :: TableState t <- mkTableVS tableConf matelms
    showRes res
      where
        myTdAttrF :: ActiveState t -> Dynamic t ETd
        myTdAttrF ast = defTdAttrF
                  ( ast & set activeStateActiveGl (style "color: red" def)
                  & set activeStateNotActiveGl (style "color: black" def)
                  & set activeStateNotActivableGl
                    (style "color: white; background-color: darkgray" def))
        astNotActivable = def
            & set activeStateElem (ActErow 0)
            & set activeStateActivable (constDyn False)
            & set activeStateNotActivableGl (style "background-color: blue" def)

--------------------------------------------------------------------------------

exampleVS6 :: forall t m.
  (Reflex t, DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadJSM m
    , MonadFix m , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
    => m ()
exampleVS6 = do
   let colsS1 = toSing 3
       rowsS1 = toSing 8
   case (colsS1,rowsS1) of
       (SomeSing colS, SomeSing rowS) ->
           withKnownNat colS $
               withKnownNat rowS (exampleVS6_ rowS colS)

exampleVS6_ :: forall colS rowS t m.
  (KnownNat colS, KnownNat rowS, Reflex t, DomBuilder t m, PostBuild t m
    , TriggerEvent t m, MonadJSM m, MonadFix m, MonadHold t m
    , DomBuilderSpace m ~ GhcjsDomSpace)
    => Sing rowS -> Sing colS -> m ()
exampleVS6_ rowS colS = do
    let rws = fromIntegral (fromSing rowS)
        cls = fromIntegral (fromSing colS)
        matelms :: (KnownNat colS, KnownNat rowS)
                => V.Vector rowS (V.Vector colS (Int,Int))
        matelms = mkVSMat
        fs = CommonADEfuns
            listenMe  -- each cell listens only events happening on it
            actMU     -- and change states on mouse up -events
            drawDivActElemEx -- how to draw/make each cell
            cellEvF    -- how to construct the events cell is tracking/using
        fns = def { _tdfADEs = fs } :: TdFuns t m (Int,Int)
        colVec :: V.Vector colS (Dynamic t ECol)
        colVec = V.generate $
          (\i -> constDyn $
                  if even i
                     then style "background-color: lightgrey" def
                     else style "background-color: darkgrey" def
          )
        txtVec :: Maybe (V.Vector colS Text)
        txtVec = V.fromList $ (\i -> "col h" <> (T.pack . show) i ) <$> [1..cls]
        txtSumVec :: Maybe (V.Vector colS Text)
        txtSumVec = V.fromList $ (\i -> "sum " <> (T.pack . show) i ) <$> [1..cls]
        ades = CommonADEfuns listenMe actMU drawDivAECntEx cellEvF
            :: CommonADEfuns t m Text
        colDfs :: Maybe (HeaderConfVS colS t m)
        colDfs = (\sV -> HeaderConfVS (def {_thfADEs = ades}) colVec sV def)
          <$> txtVec
        sumDfs :: Maybe (FootConfVS colS t m)
        sumDfs = (\sV -> FootConfVS (def {_tfootADEs = ades}) sV def) <$> txtSumVec
        capDfs = Just (CaptionConf "A table example" $
            style ("background-color: black; color: white; " <>
                   "letter-spacing: 2px; font-weight: bold")
            def)
        tableConf = set tableTdFunsVS fns $ set tableCaptionVS capDfs
            $ set tableHeaderVS colDfs $ set tableFooterVS sumDfs def
    eH2N $ text "exampleVS6"
    ePN $ text $ "Colgroup usage " <>
        "and straigthforward decorations. Yet another way to listen events. "
    res :: TableState t <- mkTableVS tableConf matelms
    showRes res



--------------------------------------------------------------------------------

exampleVS7 :: forall t m.
  (Reflex t, DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadJSM m
    , MonadFix m , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
    => m ()
exampleVS7 = do
   let colsS1 = toSing 3
       rowsS1 = toSing 3
   case (colsS1,rowsS1) of
       (SomeSing colS, SomeSing rowS) ->
           withKnownNat colS $
               withKnownNat rowS (exampleVS7_ rowS colS)

exampleVS7_ :: forall colS rowS t m.
  (KnownNat colS, KnownNat rowS, Reflex t, DomBuilder t m, PostBuild t m
    , TriggerEvent t m, MonadJSM m, MonadFix m, MonadHold t m
    , DomBuilderSpace m ~ GhcjsDomSpace)
    => Sing rowS -> Sing colS -> m ()
exampleVS7_ rowS colS = do
    let matelms :: (KnownNat colS, KnownNat rowS)
                => V.Vector rowS (V.Vector colS (Char,Char))
        matelms = mkChrMat
        fs = CommonADEfuns
            listenMe  -- each cell listens only events happening on it
            actSwitchMU -- we want state switching cells
            drawDivContentEx -- we draw only the content into the div's
            cellEvF
        fns = def & set tdfADEs fs & set tdfTdAttr myTdAttrF
            :: TdFuns t m (Char,Char)
        tableConf = set tableTdFunsVS fns def
    eH2N $ text "exampleVS7"
    ePN $ text "State switching (click on different cells)."
    res :: TableState t <- mkTableVS tableConf matelms
    showRes res
      where
        myTdAttrF :: ActiveState t -> Dynamic t ETd
        myTdAttrF ast = defTdAttrF
              (ast & set activeStateActiveGl (style "border: 3px solid purple" def)
              & set activeStateNotActiveGl (style "border: 3px solid purple" def))



--------------------------------------------------------------------------------

exampleVS8 :: forall t m.
  (Reflex t, DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadJSM m
    , MonadFix m , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
    => m ()
exampleVS8 = do
   let colsS1 = toSing 4
       rowsS1 = toSing 4
   case (colsS1,rowsS1) of
       (SomeSing colS, SomeSing rowS) ->
           withKnownNat colS $
               withKnownNat rowS (exampleVS8_ rowS colS)

exampleVS8_ :: forall colS rowS t m.
  (KnownNat colS, KnownNat rowS, Reflex t, DomBuilder t m, PostBuild t m
    , TriggerEvent t m, MonadJSM m, MonadFix m, MonadHold t m
    , DomBuilderSpace m ~ GhcjsDomSpace)
    => Sing rowS -> Sing colS -> m ()
exampleVS8_ rowS colS = do
    let matelms :: (KnownNat colS, KnownNat rowS)
                => V.Vector rowS (V.Vector colS (Int,Int))
        matelms = mkVSMat
        fs = CommonADEfuns
            listenMe
            actAreaMDUsel
            drawDivActElemEx -- we draw only the "cell meta info" into the div's
            cellEvF
        fns = def & set tdfADEs fs & set tdfTdAttr myTdAttrF :: TdFuns t m (Int,Int)
        tableConf = set tableTdFunsVS fns def
    eH2N $ text "exampleVS8"
    ePN $ text "State selection on a user selected area (single area)."
    res :: TableState t <- mkTableVS tableConf matelms
    showRes res
      where
        myTdAttrF :: ActiveState t -> Dynamic t ETd
        myTdAttrF ast = defTdAttrF
              (ast & set activeStateActiveGl (style "border: 3px solid cyan" def)
              & set activeStateNotActiveGl (style "border: 3px solid cyan" def))


--------------------------------------------------------------------------------

exampleVS9 :: forall t m.
  (Reflex t, DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadJSM m
    , MonadFix m , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
    => m ()
exampleVS9 = do
   let colsS1 = toSing 4
       rowsS1 = toSing 4
   case (colsS1,rowsS1) of
       (SomeSing colS, SomeSing rowS) ->
           withKnownNat colS $
               withKnownNat rowS (exampleVS9_ rowS colS)

exampleVS9_ :: forall colS rowS t m.
  (KnownNat colS, KnownNat rowS, Reflex t, DomBuilder t m, PostBuild t m
    , TriggerEvent t m, MonadJSM m, MonadFix m, MonadHold t m
    , DomBuilderSpace m ~ GhcjsDomSpace)
    => Sing rowS -> Sing colS -> m ()
exampleVS9_ rowS colS = do
    let matelms :: (KnownNat colS, KnownNat rowS)
                => V.Vector rowS (V.Vector colS (Int,Int))
        matelms = mkVSMat
        fs = CommonADEfuns
            listenMe  -- each cell listens only events happening on it
            actSwitchMDUsel
            drawDivActElemEx
            cellEvF
        fns = def { _tdfADEs = fs } & set tdfTdAttr myTdAttrF
            :: TdFuns t m (Int,Int)
        tableConf = set tableTdFunsVS fns def
    eH2N $ text "exampleVS9"
    ePN $ text "State swithing on a user selected area (multiple, overlapping)"
    res :: TableState t <- mkTableVS tableConf matelms
    showRes res
      where
        myTdAttrF :: ActiveState t -> Dynamic t ETd
        myTdAttrF ast = defTdAttrF
              (ast & set activeStateActiveGl (style "border: 3px solid cyan" def)
              & set activeStateNotActiveGl (style "border: 3px solid cyan" def))


--------------------------------------------------------------------------------


exampleVS10 :: forall t m.
  (Reflex t, DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadJSM m
    , MonadFix m , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
    => m ()
exampleVS10 = do
   let colsS1 = toSing 5
       rowsS1 = toSing 3
   case (colsS1,rowsS1) of
       (SomeSing colS, SomeSing rowS) ->
           withKnownNat colS $
               withKnownNat rowS (exampleVS10_ rowS colS)

exampleVS10_ :: forall colS rowS t m.
  (KnownNat colS, KnownNat rowS, Reflex t, DomBuilder t m, PostBuild t m
    , TriggerEvent t m, MonadJSM m, MonadFix m, MonadHold t m
    , DomBuilderSpace m ~ GhcjsDomSpace)
    => Sing rowS -> Sing colS -> m ()
exampleVS10_ rowS colS = do
    let cls = fromIntegral (fromSing colS)
        matelms :: (KnownNat colS, KnownNat rowS)
                => V.Vector rowS (V.Vector colS (Int,Int))
        matelms = mkVSMat
        fs = CommonADEfuns
            listenMe  -- each cell listens only events happening on it
            actMU     -- and just mouse up -events
            drawDivActElemEx   -- how to draw/make each cell
            cellEvF   -- how to construct the events cell is tracking/using
        fns = TdFuns fs (exTdElm cls) myTdAttrF (const def) def
        tableConf = def
          & set tableTdFunsVS fns
          & set tableTableAttrVS (style "border: 3px solid purple" def)
    eH2N $ text "exampleVS10"
    ePN $ text $ "Click and enter events, custom elements inside td. "
        <> "Rightmost cell is listening the cells on the left but doesn't deliver "
        <> "events. "
    res :: TableState t <- mkTableVS tableConf matelms
    showRes res
      where
        myTdAttrF :: ActiveState t -> Dynamic t ETd
        myTdAttrF ast = defTdAttrF
              (ast & set activeStateActiveGl (style "border: 3px solid purple" def)
              & set activeStateNotActiveGl (style "border: 3px solid purple" def))
        -- See tdComb for the signature. This has one extra param to give
        -- the length of a vector.
        exTdElm :: (Reflex t, MonadHold t m, TriggerEvent t m
                   , PostBuild t m, DomBuilder t m,  MonadJSM m
                   , DomBuilderSpace m ~ GhcjsDomSpace)
          => Int -> TdFuns t m a
          -> Dynamic t (Maybe [ActiveState t])
          -> ActiveState t -> a -> Event t (TableEvent t)
          -> TableState t -> m (TableEvent t)
        exTdElm vl tdFuns ae me ipair eTB tblSt = do
            let aeMe = view activeStateElem me
                ActERC (x,y) = aeMe
            if y < (vl-1)
               then tdComb tdFuns ae me ipair eTB tblSt
               else do
                   -- here we make an extra td that listens to the others
                   let aercs = ActERC <$> [(x,0), (x,1), (x,2), (x,3)]
                       tdades = _tdfADEs tdFuns
                       tdF = set (tdfADEs . actListen)
                                 (listenListNotMe aercs) tdFuns
                   tdCombLstner tdF ae me ipair eTB tblSt


--------------------------------------------------------------------------------

exampleVS11 :: forall t m.
  (Reflex t, DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadJSM m
    , MonadFix m , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
    => m ()
exampleVS11 = do
   let colsS1 = toSing 4
       rowsS1 = toSing 4
   case (colsS1,rowsS1) of
       (SomeSing colS, SomeSing rowS) ->
           withKnownNat colS $
               withKnownNat rowS (exampleVS11_ rowS colS)

exampleVS11_ :: forall colS rowS t m.
  (KnownNat colS, KnownNat rowS, Reflex t, DomBuilder t m, PostBuild t m
    , TriggerEvent t m, MonadJSM m, MonadFix m, MonadHold t m
    , DomBuilderSpace m ~ GhcjsDomSpace)
    => Sing rowS -> Sing colS -> m ()
exampleVS11_ rowS colS = do
    let cls = fromIntegral (fromSing colS)
        rws = fromIntegral (fromSing colS)
        matelms :: (KnownNat colS, KnownNat rowS)
                => V.Vector rowS (V.Vector colS (Int,Int))
        matelms = mkVSMat
        hns = set (thfADEs . actListen) (listenMyCol rws)
            $ set (thfADEs . drawEl) drawDivAECntEx def
        ades = CommonADEfuns (listenMyCol rws) actMU drawDivAECntEx cellEvF
        foots = def {_tfootADEs = ades}
        fs = CommonADEfuns
            listenHeadFootMe -- each cell listens head, foot, and itself
            actMU            -- and change states on mouse up -events
            drawDivActElemEx -- how to draw/make each cell
            cellEvF          -- how to construct the events cell is tracking/using
        fns = def { _tdfADEs = fs } :: TdFuns t m (Int,Int)
        colVec :: Maybe (V.Vector colS (Dynamic t ECol))
        colVec = V.fromList $
            (\i -> constDyn $ if even i
                   then style "background-color: lightgrey" def
                   else style "background-color: darkgrey" def
            ) <$> [1..cls]
        txtVec :: Maybe (V.Vector colS Text)
        txtVec = V.fromList $ (\i -> "Col H " <> (T.pack . show ) i) <$> [1..cls]
        colDfs :: Maybe (HeaderConfVS colS t m)
        colDfs = (\cV tV -> HeaderConfVS hns cV tV def) <$> colVec <*> txtVec
        txtSumVec :: Maybe (V.Vector colS Text)
        txtSumVec = V.fromList $ (\i -> "Sum " <> (T.pack . show ) i) <$> [1..cls]
        sumDfs :: Maybe (FootConfVS colS t m)
        sumDfs = (\tV -> FootConfVS foots tV def) <$> txtSumVec
        tableConf = set tableTdFunsVS fns $ set tableHeaderVS colDfs
            $ set tableFooterVS sumDfs def
    eH2N $ text "exampleVS11"
    ePN $ text "Header events, clicking on header or footer select a column. "
    res :: TableState t <- mkTableVS tableConf matelms
    showRes res


--------------------------------------------------------------------------------

exampleVS12 :: forall t m.
  (Reflex t, DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadJSM m
    , MonadFix m , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
    => m ()
exampleVS12 = do
   let colsS1 = toSing 3
       rowsS1 = toSing 4
       rNum = Just 1 :: Maybe Int
   case (colsS1,rowsS1) of
       (SomeSing colS, SomeSing rowS) ->
           withKnownNat colS $
               withKnownNat rowS (exampleVS12_ rowS colS rNum)

exampleVS12_ :: forall colS rowS t m.
  (KnownNat colS, KnownNat rowS, Reflex t, DomBuilder t m, PostBuild t m
    , TriggerEvent t m, MonadJSM m, MonadFix m, MonadHold t m
    , DomBuilderSpace m ~ GhcjsDomSpace)
    => Sing rowS -> Sing colS -> Maybe Int -> m ()
exampleVS12_ rowS colS rNum = do
    let cls = fromIntegral (fromSing colS)
        rws = fromIntegral (fromSing colS)
        matelms :: (KnownNat colS, KnownNat rowS)
                => V.Vector rowS (V.Vector colS (Int,Int))
        matelms = mkVSMat
    rec
        let
            dAstInit = (\mi -> Just
                        [ def & set activeStateElem
                            (case mi of
                                Just i  -> ActErow i
                                Nothing -> ActEnone)
                          & set activeStateActive (constDyn True)
                          -- & set activeStateActivable (constDyn False)
                          -- & set activeStateActiveGl
                          --     (style "background-color:grey" def)
                          -- & set activeStateNotActiveGl
                          --     (style "background-color: lightgrey" def)
                        -- , def
                        --   & set activeStateActiveGl
                        --       (style "background-color:grey" def)
                        --   & set activeStateNotActiveGl
                        --       (style "background-color: lightgrey" def)
                        ]) <$> dRow
            tConf = def & set cellListenerBodyVS (listenMyRow cls)
                & set cellDrawBodyVS drawDivContentS
                -- & set cellDrawBodyVS drawDivAECntEx
                & set tableActivityConfVS dAstInit
                & set tableTableAttrVS (style "border-collapse: collapse" def)
                & set (tableTdFunsVS . tdfTrAttr) trAttrfun
        eH2N $ text "exampleVS12"
        ePN $ text $ "Coloring the whole row based on clicks with other "
            <> "decorations. "
            <> "This is otherwise the same as example 3, but initializes a row. "
        res :: TableState t <- mkTableVS tConf matelms
        let dUpAst = view tsDynURelease res
            dAE = _activeStateElem <$> dUpAst
            dRow2 = rowNum <$> dAE
            eRow = updated dRow2
        dRow <- holdDyn rNum eRow
    showRes res


--------------------------------------------------------------------------------

exampleVS13 :: forall t m.
  (Reflex t, DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadJSM m
    , MonadFix m , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
    => m ()
exampleVS13 = do
   let colsS1 = toSing 3
       rowsS1 = toSing 5
   case (colsS1,rowsS1) of
       (SomeSing colS, SomeSing rowS) ->
           withKnownNat colS $
               withKnownNat rowS (exampleVS13_ rowS colS )

exampleVS13_ :: forall colS rowS t m.
  (KnownNat colS, KnownNat rowS, Reflex t, DomBuilder t m, PostBuild t m
    , TriggerEvent t m, MonadJSM m, MonadFix m, MonadHold t m
    , DomBuilderSpace m ~ GhcjsDomSpace)
    => Sing rowS -> Sing colS -> m ()
exampleVS13_ rowS colS = do
    let cls = fromIntegral (fromSing colS)
        matelms :: (KnownNat colS, KnownNat rowS)
                => V.Vector rowS (V.Vector colS (Int,Int))
        matelms = mkVSMat
    let
        dAstInit = constDyn $ Just
                    [ def
                        & set activeStateActiveGl
                            (style "background-color:grey" def)
                        & set activeStateNotActiveGl
                            (style "background-color: lightgrey" def)
                    ]
        tConf = def & set cellListenerBodyVS (listenMyRow cls)
            & set cellDrawBodyVS drawDivContentS
            -- & set cellDrawBodyVS drawDivAECntEx
            & set cellActivityBodyVS actSwitchMU
            & set tableActivityConfVS dAstInit
            & set tableTableAttrVS (style "border-collapse: collapse" def)
            & set (tableTdFunsVS . tdfTrAttr) trAttrfun
    eH2N $ text "exampleVS13"
    ePN $ text "Row selection with switching states."
    res :: TableState t <- mkTableVS tConf matelms
    -- let dUpAst = view tsDynURelease res
    --     dAE = _activeStateElem <$> dUpAst
    --     dRow2 = rowNum <$> dAE
    --     eRow = updated dRow2
    -- dRow <- holdDyn rNum eRow
    showRes res



--------------------------------------------------------------------------------

exampleVS14 :: forall t m.
  (Reflex t, DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadJSM m
    , MonadFix m , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
    => m ()
exampleVS14 = do
   let colsS1 = toSing 10
       rowsS1 = toSing 3
   case (colsS1,rowsS1) of
       (SomeSing colS, SomeSing rowS) ->
           withKnownNat colS $
               withKnownNat rowS (exampleVS14_ rowS colS )

exampleVS14_ :: forall colS rowS t m.
  (KnownNat colS, KnownNat rowS, Reflex t, DomBuilder t m, PostBuild t m
    , TriggerEvent t m, MonadJSM m, MonadFix m, MonadHold t m
    , DomBuilderSpace m ~ GhcjsDomSpace)
    => Sing rowS -> Sing colS -> m ()
exampleVS14_ rowS colS = do
    let rws = fromIntegral (fromSing rowS)
        matelms :: (KnownNat colS, KnownNat rowS)
                => V.Vector rowS (V.Vector colS (Int,Int))
        matelms = mkVSMat
        cNum = Just 3 :: Maybe Int
    rec
        let
            dAstInit = (\mi -> Just
                        [ def & set activeStateElem
                            (case mi of
                                Just i  -> ActEcols i
                                Nothing -> ActEnone)
                          & set activeStateActive (constDyn True)
                        ]) <$> dCol
            tConf = def & set cellListenerBodyVS (listenMyCol rws)
                & set cellDrawBodyVS drawDivContentS
                -- & set cellDrawBodyVS drawDivAECntEx
                & set tableActivityConfVS dAstInit
                -- & set tableTableAttrVS (style "border-collapse: collapse" def)
                & set (tableTdFunsVS . tdfTdAttr) myTdAttrF
        eH2N $ text "exampleVS14"
        ePN $ text "Coloring the whole column based with initial selected column. "
        res :: TableState t <- mkTableVS tConf matelms
        let dUpAst = view tsDynURelease res
            eCol = updated $ (colNum . _activeStateElem ) <$> dUpAst
        dCol <- holdDyn cNum eCol
    showRes res
      where
        myTdAttrF :: ActiveState t -> Dynamic t ETd
        myTdAttrF ast = defTdAttrF
                  ( ast & set activeStateActiveGl (style "color: red" def)
                  & set activeStateNotActiveGl (style "color: blue" def))



--------------------------------------------------------------------------------

{-

exampleVS1b :: forall t m. (MonadWidget t m) => m ()
exampleVS1b = do
   let colsS1 = toSing 4
       rowsS1 = toSing 8
   case (colsS1,rowsS1) of
       (SomeSing colS, SomeSing rowS) ->
           withKnownNat colS $
               withKnownNat rowS (exampleVS1b_ rowS colS)

exampleVS1b_ :: forall colS rowS t m. (KnownNat colS, KnownNat rowS
                                     , MonadWidget t m)
            => Sing rowS -> Sing colS -> m ()
exampleVS1b_ rowS colS = do
   let
       rws = fromIntegral (fromSing rowS)
       -- hns = set (thfADEs . actListen) (listenMyCol rws) defaultThFuns
       -- hnsades = _thfADEs defaultThFuns
       hnsades = _thfADEs def
       -- hns = defaultThFuns {_thfADEs = hnsades {_actListen = listenMyCol rws}}
       hns = def {_thfADEs = hnsades {_actListen = listenMyCol rws}}
       fs = CommonADEfuns
           listenHeadMe  -- each cell listens head and itself
           actMU     -- and change states on mouse up -events
           drawDivActElemEx -- how to draw/make each cell
           cellEvF
       -- fns = defTdFuns { _tdfADEs = fs }
       fns = def { _tdfADEs = fs }
       -- actelems :: V.Vector 4 (V.Vector 4 (Int,Int))
       actelems :: (KnownNat colS, KnownNat rowS)
                => V.Vector rowS (V.Vector colS (Int,Int))
       actelems = mkVSMat
       colVec :: KnownNat colS => V.Vector colS (Dynamic t ECol)
       colVec = V.generate
           (\i -> constDyn $ if even i
                  then style "background-color: yellow" def
                  else style "background-color: green" def)
                  -- then style "background-color: lightgrey" def
                  -- else style "background-color: darkgrey" def)
       txtVec :: KnownNat colS => V.Vector colS Text
       txtVec = V.generate (\i -> "Col H " <> (T.pack . show ) i)
       colDfs :: KnownNat colS => Maybe (HeaderConfVS colS t m)
       colDfs = Just $ HeaderConfVS hns colVec txtVec def
       -- colDfs :: KnownNat colS => HeaderConfVS colS t m
       -- colDfs = ColHeaderVS colVec txtVec def
       tableConf = def
                 & set tableTdFunsVS fns
                 & set tableHeaderVS colDfs
   eH2N $ text "exampleVS1b"
   ePN $ text "Header events, vector-sized inputs "
   -- res :: TableState t <- mkTableVS fns Nothing actelems
   -- If using above, hns should not be defined.
   res :: TableState t
       -- <- mkTableVS fns Nothing (Just (hns,colDfs)) Nothing actelems
       <- mkTableVS tableConf actelems
   -- res :: TableState t
   --     <- mkTableVS_ rowS colS fns (Just (hns,colDfs)) actelems
   --     <- mkTableVS_ rowS colS tableConf actelems
   showRes res

--------------------------------------------------------------------------------

exampleVS2b :: forall t m. (MonadWidget t m) => m ()
exampleVS2b = do
   let colsS1 = toSing 4
       rowsS1 = toSing 6
   case (colsS1,rowsS1) of
       (SomeSing colS, SomeSing rowS) ->
           withKnownNat colS $
               withKnownNat rowS (exampleVS2b_ rowS colS)

exampleVS2b_ :: forall colS rowS t m. (KnownNat colS, KnownNat rowS
                                     , MonadWidget t m)
            => Sing rowS -> Sing colS -> m ()
exampleVS2b_ rowS colS = do
   eH2N $ text "exampleVS2b"
   ePN $ text "Header and footer events, vector-sized inputs, use of class-attrs."
   let tableConf = def
                 & set tableTdFunsVS fns
                 & set tableCaptionVS capDfs
                 & set tableHeaderVS colDfs
                 & set tableFooterVS sumDfs
   -- res <- mkTableVS fns capDfs (Just (hns,colDfs)) fPair actelms
   res <- mkTableVS tableConf actelms
   -- tableConf = set tableTdFunsV fns $ set tableCaptionV capDfs
   --   $ set tableHeaderV colDfs $ set tableFooterV sumDfs def

   showRes res
     where
       rws = fromIntegral (fromSing rowS)
       -- hns = set thfThAttr (const $ constDyn
       --         $ style "width: 80px" def) $
       --     set (thfADEs . drawEl) (drawDivContent2 id)  $
       --     set (thfADEs . actListen) (listenMyCol rws) defaultThFuns
       hns :: ThFuns t m
       hns = def
           -- { _thfThAttr = const $ constDyn $ style "width: 80px" def
           -- , _thfADEs = def & set drawEl drawDivActElemEx
           --              & set actListen (listenMyCol rws)
           -- }
       fs = CommonADEfuns listenHeadFootMe actMU -- select a single cell
              drawDivActElemEx cellEvF
       -- fs = CommonADEfuns (listenMyCol rws) actMU -- select the whole row
           -- (drawDivContent2 (T.pack . show) ) cellEvF
       fns = def {_tdfADEs = fs }
       -- fns = defaultTdFuns {_tdfADEs = fs }
       -- fns = set tdfADEs fs defaultTdFuns
       actelms :: (KnownNat colS, KnownNat rowS)
                => V.Vector rowS (V.Vector colS (Int,Int))
       actelms = mkVSMat
       colVec :: KnownNat colS => V.Vector colS (Dynamic t ECol)
       colVec = V.generate
           (\i -> constDyn $ if even i
                  then style "background-color: lightgrey" def
                  else style "background-color: darkgrey" def)
       txtVc :: KnownNat colS => V.Vector colS Text
       txtVc = V.generate (\i -> "Col H " <> (T.pack . show ) i)
       colDfs :: KnownNat colS => Maybe (HeaderConfVS colS t m)
       colDfs = Just $ HeaderConfVS hns colVec txtVc def
       -- cff :: ThFuns t m
       -- cff = def & set thfADEs ades
       capDfs :: Maybe (CaptionConf t )
       capDfs = Just (CaptionConf "Table example with class-attrs" $
           style ("background-color: black; color: white; " <>
                  "font-weight: bold") def)
       sumVec :: KnownNat colS => V.Vector colS Text
       sumVec = V.generate (\i -> "Sum C" <> (T.pack . show ) i)
       sumDfs :: Maybe (FootConfVS colS t m)
       sumDfs = Just $ FootConfVS dff sumVec def
       -- dff = defaultTfootFuns & set (tfootADEs . drawEl) (drawDivContent2 id)
       --         & set (tfootADEs . actListen) (listenMyCol rws)
       -- dfftfoo :: FootConfVS colS t m
       -- dfftfoo = _tfootADEs defaultTfootFuns
       dff :: TfootFuns t m
       dff = def & set tfootADEs ades
               -- & set tfootADEs = dfftfoo {_drawEl = drawDivContent2 id}}
               -- & \g -> g {_tfootADEs = dfftfoo {_actListen = listenMyCol rws}}
       -- fPair = Just (dff, sumDfs)
       ades = CommonADEfuns (listenMyCol rws) actMU drawDivAECntEx cellEvF
       -- ades = CommonADEfuns listenMe actMU drawDivAECntEx cellEvF
              :: CommonADEfuns t m Text
-}

--------------------------------------------------------------------------------


genFR :: (KnownNat n, KnownNat m) => Finite m -> Finite n -> Text
genFR r c = f r <> ":" <> f c
  where
    f = T.pack . show . (+1) . getFinite

genVec :: (KnownNat n, KnownNat m) => Finite m -> V.Vector n Text
genVec m = V.generate_ (genFR m)

type MyCell t = TdConfHo t Text
type MyRow t = TRowConfHo t 5 Text Text
type MyFoot t = TFConfHo t 5 Text Text
type MyBody t = TBConfHo t 10 5 Text Text Text

type MyTable t = TTConfHo t 10 5 Text Text Text Text Text Text


genCell :: (KnownNat n, KnownNat m, Reflex t)
        => Finite m -> Finite n -> MyCell t
genCell r c = TdConfHo (genFR r c) ac def
  where
    -- ag = ActiveGroup $ fromInteger (getFinite c)
    -- ag = ActiveGroup $ Set.singleton $ T.pack $ show (getFinite c)
    ag = ActiveGroup $ Set.singleton $ ActErow $ fromInteger (getFinite c)
    ac = def { _activeStateListen = constDyn ag}

genRow :: forall t m. (KnownNat m, Reflex t)
       -- => Finite m -> Finite n -> MyRow t
       => Finite m -> MyRow t
genRow r = mkTrConf ("myRow " <> rt) ag def vec
  where
    rt = T.pack $ show $ getFinite r
    vec = V.generate_ (genCell r)
    -- ag = ActiveGroup $ Set.singleton $ T.pack $ show (getFinite r)
    ag = ActiveGroup $ Set.singleton $ ActEcolh $ fromInteger (getFinite r)
    --  ac = ActiveConf never never (constDyn True) (constDyn ag)

genFCell :: Reflex t => Int -> MyCell t
genFCell c = TdConfHo (T.pack $ show c) ac def
  where
    -- ag = ActiveGroup $ Set.singleton $ T.pack $ show c
    ag = ActiveGroup $ Set.singleton $ ActEcolh c
    ac = def { _activeStateListen = constDyn ag}

genFoot :: forall t. (Reflex t)
        => MyFoot t
genFoot = mkTfootConf "my foot" (ActiveGroup $ Set.singleton ActEnone) def vec
  where
    vec = V.generate genFCell


genBody' :: (KnownNat m, Reflex t) => V.Vector m (MyRow t)
genBody' = V.generate_ genRow

genBody :: forall t. (Reflex t) => MyBody t
genBody = mkTBodyConf "myTable" def genBody'
-- genBody = mkTBodyConf "myTable" def (V.generate_ genRow :: V.Vector m (MyRow t))


cs = finites :: [Finite 5]
rs = finites :: [Finite 10]
m9 = V.generate_ genRow :: Reflex (SpiderTimeline Global)
                        => V.Vector 10 (MyRow (SpiderTimeline Global))


exampleVSHo1 :: forall t m. (MonadWidget t m) => m ()
exampleVSHo1 = do
    eH2N $ text "Tables with similar columns using vector-sized"
    ePN $ text $ "Note that this interface is still heavily in WIP-state. "
        <> "This example is also in WIP-state. "
    eBN $ text "Use mkTableVS instead."
    eDivN $ do
        -- let c = 10 :: Int
        --     r = 20 :: Int
        let c = 9 :: Finite 10
            r = 19 :: Finite 20
            -- cols = [1..c]
            -- rows = [1..r]
            cols = finites :: [Finite 5]
            rows = finites :: [Finite 10]
            colNames = fmap (T.pack . show . getFinite) cols
            rowNames = fmap (T.pack . show . getFinite) rows
            txt = genFR (head rows) (head cols)
            v1 = V.generate_ (genFR (head rows)) :: V.Vector 5 Text
            m1 = V.generate_ genVec :: V.Vector 10 (V.Vector 5 Text)
            m2 = V.generate_ genRow :: V.Vector 10 (MyRow t)
            capDef = Just $ CaptionConf "tCaption" def
            footDef = genFoot :: MyFoot t
            bodyConf = genBody :: MyBody t
            tblConf =
                mkTableConf ("tTag" :: Text) capDef (Just footDef) bodyConf def
        -- ePN $ do
        --   text "txt is "
        --   text txt
        ePN $ text "table is "
        tElms <- mkTableElem tblConf
        ePN $ text "Is table ok?"



