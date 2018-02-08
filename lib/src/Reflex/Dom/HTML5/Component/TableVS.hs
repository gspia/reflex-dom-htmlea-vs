{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}


{-|
Module      : Reflex.Dom.HTML5.Component.TableVS
Description : Helpers to build tables.
Copyright   : (c) gspia 2017
License     : BSD
Maintainer  : gspia

= TableVS

A table interface for vector-sized inputs.

Function 'mkTableVS' needs

    * A matrix (vector of vectors) input.
    * A set of functions that defines, how things are shown up and what happens on
      events (e.g. on mouse button click). See 'TdFuns'.
    * Optional: a definition about caption. See 'CaptionConf'.
    * Optional: a definition about table headers. See 'HeaderConfV' and 'ThFuns'.
    * Optional: a definition about table (column) footers.
      See 'FootConfV' and 'TfootFuns'.


-}

module Reflex.Dom.HTML5.Component.TableVS
    (
    module Reflex.Dom.HTML5.Component.Table.Common
    , module Reflex.Dom.HTML5.Component.Table.StateInfo
    , module Reflex.Dom.HTML5.Component.Table.TdComb
    , module Reflex.Dom.HTML5.Component.Table.ThComb
    , module Reflex.Dom.HTML5.Component.Table.TfootComb

    -- * Column headers
    --
    --
    -- | We give styling through ECol, EColGroup, EThead, ETr and ETh.
    --
    -- We give drawing function (that can use state information) in 'ThFuns',
    -- which also contains functions to specify, which cells or other headers
    -- we are listening for state changes and how to react to those.
    --
    , HeaderConfVS (..)
    , colThFunsVS
    , colConfsVS
    , colTextVS
    , colTheadAttrsVS
    , mkTheadVS

    -- * Column footers
    --
    -- | We give styling through ETfoot, ETr and  ETd.
    --
    -- We give drawing function (that can use state information) in 'TfootFuns',
    -- which also contains functions to specify, which cells or other headers
    -- we are listening for state changes and how to react to those.
    --
    , FootConfVS (..)
    , footFunsVS
    , footTextVS
    , footAttrsVS
    , mkTfootVS

    -- * Table construction
    --
    -- | We give styling through ETable and ETbody.
    --
    -- We give drawing function (that can use state information) in 'TdFuns',
    -- which also contains functions to specify, which cells or other headers
    -- we are listening for state changes and how to react to those.
    --
    , TableConfVS (..)
    , tableTdFunsVS
    , tableCaptionVS
    , tableHeaderVS
    , tableFooterVS
    , tableTableAttrVS
    , tableEventsVS
    , tableRowFunVS
    , tableRowEvFilterVS
    , tableActivityConfVS
    , cellListenerBodyVS
    , cellDrawBodyVS
    , cellActivityBodyVS
    , silentPlainTableConfVS
    , mkTableVS
    , mkRowVS
  )
  where

import           Control.Lens
import           Control.Monad.Fix
import           Data.Default
-- import           Data.Maybe                              (fromMaybe)
import           Data.Singletons.TypeLits
-- import           Data.Semigroup                          ((<>))
import           Data.Text                               (Text)
-- import qualified Data.Text                               as T
import qualified Data.Vector.Sized                       as V

import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom.Core

import           Reflex.Dom.HTML5.Component.Table.Common
import           Reflex.Dom.HTML5.Component.Table.StateInfo
import           Reflex.Dom.HTML5.Component.Table.TdComb
import           Reflex.Dom.HTML5.Component.Table.ThComb
import           Reflex.Dom.HTML5.Component.Table.TfootComb
import           Reflex.Dom.HTML5.Elements

-- import           Data.Finite
import           Data.Singletons
-- import           Data.Singletons.Decide

-- td-element represents data cell in a table.
-- th-element represents header cell in a table

--------------------------------------------------------------------------------

-- | The col and colgroup -element definitions with column header texts
-- to be used at the thead-element and before it. Text are put inside
-- thead. Thead contains a tr containing th's, that in turn, contain the texts.
-- The way each head (th) and tr are drawn (output), is specified with
-- 'ThFuns', see 'mkTheadV'.
data HeaderConfVS c t m = HeaderConfVS
    { _colThFunsVS     :: ThFuns t m
    -- ^ See 'ThFuns'.
    , _colConfsVS       :: V.Vector c (Dynamic t ECol)
    -- ^ Colgroup -definitions.
    , _colTextVS       :: V.Vector c Text
    -- ^ Column header texts inside th-elements.
    , _colTheadAttrsVS :: Dynamic t EThead
    -- ^ Thead attributes.
    }

-- | A lens.
colThFunsVS :: Lens' (HeaderConfVS c t m) (ThFuns t m)
colThFunsVS f (HeaderConfVS f1 f2 f3 f4)
  = fmap (\g -> HeaderConfVS g f2 f3 f4) (f f1)

-- | A lens.
colConfsVS :: Lens' (HeaderConfVS c t m) (V.Vector c (Dynamic t ECol))
colConfsVS f (HeaderConfVS f1 f2 f3 f4)
  = fmap (\g -> HeaderConfVS f1 g f3 f4) (f f2)

-- | A lens.
colTextVS :: Lens' (HeaderConfVS c t m) (V.Vector c Text)
colTextVS f (HeaderConfVS f1 f2 f3 f4)
  = fmap (\g -> HeaderConfVS f1 f2 g f4) (f f3)

-- | A lens.
colTheadAttrsVS :: Lens' (HeaderConfVS c t m) (Dynamic t EThead)
colTheadAttrsVS f (HeaderConfVS f1 f2 f3 f4)
  = fmap (HeaderConfVS f1 f2 f3) (f f4)

--------------------------------------------------------------------------------

-- | Construct the html for the thead-part. This also constructs the
-- colgroup-elements.
mkTheadVS ∷ forall t m cols. (Reflex t, DomBuilder t m, PostBuild t m
                       , TriggerEvent t m, MonadJSM m, MonadFix m
                       , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
         ⇒ Maybe (HeaderConfVS cols t m)
         -- ^ From table configurations. See 'ThFuns' and 'HeaderConfV'.
         →  Dynamic t (Maybe [ActiveState t])
         -- ^ User given activity information obtained from table configurations.
         → Event t (TableEvent t)
         -- ^ Event coming from table (can be from this header of from cells).
         → TableState t
         -- ^ A function combined th's can use the current 'TableState'.
         → m (Event t (TableEvent t))
         -- ^ Header returns the same events as cells (they get combined in
         -- the 'mkTableV').
mkTheadVS mColdefs mae evB tblSt =
    case mColdefs of
        Just (HeaderConfVS thFs cdefs ctxts dTheadA) -> mdo
            eColGroupN $ V.forM_ cdefs (`eColD` blank)
            eTheadD dTheadA $ do
                let hst = def :: ActiveState t
                    hElms = V.imap (\i c ->
                        (hst & set activeStateElem (ActEcolh i)
                        , c)) ctxts
                (_,evB') <- eTrN' $ do
                    eds <- V.forM hElms
                        (\(ve,txt) -> mdo
                            -- Use the conf info given by the user
                            let meAst2 = confAst2Ast mae meAst
                            te <- eThD (_thfThAttr thFs meAst2) $
                                        _thfCombFun thFs thFs
                                        mae ve txt evB tblSt
                            let meAst :: ActiveState t = _teMe te
                            pure te
                        )
                    pure $ tblEvFiring $ V.toList eds
                pure evB'
        Nothing -> pure never

--------------------------------------------------------------------------------

-- | Foot definitions specify a text for each column and attributes for
-- the tfoot-element. Tfoot contains a tr-element that contains td-elements.
-- The way each column foot (td) and tr are drawn (output), is specified with
-- 'TfootFuns', see 'mkTfootV'.
data FootConfVS c t m = FootConfVS
    { _footFunsVS  :: TfootFuns t m
    -- ^ Functions specifying behavior and look. See 'TfootFuns'.
    , _footTextVS  :: V.Vector c Text
    -- ^ Texts inside td's.
    , _footAttrsVS :: Dynamic t ETfoot
    -- ^ Tfoot attributes.
    }

-- | A lens.
footFunsVS :: Lens' (FootConfVS c t m) (TfootFuns t m)
footFunsVS f (FootConfVS f1 f2 f3) = fmap (\g -> FootConfVS g f2 f3) (f f1)

-- | A lens.
footTextVS :: Lens' (FootConfVS c t m) (V.Vector c Text)
footTextVS f (FootConfVS f1 f2 f3) = fmap (\g -> FootConfVS f1 g f3) (f f2)

-- | A lens.
footAttrsVS :: Lens' (FootConfVS c t m) (Dynamic t ETfoot)
footAttrsVS f (FootConfVS f1 f2 f3) = fmap (FootConfVS f1 f2) (f f3)

--------------------------------------------------------------------------------

-- | Construct the html for the tfoot-part.
mkTfootVS ∷ forall t m cols. (Reflex t, DomBuilder t m, PostBuild t m
                       , TriggerEvent t m, MonadJSM m, MonadFix m
                       , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
         ⇒ Maybe (FootConfVS cols t m)
         -- ^ From table configurations. See 'TdFuns' and 'FootConfV'.
         →  Dynamic t (Maybe [ActiveState t])
         -- ^ User given activity information obtained from table configurations.
         → Event t (TableEvent t)
         -- ^ Event coming from table (can be from, head, cells or from foot).
         → TableState t
         -- ^ A function combining td's can use the current 'TableState'.
         → m (Event t (TableEvent t))
         -- ^ Footer returns the same events as cells (they get combined in
         -- the 'mkTableV').
mkTfootVS mFootdefs mae evB tblSt =
    case mFootdefs of
        Just (FootConfVS tdFs ctxts dTfootA) -> mdo
            eTfootD dTfootA $ do
                let tdst = def :: ActiveState t
                    tdElms = V.imap (\i c ->
                        (tdst & set activeStateElem (ActEcols i)
                        , c)) ctxts
                (_,evB') <- eTrN' $ do
                    eds <- V.forM tdElms
                        (\(ve,txt) -> mdo
                            -- Use the conf info given by the user
                            let meAst2 = confAst2Ast mae meAst
                            te <- eTdD (_tfootTdAttr tdFs meAst2) $
                                         _tfootCombFun tdFs tdFs
                                         mae ve txt evB tblSt
                            let meAst :: ActiveState t = _teMe te
                            pure te
                        )
                    pure $ tblEvFiring $ V.toList eds
                pure evB'
        Nothing -> pure never

--------------------------------------------------------------------------------

-- | Table configuration tells, how the table behaves on different
-- table and cell events, what kind of decorations we put and how they
-- change based on user actions.
data TableConfVS c t m a = TableConfVS
    { _tableTdFunsVS ∷ TdFuns t m a
    -- ^ 'TdFuns' contains attributes and function definitions.
    , _tableCaptionVS ∷ Maybe (CaptionConf t)
    -- ^ 'CaptionConf' contains attributes and text.
    , _tableHeaderVS ∷ Maybe (HeaderConfVS c t m)
    -- ^ 'ThFuns' contains attrs and fus for Th-els.  Colgroup data with
    -- header texts and behavior.
    , _tableFooterVS ∷ Maybe (FootConfVS c t m)
    -- ^ 'TfootFuns' contains attrs and fus for Td-els with texts.
    , _tableTableAttrVS ∷ Dynamic t ETable
    -- ^ Attributes for the table element.
    , _tableEventsVS ∷ Element EventResult (DomBuilderSpace m) t
                   ->  m (TableOtherEvs t)
    -- ^ Events originating from table element (and not from cells).
    , _tableRowFunVS ∷ TableConfVS c t m a → Event t (TableEvent t) → TableState t
        → Int → V.Vector c (ActiveState t, a) → m (Event t (TableEvent t))
    -- ^ Function that makes a row.
    , _tableRowEvFilterVS ∷ Event t (TableEvent t) → Event t ActElem
    -- ^ A filtering function that row making function uses. This affects
    -- to the dynamic ActElem that is given to the function that makes
    -- tr-attributes.
    , _tableActivityConfVS :: Dynamic t (Maybe [ActiveState t])
    -- ^ Empty list means no cell is activated initially. This can also activate
    -- a row or column with a single 'ActElem' inside 'ActiveState'.
    }

-- | A lens.
tableTdFunsVS :: Lens' (TableConfVS c t m a) (TdFuns t m a)
tableTdFunsVS f (TableConfVS f1 f2 f3 f4 f5 f6 f7 f8 f9) =
    fmap (\g -> TableConfVS g f2 f3 f4 f5 f6 f7 f8 f9) (f f1)

-- | A lens.
tableCaptionVS :: Lens' (TableConfVS c t m a) (Maybe (CaptionConf t))
tableCaptionVS f (TableConfVS f1 f2 f3 f4 f5 f6 f7 f8 f9) =
    fmap (\g -> TableConfVS f1 g f3 f4 f5 f6 f7 f8 f9) (f f2)

-- | A lens.
tableHeaderVS :: Lens' (TableConfVS c t m a) (Maybe (HeaderConfVS c t m))
tableHeaderVS f (TableConfVS f1 f2 f3 f4 f5 f6 f7 f8 f9) =
    fmap (\g -> TableConfVS f1 f2 g f4 f5 f6 f7 f8 f9) (f f3)

-- | A lens.
tableFooterVS :: Lens' (TableConfVS c t m a) (Maybe (FootConfVS c t m))
tableFooterVS f (TableConfVS f1 f2 f3 f4 f5 f6 f7 f8 f9) =
    fmap (\g -> TableConfVS f1 f2 f3 g f5 f6 f7 f8 f9) (f f4)

-- | A lens.
tableTableAttrVS :: Lens' (TableConfVS c t m a) (Dynamic t ETable)
tableTableAttrVS f (TableConfVS f1 f2 f3 f4 f5 f6 f7 f8 f9) =
    fmap (\g -> TableConfVS f1 f2 f3 f4 g f6 f7 f8 f9) (f f5)

-- | A lens.
tableEventsVS :: Lens' (TableConfVS c t m a)
    (Element EventResult (DomBuilderSpace m) t ->  m (TableOtherEvs t))
tableEventsVS f (TableConfVS f1 f2 f3 f4 f5 f6 f7 f8 f9) =
    fmap (\g -> TableConfVS f1 f2 f3 f4 f5 g f7 f8 f9) (f f6)

-- | A lens.
tableRowFunVS :: Lens' (TableConfVS c t m a)
    (TableConfVS c t m a → Event t (TableEvent t) → TableState t
        → Int → V.Vector c (ActiveState t, a) → m (Event t (TableEvent t)))
tableRowFunVS f (TableConfVS f1 f2 f3 f4 f5 f6 f7 f8 f9) =
    fmap (\g -> TableConfVS f1 f2 f3 f4 f5 f6 g f8 f9) (f f7)

-- | A lens.
tableRowEvFilterVS :: Lens' (TableConfVS c t m a)
    (Event t (TableEvent t) → Event t ActElem)
tableRowEvFilterVS f (TableConfVS f1 f2 f3 f4 f5 f6 f7 f8 f9) =
    fmap (\g -> TableConfVS f1 f2 f3 f4 f5 f6 f7 g f9) (f f8)

-- | A lens.
tableActivityConfVS :: Lens' (TableConfVS c t m a)
    (Dynamic t (Maybe [ActiveState t]))
tableActivityConfVS f (TableConfVS f1 f2 f3 f4 f5 f6 f7 f8 f9) =
    fmap (TableConfVS f1 f2 f3 f4 f5 f6 f7 f8) (f f9)



-- | Cell listener -lens for tbody cells.
cellListenerBodyVS :: Lens' (TableConfVS c t m a) (ActiveState t -> ActiveState t)
cellListenerBodyVS = tableTdFunsVS . tdfADEs . actListen

-- | Cell drawing -lens for tbody-cells
cellDrawBodyVS :: Lens' (TableConfVS c t m a)
    (a -> ActiveState t
    -> m (Element EventResult (DomBuilderSpace m) t))
cellDrawBodyVS = tableTdFunsVS . tdfADEs . drawEl

-- | Cell activity function -lens for tbody-cells
cellActivityBodyVS :: Lens' (TableConfVS c t m a)
    (Dynamic t (Maybe [ActiveState t])
             → Event t (TableEvent t)
             → TableState t
             → ActiveState t → m (ActiveState t))
cellActivityBodyVS = tableTdFunsVS . tdfADEs . actFu

--------------------------------------------------------------------------------


instance (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m, DomBuilder t m
         , PostBuild t m, MonadJSM m, Show a, DomBuilderSpace m ~ GhcjsDomSpace)
  => Default (TableConfVS c t m a)
    where
      def = TableConfVS def def def def (constDyn def)
            tableEventsEnLe mkRowVS teFilterMU (constDyn def)

--------------------------------------------------------------------------------

-- | This is easier to start with if just needing a decorated table.
-- This allows normal selection and copying from the table.
silentPlainTableConfVS :: (Reflex t, MonadHold t m, TriggerEvent t m, MonadFix m
                        , DomBuilder t m, PostBuild t m, MonadJSM m, Show a
                        , DomBuilderSpace m ~ GhcjsDomSpace)
  => TableConfVS c t m a
silentPlainTableConfVS =
    TableConfVS silentPlainTdFuns def def def (constDyn def)
    tableEventsNone mkRowVS teFilterNone
    (constDyn def)



--------------------------------------------------------------------------------

-- | Basic row building function that keeps up the information about the
-- last row having an event. We can select between mouse button press
-- ('teFilterMD'), mouse button release ('teFilterMU'), mouse enter
-- ('teFilterMEnter') and having no events at all ('teFilterNone').
--
-- The function ('_tdfTrAttr') that makes tr-attributes can use the
-- last event row and current building row information.
--
-- Note: this function and the filters are probably going to move to other
-- place, when the corresponding builders are made for headers and footers.
mkRowVS ∷ forall cols t m a. (Reflex t, DomBuilder t m, PostBuild t m
     , TriggerEvent t m, MonadJSM m, MonadFix m
     , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
     ⇒ TableConfVS cols t m a
     -- ^ Table configurations.
     → Event t (TableEvent t)
     -- ^ An event somewhere on the table.
     → TableState t
     -- ^ 'TableState', i.e., the cell where last button release happened.
     → Int
     -- ^ Row number.
     → V.Vector cols (ActiveState t, a)
     -- ^ A row, that is, cell-coord info and the content.
     → m (Event t (TableEvent t))
mkRowVS tc evB tblSt i v = do
    let tdFs = view tableTdFunsVS tc
        dmlst = view tableActivityConfVS tc :: Dynamic t (Maybe [ActiveState t])
        astD = def & set activeStateElem (ActErow i) :: ActiveState t
    -- Keep state for the row and use the user supplied listeners etc.
    -- We keep row-state similarly to cell states. We may have to add
    -- separate "rowActMU/MD/Switch" functions etc.
    actS :: ActiveState t
          <- (_actFu ._tdfADEs . _tableTdFunsVS) tc
                 (_tableActivityConfVS tc) evB tblSt
             $ (_actListen . _tdfADEs . _tableTdFunsVS) tc astD
    evTE <- eTrD (_tdfTrAttr tdFs (constDyn actS) $ ActErow i) $ do
        eds <- V.forM v
            ( \(ve,ast2) -> mdo
                -- Use the conf info given by the user, this is for the
                -- td-attributes.
                let meAst = confAst2Ast dmlst $ _teMe te
                te <- eTdD (_tdfTdAttr tdFs meAst) $
                    _tdfCombFun tdFs tdFs dmlst ve ast2 evB tblSt
                    -- meAst in place of ve would cause a loop, note that
                    -- its definition requires recursive do.
                pure te
            )
        pure $ tblEvFiring $ V.toList eds
    pure evTE


--------------------------------------------------------------------------------

-- | Build a table given 'TdFuns' and a Vector of Vectors (2-dim).
-- 'TdFuns' gives the look and behavior of the table by specifying functions
-- to drawing the contents of td-elements ("the cells"), to acting on
-- different events etc. See examples on their usage.
-- 'TdFuns' and some pre-defined functions can be found from 'TableCommon' module.
mkTableVS ∷ forall rows cols t m a. (Reflex t, DomBuilder t m, PostBuild t m
     , TriggerEvent t m, MonadJSM m, MonadFix m, MonadHold t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , KnownNat rows, KnownNat cols)
         ⇒ TableConfVS cols t m a
         -- ^ 'TableConf' contains definitions, that is, functions and dynamic
         -- attributes that are used to construct a table element.
         → V.Vector rows (V.Vector cols a)
         -- ^ Contents of the table.
         → m (TableState t)
         -- ^ 'TableState' has event and state variables.
mkTableVS = mkTableVS_ sing sing

---- | See 'mkTableVS'.
mkTableVS_ ∷ forall rows cols t m a. (Reflex t, DomBuilder t m, PostBuild t m
     , TriggerEvent t m, MonadJSM m, MonadFix m, MonadHold t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , KnownNat rows, KnownNat cols)
         ⇒ Sing rows
         → Sing cols
         → TableConfVS cols t m a
         -- ^ 'TableConf' contains definitions, that is, functions and dynamic
         -- attributes that are used to construct a table element.
         → V.Vector rows (V.Vector cols a)
         -- ^ Contents of the table.
         → m (TableState t)
         -- ^ 'TableState' has event and state variables.
mkTableVS_ _s1 _s2 tc acElmsVV = mdo
    let elms = V.imap mkA4r acElmsVV
    (htTable, tblState) <- eTableD' (view tableTableAttrVS tc) $ mdo
        mkCaption (view tableCaptionVS tc)
        evB2 <- mkTheadVS (view tableHeaderVS tc)
                         (view tableActivityConfVS tc)
                         evB tblSt
        (_,evB1) <- eTbodyD' (view (tableTdFunsVS . tdfTbodyAttr) tc) $ do
            ets <- V.imapM (mkRowVS tc evB tblSt) elms
            pure $ leftmost $ V.toList ets
        evB3 <- mkTfootVS (view tableFooterVS tc)
                         (view tableActivityConfVS tc)
                         evB tblSt
        let evB = leftmost [evB1, evB2, evB3]
        tblSt <- updateTableState tblOE evB
        pure tblSt
    tblOE <- view tableEventsVS tc htTable
    pure tblState
    where
      -- TODO TODO TODO use the provided activity information in tc
      -- (or is the current way of using it in the functions making rows enough)
      ast :: ActiveState t
      ast = def
      mkA4r ∷ Int → V.Vector cols a → V.Vector cols (ActiveState t, a)
      mkA4r i = V.imap (\j e -> (ast & set activeStateElem (ActERC (i,j)) , e))
















----------------------------------------------------------------------------------

--data ColHeaderVS c t = ColHeaderVS
--    { _colDefsVS       :: W.Vector c (Dynamic t ECol)
--    , _colTextVS       :: W.Vector c Text
--    , _colTheadAttrsVS :: Dynamic t EThead
--    }

----------------------------------------------------------------------------------

---- | Construct the html for the thead-part, if present.
--mkTheadVS ∷ forall t m cols. (Reflex t, DomBuilder t m, PostBuild t m
--  , TriggerEvent t m, MonadJSM m, MonadFix m
--  , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
--  ⇒ Maybe (ThFuns t m, ColHeaderVS cols t)
--  → Event t (TableEvent t)
--  → TableState t
--  → m (Event t (TableEvent t))
--mkTheadVS mColdefs evB tblSt =
--    case mColdefs of
--        Just (thFs, ColHeaderVS cdefs ctxts dTheadA) -> mdo
--            eColGroupN $ W.forM_ cdefs (`eColD` blank)
--            eTheadD dTheadA $ do
--                let hElms = W.imap (\i c -> (ActEcolh i, c)) ctxts
--                (_,evB') <- eTrN' $ do
--                    eds <- W.forM hElms
--                        (\(ve,txt) -> eThD (_thfThAttr thFs ve)
--                            $ _thfCombFun thFs thFs ve txt evB tblSt )
--                    pure $ tblEvFiring $ W.toList eds
--                pure evB'
--        Nothing -> pure never

----------------------------------------------------------------------------------

---- | Column header definitions for col and colgroup definitions with texts
---- to be used at the thead-element and before it.
--data FootDefsVS c t = FootDefsVS
--    { _footTextVS  :: W.Vector c Text   -- ^ texts inside td's
--    , _footAttrsVS :: Dynamic t ETfoot  -- ^ tfoot attributes
--    }

----------------------------------------------------------------------------------

---- | Construct the html for the thead-part, if present.
--mkTfootVS ∷ forall t m cols. (Reflex t, DomBuilder t m, PostBuild t m
--                       , TriggerEvent t m, MonadJSM m, MonadFix m
--                       , MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace)
--         ⇒ Maybe (TfootFuns t m, FootDefsVS cols t)
--         -- ^ See 'TdFuns' and 'FootDefsV'
--         → Event t (TableEvent t)
--         -- ^ Event coming from table (can be from, head, cells or from foot)
--         → TableState t
--         -- ^ td-combining function can use the current tablestate
--         → m (Event t (TableEvent t))
--         -- ^ Footer returns the same events as cells (they get combined).
--mkTfootVS mFootdefs evB tblSt =
--    case mFootdefs of
--        Just (tdFs, FootDefsVS ctxts dTfootA) -> mdo
--            eTfootD dTfootA $ do
--                let tdElms = W.imap (\i c -> (ActEcols i, c)) ctxts
--                (_,evB') <- eTrN' $ do
--                    eds <- W.forM tdElms
--                        (\(ve,txt) -> eTdD (_tfootTdAttr tdFs ve)
--                            $ _tfootCombFun tdFs tdFs ve txt evB tblSt )
--                    pure $ tblEvFiring $ W.toList eds
--                pure evB'
--        Nothing -> pure never

----------------------------------------------------------------------------------

---- | Build a table given 'TdFuns' and a Vector of Vectors (2-dim). This module
---- use Vector-definition given in the Vector-Sized -package.
---- TdFuns gives the look and behavior of the table by specifying functions
---- to drawing the contents of td-elements ("the cells"), to acting on
---- different events etc. See examples on their use.
---- TdFuns and some pre-defined functions can be found from 'TableCommon' module.
--mkTableVS ∷ forall rows cols t m a.
--    (Reflex t, DomBuilder t m, PostBuild t m, TriggerEvent t m,
--    MonadJSM m, MonadFix m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace,
--    KnownNat rows, KnownNat cols)
--    ⇒ TdFuns t m a
--    -- ^ 'TdFuns' contains attributes and function definitions
--    → Maybe (CaptionDef t)
--    -- ^ 'CaptionDef' contains attributes and text
--    → Maybe (ThFuns t m, ColHeaderVS cols t)
--    -- ^ 'ThFuns' contains attrs and fus for Th-els.  Colgroup data with
--    -- header texts and behavior.
--    → Maybe (TfootFuns t m, FootDefsVS cols t)
--    -- ^ 'TfootFuns' contains attrs and fus for Td-els with texts.
--    → W.Vector rows  (W.Vector cols a) -- ^ Contents of the table
--    → m (TableState t) -- ^ 'TableState' gives event and state variables.
--mkTableVS = mkTableVS_ sing sing


---- | See 'mkTableVS'. Sometimes it nice to be able to give size also as
---- parameters.
--mkTableVS_ ∷ forall rows cols t m a.
--    (Reflex t, DomBuilder t m, PostBuild t m, TriggerEvent t m,
--    MonadJSM m, MonadFix m, MonadHold t m, DomBuilderSpace m ~ GhcjsDomSpace,
--    KnownNat rows, KnownNat cols)
--    ⇒  Sing rows
--    → Sing cols
--    → TdFuns t m a
--    -- ^ 'TdFuns' contains attributes and function definitions
--    → Maybe (CaptionDef t)
--    -- ^ 'CaptionDef' contains attributes and text
--    → Maybe (ThFuns t m, ColHeaderVS cols t)
--    -- ^ 'ThFuns' contains attrs and fus for Th-els.  Colgroup data with
--    -- header texts and behavior.
--    → Maybe (TfootFuns t m, FootDefsVS cols t)
--    -- ^ 'TfootFuns' contains attrs and fus for Td-els with texts.
--    → W.Vector rows  (W.Vector cols a) -- ^ Contents of the table
--    → m (TableState t) -- ^ 'TableState' gives event and state variables.
--mkTableVS_ _s1 _s2 tdFs mcapdefs mColdefs mFootdefs acElmsWW = mdo
--    let elms = W.imap mkA4r acElmsWW
--    (htTable, tblState) <- eTableD' (_tdfTableAttr tdFs) $ mdo
--        mkCaption mcapdefs
--        evB2 <- mkTheadVS mColdefs evB tblSt
--        (_htTb,evB1) <- eTbodyD' (_tdfTbodyAttr tdFs) $ do
--            ets <- W.imapM (mkRow evB tblSt) elms
--            pure $ leftmost $ W.toList ets
--        evB3 <- mkTfootVS mFootdefs evB tblSt
--        let evB = leftmost [evB1, evB2, evB3]
--        tblSt <- updateTableState tblOE evB
--        pure tblSt
--    tblOE <- tableEvents htTable
--    pure tblState
--    where
--      mkA4r ∷ Int → W.Vector cols a → W.Vector cols (ActElem, a)
--      mkA4r i = W.imap (\j e -> (ActERC (i,j),e))
--      mkRow ∷ Event t (TableEvent t)
--            → TableState t
--            → Int
--            → W.Vector cols (ActElem,a)
--            → m (Event t (TableEvent t))
--      mkRow evB tblSt2 i v =
--        eTrD (_tdfTrAttr tdFs $ ActErow i) $ do
--            eds <- W.forM v
--                (\(ve,ae) -> eTdD (_tdfTdAttr tdFs ve)
--                    $ _tdfCombFun tdFs tdFs ve ae evB tblSt2)
--                    --  _tdCombFun tdFs tdFs ve ae evB dDU eMoLe)
--                -- Note that in the _tdCombFun tdFs tdFs the first is used
--                -- to get the combining function and call it. The second is
--                -- a parameter to that function.
--            pure $ tblEvFiring $ W.toList eds
