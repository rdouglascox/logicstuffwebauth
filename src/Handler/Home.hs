{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

-- imports from problemsets
import MakePS.MakePS01
import MakePS.MakePS02
import MakePS.MakePS03
import MakePS.MakePS04
import MakePS.MakePS05
import MakePS.MakePS06
import MakePS.MakePS07
import MakePS.MakePS08
import MakePS.MakePS09
import MakePS.MakePS10
import MakePS.MakePS11

import System.Random
import Helpers.RandomPL
import Helpers.RandomGPLI

-- imports for pl trees form
import qualified Forms.PLtrees as PL
import qualified Forms.GPLItrees as GPLI
import qualified Forms.PLtables as PLT
import qualified Conversions.Conversions as CV
import qualified DPform.DPform as DPform

import ClassyPrelude.Yesod (checkBoxField, FieldSettings (FieldSettings))
import ClassyPrelude (Bool(True))
import qualified Conversions.Conversions as CV

import Text.Blaze.Html       (preEscapedToHtml)
import Text.Shakespeare.Text (stextFile)

-- | welcome and about
getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "welcome to logicstuff!"
        $(widgetFile "homepage")

-- | about 

getAboutR :: Handler Html
getAboutR = do
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "about logicstuff"
        $(widgetFile "about")

-- | tools

getToolsR :: Handler Html
getToolsR = do
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "logic tools"
        toWidget . preEscapedToHtml $ $(stextFile "templates/tools.html")

-- | help

getHelpR :: Handler Html
getHelpR = do
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "help"
        $(widgetFile "help")

-- | problemsets main page
getProblemSetsR :: Handler Html
getProblemSetsR = do
    defaultLayout $ do
        setTitle "logicstuff | problemsets"
        $(widgetFile "problemsets")

-- | problemset01
getProblemSet01R :: Handler Html
getProblemSet01R = do
    defaultLayout $ do
        ps <- liftIO mkps01html
        setTitle "logicstuff | problemsets/problemset01"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet02R :: Handler Html
getProblemSet02R = do
    defaultLayout $ do
        ps <- liftIO mkps02html
        setTitle "logicstuff | problemsets/problemset02"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet03R :: Handler Html
getProblemSet03R = do
    defaultLayout $ do
        ps <- liftIO mkps03html
        setTitle "logicstuff | problemsets/problemset03"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet04R :: Handler Html
getProblemSet04R = do
    defaultLayout $ do
        ps <- liftIO mkps04html
        setTitle "logicstuff | problemsets/problemset04"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet05R :: Handler Html
getProblemSet05R = do
    defaultLayout $ do
        ps <- liftIO mkps05html
        setTitle "logicstuff | problemsets/problemset05"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet06R :: Handler Html
getProblemSet06R = do
    defaultLayout $ do
        ps <- liftIO mkps06html
        setTitle "logicstuff | problemsets/problemset06"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet07R :: Handler Html
getProblemSet07R = do
    defaultLayout $ do
        ps <- liftIO mkps07html
        setTitle "logicstuff | problemsets/problemset07"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet08R :: Handler Html
getProblemSet08R = do
    defaultLayout $ do
        ps <- liftIO mkps08html
        setTitle "logicstuff | problemsets/problemset08"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet09R :: Handler Html
getProblemSet09R = do
    defaultLayout $ do
        ps <- liftIO mkps09html
        setTitle "logicstuff | problemsets/problemset09"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet10R :: Handler Html
getProblemSet10R = do
    defaultLayout $ do
        ps <- liftIO mkps10html
        setTitle "logicstuff | problemsets/problemset10"
        $(widgetFile "problemsetscommon")

-- | problemsets main page
getProblemSet11R :: Handler Html
getProblemSet11R = do
    defaultLayout $ do
        ps <- liftIO mkps11html
        setTitle "logicstuff | problemsets/problemset11"
        $(widgetFile "problemsetscommon")


-- | here's is how we do forms in yesod

data PropForm = PropForm   -- my own thingy for getting propositions
    { propInput1 :: Text
    , isArg1 :: Bool
    }

propForm :: Form PropForm
propForm = renderBootstrap3 BootstrapBasicForm $ PropForm
    <$> areq textField textSettings Nothing
    <*> areq checkBoxField boxSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "Enter a list of propositions here."
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "e.g. (A->B),(CvD),((E&G)<->D)")
                ]
            }
          boxSettings = FieldSettings
            { fsLabel = "Is the input an argument? "
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs = []
            }

data PropForm1 = PropForm1   -- my own thingy for getting propositions
    { propInput :: Text
    , isArg :: Bool
    }

propForm1 :: Form PropForm1
propForm1 = renderBootstrap3 BootstrapBasicForm $ PropForm1
    <$> areq textField textSettings Nothing
    <*> areq checkBoxField boxSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "Enter a list of propositions here."
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "e.g. @x(Ax->Bx)")
                ]
            }
          boxSettings = FieldSettings
            { fsLabel = "Is the input an argument? "
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs = []
            }

-- let's give my form a page of its own

getPLTreesR :: Handler Html
getPLTreesR = do
    (formWidget', formEnctype') <- generateFormPost propForm   -- my own form
    defaultLayout $ do
        setTitle "logicstuff | truth trees"
        $(widgetFile "pltrees") 

postPLTreesR :: Handler Html
postPLTreesR = do
    ((result', formWidget'), formEnctype') <- runFormPost propForm
    let submission' = case result' of
            FormSuccess res -> Just res
            _ -> Nothing
    defaultLayout $ do
            let mytreehtml = case submission' of
                    Nothing -> "" 
                    Just (PropForm prop _) -> prop
            let arg = case submission' of
                      Nothing -> False
                      Just (PropForm _ True) -> True
                      Just (PropForm _ False) -> False
            if arg 
                then do
                mytree <- liftIO (PL.treeformHTMLa mytreehtml)
                setTitle "logicstuff | truth trees"
                $(widgetFile "pltreesresult")
                else do
                mytree <- liftIO (PL.treeformHTML mytreehtml)
                setTitle "logicstuff | truth trees"
                $(widgetFile "pltreesresult")

-- pl tables 

getPLTablesR :: Handler Html
getPLTablesR = do
    (formWidget', formEnctype') <- generateFormPost propForm   -- my own form
    defaultLayout $ do
        setTitle "logicstuff | truth tables"
        $(widgetFile "pltables") 

postPLTablesR :: Handler Html
postPLTablesR = do
    ((result', formWidget'), formEnctype') <- runFormPost propForm
    let submission' = case result' of
            FormSuccess res -> Just res
            _ -> Nothing
    defaultLayout $ do
            let mytreehtml = case submission' of
                    Nothing -> "" 
                    Just (PropForm prop _) -> prop
            let arg = case submission' of
                      Nothing -> False
                      Just (PropForm _ True) -> True
                      Just (PropForm _ False) -> False
            let mytable = PLT.tableformHTML mytreehtml
            setTitle "logicstuff | truth tables"
            $(widgetFile "pltablesresult")




-- gpli treees

getGPLITreesR :: Handler Html
getGPLITreesR = do
    (formWidget', formEnctype') <- generateFormPost propForm1   -- my own form
    defaultLayout $ do
        setTitle "logicstuff | gpli truth trees"
        $(widgetFile "gplitrees") 

postGPLITreesR :: Handler Html
postGPLITreesR = do
    ((result', formWidget'), formEnctype') <- runFormPost propForm1
    let submission' = case result' of
            FormSuccess res -> Just res
            _ -> Nothing
    defaultLayout $ do
            let mytreehtml = case submission' of
                    Nothing -> "" 
                    Just (PropForm1 prop _) -> prop
            let arg = case submission' of
                      Nothing -> False
                      Just (PropForm1 _ True) -> True
                      Just (PropForm1 _ False) -> False
            if arg 
                then do
                mytree <- liftIO (GPLI.treeformHTMLa mytreehtml)
                setTitle "logicstuff | gpli truth trees"
                $(widgetFile "gplitreesresult")
                else do
                mytree <- liftIO (GPLI.treeformHTML mytreehtml)
                setTitle "logicstuff | gpli truth trees"
                $(widgetFile "gplitreesresult")

-- dp

getDPR :: Handler Html
getDPR = do
    (formWidget', formEnctype') <- generateFormPost propForm   -- my own form
    defaultLayout $ do
        setTitle "logicstuff | dp"
        $(widgetFile "dp") 

postDPR :: Handler Html
postDPR = do
    ((result', formWidget'), formEnctype') <- runFormPost propForm
    let submission' = case result' of
            FormSuccess res -> Just res
            _ -> Nothing
    defaultLayout $ do
            let mytreehtml = case submission' of
                    Nothing -> "" 
                    Just (PropForm prop _) -> prop
            let arg = case submission' of
                      Nothing -> False
                      Just (PropForm _ True) -> True
                      Just (PropForm _ False) -> False
            if arg 
                then do
                let mytree = DPform.dpformHTMLa mytreehtml
                setTitle "logicstuff | dp"
                $(widgetFile "dpresult")
                else do
                let mytree = DPform.dpformHTML  mytreehtml
                setTitle "logicstuff | gpli truth trees"
                $(widgetFile "dpresult")

-- conversions 

getConversionsR :: Handler Html
getConversionsR = do
    (formWidget', formEnctype') <- generateFormPost conversionForm   -- my own form
    defaultLayout $ do
        setTitle "logicstuff | conversions"
        $(widgetFile "conversions") 

postConversionsR :: Handler Html
postConversionsR = do
    ((result', formWidget'), formEnctype') <- runFormPost conversionForm
    let submission' = case result' of
            FormSuccess res -> Just res
            _ -> Nothing
    defaultLayout $ do
            let mytreehtml = case submission' of
                    Nothing -> "" 
                    Just (ConversionForm prop _ _ _ _) -> prop
            let arg = case submission' of
                      Nothing -> (False,False,False,False)
                      Just (ConversionForm _ n c d p) -> (n,c,d,p)
            let mytree = CV.safeconversions mytreehtml arg
            setTitle "logicstuff | conversions"
            $(widgetFile "conversionsresult")

-- form for conversions 

data ConversionForm = ConversionForm   -- my own thingy for getting propositions
    { conversionpropInput :: Text
    , donnf :: Bool
    , docnf :: Bool
    , dodnf :: Bool 
    , dopnf :: Bool
    }

conversionForm :: Form ConversionForm
conversionForm = renderBootstrap3 BootstrapBasicForm $ ConversionForm
    <$> areq textField textSettings Nothing
    <*> areq checkBoxField boxSettings1 Nothing
    <*> areq checkBoxField boxSettings2 Nothing
    <*> areq checkBoxField boxSettings3 Nothing
    <*> areq checkBoxField boxSettings4 Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "Enter a proposition here."
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "e.g. @x(Ax->Bx)")
                ]
            }
          boxSettings1 = FieldSettings
            { fsLabel = "Negation Normal Form "
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs = []
            }
          boxSettings2 = FieldSettings
            { fsLabel = "Conjunctive Normal Form "
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs = []
            }
          boxSettings3 = FieldSettings
            { fsLabel = "Disjunctive Normal Form "
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs = []
            }
          boxSettings4 = FieldSettings
            { fsLabel = "Prenex Normal Form "
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs = []
            }

-- random pl props

data RPLForm = RPLForm (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Text) (Maybe Bool)


rplForm :: Form RPLForm
rplForm = renderBootstrap3 BootstrapBasicForm $ RPLForm
    <$> aopt intField intSettings1 Nothing
    <*> aopt intField intSettings2 Nothing
    <*> aopt intField intSettings3 Nothing
    <*> aopt textField textSettings Nothing
    <*> aopt checkBoxField boxSettings1 Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "Basic proposition symbols:"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "e.g. PQR")
                ]
            }
          intSettings1 = FieldSettings
            { fsLabel = "The minimum number of connectives in a proposition:"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs = [("placeholder", "e.g. 1")]
            }
          intSettings2 = FieldSettings
            { fsLabel = "The maximum number of connectives in a proposition:"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs = [("placeholder", "e.g. 3")]
            }
          intSettings3 = FieldSettings
            { fsLabel = "The number of propositions in list of propositions: "
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs = [("placeholder", "e.g. 3")]
            }
          boxSettings1 = FieldSettings
            { fsLabel = "ASCII output? "
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs = []
            }

getRPLR :: Handler Html
getRPLR = do
    (formWidget', formEnctype') <- generateFormPost rplForm   -- my own form
    defaultLayout $ do
        setTitle "logicstuff | rpl"
        $(widgetFile "rpl") 

postRPLR :: Handler Html
postRPLR = do
    ((result', formWidget'), formEnctype') <- runFormPost rplForm
    let submission' = case result' of
            FormSuccess res -> Just res
            _ -> Nothing
    defaultLayout $ do
            gen <- liftIO newStdGen
            let proplist = case submission' of
                             Nothing -> toHtml ("Form error" :: String)
                             Just (RPLForm x y z u b) -> getrPL gen x y z u b
            setTitle "logicstuff | rpl"
            $(widgetFile "rplresults")

-- random gpli propositions


data RGPLIForm = RGPLIForm (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Text) (Maybe Text) (Maybe Bool)


rgpliForm :: Form RGPLIForm
rgpliForm = renderBootstrap3 BootstrapBasicForm $ RGPLIForm
    <$> aopt intField intSettings1 Nothing
    <*> aopt intField intSettings2 Nothing
    <*> aopt intField intSettings3 Nothing
    <*> aopt textField textSettings Nothing
    <*> aopt textField textSettings1 Nothing
    <*> aopt checkBoxField boxSettings1 Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "Predicate symbols:"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "e.g. FGH")
                ]
            }
          textSettings1 = FieldSettings
            { fsLabel = "Names:"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "e.g. abc")
                ]
            }
          intSettings1 = FieldSettings
            { fsLabel = "The minimum number of connectives in a proposition:"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs = [("placeholder", "e.g. 1")]
            }
          intSettings2 = FieldSettings
            { fsLabel = "The maximum number of connectives in a proposition:"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs = [("placeholder", "e.g. 3")]
            }
          intSettings3 = FieldSettings
            { fsLabel = "The number of propositions in list of propositions: "
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs = [("placeholder", "e.g. 3")]
            }
          boxSettings1 = FieldSettings
            { fsLabel = "ASCII output? "
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs = []
            }

getRGPLIR :: Handler Html
getRGPLIR = do
    (formWidget', formEnctype') <- generateFormPost rgpliForm   -- my own form
    defaultLayout $ do
        setTitle "logicstuff | rgpli"
        $(widgetFile "rgpli") 

postRGPLIR :: Handler Html
postRGPLIR = do
    ((result', formWidget'), formEnctype') <- runFormPost rgpliForm
    let submission' = case result' of
            FormSuccess res -> Just res
            _ -> Nothing
    defaultLayout $ do
            gen <- liftIO newStdGen
            let proplist = case submission' of
                             Nothing -> toHtml ("Form error" :: String)
                             Just (RGPLIForm x y z u1 u2 b) -> getrGPLI gen x y z u1 u2 b
            setTitle "logicstuff | rgpli"
            $(widgetFile "rgpliresults")
