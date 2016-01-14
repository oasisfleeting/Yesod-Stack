module Handler.Ring where

import Import
import qualified Network.URI
import qualified Data.Text as T


entryForm :: Form Site
entryForm = renderDivs $ Site
    <$> areq textField (fieldSettingsLabel MsgOwner) Nothing
    <*> areq textField (fieldSettingsLabel MsgUrl) Nothing

getRingR :: Handler Html
getRingR = do
    (formWidget, formEnctype) <- generateFormPost entryForm
    let handlerName = "getRingR" :: Text
    sites <- runDB $ selectList [] [Asc SiteOwner]
    defaultLayout $ do
	setTitleI MsgWebring
        $(widgetFile "ring")

postRingR :: Handler Html
postRingR = do
    ((result, _), _) <- runFormPost entryForm
    runDB $ do 
      return ()
      case result of
        FormSuccess res -> insertUnique res >> return ()
        _ -> return ()
    redirect RingR

