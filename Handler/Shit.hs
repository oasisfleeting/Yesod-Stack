module Handler.Shit where

import Data.Time
import Import
import qualified Data.Text as T

entryForm :: Form Post
entryForm = renderDivs $ Post
    <$> areq   textField (fieldSettingsLabel MsgAuthor) Nothing
    <*> areq   textareaField (fieldSettingsLabel MsgContents) Nothing
    <*> lift (liftIO getCurrentTime)

getShitR :: Handler Html
getShitR = do
    (formWidget, formEnctype) <- generateFormPost entryForm
    let handlerName = "getShitR" :: Text
    shits <- runDB $ selectList [] [Desc PostCreation]
    defaultLayout $ do
	setTitleI $ MsgShitPeopleHavePosted
        $(widgetFile "shit")


postShitR :: Handler Html
postShitR = do
    ((result, _), _) <- runFormPost entryForm
    runDB $ do 
      case result of
        FormSuccess res -> insertUnique res >> return ()
        _ -> return ()
    redirect ShitR


