module Handler.TicTacToe where

import Import

canvasSize :: Int
canvasSize = 600

getTicTacToeR :: Handler Html
getTicTacToeR = 
    defaultLayout $ do
	setTitleI MsgTicTacToe
	addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
        $(widgetFile "tictactoe")

