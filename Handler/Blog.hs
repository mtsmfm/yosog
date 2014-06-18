module Handler.Blog
  ( getBlogR
  , postBlogR
  )
where

import Import

-- to use Html into forms

import Yesod.Form.Nic (YesodNic, nicHtmlField)
instance YesodNic App

-- The view showing the list of articles
getBlogR :: Handler Html
getBlogR = do
  -- Get the list of articles inside the database.
  articles <- runDB $ selectList [] [Desc ArticleTitle]
  -- We'll need the tow "objects": articleWidget aned enctype
  -- to construct the form (see templates/articles.hamlet)
  (articleWidget, enctype) <- generateFormPost entryForm
  defaultLayout $ do
    $(widgetFile "articles")

entryForm :: Form Article
entryForm = renderDivs $ Article
  <$> areq textField "Title" Nothing
  <*> areq nicHtmlField "Content" Nothing

postBlogR :: Handler Html
postBlogR = do
  ((res, articleWidget), enctype) <- runFormPost entryForm
  case res of
    _ -> defaultLayout $ do
      setTitle "Please correct your entry form"
      $(widgetFile "articleAddError")
