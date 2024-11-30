module MiniHtml
  ( el
  , b_
  , body_
  , html_
  , head_
  , title_
  , h1_
  , h2_
  , i_
  , p_
  , a_
  , u_
  , ul_
  , ol_
  , style_
  , table_
  , tr_
  , td_
  , linkstyle
  , makeHtml) where

el :: String -> String -> String
el tag content =
  "\n<" <> tag <> ">" <> content <> "</" <> tag <> ">"

body_, html_, head_, title_, h1_, h2_, table_, td_ :: String -> String
body_ = el "body"
html_ = el "html"
head_ = el "head"
title_ = el "title"
h1_ = el "h1"
h2_ = el "h2"
table_ = el "table"
tr_ = el "tr"
td_ = el "td"

-- italic, bold, underscore
i_, b_, u_ :: String -> String
i_ = el "i"
b_ = el "p"
u_ = el "u"

p_ :: String -> String
p_ = el "p"

li_ :: String -> String
li_ = el "li"

ul_, ol_ :: Foldable t => t String -> String
ul_ lst = el "ul" $ concatMap li_ lst

ol_ lst = el "ol" $ concatMap li_ lst

a_ :: String -> String -> String
a_  alink declared = "<a href=\"" <> alink <> "\">" <> declared <> "</a>"
style_ = el "style"

linkstyle :: String
linkstyle = "<link rel=\"stylesheet\" href=\"mystyle.css\">"

makeHtml :: String -> String -> String
makeHtml pageTitle content =
   html_ $ head_ (linkstyle <> title_ pageTitle) <> body_ content