module MiniHtml
  ( el
  , body_
  , html_
  , head_
  , title_
  , h1_
  , h2_
  , p_
  , a_
  , ul_
  , ol_
  , linkstyle
  , makeHtml) where

el :: String -> String -> String
el tag content =
  "\n<" <> tag <> ">\n" <> content <> "\n</" <> tag <> ">"

body_ :: String -> String
body_ = el "body"

html_ :: String -> String
html_ = el "html"

head_ :: String -> String
head_ = el "head"

title_ :: String -> String
title_ = el "title"

h1_ :: String -> String
h1_ = el "h1"

h2_ :: String -> String
h2_ = el "h2"

p_ :: String -> String
p_ = el "p"

a_ :: String -> String -> String
a_  alink declared = "<a href=\"" <> alink <> "\">" <> declared <> "</a>"

li_ = el "li"

ul_ lst = el "ul" $ concatMap li_ lst

ol_ lst = el "ol" $ concatMap li_ lst

linkstyle :: String
linkstyle = "<link rel=\"stylesheet\" href=\"mystyle.css\">"

makeHtml :: String -> String -> String
makeHtml pageTitle content =
   html_ $ head_ (linkstyle <> title_ pageTitle) <> body_ content