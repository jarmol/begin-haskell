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
  , linkstyle
  , makeHtml) where

el :: String -> String -> String
el tag content =
  "\n<" <> tag <> ">\n" <> content <> "\n</" <> tag <> ">"

b_ :: String -> String
b_ = el "b"

body_ :: String -> String
body_ = el "body"

html_ :: String -> String
html_ = el "html"

head_ :: String -> String
head_ = el "head"

i_ :: String -> String
i_ = el "i"

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

li_ :: String -> String
li_ = el "li"

u_ :: String -> String
u_ = el "u"

ul_ :: Foldable t => t String -> String
ul_ lst = el "ul" $ concatMap li_ lst

ol_ :: Foldable t => t String -> String
ol_ lst = el "ol" $ concatMap li_ lst

linkstyle :: String
linkstyle = "<link rel=\"stylesheet\" href=\"mystyle.css\">"

makeHtml :: String -> String -> String
makeHtml pageTitle content =
   html_ $ head_ (linkstyle <> title_ pageTitle) <> body_ content