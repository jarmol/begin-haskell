module HtmlBuilder (main)
   where

import MiniHtml ( el
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
  , makeHtml, b_)


main :: IO ()
main = do
   putStrLn
     (makeHtml "BLOG-GENERATOR" (h1_ "Lorem ipsum odor amet"
      <> h2_"Here you see, how to make html with Haskell."
      <> p_ ("b_ = " <> b_ "Bold " <> ", i_ = " <> i_ "Italic "
      <> ", u_ = " <> u_ "Underscore")
      <> ul_ ["lesson A Elementary","lesson B Advanced","lesson C Academic"]
      <> ol_ ["Order specifications","Planning"
      , "Production", "Control","Correction"
      ,"Test reports","Documentation","Delivery"]
      <> p_ (a_ "https://learn-haskell.blog/03-html/01-html_content.html"
       "Learn Haskell by building a blog generator")
      <> p_ (a_ "https://suncalc.lammi.cc/" "Homepage")
      <> p_ (a_ "https://raw.githubusercontent.com/jarmol/begin-haskell/refs/heads/main/html-builder.hs"
       "Html-builder Haskell-source")
      <> p_ "Made with GHCi 9.4.8"
     ))
