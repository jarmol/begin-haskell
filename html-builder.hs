module HtmlBuilder (main)
   where

import MiniHtml ( el
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
  , makeHtml)


main :: IO ()
main = do
   putStrLn
     (makeHtml "BLOG-GENERATOR" (h1_ "Lorem ipsum odor amet"
      <> h2_"Here you see, how to make html with Haskell."
      <> ul_ ["lesson A Elementary","lesson B Advanced","lesson C Academic"]
      <> ol_ ["Order specificaions","Planning", "Production", "Control","Correction","Test reports","Documentation","Delivery"]
      <> p_ (a_ "https://learn-haskell.blog/03-html/01-html_content.html" "Learn Haskell by building a blog generator")
      <> p_ (a_ "https://suncalc.lammi.cc/" "Homepage")
      <> p_ (a_ "https://github.com/jarmol/begin-haskell/blob/main/html-builder.hs" "Html-builder Haskell-source")
      <> p_ "Made with GHCi 9.4.8"
     ))