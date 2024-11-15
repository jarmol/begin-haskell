module HtmlBuilder (main)
   where


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


a_  alink declared = "<a href=\"" <> alink <> "\">" <> declared <> "</a>"  

linkstyle = "<link rel=\"stylesheet\" href=\"mystyle.css\">"

makeHtml :: String -> String -> String
makeHtml pageTitle content =
   html_ $ head_ (linkstyle <> title_ pageTitle) <> body_ content

main :: IO ()
main = do
   putStrLn
     (makeHtml "BLOG-GENERATOR" (h1_ "YIPPEE, RUM PUM PUM!"
      <> h2_"Here you see, how to make html with Haskell."
      <> p_ (a_ "https://learn-haskell.blog/03-html/01-html_content.html" "Learn Haskell by building a blog generator")
      <> p_ (a_ "https://suncalc.lammi.cc/" "Homepage")
      <> p_ (a_ "https://github.com/jarmol/begin-haskell/blob/main/html-builder.hs" "Html-builder Haskell-source")
      <> p_ "Made with GHCi 9.4.8"
     ))

     

