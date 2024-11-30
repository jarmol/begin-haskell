import Text.Read.Lex (Number)
import Numeric
import MiniHtml (html_
  , head_
  , body_
  , h1_
  , title_
  , style_
  , table_
  , tr_
  , td_
  ,a_
  ,ul_
  )

-- Generate 2D-list
-- row = height cm
-- col = weight kg
-- cell [row,column] [1,1]
-- bmi = weight[1] / height[1]^2

bmi r c  =
    let w = head results !! c
        h = head (results !! r)
    in  w / (h / 100)^2

-- Function to generate HTML table from the 2D list
generateHtmlTable :: [[Float]] -> String
generateHtmlTable table =
    let wmin = head table !! 1; wmax = head table   !! 6 -- Weight [kg]
        hmin = head (table !! 1) ; hmax = head ( table  !! 6) -- Height [cm]
        legend1 = "<p>Weights " ++ show wmin ++ " - " ++ show wmax ++ " kg</p>"
        legend2 = "<p>Heights " ++ show hmin ++ " - " ++ show hmax ++ " cm</p>"
    in  html_
     (  head_ 
     (  title_ "BMI-index</title>"
    <> style_
     ( "body {margin-left: 5%; font-family: Arial, Helvetica, sans-serif;}"
    <> "h1 {color: red;}"
    <> "table, td {border: 2px solid blue; border-collapse: collapse; font-size: 1.2em;}"
    <> "td {padding: 5px;}"
    <> "p {color: blue; font-size: 1.2em;}"
    <> "hr {width: 40%; margin-left: 0%;}"
    <> "ul {font-size: 1.1em}"
     )
     )
    <> body_
     ( h1_ "BODY-MASS INDEX"
    <> table_ ( concatMap generateHtmlRow table)
    <> legend1 ++ legend2
    <> "<hr><br>"
    <> ul_ [a_ "https://raw.githubusercontent.com/jarmol/elmSunPosCalc/refs/heads/master/haskell/tabletohtml.hs" "Haskell code"
      , a_ "https://hackage.haskell.org/package/ghc-9.4.8" "Made with Haskell GHC 9.4.8"]
    <> " © Polarit 2024"
     )
     )
-- Function to generate HTML for a single row
generateHtmlRow :: [Float] -> String
generateHtmlRow row =
    tr_ (concatMap generateHtmlCell row)

-- Function to generate HTML for a single cell
generateHtmlCell :: Float -> String
generateHtmlCell cell =
    td_ (formD cell)

results =
    [[0,60,65,70,75,80,85]
    ,[160,bmi 1 1,bmi 1 2,bmi 1 3,bmi 1 4,bmi 1 5,bmi 1 6]
    ,[165,bmi 2 1,bmi 2 2,bmi 2 3,bmi 2 4,bmi 2 5,bmi 2 6]
    ,[170,bmi 3 1,bmi 3 2,bmi 3 3,bmi 3 4,bmi 3 5,bmi 3 6]
    ,[175,bmi 4 1,bmi 4 2,bmi 4 3,bmi 4 4,bmi 4 5,bmi 4 6]
    ,[180,bmi 5 1,bmi 5 2,bmi 5 3,bmi 5 4,bmi 5 5,bmi 5 6]
    ,[185,bmi 6 1,bmi 6 2,bmi 6 3,bmi 6 4,bmi 6 5,bmi 6 6]
    ]


formD v = showFFloat (Just 2) v ""

-- Main function to display the HTML table
main :: IO ()
main = do
    let htmlTable = generateHtmlTable results
    putStrLn htmlTable
