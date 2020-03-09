module Main where

import Report (report)

main = do
    print "Generating report"
    ret <- report
    print (ret)
