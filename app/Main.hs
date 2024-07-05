import Scanner

main :: IO ()
main = do
  content <- readFile "../test/lox/scanner.lox"
  print "----"
  print "File Content"
  print content
  let tokens = scan content
  print "----"
  print "Tokens"
  print tokens
