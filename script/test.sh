echo "CM.make \"src/diophantine.cm\"; val _ = SMLofNJ.exportFn (\"runTests\", Tests.runTests)" | sml > /dev/null && sml @SMLload=runTests
