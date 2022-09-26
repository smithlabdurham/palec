app <- ShinyDriver$new("../../", seed = 0)
app$snapshotInit("survive")

app$snapshot()
app$setInputs(logX = TRUE)
app$setInputs(logY = FALSE)
app$setInputs(xlab = "Test axis")
app$snapshot()
