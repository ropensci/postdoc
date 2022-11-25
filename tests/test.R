# Same as examples, but it takes longer than 10s on Windows
library(postdoc)
out <- render_package_manual('parallel', tempfile())
unlink(out)
