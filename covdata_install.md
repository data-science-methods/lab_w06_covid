The `covdata` package appears to have problems with installation on some systems, possibly due to memory limits.  You can read more about this here:  <https://github.com/kjhealy/covdata/issues/9>

This came to my attention on RStudio Cloud, which has a cap of 1 GB of RAM per instance in the free tier.  

If you encounter this `Killed` message, first try loading `covdata` as usual: `library(covdata)`. It may be that the package was installed correctly, but loading all of the data files simultaneously in the test load ran into the memory cap.  

If that doesn't work, run the following: 
```{r}
devtools::install_github('kjhealy/covdata', INSTALL_opts = c('--no-lock'))
```

*This will not prevent the `Killed` message*, but it skips the use of a "locked" installation directory.  You can read more about the purpose of locked directories here: <https://developer.r-project.org/Blog/public/2019/02/14/staged-install/index.html>.  

After installing with the `--no-lock` option, despite still getting the error message, you should be able to load `covdata` as usual. 

**Important notes:**

1. Unless you delete it manually, you will still have the "locked" `covdata` folder in your R library, along with the installed version.  I don't think this will cause problems, because R loads the installed version.  But I'm not sure.  

2. It's not clear (to me) exactly when the installation process gets killed.  If this happens, say, partway through copying files, it may be that your installation of `covdata` is actually corrupted.  The automatic checks may or may not detect this kind of problem, because they use the installed files to build the references for comparison to your answers.  We'll deal with this problem if and when we come to it.  

You only need to install `covdata` once.  