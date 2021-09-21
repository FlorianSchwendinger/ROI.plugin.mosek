# Installation
Before `ROI.plugin.mosek` can be installed `mosek` and its corresponding 
**R** package `Rmosek` need to be installed. Both can be obtained from
[http://docs.mosek.com](http://docs.mosek.com) 
and more information about the installation can be found in the `Rmosek` documentation at
[https://docs.mosek.com/9.2/rmosek/install-interface.html](https://docs.mosek.com/9.2/rmosek/install-interface.html).    


`ROI.plugin.mosek` was tested on `Debian GNU/Linux 10` and `Windows 10` with the
`mosek` version `9.2`.      


After `mosek` and `Rmosek` are installed simply use
```r
remotes:::install_github("FlorianSchwendinger/ROI.plugin.mosek@mosek9.2", INSTALL_opts = "--no-multiarch")
```
to install `ROI.plugin.mosek` for MOSEK version 9.2.
