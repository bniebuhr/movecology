# Home range accumulation curves

This folder contains scripts for building acculation curves for home range estimation 
based on animal movement data. This was implemented using Maximum Convex Polygon (MCP) 
and the conventional approach kernel density estimation (KDE) approaches.

The basic idea is to estimate home range through any method and plot how this estimate changes when one
increases the number of sampled positions. This may be done considering the order the relocations were
collected or randomizing them before doing the estimation. Take a look at the figures below, using VHF
movement data for Turdus birds in Southeaestern Brazil (data from [Movebank](https://www.movebank.org/) from this nice [study](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0156688)). This first one was done with MCP.

![accumulation curves MCP](https://github.com/bniebuhr/movecology/blob/master/home_range_accumulation_curves/accumulation_curves.png)

Here we show the same thing using a KDE approach.

![accumulation curves KDE](https://github.com/bniebuhr/movecology/blob/master/home_range_accumulation_curves/kde_accumulation_curves.png)

**BUT**: If you are going further in movement ecology analysis, consider performing more robust assessments 
(such as exploring the semi-variance of the movement data through [ctmm package](https://github.com/ctmm-initiative/ctmm), for instance). It is more suitable for GPS and other techonologies where a huge amount of data points is collected, but it the authors say it also works for VHF and other kinds of movement data.
