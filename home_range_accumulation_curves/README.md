# Home range accumulation curves

This folder contains scripts for building acculation curves for home range estimation 
based on animal movement data. Until now, this is implemented only using Maximum Convex Polygon approach,
but adaptation to KDE-like approaches may not be difficult (tell me if you need them or please upload 
it here if you made it!)

The basic idea is to estiumate home range though any method and plot how this estimate changes when one
increases the number of sampled relocations. This may be done considering the order the relocations were
collected or randomizing them before doing the estimation. The a look at the figure below, using VHF
movement data for Turdus birds in Southeaestern Brazil (data from [Movebank](https://www.movebank.org/) from this nice [study](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0156688)).

![accumulation curves](https://github.com/bniebuhr/movecology/blob/master/home_range_accumulation_curves/accumulation_curves.png)

**BUT**: If you are going further in movement ecology analysis, consider performin more robust assessments 
(such as exploring the semi-variance of the movement data thorugh [ctmm package](https://github.com/ctmm-initiative/ctmm), for instance).
