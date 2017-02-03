# Movement analysis for thrushes in a fragmented landscape

The code and data presented here are the appendices of

Da Silveira NS, Niebuhr BBS, Muylaert RdL, Ribeiro MC, Pizo MA (2016) [Effects of Land Cover on the Movement of Frugivorous Birds in a Heterogeneous Landscape] (http://journals.plos.org/plosone/article?id=10.1371%2Fjournal.pone.0156688). PLoS ONE 11(6): e0156688. doi:10.1371/journal.pone.0156688

Here we present the code for movement analysis of thrushes movement patterns in a fragmented landscape. Thrush movement was collected using radiotelemetry. We used a likelihood-based approach to fit models with different distributions and covariates to movement variables calculated based on this data - average speed and turning angles. 

The analyses follow this sequence of steps:
- Choose a distribution for the response variable
- Select different combinations of covariates
- Write the likelihood function for a model with each combination of covariates
- Fit the model - search the best parameters, based on data and the likelihood function
- Model comparison using Akaike Information Criteria (AIC)


This repository has the following files:
* [`dados_final.csv`](dados_final.csv): Data for analysis
* [`Silveira_etal_Appendix_S1_R_code.R`](Silveira_etal_Appendix_S1_R_code.R): Code for the main analysis (Appendix 1 of the manuscript)
* [`lik.int_source_code.R`](lik.int_source_code.R): Auxiliary R function to plot likelihood intervals


If you have any questions, feel free to contact us:
* Natalia S. Silveira <<nat.stefanini@gmail.com>>
* Bernardo B. S. Niebuhr <<bernardo_brandaum@yahoo.com.br>>
