# Prediction for large datasets

Here ou may find the function `predict_large`, to be used for prediction of models (e.g. 
lm, glm) for very large datasets, when generally a traditional prediction with "predict" returns memory related errors. This has been used in the context of predicting habitat selection functions into maps.

## Usage

```r
souce(predict_large)

predict_large(model, newdata, n.parts = 2, remove.sub.memory = T,
              verbose = F, ...)
```