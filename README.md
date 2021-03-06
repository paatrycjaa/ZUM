# ZUM
Project for Advanced Machine Learning course at EITI-WUT

The main goal for this project was to implement library in R for time series data vectorization. In the research part, we checked the impact of the time window size and the applied aggregating functions on the model accuracy.

Used dataset:
1) WESAD (Werable Stress and Affect Detection) - Multi-class Classification
2) Bar Crawl: Detecting Heavy Drinking - Binary Classification
3) Energy Prediction - Regression

library for Vectorization - tsvectorization

Functions for features importance in file features.R
Preprocessing-vectorization in files: drinking-prerocessing.R , energy_preprocessing.R , stres_preprocessing.R
Models calculation in files: heavy_drink.R, energy.R, stress.R

@article{stress,
    author = "Philip Schmidt and Attila Reiss and Robert Duerichen and Claus Marberger andKristof Van Laerhoven",
    title = "Introducing WESAD, a multimodal dataset for Wearable Stress and Affect Detection",
    journal = "ICMI '18: Proceedings of the 20th ACM International Conference on Multimodal Interaction",
    pages = "400-408",
    year = "2018"
}
@article{drink,
    author = "Killian, J.A. and Passino, K.M. and Nandi, A. Madden and D.R.  Clapp, J.",
    title = " Learning to Detect Heavy Drinking Episodes Using Smartphone Accelerometer Data",
    journal = "In Proceedings of the 4th International Workshop on Knowledge Discovery in Healthcare Data co-located with the 28th International Joint Conference on Artificial Intelligence",
    pages = "35-42",
    year = "2019"
}
@article{energy,
    author = "Luis M. Candanedo and Véronique Feldheim and Dominique Deramaix",
    title = "Data driven prediction models of energy use of appliances in a low-energy house",
    journal = "Energy and Buildings",
    pages = "81--97",
    year = "2017"
}
