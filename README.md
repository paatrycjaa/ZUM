# ZUM
Project for Advanced Machine Learning course at EITI-WUT

The main goal for this project was to implement library in R for time series data vectorization. In the research part, we checked the impact of the time window size and the applied aggregating functions on the model accuracy.

Used dataset:
1) WESAD (Werable Stress and Affect Detection) - Multi-class Classification - https://archive.ics.uci.edu/ml/datasets/WESAD+%28Wearable+Stress+and+Affect+Detection%29
2) Bar Crawl: Detecting Heavy Drinking - Binary Classification - https://archive.ics.uci.edu/ml/datasets/Bar+Crawl%3A+Detecting+Heavy+Drinking
3) Apliances Energy Prediction - Regression - https://archive.ics.uci.edu/ml/datasets/Appliances+energy+prediction

library for Vectorization - tsvectorization

Functions for features importance in file : features.R

Preprocessing-vectorization in files: drinking-prerocessing.R , energy_preprocessing.R , stres_preprocessing.R

Models calculation in files: heavy_drink.R, energy.R, stress.R

Bibliography:
1) J.A. Killian i in. “Learning to Detect Heavy Drinking Episodes Using Smartphone Accelerometer Data”. W: In Proceedings of the 4th International Workshop on Knowledge Discovery in Healthcare Data co-located with the 28th International Joint Conference on Artificial Intelligence (2019), s. 35–42
2) Philip Schmidt i in. “Introducing WESAD, a multimodal dataset for Wearable Stress and Affect Detection”. W: ICMI ’18: Proceedings of the 20th ACM International Conference on Multimodal Interaction (2018), s. 400–408.
3)  Luis M. Candanedo, Véronique Feldheim i Dominique Deramaix. “Data driven prediction models of energy use of appliances in a low-energy house”. W: Energy and
Buildings (2017), s. 81–97
