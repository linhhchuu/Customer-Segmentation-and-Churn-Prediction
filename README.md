# Customer-Segmentation-and-Churn-Prediction

## Context
As part of a Customer Insights & Analysis class, I was tasked with evaluating how different forms of customer commitment influence churn at Crew’s Cup Fitness —  a premium boutique fitness brand undergoing a turnaround. 

## Objective
To recommend the best strategic investment to reduce customer churn by:

* Identifying underlying dimensions of customer commitment.
* Segmenting customers based on commitment profiles.
* Modeling how these commitment factors impact repurchase intent.

## Data Sources
* Customer survey (n = 1,507): Ratings on 5 commitment dimensions using 15 statements (Likert scale 1–7)
* Behavioral CRM data: Profitability, class frequency, tenure
* Experimental stimuli: 4 randomized test groups (control, ad campaign, new product, subscription model)

## Methods & Tools
* Exploratory Factor Analysis (EFA): Identified latent dimensions of customer commitment
* K-Means Clustering: Segmented customers based on factor scores
* Linear Regression: Modeled intent to continue using the service

## Key Insights
* **Five-Factor Commitment Model Validated**
EFA revealed clear loading patterns for affective, normative, economic, forced, and habitual commitment. Cronbach’s alpha scores exceeded 0.7 for all dimensions, confirming reliability.

* **Three Distinct Segments Identified**
K-means clustering suggested 3 clear audience segments that closely matched marketing personas:
  * The Faithful: high affective/habitual/economic/forced
  * Bargain Hunters (high affective, low economic)
  * Fitness Buffs (moderate across all)
<img width="1689" alt="image" src="https://github.com/user-attachments/assets/d5d43c7d-d920-49e3-bea4-0bc0963c80b6" />
<img width="1688" alt="image" src="https://github.com/user-attachments/assets/171793c3-10df-43a1-bf86-8481540afbde" />

* **Most Influential Predictors of Retention**:
  * Linear regression showed:
    * Economic commitment had the strongest positive influence on future intent, followed by habitual and affectivecommitment.
    * Normative only slightly increase purchase intent.
  * To understand the impact of Experiment stimuli on each customer segment, interaction terms were added into the model:
    * **Ad Campaign** works on The Faithful (has low normative commitment), which validated Monk’s proposal. The campaign expects to lift the average. intent of the segment from 4.66 to 4.95 (+0.29), which borderlines the retention threshold of 5.
    * **New Product** shows strong effect on The Faithful (+0.47) and slight effect on Bargain Hunters (+0.14). This is probably due to the high forced in The Faithful, which is interesting because the impact was not large in previous models. The lifted intent of 5.12 passed the threshold.
    * **Subscription Model** attracts Bargain Hunters (+0.45) to 3.82, which makes sense because this group has low economic factor. The subscription would position Crew’s Cup as a good deal for this price-sensitive customer group. However, lifted intent still falls short from retention threshold
<img width="1688" alt="image" src="https://github.com/user-attachments/assets/1037b51d-6089-44f0-9e45-697225355156" />

## Recommendation
<img width="1690" alt="image" src="https://github.com/user-attachments/assets/14d7bc93-e859-41e8-9f83-e1e277c84341" />

## What I Learned
* How to operationalize a psychological model (5-factor commitment) using real data
* The importance of segment-specific strategy in churn reduction
* Combining experimental design, survey analytics, and CRM integration
