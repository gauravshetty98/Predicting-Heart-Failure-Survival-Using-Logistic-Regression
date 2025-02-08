# Predicting Heart Failure Survival

## 📌 Authors
**Gaurav Shetty, Naimish Sharma**

## 📝 Overview
This project was created to predict the chances of survival after heart failure using a **logistic regression model** in **R-Studio**.

## 📊 Dataset
The dataset consists of **299 observations** with **12 independent variables** and a **binary target variable** (**Death_event**), indicating whether a patient survived or not.

### 🔹 Features:
- 🏷 **Age**
- 🩸 **Anaemia** (Boolean)
- 💓 **High blood pressure** (Boolean)
- 🧪 **Creatinine phosphokinase** (Continuous)
- 🍬 **Diabetes** (Boolean)
- 💙 **Ejection Fraction** (Continuous)
- 🚻 **Sex** (Boolean)
- 🧬 **Platelets** (Continuous)
- 🏥 **Serum Creatinine** (Continuous)
- ⚛ **Serum Sodium** (Continuous)
- 🚬 **Smoking** (Boolean)
- ⏳ **Time** (Follow-up duration)

There were **no missing values** in the dataset, and **data exploration** was conducted to understand feature relationships.

## 📈 Data Exploration
- 📌 **Correlation Matrix:** Features like age and serum creatinine were positively correlated with mortality, whereas ejection fraction and serum sodium had negative correlations.
- 📊 **Boxplots & Data Distribution:** Used to analyze the spread of features across different survival outcomes.

## 🏗 Model Building and Evaluation
### 🔹 Full Model
- The initial model included **all features** to predict **Death_event**.
- **Null Deviance:** `375.35` (298 degrees of freedom)
- **Residual Deviance:** `219.55` (286 degrees of freedom)
- **AIC Score:** `245.55`

### 🔹 Feature Selection
Using **p-values (<0.05)**, the most significant features were:
- **Age**
- **Ejection Fraction**
- **Serum Creatinine**
- **Time** (later excluded for fairness)

### 🔹 Final Model (Without Time Feature)
- **Residual Deviance:** `294.48`
- **AIC Score:** `318.28`

## 🏆 Model Selection
We implemented:
- ✅ **Stepwise Selection**
- 🔼 **Forward Selection**
- 🔽 **Backward Selection**

### **Final Selected Model Features:**
- **Age**
- **Anaemia**
- **Creatinine**
- **Ejection Fraction**
- **Serum Creatinine**
- **Blood Pressure**
- **Serum Sodium**

## 📊 Model Performance
📌 **Confusion Matrix Evaluation:**
- 🎯 **Accuracy:** `75.9%`
- 🔍 **Recall:** `89.6%`
- ✅ **Precision:** `78.1%`
- 📊 **F1 Score:** `41.7%`

Since survival prediction is critical, **recall** was prioritized.

## 📊 Inference About Model Parameters
Using **Odds Ratio Analysis**:
- 📌 **Every 1-year increase in Age** → `5.4% increase` in mortality odds.
- 💙 **Every 1 unit increase in Ejection Fraction** → `6.56% decrease` in mortality odds.
- 🏥 **Every 1 unit increase in Serum Creatinine** → `93.8% increase` in mortality odds.
- ⚛ **Every 1 unit increase in Serum Sodium** → `5.52% decrease` in mortality odds.
- 🔴 **Anaemia & High Blood Pressure** showed strong association with mortality.

## 🚀 Future Work
- Implementing **Random Forest**, **K-Means Clustering**, and other classification techniques.
- Exploring **Feature Engineering** to improve model robustness.
- Increasing dataset size for improved generalization.

## ✅ Conclusion
We successfully built a model that predicts **patient survival after heart failure** with high recall. The key features influencing survival were **Age, Ejection Fraction, Serum Creatinine, Serum Sodium, Anaemia, and High Blood Pressure**. Further optimization and alternative models can enhance predictive accuracy.

---
## 📂 Repository Structure
This project was created to predict the chances of survival after heart failure using a **logistic regression model** in **R-Studio**. Below are the relevant files:

```
📂 Project Folder
 ├── 📄 Logistic Regression Model.R    # Model creation and evaluation code
 ├── 📄 Patient_details.csv            # Dataset
 ├── 📄 Observation and findings.pptx  # Project details (observations and findings)
```

For more details, refer to the **PowerPoint presentation**.

📧 _For any questions, feel free to reach out!_ 🚀

