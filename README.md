## Optimal Portfolio Analysis of LQ45 Stocks Using Multi-Objective Method and Portfolio Performance Measurement with the Sharpe Index at the Start and End of the COVID-19 Pandemic

### Objective  
This project aims to optimize stock portfolios by selecting stocks from the LQ45 index and constructing a portfolio that maximizes returns while minimizing risks. The performance of the portfolio is evaluated at both the start and end of the COVID-19 pandemic. The portfolio is optimized using K-Means clustering, followed by multi-objective optimization, and performance is measured using the Sharpe Index to compare the results before and after the pandemic.

### Data  
The study uses LQ45 stock data, representing the 45 most liquid stocks on the Indonesian stock exchange. The data provides insights into stock performances over the course of the pandemic, which is crucial for assessing portfolio dynamics during this period.

### Method  
- **K-Means Clustering**: Applied to select stocks for portfolio optimization based on clustering stocks with similar performance characteristics.
- **Multi-Objective Optimization**: Used to determine the optimal stock weights that balance maximizing returns and minimizing risks.
- **Sharpe Index**: The performance of the optimized portfolio is evaluated using the Sharpe Ratio, which measures the risk-adjusted return of the portfolio at the start and end of the pandemic.

### Results  
- The optimal portfolio with **k = 10** stock selections consisted of the following weights:  
  - MDKA (24.96%), HRUM (15.73%), EMTK (23.28%), UNTR (29.28%), ERAA (1.13%), ARTO (5.61%).
- The expected return of the portfolio was **1.44%**, with a portfolio risk of **0.015%**.
- The Sharpe Index at the start of the pandemic was **0.2308**, while at the end, it decreased to **0.11499**, reflecting a decline in the portfolio's risk-adjusted performance during the pandemic.
