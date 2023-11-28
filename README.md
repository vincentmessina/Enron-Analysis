# Enron Email Analysis Project

This project conducts a network analysis on the Enron email corpus, which contains data on emails exchanged between Enron employees from November 1998 to June 2002. The objective is to uncover communication patterns and changes within the company during its rise and fall, particularly around the time of the scandal that led to its bankruptcy in December 2001.

## Key Components:
- **Data Ingestion:** Emails and employee information are loaded from `enron_emails.csv` and `enron_employees.csv` files.
- **Data Cleaning:** Subsets of the data are created for specific months (September and October 2001), and entries with missing values (`NAs`) are addressed.
- **Network Analysis:** Weight matrices for the email exchange network are constructed to calculate the average degree, and adjacency matrices are used to determine the network density for specified months.
- **Influence Measurement:** The influence of individual employees on the network's average degree is quantified, identifying key personnel and interpreting their roles during the crisis.
- **Visualization:** Raincloud plots are generated to visualize the degree distribution, and influence plots map the impact of each employee on the network's communication dynamics.

## Outcomes:
The analysis aims to provide insights into the Enron communication network's structure before and after the scandal, assessing how internal communication was affected by the unfolding events of the scandal.
