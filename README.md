# Extension Functions for Power BI Desktop and Excel
PBI Desktop (and Excel 2016) Query Functions that you can use to make life easy-peasy. Submit issues or pull requests for functions you'd like to see added. Most functions are self-documenting, and new documentation is being added all the time. 


# Getting Started
## 1. Create a blank query
![New Blank Query](https://cloud.githubusercontent.com/assets/1501159/18407239/dba3ccf4-76bf-11e6-8164-ddcc613a1202.PNG)

## 2. Open the advanced editor
![AdvancedEditor](https://cloud.githubusercontent.com/assets/1501159/18407240/dcfcd4b0-76bf-11e6-894c-eee851d0df89.PNG)

## 3. Paste in the library
#### Option 1: Always get the latest from GitHub - Requires data privacy considersations
Copying the below will give you the latest versions and all times. You can connect with anonymous credentials. Be sure to set the data privacy level appropriately when prompted.
~~~
Expression.Evaluate(Text.FromBinary(Web.Contents("https://raw.githubusercontent.com/tnclark8012/Power-BI-Desktop-Query-Extensions/master/power-query-extensions.m")),#shared)
~~~


#### Option 2: Copy the current version locally - No issues with data privacy
Copy paste the contents from https://raw.githubusercontent.com/tnclark8012/Power-BI-Desktop-Query-Extensions/master/power-query-extensions.m

![AdvancedEditor](https://cloud.githubusercontent.com/assets/1501159/18407241/dfa28b60-76bf-11e6-8a5e-ce0a70063de0.PNG)

## 4. Name the query something short and sweet (like "PBI")
## 5. Click on function names to see documentation!
![Create](https://cloud.githubusercontent.com/assets/1501159/18407244/e3d7df5a-76bf-11e6-8258-bb7f327369e3.PNG)

## 6. Use it by referencing PBI[TheFunctionName]

### Before:
![Create](https://cloud.githubusercontent.com/assets/1501159/18407245/e6d63e5e-76bf-11e6-9413-d3e6b65ca689.PNG)

### After:
![Create](https://cloud.githubusercontent.com/assets/1501159/18407246/e812ba40-76bf-11e6-8c5b-04c20885f647.PNG)

# Contributing
The more the merrier! Be sure to document your function and include a test
