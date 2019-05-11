# WUM_projekt2

Dane techniczne do uzgadniania:

- pytongue 3.x

  - sklearn
  - pandas
  - matplotlib.pyplot
  - seaborn
  - ggplot (zamiast matplotlib?)

```python
from pandas import *
from sklearn import *
from matplotlib.pyplot import *
import seaborn as sb
# from ggplot import *

ob = read_excel("./bazy_danych/01A Obserwacje_nowe.xlsx")
po = read_excel("./bazy_danych/01B Po osobach - nowe.xlsx")
```

- jowysz

Linki do nauki:

- seaborn https://s3.amazonaws.com/assets.datacamp.com/blog_assets/Python_Seaborn_Cheat_Sheet.pdf

- pandas https://pandas.pydata.org/Pandas_Cheat_Sheet.pdf

- sklearn https://s3.amazonaws.com/assets.datacamp.com/blog_assets/Scikit_Learn_Cheat_Sheet_Python.pdf

- matplotlib https://s3.amazonaws.com/assets.datacamp.com/blog_assets/Python_Matplotlib_Cheat_Sheet.pdf

- jowysz https://www.cheatography.com/weidadeyue/cheat-sheets/jupyter-notebook/pdf_bw/
  - enter - wejdź w trybu edytowania
  - esc - wyjdź z trybu edytowania
  - A/B - super ważne
  - X - jeszcze ważniejsze
  - M - markdown
  - Y - jak przez przypadek zmieniliśmy na markdown...
  - shift+enter - run cell + select below
  - ctrl+enter - run cell
  - ctrl + shift + - - by podzielić cell
  

Chapter 1 : All the nonsense with learning pytongue all over again

Przydatne funckje:

```python
ob.describe()

```
