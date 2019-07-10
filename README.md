```R 
packrat::init(options = list(use.cache = TRUE)) 

git config --global user.email "rene_houtman@hotmail.com"
git config --global user.name "Rene Houtman"
  
git tag -a 0.9 -m "nr profile plot"
git push --tags

```