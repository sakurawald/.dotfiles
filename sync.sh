cp ~/.lem/init.lisp ./lem/ 

cp ~/.ideavimrc ./ideavim/

cp ~/.config/nvim/init.vim ./nvim/

git add .
git commit -m "sync"
git push
