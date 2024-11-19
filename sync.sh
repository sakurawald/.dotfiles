cp -r ~/.lem ./lem/ 

cp ~/.ideavimrc ./ideavim/

cp ~/.config/nvim/init.vim ./nvim/

git add .
git commit -m "sync"
git push
