cp ~/.lem/init.lisp ./lem/ 

cp ~/.ideavimrc ./ideavim/

cp ~/.config/nvim/init.vim ./nvim/

cp ~/.i3/config ./i3/.i3

cp ~/.roswell/init.lisp ./roswell/.roswell/


git add .
git commit -m "sync"
git push
