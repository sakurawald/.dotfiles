# lem editor
cp ~/.lem/init.lisp ./lem/.lem/ 

# emacs
cp ~/.emacs.d/init.el ./emacs/.emacs.d/
cp ~/.emacs.d/early-init.el ./emacs/.emacs.d/

# jetbrains: ideavim plugin
cp ~/.ideavimrc ./ideavim/

# nvim editor
cp ~/.config/nvim/init.vim ./nvim/

# i3 window manager
# cp ~/.i3/config ./i3/.i3/

# roswell launcher
cp ~/.roswell/init.lisp ./roswell/.roswell/

# tmux multiplexer
cp ~/.tmux.conf ./tmux

# spicy
cp ~/.config/spicy/settings ./spicy/

# ranger
cp ~/.config/ranger/rc.conf ./ranger/
cp ~/.config/ranger/rifle.conf ./ranger/

# sioyek
cp ~/.config/sioyek/keys_user.config ./sioyek/

# alacritty
cp ~/.config/alacritty/alacritty.toml ./alacritty/

# swank
cp ~/.swank.lisp ./swank/

# spectacle
# cp ~/.config/spectaclerc ./spectacle/

# git
git add .
git commit -m "sync"
git push

echo "=== Remember to check the result file ==="
echo "=== Remember to check the result file ==="
echo "=== Remember to check the result file ==="
