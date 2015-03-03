set -x
# Setup git
git config --global user.email "ryan@yummly.com"
git config --global user.name "Ryan Smith"
git config --global color.ui auto

# Setup emacs
rm ~/.emacs.d.bak -rf
mv -f ~/.emacs.d ~/.emacs.d.bak
git clone https://github.com/tanzoniteblack/dotemacs.git ~/.emacs.d

# Create ~/.lein/profiles.clj
mkdir -p ~/.lein/
sudo chown -R $USER ~/.lein/
ln ~/.emacs.d/profiles.clj ~/.lein/profiles.clj

# Install helper programs (Ubuntu only)
sudo apt-get install -y zsh aspell htop nethogs

# Setup zsh
rm -rf ~/.oh-my-zsh.bak
mv ~/.oh-my-zsh ~/.oh-my-zsh.bak
git clone https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh/
git clone git://github.com/zsh-users/zsh-syntax-highlighting.git ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting/
rm ~/.zshrc
ln ~/.emacs.d/.zshrc ~/.zshrc
sudo chsh $USER -s /bin/zsh

# Setup tmux
rm ~/.tmux.conf
ln ~/.emacs.d/tmux.conf ~/.tmux.conf
