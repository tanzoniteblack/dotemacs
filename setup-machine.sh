git config --global user.email "ryan@yummly.com"
git config --global user.name "Ryan Smith"
git config --global color.ui auto
rm ~/.emacs.d.bak -rf
mv -f ~/.emacs.d ~/.emacs.d.bak
git clone https://github.com/tanzoniteblack/dotemacs.git ~/.emacs.d
mkdir -p ~/.lein/
sudo chown -R $USER ~/.lein/
cp ~/.emacs.d/profiles.clj > ~/.lein/profiles.clj
rm -rf ~/.oh-my-zsh.bak
mv ~/.oh-my-zsh ~/.oh-my-zsh.bak
git clone https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh/
git clone git://github.com/zsh-users/zsh-syntax-highlighting.git ~/.oh-my-zsh/custom/plugins
cp ~/.emacs.d/.zshrc ~/.zshrc
sudo apt-get install -y zsh
sudo chsh $USER -s /bin/zsh
