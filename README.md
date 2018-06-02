# dotfiles
Generic dotfiles repo

## Steps

* [oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh)
* *~/.emacs* should look like
```
;; (package-initialize)
(load-file "dotfiles/.emacs")
```

* Enable/disable stuff in [.emacs](.emacs)

## Terminal

* tmux just feels like it's in the way
* multi-term is okay but get ugly sometimes (e.g. with SSH)
* Current setup: tilix + screen
  * detach and exit automatically close panes
  * new-screen script automates open pane + attach

## Fix for Emacs/tramp-mode
https://www.emacswiki.org/emacs/TrampMode#toc9
