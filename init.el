(load (expand-file-name "crown.el" user-emacs-directory))
(require 'crown)

(crown-init)

(crown-cut-jewel crown-themes
  :url "http://github.com/syl20bnr/crown-themes.git"
  :update nil)
(crown-set-jewel crown-themes)
