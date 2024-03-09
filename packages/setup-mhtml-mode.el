(use-package mhtml-mode
  :config
  (require 'css-completions)
  (add-hook 'mhtml-mode-hook 'cssc/enable-for-html))
