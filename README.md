# mullvadel - A Transient Interface for Mullvad VPN in Emacs.

## Table of contents

- [Installation](#installation)
  - [Manual (Clone)](#manual-clone)
  - [Using straight.el](#straightel)
  - [Doom Emacs](#doom-emacs)
- [License](#license)

## Installation

### Manual (Clone)

Execute the following in your terminal:

``` shell
git clone https://github.com/yelobat/mullvadel.git ~/.emacs.d/mullvadel
```

Add the directory to your Emacs `load-path` and require the package in your init file:

``` emacs-lisp
(add-to-list 'load-path "~/.emacs.d/mullvadel")
(require 'mullvadel)
```

Use `C-c C-e (elisp-eval-region-or-buffer)` or simply restart Emacs for the changes to take effect.

### **`straight.el`**

If you have `straight.el`, simply add the repo via `straight-use-package`:

``` emacs-lisp
(straight-use-package
 '(mullvadel
   :type git
   :host github
   :repo "yelobat/mullvadel"))
(require 'mullvadel)
```

Use `C-c C-e (elisp-eval-region-or-buffer)` or simply restart Emacs for the changes to take effect.

### Doom Emacs

Add the following to your packages.el file:

``` emacs-lisp
(package! mullvadel
  :recipe (:host github :repo "yelobat/mullvadel"))
```

And the following to your config.el:

``` emacs-lisp
(use-package! mullvadel)
```

Run `doom sync` and restart Emacs for the changes to take effect.

## License

This project is released under the `GPL-3.0 license`. See `LICENSE` for more details.
