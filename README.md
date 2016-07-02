# `ssh-deploy`

The `ssh-deploy` plugin makes it possible to effortlessly sync local files with remote files via SSH. It makes it possible to define remote paths and whether or not you want to explicitly sync on explicit save actions or not. It also enabled manual upload and download of files. *You need to have a setup which allows password-less key-based logins to servers*.

`ssh-deploy` works with `DirectoryVariables` so you can have different sync setups in different ways for different folders.

This application is made by Christian Johansson <christian@cvj.se> 2016 and is licensed under GNU General Public License 3.


## An example

* Download ssh-deploy and place it at `~/.emacs.d/ssh-deploy/`. You need to have `scp` installed locally and enable key-based SSH logins to your remote hosts.

* Create this `DirectoryVariables` file in your project root at `/Users/username/Web/MySite/.dir-locals.el`.

``` elisp
((nil . ((ssh-deploy-root-local . "/Users/username/Web/MySite/")
(ssh-deploy-root-remote . "/Volumes/myserver.com/MySite/")
(ssh-deploy-on-explicity-save . t))))
```
* And add this to your *emacs-init-script*:

``` elisp
;; ssh-deploy - prefix = C-c C-z, u = upload, d = download, x = diff
(add-to-list 'load-path "~/.emacs.d/ssh-deploy/")
(use-package ssh-deploy)
```
* Now when you save a file somewhere under the root `/Users/username/Web/MySite/`, the script will launch and sync the file with the remote server. You can also trigger a manual deploy by pressing `C-c C-z u`.

The above configuration uses the plugin `use-package` which I highly recommend.

## Read more
* <https://www.emacswiki.org/emacs/DirectoryVariables>
* <https://github.com/jwiegley/use-package>
